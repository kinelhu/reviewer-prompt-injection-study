"""Model interface for querying LLM providers (Ollama, Anthropic)."""

import os
import time
import logging
from typing import Optional

import requests
import json

try:
    from dotenv import load_dotenv
    load_dotenv()
except ImportError:
    pass


class OllamaModel:
    """Interface for local Ollama models."""

    def __init__(self, model_string: str, endpoint: str = "http://localhost:11434",
                 temperature: float = 0.7, max_tokens: int = 2000,
                 think: Optional[bool] = None):
        self.model_string = model_string
        self.endpoint = endpoint
        self.temperature = temperature
        self.max_tokens = max_tokens
        # None = Ollama default (preserves the behavior of the original runs);
        # False pins thinking off for hybrid thinking models (R1: qwen3.8).
        self.think = think
        self.logger = logging.getLogger(__name__)

    def query(self, user_prompt: str, system_prompt: Optional[str] = None) -> tuple[str, dict]:
        """
        Query the model. Returns (response_text, usage_dict).
        usage_dict keys: prompt_tokens, completion_tokens (best-effort from Ollama).
        """
        url = f"{self.endpoint}/api/chat"

        messages = []
        if system_prompt:
            messages.append({"role": "system", "content": system_prompt})
        messages.append({"role": "user", "content": user_prompt})

        payload = {
            "model": self.model_string,
            "messages": messages,
            "stream": False,
            "options": {
                "temperature": self.temperature,
                "num_predict": self.max_tokens,
            },
        }
        if self.think is not None:
            payload["think"] = self.think

        try:
            response = requests.post(url, json=payload, timeout=300)
            response.raise_for_status()
            result = response.json()
            text = result["message"]["content"]
            usage = {
                "prompt_tokens": result.get("prompt_eval_count"),
                "completion_tokens": result.get("eval_count"),
            }
            return text, usage
        except requests.exceptions.RequestException as e:
            self.logger.error(f"Ollama error: {e}")
            raise
        except (KeyError, json.JSONDecodeError) as e:
            self.logger.error(f"Failed to parse Ollama response: {e}")
            raise


class AnthropicModel:
    """Interface for Anthropic Claude API."""

    def __init__(self, model_string: str, api_key: Optional[str] = None,
                 temperature: float = 0.7, max_tokens: int = 2000,
                 max_retries: int = 5, retry_delay: float = 2.0):
        self.model_string = model_string
        self.temperature = temperature
        self.max_tokens = max_tokens
        self.max_retries = max_retries
        self.retry_delay = retry_delay
        self.logger = logging.getLogger(__name__)
        self.api_key = api_key or os.environ.get("ANTHROPIC_API_KEY")
        if not self.api_key:
            raise ValueError("ANTHROPIC_API_KEY not set.")

    def query(self, user_prompt: str, system_prompt: Optional[str] = None) -> tuple[str, dict]:
        """Returns (response_text, usage_dict)."""
        url = "https://api.anthropic.com/v1/messages"
        headers = {
            "Content-Type": "application/json",
            "x-api-key": self.api_key,
            "anthropic-version": "2023-06-01",
        }
        payload: dict = {
            "model": self.model_string,
            "messages": [{"role": "user", "content": user_prompt}],
            "temperature": self.temperature,
            "max_tokens": self.max_tokens,
        }
        if system_prompt:
            payload["system"] = system_prompt

        for attempt in range(1, self.max_retries + 1):
            try:
                resp = requests.post(url, headers=headers, json=payload, timeout=120)

                if resp.status_code in (429,) or resp.status_code >= 500:
                    wait = float(resp.headers.get("Retry-After", self.retry_delay * attempt))
                    self.logger.warning(f"Anthropic HTTP {resp.status_code}, retry {attempt}/{self.max_retries} in {wait:.1f}s")
                    if attempt == self.max_retries:
                        resp.raise_for_status()
                    time.sleep(wait)
                    continue

                resp.raise_for_status()
                result = resp.json()
                text = result["content"][0]["text"]
                usage_raw = result.get("usage", {})
                usage = {
                    "prompt_tokens": usage_raw.get("input_tokens"),
                    "completion_tokens": usage_raw.get("output_tokens"),
                }
                return text, usage

            except requests.exceptions.ConnectionError as e:
                wait = self.retry_delay * attempt
                self.logger.warning(f"Anthropic connection error attempt {attempt}: {e}. Retry in {wait:.1f}s")
                if attempt == self.max_retries:
                    raise
                time.sleep(wait)
            except requests.exceptions.Timeout:
                wait = self.retry_delay * attempt
                self.logger.warning(f"Anthropic timeout attempt {attempt}. Retry in {wait:.1f}s")
                if attempt == self.max_retries:
                    raise
                time.sleep(wait)
            except (KeyError, json.JSONDecodeError) as e:
                self.logger.error(f"Failed to parse Anthropic response: {e}")
                raise


class OpenAIModel:
    """Interface for OpenAI-compatible APIs (OpenAI, OpenRouter, etc.)."""

    def __init__(self, model_string: str, api_key: Optional[str] = None,
                 base_url: Optional[str] = None,
                 temperature: Optional[float] = None,
                 reasoning_effort: Optional[str] = None,
                 max_tokens: int = 4000,
                 max_retries: int = 5, retry_delay: float = 2.0,
                 request_delay: float = 0.0):
        self.model_string = model_string
        self.temperature = temperature
        self.reasoning_effort = reasoning_effort
        self.max_tokens = max_tokens
        self.max_retries = max_retries
        self.retry_delay = retry_delay
        self.request_delay = request_delay
        self.logger = logging.getLogger(__name__)

        resolved_key = api_key or os.environ.get("OPENAI_API_KEY")
        if not resolved_key:
            raise ValueError("No API key provided (set OPENAI_API_KEY or pass api_key).")

        try:
            from openai import OpenAI, RateLimitError, APIStatusError
            self._RateLimitError = RateLimitError
            self._APIStatusError = APIStatusError
            kwargs: dict = {"api_key": resolved_key, "max_retries": 0}
            if base_url:
                kwargs["base_url"] = base_url
            self.client = OpenAI(**kwargs)
        except ImportError:
            raise ImportError("openai package required. Run: uv add openai")

    def query(self, user_prompt: str, system_prompt: Optional[str] = None) -> tuple[str, dict]:
        """Returns (response_text, usage_dict)."""
        if self.request_delay > 0:
            time.sleep(self.request_delay)

        messages = []
        if system_prompt:
            messages.append({"role": "system", "content": system_prompt})
        messages.append({"role": "user", "content": user_prompt})

        # o-series models: reasoning_effort + max_completion_tokens, no temperature
        kwargs: dict = {"model": self.model_string, "messages": messages}
        if self.reasoning_effort is not None:
            kwargs["reasoning_effort"] = self.reasoning_effort
            kwargs["max_completion_tokens"] = self.max_tokens
        else:
            kwargs["max_tokens"] = self.max_tokens
            if self.temperature is not None:
                kwargs["temperature"] = self.temperature

        for attempt in range(1, self.max_retries + 1):
            try:
                response = self.client.chat.completions.create(**kwargs)
                text = response.choices[0].message.content
                usage = {
                    "prompt_tokens": response.usage.prompt_tokens,
                    "completion_tokens": response.usage.completion_tokens,
                }
                return text, usage
            except self._RateLimitError as e:
                wait = self.retry_delay * attempt
                self.logger.warning(f"OpenAI rate limit attempt {attempt}/{self.max_retries}, retry in {wait:.1f}s: {e}")
                if attempt == self.max_retries:
                    raise
                time.sleep(wait)
            except self._APIStatusError as e:
                if e.status_code >= 500:
                    wait = self.retry_delay * attempt
                    self.logger.warning(f"OpenAI server error {e.status_code} attempt {attempt}/{self.max_retries}, retry in {wait:.1f}s")
                    if attempt == self.max_retries:
                        raise
                    time.sleep(wait)
                else:
                    raise


class ModelFactory:
    """Creates model instances from config dicts."""

    @staticmethod
    def create(config: dict):
        provider = config["provider"]
        if provider == "ollama":
            return OllamaModel(
                model_string=config["model_string"],
                endpoint=config.get("endpoint", "http://localhost:11434"),
                temperature=config.get("temperature", 0.7),
                max_tokens=config.get("max_tokens", 2000),
                think=config.get("think"),
            )
        elif provider == "anthropic":
            return AnthropicModel(
                model_string=config["model_string"],
                temperature=config.get("temperature", 0.7),
                max_tokens=config.get("max_tokens", 2000),
                max_retries=config.get("max_retries", 5),
                retry_delay=config.get("retry_delay", 2.0),
            )
        elif provider == "openai":
            return OpenAIModel(
                model_string=config["model_string"],
                temperature=config.get("temperature"),
                reasoning_effort=config.get("reasoning_effort"),
                max_tokens=config.get("max_tokens", 4000),
                max_retries=config.get("max_retries", 5),
                retry_delay=config.get("retry_delay", 2.0),
                request_delay=config.get("request_delay", 0.0),
            )
        elif provider == "openrouter":
            api_key = os.environ.get("OPENROUTER_API_KEY")
            if not api_key:
                raise ValueError("OPENROUTER_API_KEY not set in environment.")
            return OpenAIModel(
                model_string=config["model_string"],
                api_key=api_key,
                base_url="https://openrouter.ai/api/v1",
                temperature=config.get("temperature", 0.0),
                max_tokens=config.get("max_tokens", 4000),
                max_retries=config.get("max_retries", 8),
                retry_delay=config.get("retry_delay", 15.0),
                request_delay=config.get("request_delay", 4.0),  # ~15 req/min
            )
        else:
            raise ValueError(f"Unsupported provider: {provider}")
