"""Text injection logic: inserts adversarial strings into paper text."""


def _insert_at_beginning(paper_text: str, injection: str) -> str:
    return f"{injection}\n\n{paper_text}"


def _insert_at_middle(paper_text: str, injection: str) -> str:
    words = paper_text.split()
    mid = len(words) // 2
    before = " ".join(words[:mid])
    after = " ".join(words[mid:])
    return f"{before}\n\n{injection}\n\n{after}"


def _insert_at_end(paper_text: str, injection: str) -> str:
    return f"{paper_text}\n\n{injection}"


class Injector:
    """
    Inserts injection text into paper content according to condition parameters.

    Args:
        templates: Nested dict loaded from config['injection_templates'].
                   Access path: templates[direction][length]
    """

    def __init__(self, templates: dict):
        self.templates = templates

    def inject(
        self,
        paper_text: str,
        direction: str,
        placement: str,
        length: str,
    ) -> tuple[str, str]:
        """
        Build the injected paper and return (injected_paper_text, injected_string_used).

        For baseline conditions (direction='baseline'), returns the paper unchanged.
        """
        if direction == "baseline":
            return paper_text, ""

        injection = self.templates[direction][length]

        if placement == "beginning":
            injected_paper = _insert_at_beginning(paper_text, injection)
        elif placement == "middle":
            injected_paper = _insert_at_middle(paper_text, injection)
        elif placement == "end":
            injected_paper = _insert_at_end(paper_text, injection)
        else:
            raise ValueError(f"Unknown placement: {placement}")

        return injected_paper, injection
