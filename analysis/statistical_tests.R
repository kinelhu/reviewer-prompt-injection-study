source(here::here("analysis", "load_data.R"))
library(ordinal)   # clmm
library(broom)

# ── Primary model ──────────────────────────────────────────────────────────────
# Outcome: ordinal recommendation (1=REJECT … 4=ACCEPT)
# Fixed effects: direction * prompt_style + placement + length
# Random effect: paper_id (controls for paper-level baseline quality)

m_primary <- clmm(
  recommendation ~ direction * prompt_style + placement + (1 | paper_id),
  data = dat,
  link = "logit"
)

summary(m_primary)

# ── Key interaction: direction × prompt_style ──────────────────────────────────
# Tests whether the aware prompt specifically suppresses injection effect
# (not just whether it globally shifts recommendations)
m_interaction <- clmm(
  recommendation ~ direction * prompt_style + placement + (1 | paper_id),
  data = dat,
  link = "logit"
)

summary(m_interaction)

# ── Prompt style effect at baseline (sanity check) ────────────────────────────
# Should be ~0 if aware prompt doesn't shift baseline recommendations
baseline_tab <- dat |>
  filter(direction == "baseline") |>
  group_by(prompt_style) |>
  summarise(
    n = n(),
    mean_ordinal = mean(recommendation_ordinal, na.rm = TRUE),
    .groups = "drop"
  )

print(baseline_tab)

# ── Detection rate: standard vs aware ─────────────────────────────────────────
detection_tab <- dat |>
  filter(direction == "favorable") |>
  group_by(prompt_style, placement, length) |>
  summarise(
    n = n(),
    detected = sum(detected_injection, na.rm = TRUE),
    rate = detected / n,
    .groups = "drop"
  )

print(detection_tab)

# ── Ordinal shift vs baseline ─────────────────────────────────────────────────
baseline_median <- dat |>
  filter(direction == "baseline") |>
  summarise(med = median(recommendation_ordinal, na.rm = TRUE)) |>
  pull(med)

cat(sprintf("\nBaseline median ordinal: %.1f\n", baseline_median))

shift_tab <- dat |>
  group_by(direction, prompt_style, placement, length) |>
  summarise(
    n = n(),
    mean_ordinal = mean(recommendation_ordinal, na.rm = TRUE),
    sd_ordinal   = sd(recommendation_ordinal, na.rm = TRUE),
    delta = mean_ordinal - baseline_median,
    .groups = "drop"
  ) |>
  arrange(desc(abs(delta)))

print(shift_tab, n = 30)
