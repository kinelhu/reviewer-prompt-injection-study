# Guard against re-loading if dat is already in scope (e.g. sourced from report.Rmd)
if (!exists("dat")) source(here::here("analysis", "load_data.R"))
library(ggplot2)
library(patchwork)

theme_set(
  theme_bw(base_size = 11) +
  theme(
    strip.background  = element_rect(fill = "#E8F4F5", color = "#0D7377"),
    strip.text        = element_text(face = "bold", color = "#0D7377"),
    panel.border      = element_rect(color = "#E0E0E0"),
    axis.text         = element_text(color = "#1A1A1A"),
    plot.title        = element_text(face = "bold", color = "#0D7377", size = 12),
    plot.subtitle     = element_text(color = "#6B6B6B", size = 10),
    legend.position   = "bottom",
    legend.title      = element_text(face = "bold", size = 9),
    legend.text       = element_text(size = 9)
  )
)

REC_COLORS <- c(
  "REJECT"         = "#C75450",
  "MAJOR_REVISION" = "#fc8d59",
  "MINOR_REVISION" = "#fee090",
  "ACCEPT"         = "#2D6A4F"
)

dir.create(here::here("figures"), showWarnings = FALSE)

baseline_mean <- mean(dat$recommendation_ordinal[dat$direction == "baseline"], na.rm = TRUE)

# ── Figure 1: Per-paper recommendation tile chart ───────────────────────────────
# With a binary effective outcome (MAJOR vs MINOR), stacked proportional bars
# are uninformative. Show individual paper × condition cells instead.
tile_dat <- dat |>
  filter(!is.na(recommendation)) |>
  mutate(
    prompt_label   = factor(
      ifelse(prompt_style == "standard", "Standard prompt", "Injection-aware prompt"),
      levels = c("Standard prompt", "Injection-aware prompt")),
    condition_label = case_when(
      direction == "baseline"                  ~ "Baseline",
      placement  == "beginning"                ~ "Beginning",
      placement  == "middle"                   ~ "Middle",
      placement  == "end"                      ~ "End"
    ),
    condition_label = factor(condition_label,
      levels = c("Baseline", "Beginning", "Middle", "End"))
  )

p1 <- tile_dat |>
  ggplot(aes(x = condition_label, y = factor(paper_id), fill = recommendation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = substr(recommendation, 1, 2)), size = 2.8,
            color = "#1A1A1A", fontface = "bold") +
  facet_wrap(~prompt_label) +
  scale_fill_manual(
    values = REC_COLORS,
    breaks = c("REJECT", "MAJOR_REVISION", "MINOR_REVISION", "ACCEPT"),
    labels = c("Reject", "Major revision", "Minor revision", "Accept")
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Per-paper recommendations by injection condition and prompt style",
    subtitle = "Each cell = one paper. MA = Major revision, MI = Minor revision.",
    x = NULL, y = "Paper ID", fill = "Recommendation"
  ) +
  theme(
    axis.text.y       = element_text(size = 8),
    panel.grid        = element_blank()
  )

ggsave(here::here("figures", "F1_recommendation_tiles.pdf"), p1, width = 8, height = 5)

# ── Figure 2: Ordinal shift by placement × prompt_style (dot + range) ───────────
# With a single injection length, the heatmap had a degenerate y axis.
# Replace with a dot plot: placement on x, delta on y, prompt_style as color.
shift_dat <- dat |>
  filter(direction == "favorable") |>
  mutate(prompt_label = factor(
    ifelse(prompt_style == "standard", "Standard", "Aware"),
    levels = c("Standard", "Aware"))) |>
  group_by(prompt_label, placement) |>
  summarise(
    mean_delta = mean(recommendation_ordinal, na.rm = TRUE) - baseline_mean,
    se_delta   = sd(recommendation_ordinal, na.rm = TRUE) / sqrt(n()),
    n          = n(),
    .groups    = "drop"
  ) |>
  mutate(placement = factor(placement, levels = c("beginning", "middle", "end"),
                            labels = c("Beginning", "Middle", "End")))

p2 <- shift_dat |>
  ggplot(aes(x = placement, y = mean_delta, color = prompt_label, group = prompt_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#AAAAAA") +
  geom_line(linewidth = 0.8, alpha = 0.5) +
  geom_pointrange(
    aes(ymin = mean_delta - se_delta, ymax = mean_delta + se_delta),
    size = 0.7, linewidth = 1.1,
    position = position_dodge(width = 0.2)
  ) +
  geom_text(
    aes(label = sprintf("%+.2f", mean_delta)),
    vjust = -1.1, size = 3.2, show.legend = FALSE,
    position = position_dodge(width = 0.2)
  ) +
  scale_color_manual(values = c("Standard" = "#0D7377", "Aware" = "#C75450")) +
  scale_y_continuous(limits = c(-2.2, 2.2), breaks = seq(-2, 2, 1)) +
  labs(
    title    = "Ordinal shift from baseline by injection placement and prompt style",
    subtitle = "Points = mean delta vs baseline; bars = +/- 1 SE. Dashed line = no shift.",
    x = "Injection placement", y = "Delta ordinal (vs baseline)", color = "Prompt style"
  )

ggsave(here::here("figures", "F2_ordinal_shift.pdf"), p2, width = 7, height = 4)

# ── Figure 3: Detection rate by placement × prompt_style ────────────────────────
# Single injection length: remove dodge, show clean dot chart.
detection_dat <- dat |>
  filter(direction == "favorable") |>
  mutate(prompt_label = factor(
    ifelse(prompt_style == "standard", "Standard", "Aware"),
    levels = c("Standard", "Aware"))) |>
  group_by(prompt_label, placement) |>
  summarise(
    detected = sum(detected_injection, na.rm = TRUE),
    n        = n(),
    rate     = detected / n,
    .groups  = "drop"
  ) |>
  mutate(placement = factor(placement, levels = c("beginning", "middle", "end"),
                            labels = c("Beginning", "Middle", "End")))

p3 <- detection_dat |>
  ggplot(aes(x = placement, y = rate, color = prompt_label, group = prompt_label)) +
  geom_line(linewidth = 0.8, alpha = 0.5) +
  geom_point(size = 4) +
  geom_text(
    aes(label = paste0(detected, "/", n)),
    vjust = -1.2, size = 3.2, show.legend = FALSE
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-0.05, 1.15)) +
  scale_color_manual(values = c("Standard" = "#0D7377", "Aware" = "#C75450")) +
  labs(
    title    = "Injection detection rate by placement and prompt style",
    subtitle = "Detection = reviewer explicitly flagged embedded instructions. Labels = n detected / n reviews.",
    x = "Injection placement", y = "Detection rate", color = "Prompt style"
  )

ggsave(here::here("figures", "F3_detection_rate.pdf"), p3, width = 7, height = 4)

# ── Figure 4: Secondary outcomes by direction × prompt_style ───────────────────
# Boxplots + jitter for small n. Shows individual data points.
secondary_long <- dat |>
  filter(!is.na(sentiment_score)) |>
  mutate(
    prompt_label = factor(
      ifelse(prompt_style == "standard", "Standard", "Aware"),
      levels = c("Standard", "Aware")),
    dir_label    = ifelse(direction == "baseline", "Baseline", "Favorable"),
    group_label  = paste(prompt_label, dir_label)
  ) |>
  tidyr::pivot_longer(
    cols      = c(sentiment_score, critique_count, strength_count),
    names_to  = "metric",
    values_to = "value"
  ) |>
  filter(!is.na(value)) |>
  mutate(metric = factor(metric,
    levels = c("sentiment_score", "critique_count", "strength_count"),
    labels = c("Sentiment (0-10)", "Critiques (n)", "Strengths (n)")
  ))

p4 <- secondary_long |>
  ggplot(aes(x = dir_label, y = value, fill = prompt_label)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.5,
               position = position_dodge(0.6)) +
  geom_jitter(
    aes(color = prompt_label),
    position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.6),
    size = 1.8, alpha = 0.7
  ) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values  = c("Standard" = "#0D7377", "Aware" = "#C75450")) +
  scale_color_manual(values = c("Standard" = "#0D7377", "Aware" = "#C75450")) +
  guides(color = "none") +
  labs(
    title    = "Secondary outcomes by injection direction and prompt style",
    subtitle = "Sentiment = review tone (0 harsh to 10 enthusiastic). Points = individual reviews.",
    x = NULL, y = NULL, fill = "Prompt style"
  )

ggsave(here::here("figures", "F4_secondary_outcomes.pdf"), p4, width = 9, height = 4)

# ── Alternative Figure 1 variants (for scale ≥ 30 papers) ───────────────────────
# The tile chart (p1) is readable up to ~20 papers. These three alternatives
# each scale to 100+ without loss of clarity.

# Paired dataset: join each paper's baseline to its injection conditions
paired_dat <- dat |>
  filter(!is.na(recommendation_ordinal)) |>
  mutate(prompt_label = factor(
    ifelse(prompt_style == "standard", "Standard prompt", "Injection-aware prompt"),
    levels = c("Standard prompt", "Injection-aware prompt")))

baseline_vals <- paired_dat |>
  filter(direction == "baseline") |>
  select(paper_id, prompt_label, baseline_ordinal = recommendation_ordinal,
         baseline_rec = recommendation)

injection_vals <- paired_dat |>
  filter(direction == "favorable") |>
  select(paper_id, prompt_label, placement, injection_ordinal = recommendation_ordinal,
         injection_rec = recommendation)

paired <- inner_join(injection_vals, baseline_vals, by = c("paper_id", "prompt_label")) |>
  mutate(
    shift       = injection_ordinal - baseline_ordinal,
    shift_label = case_when(
      shift > 0  ~ "Shifted up",
      shift == 0 ~ "No change",
      shift < 0  ~ "Shifted down"
    ),
    shift_label = factor(shift_label, levels = c("Shifted up", "No change", "Shifted down")),
    placement   = factor(placement, levels = c("beginning", "middle", "end"),
                         labels = c("Beginning", "Middle", "End"))
  )

# ── Alt 1: Alluvial / transition diagram ────────────────────────────────────────
# Shows recommendation flow: baseline → injection. At scale this is the clearest
# way to see the conversion rate (e.g. 80% of MAJOR → MINOR under injection).
# Requires ggalluvial.
if (!requireNamespace("ggalluvial", quietly = TRUE)) {
  message("Install ggalluvial for Alt 1: install.packages('ggalluvial')")
} else {
  library(ggalluvial)

  alluvial_dat <- paired |>
    count(prompt_label, baseline_rec, injection_rec) |>
    mutate(
      baseline_rec  = factor(baseline_rec,
        levels = c("REJECT", "MAJOR_REVISION", "MINOR_REVISION", "ACCEPT")),
      injection_rec = factor(injection_rec,
        levels = c("REJECT", "MAJOR_REVISION", "MINOR_REVISION", "ACCEPT"))
    )

  p1_alluvial <- alluvial_dat |>
    ggplot(aes(axis1 = baseline_rec, axis2 = injection_rec, y = n)) +
    geom_alluvium(aes(fill = baseline_rec), alpha = 0.8, width = 0.3) +
    geom_stratum(width = 0.3, fill = "white", color = "#555555") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    facet_wrap(~prompt_label) +
    scale_fill_manual(values = REC_COLORS, guide = "none") +
    scale_x_discrete(limits = c("Baseline", "Injection"), expand = c(0.15, 0.05)) +
    labs(
      title    = "Recommendation transitions: baseline to injection",
      subtitle = "Flow width proportional to number of papers. Scaling to any n.",
      x = NULL, y = "Number of reviews"
    )

  ggsave(here::here("figures", "F1_alt_alluvial.pdf"), p1_alluvial, width = 8, height = 5)
}

# ── Alt 2: Paired slope chart ───────────────────────────────────────────────────
# One line per paper per condition, connecting baseline ordinal to injection
# ordinal. Color encodes direction of shift. Legible up to ~50 papers; at 100
# use alpha = 0.3. Facet by prompt_style; panels for each placement.
slope_long <- paired |>
  tidyr::pivot_longer(
    cols      = c(baseline_ordinal, injection_ordinal),
    names_to  = "stage",
    values_to = "ordinal"
  ) |>
  mutate(stage = factor(stage,
    levels = c("baseline_ordinal", "injection_ordinal"),
    labels = c("Baseline", "Injection")))

p1_slope <- slope_long |>
  ggplot(aes(x = stage, y = ordinal,
             group = interaction(paper_id, placement),
             color = shift_label)) +
  geom_line(alpha = 0.6, linewidth = 0.9) +
  geom_point(size = 2, alpha = 0.8) +
  facet_grid(prompt_label ~ placement) +
  scale_color_manual(values = c(
    "Shifted up"   = "#2D6A4F",
    "No change"    = "#AAAAAA",
    "Shifted down" = "#C75450"
  )) +
  scale_y_continuous(
    breaks = 1:4,
    labels = c("Reject", "Major", "Minor", "Accept"),
    limits = c(0.8, 4.2)
  ) +
  labs(
    title    = "Per-paper ordinal shift: baseline to injection",
    subtitle = "Each line = one paper. Green = favorable shift; grey = no change; red = unfavorable.",
    x = NULL, y = "Recommendation", color = "Shift direction"
  ) +
  theme(axis.text.x = element_text(size = 9))

ggsave(here::here("figures", "F1_alt_slope.pdf"), p1_slope, width = 9, height = 5.5)

# ── Alt 3: Proportion shift bar chart ───────────────────────────────────────────
# Most compact and journal-friendly. Shows % of reviews at MINOR or above
# (i.e., % shifted favorably) per condition. Scales to any n; loses individual
# paper information but maximises clarity for a clinical audience.
prop_dat <- bind_rows(
  # Baseline: proportion per prompt_style
  dat |>
    filter(direction == "baseline", !is.na(recommendation_ordinal)) |>
    mutate(prompt_label = factor(
      ifelse(prompt_style == "standard", "Standard prompt", "Injection-aware prompt"),
      levels = c("Standard prompt", "Injection-aware prompt")),
      condition_label = "Baseline",
      condition_label = factor(condition_label,
        levels = c("Baseline", "Beginning", "Middle", "End"))) |>
    group_by(prompt_label, condition_label) |>
    summarise(pct_minor_or_better = mean(recommendation_ordinal >= 3, na.rm = TRUE),
              n = n(), .groups = "drop"),
  # Injection: proportion per placement × prompt_style
  dat |>
    filter(direction == "favorable", !is.na(recommendation_ordinal)) |>
    mutate(prompt_label = factor(
      ifelse(prompt_style == "standard", "Standard prompt", "Injection-aware prompt"),
      levels = c("Standard prompt", "Injection-aware prompt")),
      condition_label = factor(
        case_when(placement == "beginning" ~ "Beginning",
                  placement == "middle"    ~ "Middle",
                  placement == "end"       ~ "End"),
        levels = c("Baseline", "Beginning", "Middle", "End"))) |>
    group_by(prompt_label, condition_label) |>
    summarise(pct_minor_or_better = mean(recommendation_ordinal >= 3, na.rm = TRUE),
              n = n(), .groups = "drop")
)

p1_prop <- prop_dat |>
  ggplot(aes(x = condition_label, y = pct_minor_or_better, fill = condition_label == "Baseline")) +
  geom_col(width = 0.65) +
  geom_text(aes(label = scales::percent(pct_minor_or_better, accuracy = 1)),
            vjust = -0.4, size = 3.2) +
  facet_wrap(~prompt_label) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.12)) +
  scale_fill_manual(values = c("TRUE" = "#AAAAAA", "FALSE" = "#0D7377"), guide = "none") +
  labs(
    title    = "Proportion of reviews at Minor Revision or above",
    subtitle = "Grey bar = baseline (no injection). Teal = injection present.",
    x = NULL, y = "% Minor revision or better"
  )

ggsave(here::here("figures", "F1_alt_proportion.pdf"), p1_prop, width = 8, height = 4.5)

message("Figures saved to figures/")
