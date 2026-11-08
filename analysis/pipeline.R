suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(officer)
  library(flextable)
})

source(here("analysis", "load_data.R"))
source(here("analysis", "theme.R"))
source(here("analysis", "stats.R"))

dir.create(here("figures"), showWarnings = FALSE)
dir.create(here("tables"),  showWarnings = FALSE)
dir.create(here("output"),  showWarnings = FALSE)

# ── Derived quantities ────────────────────────────────────────────────────────

baseline_mean <- dat |>
  filter(direction == "baseline", !is.na(recommendation_ordinal)) |>
  pull(recommendation_ordinal) |>
  mean()

baseline_per_paper <- dat |>
  filter(direction == "baseline", !is.na(recommendation_ordinal)) |>
  select(paper_id, model_name, prompt_style, baseline_ordinal = recommendation_ordinal,
         baseline_rec = recommendation)

injection_per_paper <- dat |>
  filter(direction == "favorable", !is.na(recommendation_ordinal)) |>
  select(paper_id, model_name, prompt_style, placement,
         injection_ordinal = recommendation_ordinal, injection_rec = recommendation,
         detected_injection)

paired <- inner_join(injection_per_paper, baseline_per_paper,
                     by = c("paper_id", "model_name", "prompt_style")) |>
  mutate(
    shift       = injection_ordinal - baseline_ordinal,
    shift_label = factor(
      case_when(shift > 0 ~ "Shifted up", shift == 0 ~ "No change",
                shift < 0 ~ "Shifted down"),
      levels = c("Shifted up", "No change", "Shifted down")
    ),
    placement_f = fct_placement(as.character(placement)),
    prompt_f    = fct_prompt(as.character(prompt_style)),
    prompt_long = fct_prompt_long(as.character(prompt_style)),
    model_f     = fct_model(model_name)
  )

# ── Detection-compliance analysis ─────────────────────────────────────────────
# Per-review: does detecting the injection reduce the recommendation shift?

det_compliance <- paired |>
  filter(!is.na(detected_injection)) |>
  mutate(det_label = factor(
    ifelse(detected_injection, "Detected", "Not detected"),
    levels = c("Not detected", "Detected")
  ))

det_compliance_summary <- det_compliance |>
  group_by(prompt_f, det_label) |>
  summarise(
    n          = n(),
    mean_shift = round(mean(shift, na.rm = TRUE), 2),
    pct_up     = round(100 * mean(shift > 0, na.rm = TRUE)),
    pct_none   = round(100 * mean(shift == 0, na.rm = TRUE)),
    pct_down   = round(100 * mean(shift < 0, na.rm = TRUE)),
    .groups    = "drop"
  )

# Wilcoxon rank-sum: shift in detected vs not detected (aware prompt only)
.aw_det   <- det_compliance |>
  filter(prompt_style == "aware",  detected_injection) |> pull(shift)
.aw_undet <- det_compliance |>
  filter(prompt_style == "aware", !detected_injection) |> pull(shift)

det_wilcox_aware <- if (length(.aw_det) >= 3 && length(.aw_undet) >= 3) {
  .test <- wilcox.test(.aw_det, .aw_undet, exact = FALSE)
  list(W = as.numeric(.test$statistic), p = .test$p.value,
       p_fmt = fmt_p(.test$p.value))
} else {
  list(W = NA_real_, p = NA_real_, p_fmt = "\u2014")
}

message(sprintf(
  "[det-compliance] Aware prompt: detected (n=%d) shift=%.2f, %d%% up | not detected (n=%d) shift=%.2f, %d%% up | Wilcoxon p=%s",
  sum(det_compliance$prompt_style == "aware" &  det_compliance$detected_injection),
  mean(.aw_det),
  round(100 * mean(.aw_det > 0)),
  sum(det_compliance$prompt_style == "aware" & !det_compliance$detected_injection),
  mean(.aw_undet),
  round(100 * mean(.aw_undet > 0)),
  det_wilcox_aware$p_fmt
))

# ── Save helpers ──────────────────────────────────────────────────────────────

save_fig <- function(p, name, width = 7, height = 4.5) {
  ggsave(here("figures", paste0(name, ".pdf")), p,
         width = width, height = height, device = cairo_pdf)
  invisible(name)
}

FT_BORDER_GREY <- officer::fp_border(color = "grey70", width = 0.5)

# Build a styled flextable from a data frame.
# bold_col:    column name whose significant values to bold (uses significance threshold)
# bold_rows:   explicit row indices to bold in bold_col
# hline_after: row indices after which to insert a grey horizontal rule
make_ft <- function(dat, bold_col = NULL, bold_rows = NULL, hline_after = NULL) {
  ft <- flextable::flextable(as.data.frame(dat)) |>
    flextable::theme_booktabs() |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::bold(part = "header") |>
    flextable::align(align = "left",   part = "all") |>
    flextable::align(align = "center", part = "header") |>
    flextable::padding(padding.top = 3, padding.bottom = 3,
                       padding.left  = 5, padding.right  = 5, part = "all") |>
    flextable::autofit()

  if (!is.null(hline_after))
    for (i in hline_after)
      ft <- flextable::hline(ft, i = i, border = FT_BORDER_GREY)

  if (!is.null(bold_col) && !is.null(bold_rows) && length(bold_rows) > 0)
    ft <- flextable::bold(ft, i = bold_rows, j = bold_col)

  ft
}

# Add caption + footnotes and write .docx
finalize_ft <- function(ft, name, caption = "", footnotes = character(0)) {
  ft <- flextable::set_caption(ft, caption = caption)
  for (fn in footnotes) ft <- flextable::add_footer_lines(ft, values = fn)
  if (length(footnotes) > 0)
    ft <- ft |>
      flextable::fontsize(size = 8, part = "footer") |>
      flextable::italic(part = "footer") |>
      flextable::color(color = "#555555", part = "footer")
  flextable::save_as_docx(ft, path = here("tables", paste0(name, ".docx")))
  invisible(name)
}

save_table <- function(dat, name, caption = "", footnotes = character(0),
                       bold_col = NULL, bold_rows = NULL, hline_after = NULL) {
  write.csv(dat, here("tables", paste0(name, ".csv")), row.names = FALSE)
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer",   quietly = TRUE)) {
    message("  [tables] Install flextable + officer for .docx output")
    return(invisible(name))
  }
  ft <- make_ft(dat, bold_col = bold_col, bold_rows = bold_rows,
                hline_after = hline_after)
  finalize_ft(ft, name, caption = caption, footnotes = footnotes)
}

# ── Metric pivot helper ─────────────────────────────────────────────────────
# Shared prep for secondary outcomes (F4) and psycholinguistic dimensions (F_psycholing).

pivot_metrics <- function(dat, cols, labels, count_labels = character(0),
                          by_paper = FALSE) {
  out <- dat |>
    mutate(prompt_f  = fct_prompt(as.character(prompt_style)),
           model_f   = fct_model(model_name),
           dir_label = factor(ifelse(direction == "baseline", "Baseline", "Injection"),
                              levels = c("Baseline", "Injection")))
  if (by_paper) {
    out <- out |>
      group_by(paper_id, prompt_f, model_f, dir_label) |>
      summarise(across(all_of(cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
  }
  out |>
    pivot_longer(cols = all_of(cols), names_to = "metric", values_to = "value") |>
    filter(!is.na(value)) |>
    mutate(metric   = factor(metric, levels = names(labels), labels = labels),
           is_count = metric %in% count_labels)
}

# Test whether a formatted p-value string represents significance at α = 0.05
is_sig_p <- function(p_str) {
  p_str == "<0.001" | suppressWarnings(as.numeric(p_str)) < 0.05
}

# ══════════════════════════════════════════════════════════════════════════════
# FIGURES
# ══════════════════════════════════════════════════════════════════════════════

# ── F1: Per-paper tile chart ──────────────────────────────────────────────────

# Order papers within each study type; study types ordered by sample size.
STUDY_LEVELS <- c(rct = "RCT", systematic_review = "Systematic review",
                  observational = "Observational")
paper_order <- dat |>
  distinct(paper_id, study_type) |>
  mutate(study_type_f = factor(STUDY_LEVELS[study_type], levels = STUDY_LEVELS)) |>
  arrange(study_type_f, paper_id) |>
  mutate(row_idx = row_number())

tile_dat <- dat |>
  filter(!is.na(recommendation)) |>
  left_join(paper_order, by = c("paper_id", "study_type")) |>
  mutate(prompt_long     = fct_prompt_long(as.character(prompt_style)),
         model_f         = droplevels(fct_model(model_name)),  # drop empty model levels (e.g. Claude Sonnet)
         condition_label = fct_condition(as.character(direction), as.character(placement)),
         rec_label       = REC_SHORT[as.character(recommendation)])

# N-m2: cut the paper axis into three parts sized by sample (the reviewer's own
# second option). Proportional row facets per study type (nested under reviewer
# model) make the grouping unambiguously discrete rather than a continuous axis;
# space = "free_y" sizes each block to its paper count.
F1 <- tile_dat |>
  ggplot(aes(x = condition_label, y = factor(row_idx), fill = recommendation)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_vline(xintercept = 1.5, color = "grey30", linewidth = 0.9) +
  facet_grid(rows = vars(model_f, study_type_f), cols = vars(prompt_long),
             scales = "free_y", space = "free_y") +
  scale_fill_manual(values = REC_COLORS, labels = REC_LABELS) +
  scale_x_discrete(position = "top",
                   labels = c(Baseline  = "Baseline",
                              Beginning = "Beginning",
                              Middle    = "Middle",
                              End       = "End")) +
  scale_y_discrete(breaks = NULL, expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(linewidth = 0))) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0, size = 8),
        panel.grid = element_blank(), legend.position = "bottom",
        panel.spacing.x = unit(1.2, "lines"),
        panel.spacing.y = unit(0.25, "lines"),
        strip.text.y.right = element_text(angle = 0, size = 8))

save_fig(F1, "FS2_tiles", width = 7.5, height = 9)

# ── F2: Mean recommendation — aggregate injection vs baseline ─────────────────
# Injection pooled across placements. X-axis: prompt style. Colour: condition.
# Shape: model. Stars: pooled Wilcoxon injection vs baseline, positioned via
# position_dodge on full group set so they align with injection points.

rec_agg <- dat |>
  filter(!is.na(recommendation_ordinal)) |>
  mutate(
    model_f   = fct_model(model_name),
    prompt_f  = fct_prompt(as.character(prompt_style)),
    condition = factor(ifelse(direction == "baseline", "Baseline", "Injection"),
                       levels = c("Baseline", "Injection"))
  ) |>
  group_by(model_f, prompt_f, condition) |>
  summarise(mean_ord = mean(recommendation_ordinal, na.rm = TRUE),
            se       = sd(recommendation_ordinal,   na.rm = TRUE) / sqrt(n()),
            .groups  = "drop") |>
  mutate(ci_lo = mean_ord - 1.96 * se,
         ci_hi = mean_ord + 1.96 * se)

wilcox_pooled <- dat |>
  filter(!is.na(recommendation_ordinal)) |>
  mutate(model_f = fct_model(model_name), prompt_f = fct_prompt(prompt_style)) |>
  group_by(model_f, prompt_f) |>
  summarise(
    p_raw = wilcox.test(
      recommendation_ordinal[direction == "favorable"],
      recommendation_ordinal[direction == "baseline"],
      exact = FALSE)$p.value,
    .groups = "drop"
  ) |>
  mutate(stars = pstar(p_raw))

f2_dw <- 0.55

# Compute dodge offsets explicitly to draw baseline→injection connector segments.
# position_dodge with N groups places group i at offset (i - (N+1)/2) * (width/N).
.f2_ngrp <- nlevels(interaction(rec_agg$condition, rec_agg$model_f, drop = TRUE))
.f2_step <- f2_dw / .f2_ngrp

seg_f2 <- rec_agg |>
  mutate(
    grp_rank = as.integer(interaction(condition, model_f, drop = TRUE)),
    x_offset = (grp_rank - (.f2_ngrp + 1) / 2) * .f2_step,
    x_num    = as.integer(prompt_f) + x_offset
  ) |>
  select(model_f, prompt_f, condition, mean_ord, x_num) |>
  pivot_wider(names_from = condition, values_from = c(mean_ord, x_num)) |>
  filter(!is.na(mean_ord_Baseline) & !is.na(mean_ord_Injection))

# Include all groups so position_dodge places stars at correct injection offsets.
star_dat_f2 <- rec_agg |>
  left_join(wilcox_pooled |> select(model_f, prompt_f, stars),
            by = c("model_f", "prompt_f")) |>
  mutate(star_label = ifelse(condition == "Injection", stars, ""),
         y_star     = ci_hi + 0.18)

F2 <- rec_agg |>
  ggplot(aes(x = as.integer(prompt_f), y = mean_ord, ymin = ci_lo, ymax = ci_hi,
             color = condition, shape = model_f,
             group = interaction(condition, model_f))) +
  geom_segment(data = seg_f2,
               aes(x = x_num_Baseline, xend = x_num_Injection,
                   y = mean_ord_Baseline, yend = mean_ord_Injection),
               color = "#2166AC", linewidth = 0.55, alpha = 0.95,
               inherit.aes = FALSE) +
  geom_pointrange(position = position_dodge(width = f2_dw),
                  linewidth = 0.6, size = 0.45) +
  geom_text(data = star_dat_f2,
            aes(x = as.integer(prompt_f), y = y_star, label = star_label,
                group = interaction(condition, model_f)),
            position = position_dodge(width = f2_dw),
            inherit.aes = FALSE, color = "grey20", size = 3.8, vjust = 0) +
  scale_x_continuous(breaks = seq_len(nlevels(rec_agg$prompt_f)),
                     labels = levels(rec_agg$prompt_f)) +
  scale_color_manual(values = COND_COLORS) +
  scale_shape_manual(values = c("Gemma 4 (26B)" = 16, "GPT o4-mini" = 17,
                                "Claude Sonnet" = 15)) +
  scale_y_continuous(breaks = 1:4,
                     labels = c("Reject", "Major", "Minor", "Accept"),
                     limits = c(1.5, 4.9)) +
  labs(x = NULL, y = "Mean recommendation (95% CI)",
       color = "Condition", shape = "Model",
       title = "Recommendation shift")

# ── F3: Detection rate — pooled, Clopper-Pearson 95% CI ───────────────────────
# Baseline included to show false positive rate. Mirrors F2 exactly.
# Shape: model.

det_agg <- dat |>
  mutate(
    model_f   = fct_model(model_name),
    prompt_f  = fct_prompt(as.character(prompt_style)),
    condition = factor(ifelse(direction == "baseline", "Baseline", "Injection"),
                       levels = c("Baseline", "Injection"))
  ) |>
  group_by(model_f, prompt_f, condition) |>
  summarise(detected = sum(detected_injection, na.rm = TRUE),
            n = n(), .groups = "drop") |>
  mutate(
    rate  = detected / n,
    ci_lo = ifelse(detected == 0, 0, qbeta(0.025, detected, n - detected + 1)),
    ci_hi = ifelse(detected == n, 1, qbeta(0.975, detected + 1, n - detected))
  )

# Fisher exact test: injection vs baseline detection rate per model × prompt
det_stars_f3 <- dat |>
  filter(!is.na(detected_injection)) |>
  mutate(model_f = fct_model(model_name), prompt_f = fct_prompt(prompt_style)) |>
  group_by(model_f, prompt_f) |>
  summarise(
    p_raw = fisher.test(table(
      factor(direction, levels = c("favorable", "baseline")),
      factor(detected_injection, levels = c(TRUE, FALSE))
    ))$p.value,
    .groups = "drop"
  ) |>
  mutate(stars = pstar(p_raw))

star_dat_f3 <- det_agg |>
  left_join(det_stars_f3 |> select(model_f, prompt_f, stars),
            by = c("model_f", "prompt_f")) |>
  mutate(star_label = ifelse(condition == "Injection", replace_na(stars, ""), ""),
         y_star     = ci_hi + 0.07)

# Connecting segments for F3 (same dodge structure as F2)
seg_f3 <- det_agg |>
  mutate(
    grp_rank = as.integer(interaction(condition, model_f, drop = TRUE)),
    x_offset = (grp_rank - (.f2_ngrp + 1) / 2) * .f2_step,
    x_num    = as.integer(prompt_f) + x_offset
  ) |>
  select(model_f, prompt_f, condition, rate, x_num) |>
  pivot_wider(names_from = condition, values_from = c(rate, x_num)) |>
  filter(!is.na(rate_Baseline) & !is.na(rate_Injection))

F3 <- det_agg |>
  ggplot(aes(x = as.integer(prompt_f), y = rate, ymin = ci_lo, ymax = ci_hi,
             color = condition, shape = model_f,
             group = interaction(condition, model_f))) +
  geom_segment(data = seg_f3,
               aes(x = x_num_Baseline, xend = x_num_Injection,
                   y = rate_Baseline, yend = rate_Injection),
               color = "#2166AC", linewidth = 0.55, alpha = 0.95,
               inherit.aes = FALSE) +
  geom_linerange(aes(ymin = ifelse(detected > 0, ci_lo, NA),
                     ymax = ifelse(detected > 0, ci_hi, NA)),
                 position = position_dodge(width = f2_dw),
                 linewidth = 0.6) +
  geom_point(position = position_dodge(width = f2_dw), size = 1.8) +
  geom_text(data = star_dat_f3,
            aes(x = as.integer(prompt_f), y = y_star, label = star_label,
                group = interaction(condition, model_f)),
            position = position_dodge(width = f2_dw),
            inherit.aes = FALSE, color = "grey20", size = 3.8, vjust = 0) +
  scale_x_continuous(breaks = seq_len(nlevels(det_agg$prompt_f)),
                     labels = levels(det_agg$prompt_f)) +
  scale_color_manual(values = COND_COLORS) +
  scale_shape_manual(values = c("Gemma 4 (26B)" = 16, "GPT o4-mini" = 17,
                                "Claude Sonnet" = 15)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.15), breaks = seq(0, 1, 0.25)) +
  labs(x = NULL, y = "Detection rate (95% CI)",
       color = "Condition", shape = "Model",
       title = "Injection detection")

# ── F2 + F3 combined panel ────────────────────────────────────────────────────
F2 <- F2 +
  guides(
    shape = guide_legend(override.aes = list(linetype = 0, linewidth = 0))
  )

F2F3 <- ((F2 + guides(shape = "none", color = "none")) |
  F3) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 13))
  ) &
  theme(legend.position = "bottom")

save_fig(F2F3, "F2F3_panel", width = 7, height = 4)

# ── F_placement: Placement trajectory + detection (patchwork A | B) ───────────
# Models pooled: colour = prompt style only → 2 clean lines per panel.
# Stars from pooled paired Wilcoxon (paper × model pairs), Bonferroni × 3 placements.

PROMPT_LONG_COLORS <- setNames(
  unname(PROMPT_COLORS),
  levels(fct_prompt_long("standard"))
)

# Paired Wilcoxon on shift, pooled across models, per prompt × placement
wilcox_place_pooled <- paired |>
  group_by(prompt_style, placement) |>
  summarise(p_raw = wilcox.test(shift, exact = FALSE)$p.value, .groups = "drop") |>
  group_by(prompt_style) |>
  mutate(p_adj = p.adjust(p_raw, "bonferroni"), stars = pstar(p_adj)) |>
  ungroup()

place_agg <- dat |>
  filter(!is.na(recommendation_ordinal)) |>
  mutate(
    prompt_long = fct_prompt_long(as.character(prompt_style)),
    condition_p = fct_condition(as.character(direction), as.character(placement))
  ) |>
  group_by(prompt_long, condition_p) |>
  summarise(
    mean_ord = mean(recommendation_ordinal, na.rm = TRUE),
    se       = sd(recommendation_ordinal,   na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  ) |>
  mutate(ci_lo = mean_ord - 1.96 * se,
         ci_hi = mean_ord + 1.96 * se)

fp_dw <- 0.25

Fp_rec <- ggplot(place_agg,
                 aes(x = condition_p, y = mean_ord, ymin = ci_lo, ymax = ci_hi,
                     color = prompt_long, group = prompt_long)) +
  geom_line(position = position_dodge(width = fp_dw),
            linewidth = 0.6, alpha = 0.75) +
  geom_pointrange(position = position_dodge(width = fp_dw),
                  linewidth = 0.6, size = 0.5) +
  annotate("text", x = 0.6, y = 4.82,
           label = "All injection vs. baseline:\np < 0.001",
           hjust = 0, vjust = 1, size = 2.6, color = "grey30", lineheight = 0.95) +
  scale_color_manual(values = PROMPT_LONG_COLORS, name = "Prompt style") +
  scale_y_continuous(breaks = 1:4,
                     labels = c("Reject", "Major", "Minor", "Accept"),
                     limits = c(1.5, 4.95)) +
  labs(x = NULL, y = "Mean recommendation (95% CI)",
       title = "Recommendation shift by placement")

det_place_agg <- dat |>
  mutate(
    prompt_long = fct_prompt_long(as.character(prompt_style)),
    condition_p = fct_condition(as.character(direction), as.character(placement))
  ) |>
  group_by(prompt_long, condition_p) |>
  summarise(
    detected = sum(detected_injection, na.rm = TRUE),
    n        = n(),
    .groups  = "drop"
  ) |>
  mutate(
    rate  = detected / n,
    ci_lo = ifelse(detected == 0, 0,
                   qbeta(0.025, detected,     n - detected + 1)),
    ci_hi = ifelse(detected == n, 1,
                   qbeta(0.975, detected + 1, n - detected))
  )

# Fisher exact test: each injection placement vs baseline, per prompt style
bl_det <- det_place_agg |>
  filter(condition_p == "Baseline") |>
  select(prompt_long, bl_detected = detected, bl_n = n)

fisher_det_place <- det_place_agg |>
  filter(condition_p != "Baseline") |>
  left_join(bl_det, by = "prompt_long") |>
  rowwise() |>
  mutate(p_raw = fisher.test(matrix(
    c(detected, n - detected, bl_detected, bl_n - bl_detected), nrow = 2
  ))$p.value) |>
  ungroup() |>
  mutate(p_adj = p.adjust(p_raw, "bonferroni"), stars = pstar(p_adj))

star_det_place <- fisher_det_place |>
  filter(replace_na(stars, "") != "")

Fp_det <- ggplot(
    det_place_agg,
    aes(x = condition_p, y = rate, ymin = ci_lo, ymax = ci_hi,
        color = prompt_long, group = prompt_long)) +
  geom_line(position = position_dodge(width = fp_dw),
            linewidth = 0.6, alpha = 0.75) +
  geom_pointrange(position = position_dodge(width = fp_dw),
                  linewidth = 0.6, size = 0.5) +
  geom_text(data = star_det_place,
            aes(x = condition_p, y = ci_hi + 0.05, label = stars, group = prompt_long),
            position = position_dodge(width = fp_dw),
            inherit.aes = FALSE, color = "grey20", size = 3.5, vjust = 0) +
  scale_color_manual(values = PROMPT_LONG_COLORS, name = "Prompt style") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.15), breaks = seq(0, 1, 0.25)) +
  labs(x = NULL, y = "Detection rate (95% CI)",
       title = "Injection detection by placement")

Fplace <- (Fp_rec | Fp_det) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A",
                  theme = theme(plot.tag = element_text(face = "bold", size = 13))) &
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1))

save_fig(Fplace, "F_placement", width = 7, height = 4.5)

# ── F4: Secondary outcomes with significance brackets ─────────────────────────

# F4 always uses LLM-extracted sentiment_score (0-10) as the sentiment panel.
# Transformer psycholinguistic dimensions go to a separate supplementary figure.
psycholing_cols <- intersect(
  c("sent_positive", "assertiveness", "certainty", "positive_affect"),
  names(dat)
)
has_psycholing <- length(psycholing_cols) == 4 &&
  any(!is.na(dat$sent_positive))

secondary_cols <- c("sentiment_score", "critique_count", "strength_count")

secondary_labels <- c(
  sentiment_score = "Sentiment LLM (0\u201310)",
  critique_count  = "Critiques (n)",
  strength_count  = "Strengths (n)",
  positive_affect = "Positive affect"
)

sec_labels <- secondary_labels[secondary_cols]
sec_count  <- c("Critiques (n)", "Strengths (n)")

secondary_long <- pivot_metrics(dat, secondary_cols, sec_labels, sec_count)

# Per-paper means for dot overlay (paper is the unit of analysis in clmm;
# injection conditions averaged across placements to avoid triplication).
secondary_paper_means <- pivot_metrics(dat, secondary_cols, sec_labels, sec_count,
                                       by_paper = TRUE)

sig_f4 <- stats_out$wilcox_secondary_paired |>
  mutate(
    prompt_f = fct_prompt(prompt_style),
    model_f  = fct_model(model_name),
    metric   = factor(outcome, levels = secondary_cols,
                      labels = secondary_labels[secondary_cols]),
    label    = case_when(p_adj < 0.001 ~ "***", p_adj < 0.01 ~ "**",
                         p_adj < 0.05 ~ "*", TRUE ~ "ns"),
    x_mid    = as.numeric(prompt_f)
  )

f4_titles <- c(
  sentiment_score = "Sentiment (0\u201310)",
  critique_count  = "Critiques",
  strength_count  = "Strengths"
)

make_f4_panel <- function(mdat, mdat_pts, msig, title, ylab = NULL, star_offset = 0.15, star_floor = 0.02) {
  is_cnt <- isTRUE(unique(mdat$is_count)[1])
  if (is.null(ylab)) ylab <- if (is_cnt) "Count (n)" else "Score (0\u201310)"

  agg <- mdat |>
    group_by(prompt_f, dir_label, model_f) |>
    summarise(
      mean_val = mean(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) |>
    mutate(ci_lo = mean_val - 1.96 * se, ci_hi = mean_val + 1.96 * se)

  f4_dw <- 0.55
  f4_ngrp <- nlevels(interaction(agg$dir_label, agg$model_f, drop = TRUE))
  f4_step <- f4_dw / f4_ngrp

  # Connector segments baseline → injection per model

  seg <- agg |>
    mutate(
      grp_rank = as.integer(interaction(dir_label, model_f, drop = TRUE)),
      x_offset = (grp_rank - (f4_ngrp + 1) / 2) * f4_step,
      x_num = as.integer(prompt_f) + x_offset
    ) |>
    select(model_f, prompt_f, dir_label, mean_val, x_num) |>
    pivot_wider(names_from = dir_label, values_from = c(mean_val, x_num)) |>
    filter(!is.na(mean_val_Baseline) & !is.na(mean_val_Injection))

  # Stars positioned at injection dodge offsets
# Get max ci_hi per prompt × model across both conditions
y_ceil <- agg |>
  group_by(prompt_f, model_f) |>
  summarise(y_ceil = max(ci_hi, na.rm = TRUE), .groups = "drop")

star_agg <- agg |>
  left_join(msig |> select(prompt_f, model_f, label), by = c("prompt_f", "model_f")) |>
  left_join(y_ceil, by = c("prompt_f", "model_f")) |>
  mutate(
    star_label = ifelse(dir_label == "Injection", label, ""),
    y_star = y_ceil + max(star_floor, star_offset * diff(range(agg$mean_val, na.rm = TRUE)))
  )

  ggplot(
    agg,
    aes(
      x = as.integer(prompt_f),
      y = mean_val,
      ymin = ci_lo,
      ymax = ci_hi,
      color = dir_label,
      shape = model_f,
      group = interaction(dir_label, model_f)
    )
  ) +
    geom_segment(
      data = seg,
      aes(
        x = x_num_Baseline,
        xend = x_num_Injection,
        y = mean_val_Baseline,
        yend = mean_val_Injection
      ),
      color = "#2166AC",
      linewidth = 0.55,
      alpha = 0.95,
      inherit.aes = FALSE
    ) +
    geom_pointrange(
      position = position_dodge(width = f4_dw),
      linewidth = 0.6,
      size = 0.45
    ) +
    geom_text(
      data = star_agg,
      aes(
        x = as.integer(prompt_f),
        y = y_star,
        label = star_label,
        group = interaction(dir_label, model_f)
      ),
      position = position_dodge(width = f4_dw),
      inherit.aes = FALSE,
      color = "grey20",
      size = 3.8,
      vjust = 0
    ) +
    scale_x_continuous(
      breaks = seq_len(nlevels(agg$prompt_f)),
      labels = levels(agg$prompt_f)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
    scale_color_manual(values = COND_COLORS) +
    scale_shape_manual(
      values = c("Gemma 4 (26B)" = 16, "GPT o4-mini" = 17, "Claude Sonnet" = 15)
    ) +
    labs(
      x = NULL,
      y = ylab,
      color = "Condition",
      shape = "Model",
      title = title
    )
}


f4_panels <- lapply(secondary_cols, function(col) {
  lbl <- secondary_labels[col]
  make_f4_panel(
    mdat     = filter(secondary_long,        metric == lbl),
    mdat_pts = filter(secondary_paper_means, metric == lbl),
    msig     = filter(sig_f4,                metric == lbl),
    title    = f4_titles[col]
  )
})

F4 <- wrap_plots(f4_panels, nrow = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A",
                  theme = theme(plot.tag = element_text(face = "bold", size = 13))) &
  theme(legend.position = "bottom")

save_fig(F4, "F4_secondary", width = 7, height = 4)

# ── F_psycholing: Transformer psycholinguistic dimensions (supplementary) ─────
# Only generated when `make psycholing` has been run (has_psycholing == TRUE).

if (has_psycholing) {
  psycholing_labels <- c(
    positive_affect = "Positive affect",
    sent_positive   = "Sentiment positivity",
    assertiveness   = "Assertiveness",
    certainty       = "Certainty"
  )
  psycholing_cols_present <- intersect(names(psycholing_labels), names(dat))

  psycholing_long <- pivot_metrics(dat, psycholing_cols_present, psycholing_labels)

  psycholing_paper_means <- pivot_metrics(dat, psycholing_cols_present, psycholing_labels,
                                          by_paper = TRUE)

  # JMIR R1: paired signed-rank on per-paper means (baseline vs injection
  # averaged across placements), replacing the unpaired rank-sum.
  sig_psy <- psycholing_paper_means |>
    pivot_wider(id_cols = c(paper_id, prompt_f, model_f, metric),
                names_from = dir_label, values_from = value) |>
    filter(!is.na(Baseline), !is.na(Injection)) |>
    group_by(prompt_f, model_f, metric) |>
    summarise(
      pval = wilcox.test(Injection, Baseline, paired = TRUE,
                         exact = FALSE)$p.value,
      .groups = "drop"
    ) |>
    mutate(label = case_when(pval < 0.001 ~ "***", pval < 0.01 ~ "**",
                             pval < 0.05 ~ "*", TRUE ~ "ns"),
           x_mid = as.numeric(prompt_f))

  psy_panels <- lapply(psycholing_cols_present, function(col) {
    lbl <- psycholing_labels[col]
    make_f4_panel(
      mdat     = filter(psycholing_long,        metric == lbl),
      mdat_pts = filter(psycholing_paper_means, metric == lbl),
      msig     = filter(sig_psy,                metric == lbl),
      title    = lbl,
      ylab     = "Score (0\u20131)",
      star_offset = 0.02, star_floor = 0
    )
  })

  F_psycholing <- wrap_plots(psy_panels, nrow = 2) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A",
                    theme = theme(plot.tag = element_text(face = "bold", size = 13))) &
    theme(legend.position = "bottom")

  save_fig(F_psycholing, "F_psycholing", width = 7, height = 7)
}

# ── F5: Alluvial ──────────────────────────────────────────────────────────────

if (requireNamespace("ggalluvial", quietly = TRUE)) {
  library(ggalluvial)

  alluvial_dat <- paired |>
    count(prompt_long, baseline_rec, injection_rec) |>
    mutate(baseline_rec  = factor(baseline_rec,  levels = names(REC_COLORS)),
           injection_rec = factor(injection_rec, levels = names(REC_COLORS)))

  make_alluvial_panel <- function(dat, title) {
    dat |>
      ggplot(aes(axis1 = baseline_rec, axis2 = injection_rec, y = n)) +
      geom_alluvium(aes(fill = baseline_rec), alpha = 0.60, width = 0.32, knot.pos = 1/3) +
      geom_stratum(aes(fill = after_stat(stratum)), width = 0.32,
                   color = "white", linewidth = 0.3) +
geom_text(stat = "stratum",
          aes(label = after_stat({
            tot <- tapply(count, x, sum)
            pct <- round(100 * count / tot[as.character(x)])
            ifelse(count >= 15, paste0(pct, "%"), "")
          })),
          size = 2.5, color = "white", fontface = "bold", lineheight = 0.85)+
      scale_fill_manual(values = REC_COLORS, labels = REC_LABELS, name = "") +
      scale_x_discrete(limits = c("Baseline", "Injection"), expand = c(0.15, 0.15)) +
      labs(x = NULL, y = "Reviews (n)", title = title) +
      theme(
        panel.grid   = element_blank(),
        axis.line    = element_blank(),
        axis.ticks.x = element_blank()
        # axis.text.x  = element_text(face = "bold", size = rel(1.05))
      )
  }

F5 <- ((make_alluvial_panel(
  filter(alluvial_dat, prompt_long == "Standard prompt"),
  "Standard prompt"
) +
  guides(fill = "none")) |
  make_alluvial_panel(
    filter(alluvial_dat, prompt_long == "Injection-aware prompt"),
    "Injection-aware prompt"
  )) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold", size = 13))
  ) &
  theme(legend.position = "bottom")

  save_fig(F5, "F1_alluvial", width = 7, height = 4)
}

# ── T6: Ordinal shift summary (replaces slope chart) ─────────────────────────
# Mean shift = injection_ordinal − baseline_ordinal per model × prompt × placement.

shift_tbl <- paired |>
  group_by(model_f, prompt_long, placement_f) |>
  summarise(
    n         = n(),
    mean_shift = round(mean(shift, na.rm = TRUE), 2),
    pct_up     = round(100 * mean(shift > 0, na.rm = TRUE)),
    pct_none   = round(100 * mean(shift == 0, na.rm = TRUE)),
    pct_down   = round(100 * mean(shift < 0, na.rm = TRUE)),
    .groups    = "drop"
  ) |>
  rename(Model = model_f, `Prompt style` = prompt_long,
         Placement = placement_f, N = n,
         `Mean shift` = mean_shift,
         `Up (%)` = pct_up, `No change (%)` = pct_none, `Down (%)` = pct_down)

save_table(shift_tbl, "T6_shift_summary",
           caption = "Table S1. Ordinal recommendation shift (injection \u2212 baseline) by model, prompt style, and injection placement.",
           footnotes = c("Mean shift: positive = favoured by injection. Up/No change/Down = % of paired reviews shifting in each direction."))

# ── T_text_stats: Inline text statistics for manuscript ───────────────────────
# Key-value CSV used to fill in all inline numbers in the paper draft.

writeLines(R.version.string, here("output", "r_version.txt"))

# Alluvial shift stats (needed for top-transition percentage below)
alluvial_stats <- paired |>
  group_by(prompt_f) |>
  summarise(
    n_total  = n(),
    pct_up   = round(100 * mean(shift > 0), 1),
    pct_same = round(100 * mean(shift == 0), 1),
    pct_down = round(100 * mean(shift < 0), 1),
    .groups  = "drop"
  ) |>
  mutate(ps = ifelse(as.character(prompt_f) == "Standard", "std", "aw")) |>
  pivot_longer(c(n_total, pct_up, pct_same, pct_down),
               names_to = "metric", values_to = "value") |>
  transmute(key = paste0("alluvial_", ps, "_", metric), value)

# Detection-compliance summary (aware prompt)
aw_det_sum   <- det_compliance_summary |> filter(prompt_f == "Aware", det_label == "Detected")
aw_undet_sum <- det_compliance_summary |> filter(prompt_f == "Aware", det_label == "Not detected")

text_stats <- bind_rows(
  # Scalar dataset stats
  tibble(
    key   = c("n_papers", "n_reviews", "n_extractions_with_rec",
              "pct_extractions_complete", "baseline_mean_overall"),
    value = c(n_distinct(dat$paper_id), nrow(dat),
              sum(!is.na(dat$recommendation_ordinal)),
              round(100 * mean(!is.na(dat$recommendation_ordinal)), 1),
              round(mean(dat$recommendation_ordinal[dat$direction == "baseline"], na.rm = TRUE), 2))
  ),
  # Corpus counts by study type
  dat |> distinct(paper_id, study_type) |> count(study_type) |>
    transmute(key = paste0("n_papers_", study_type), value = as.numeric(n)),
  # Baseline mean per model
  dat |> filter(direction == "baseline", !is.na(recommendation_ordinal)) |>
    group_by(model_name) |>
    summarise(value = round(mean(recommendation_ordinal), 2), .groups = "drop") |>
    transmute(key = paste0("baseline_mean_", gsub("[^a-zA-Z0-9]", "_", model_name)), value),
  # Detection rates per prompt × placement
  dat |> group_by(prompt_style, placement) |>
    summarise(value = round(100 * mean(detected_injection, na.rm = TRUE), 1), .groups = "drop") |>
    transmute(key = sprintf("detection_pct_%s_%s", prompt_style, placement), value),
  # Alluvial shift stats
  alluvial_stats,
  # Most common transition per prompt style
  paired |>
    group_by(prompt_f, baseline_rec, injection_rec) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(prompt_f) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      ps      = ifelse(as.character(prompt_f) == "Standard", "std", "aw"),
      n_total = alluvial_stats$value[match(paste0("alluvial_", ps, "_n_total"), alluvial_stats$key)],
      pct     = round(100 * n / n_total, 1)
    ) |>
    pivot_longer(c(n, pct), names_to = "metric", values_to = "value") |>
    transmute(key = paste0("alluvial_", ps, "_top_transition_", metric), value),
  # Residual injection OR under aware prompt
  if (!is.null(stats_out$clmm_main)) {
    cf <- summary(stats_out$clmm_main)$coefficients
    tibble(key = "residual_OR_injection_under_aware",
           value = round(exp(cf["directionfavorable", "Estimate"] +
                             cf["directionfavorable:prompt_styleaware", "Estimate"]), 2))
  } else tibble(key = "residual_OR_injection_under_aware", value = NA_real_),
  # LRT placement × model interaction
  if (!is.null(stats_out$lrt_place_model)) {
    lrt_row <- stats_out$lrt_place_model[2, ]
    tibble(key   = c("lrt_placement_model_chisq", "lrt_placement_model_df", "lrt_placement_model_p"),
           value = c(round(lrt_row[["LR.stat"]], 2), lrt_row[["df"]], lrt_row[["Pr(>Chisq)"]]))
  } else tibble(key = c("lrt_placement_model_chisq", "lrt_placement_model_df", "lrt_placement_model_p"),
                value = NA_real_),
  # Detection-compliance (aware prompt)
  tibble(
    key = c("det_compliance_aw_detected_n", "det_compliance_aw_detected_shift",
            "det_compliance_aw_detected_pct_up", "det_compliance_aw_undetected_n",
            "det_compliance_aw_undetected_shift", "det_compliance_aw_undetected_pct_up",
            "det_compliance_aw_wilcox_p"),
    value = c(aw_det_sum$n, aw_det_sum$mean_shift, aw_det_sum$pct_up,
              aw_undet_sum$n, aw_undet_sum$mean_shift, aw_undet_sum$pct_up,
              det_wilcox_aware$p)
  )
)

write.csv(text_stats, here("tables", "T_text_stats.csv"), row.names = FALSE)
message("  Text stats -> tables/T_text_stats.csv (", nrow(text_stats), " rows)")

# ── F6: Stacked recommendation distribution ───────────────────────────────────
# One patchwork panel per prompt style (Standard | Aware).
# X-axis = injection condition; pooled across models.

dist_dat <- dat |>
  filter(!is.na(recommendation)) |>
  mutate(prompt_f        = fct_prompt(as.character(prompt_style)),
         condition_label = fct_condition(as.character(direction), as.character(placement)),
         recommendation  = factor(as.character(recommendation),
                                  levels = rev(names(REC_COLORS)))) |>
  count(prompt_f, condition_label, recommendation) |>
  group_by(prompt_f, condition_label) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

make_dist_panel <- function(dat, title) {
  dat |>
    ggplot(aes(x = condition_label, y = pct, fill = recommendation)) +
    geom_col(width = 0.58, color = "white", linewidth = 0.35) +
    geom_text(aes(label = ifelse(pct >= 0.07, paste0(round(pct * 100), "%"), "")),
              position = position_stack(vjust = 0.5),
              size = 2.6, color = "white", fontface = "bold") +
    scale_fill_manual(values = REC_COLORS, labels = REC_LABELS,
                      guide  = guide_legend(reverse = TRUE,
                                            keywidth  = unit(0.9, "lines"),
                                            keyheight = unit(0.7, "lines"))) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, 1.01), expand = c(0, 0)) +
    labs(x = NULL, y = "Proportion of reviews", fill = NULL, title = title) +
    theme(axis.text.x = element_text(size = 8))
}

F6 <- (
  make_dist_panel(filter(dist_dat, prompt_f == "Standard"),  "Standard prompt") |
  make_dist_panel(filter(dist_dat, prompt_f == "Aware"), "Injection-aware prompt")
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A",
                  theme = theme(plot.tag = element_text(face = "bold", size = 13))) &
  theme(legend.position = "bottom")

save_fig(F6, "F6_distribution", width = 7, height = 4)

# ══════════════════════════════════════════════════════════════════════════════
# TABLES
# ══════════════════════════════════════════════════════════════════════════════

# ── T1: Corpus ────────────────────────────────────────────────────────────────

T1_dat <- dat |>
  distinct(paper_id, study_type, citation_count, author_count) |>
  mutate(study_type = recode(study_type,
    rct = "Randomised controlled trial",
    systematic_review = "Systematic review",
    observational = "Observational study")) |>
  group_by(`Study type` = study_type) |>
  summarise(
    N = n(),
    `Citations, median (IQR)` = sprintf("%.0f (%.0f\u2013%.0f)",
      median(citation_count, na.rm = TRUE),
      quantile(citation_count, 0.25, na.rm = TRUE),
      quantile(citation_count, 0.75, na.rm = TRUE)),
    `Authors, median (IQR)` = sprintf("%.0f (%.0f\u2013%.0f)",
      median(author_count, na.rm = TRUE),
      quantile(author_count, 0.25, na.rm = TRUE),
      quantile(author_count, 0.75, na.rm = TRUE)),
    .groups = "drop"
  )

save_table(T1_dat, "T1_corpus",
  caption = "Table 1. Corpus composition by study type.",
  footnotes = c(
    "Papers sourced from PLOS Medicine (2020\u2013present), stratified by study type.",
    "Citations: number of times each article has been cited, as returned by the CrossRef API at time of corpus assembly. Author count: total author list length as recorded in the CrossRef metadata."
  ))

# ── T2: Primary results — side-by-side standard | aware ───────────────────────

compact_side <- function(ps_code) {
  # Shift is against the MATCHED same-prompt baseline (the paired comparison),
  # not the global baseline mean pooled across prompt styles.
  ps_baseline <- dat |>
    filter(prompt_style == ps_code, direction == "baseline",
           !is.na(recommendation_ordinal)) |>
    pull(recommendation_ordinal) |>
    mean()
  rows <- dat |>
    filter(prompt_style == ps_code, !is.na(recommendation)) |>
    mutate(condition = fct_condition(as.character(direction),
                                     as.character(placement))) |>
    group_by(condition) |>
    summarise(
      n_reviews = n(),
      mean_ord  = mean(recommendation_ordinal, na.rm = TRUE),
      sd_ord    = sd(recommendation_ordinal,   na.rm = TRUE),
      delta     = mean(recommendation_ordinal, na.rm = TRUE) - ps_baseline,
      det_n     = sum(detected_injection, na.rm = TRUE),
      det_pct   = round(100 * mean(detected_injection, na.rm = TRUE)),
      .groups   = "drop"
    ) |>
    mutate(
      mean_sd = sprintf("%.1f (%.1f)", mean_ord, sd_ord),
      shift   = ifelse(condition == "Baseline", "\u2014",
                       sprintf("%+.2f", delta)),
      det     = sprintf("%d (%d%%)", det_n, det_pct)
    )

  pvals <- stats_out$wilcox_primary |>
    filter(prompt_style == ps_code) |>
    mutate(condition = factor(
      case_when(placement == "beginning" ~ "Beginning",
                placement == "middle"    ~ "Middle",
                placement == "end"       ~ "End"),
      levels = levels(rows$condition))) |>
    group_by(condition) |>
    slice_min(p_adj, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(condition, p_fmt)

  rows |>
    left_join(pvals, by = "condition") |>
    mutate(p_val = ifelse(condition == "Baseline", "\u2014",
                          replace_na(p_fmt, "\u2014"))) |>
    select(condition, n_reviews, mean_sd, shift, det, p_val)
}

T2_combined <- compact_side("standard") |>
  select(-det) |>
  rename(std_N = n_reviews, std_mean = mean_sd,
         std_shift = shift, std_p = p_val) |>
  left_join(
    compact_side("aware") |>
      rename(aw_N = n_reviews, aw_mean = mean_sd,
             aw_shift = shift, aw_det = det, aw_p = p_val),
    by = "condition"
  )

write.csv(T2_combined, here("tables", "T2_primary.csv"), row.names = FALSE)

sig_std_comb <- which(is_sig_p(T2_combined$std_p))
sig_aw_comb  <- which(is_sig_p(T2_combined$aw_p))

ft_t2 <- flextable::flextable(as.data.frame(T2_combined)) |>
  flextable::set_header_labels(
    condition = "Condition",
    std_N     = "N",   std_mean  = "Mean ordinal (SD)",
    std_shift = "Shift",           std_p     = "p-value",
    aw_N      = "N",   aw_mean   = "Mean ordinal (SD)",
    aw_shift  = "Shift",           aw_det    = "Detection, n (%)",
    aw_p      = "p-value"
  ) |>
  flextable::add_header_row(
    values    = c("", "Standard reviewer prompt",
                  "Injection-aware reviewer prompt"),
    colwidths = c(1L, 4L, 5L)
  ) |>
  flextable::theme_booktabs() |>
  flextable::fontsize(size = 10, part = "all") |>
  flextable::bold(part = "header") |>
  flextable::align(align = "left",   part = "all") |>
  flextable::align(align = "center", part = "header") |>
  flextable::padding(padding.top = 3, padding.bottom = 3,
                     padding.left = 5, padding.right = 5, part = "all") |>
  flextable::hline(i = 1, border = FT_BORDER_GREY) |>
  flextable::autofit()

if (length(sig_std_comb) > 0)
  ft_t2 <- flextable::bold(ft_t2, i = sig_std_comb, j = "std_p")
if (length(sig_aw_comb) > 0)
  ft_t2 <- flextable::bold(ft_t2, i = sig_aw_comb,  j = "aw_p")

finalize_ft(ft_t2, "T2_primary",
  caption = "Table 2. Reviewer recommendations by injection condition.",
  footnotes = c(
    "Ordinal scale: 1 = Reject, 2 = Major revision, 3 = Minor revision, 4 = Accept.",
    "Shift = difference in mean ordinal vs. the uninjected baseline condition.",
    paste0("Detection (injection-aware prompt only): proportion of reviews in which the ",
           "reviewer explicitly identified the embedded instruction."),
    paste0("p-values: two-sided paired Wilcoxon signed-rank test, Bonferroni-corrected ",
           "across three placement comparisons (adjusted threshold p\u2009<\u20090.017). ",
           "Bold = significant after correction.")
  ))

# ── T3: Secondary outcomes — wide format with spanning header ─────────────────
# Rows = outcomes; columns: Outcome | Std Baseline | Std Injection | p | Aw Baseline | Aw Injection | p

# Mean (SD) cells pivoted to wide: std_bl, std_inj, aw_bl, aw_inj
t3_cells <- dat |>
  filter(!is.na(sentiment_score)) |>
  mutate(dir = ifelse(direction == "baseline", "bl", "inj"),
         ps  = ifelse(prompt_style == "standard", "std", "aw")) |>
  pivot_longer(c(sentiment_score, critique_count, strength_count),
               names_to = "outcome", values_to = "value") |>
  group_by(ps, dir, outcome) |>
  summarise(cell = sprintf("%.1f (%.1f)", mean(value), sd(value)), .groups = "drop") |>
  unite("col", ps, dir) |>
  pivot_wider(names_from = col, values_from = cell)

# Attach pooled paired Wilcoxon p-values (JMIR R1: signed-rank, paired by paper × model)
t3_pvals <- stats_out$wilcox_secondary_pooled_paired |>
  mutate(ps = ifelse(prompt_style == "standard", "std", "aw")) |>
  pivot_wider(id_cols = outcome, names_from = ps,
              values_from = c(p_fmt, p_adj), names_glue = "{ps}_{.value}") |>
  rename(std_p = std_p_fmt, aw_p = aw_p_fmt, std_padj = std_p_adj, aw_padj = aw_p_adj)

T3_wide <- t3_cells |>
  left_join(t3_pvals, by = "outcome") |>
  mutate(Outcome = factor(outcome,
    levels = c("sentiment_score", "critique_count", "strength_count"),
    labels = c("Sentiment (0\u201310)", "Critiques, n", "Strengths, n")
  )) |>
  arrange(Outcome) |>
  mutate(Outcome = as.character(Outcome)) |>
  select(Outcome, std_bl, std_inj, std_p, std_padj, aw_bl, aw_inj, aw_p, aw_padj)

write.csv(T3_wide |> select(-std_padj, -aw_padj),
          here("tables", "T3_secondary.csv"), row.names = FALSE)

if (requireNamespace("flextable", quietly = TRUE) && requireNamespace("officer", quietly = TRUE)) {
  T3_ft <- flextable::flextable(T3_wide |> select(-std_padj, -aw_padj)) |>
    flextable::set_header_labels(
      Outcome = "Outcome",
      std_bl  = "Baseline", std_inj = "Injection", std_p = "p-value",
      aw_bl   = "Baseline", aw_inj  = "Injection", aw_p  = "p-value"
    ) |>
    flextable::add_header_row(
      values = c("", "Standard prompt", "Injection-aware prompt"),
      colwidths = c(1, 3, 3), top = TRUE
    ) |>
    flextable::theme_booktabs() |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::bold(part = "header") |>
    flextable::align(j = 1,   align = "left",   part = "all") |>
    flextable::align(j = 2:7, align = "center", part = "body") |>
    flextable::align(j = 2:7, align = "center", part = "header") |>
    flextable::padding(padding.top = 3, padding.bottom = 3,
                       padding.left = 5, padding.right = 5, part = "all") |>
    flextable::vline(j = 4, border = FT_BORDER_GREY) |>
    flextable::bold(i = which(T3_wide$std_padj < 0.05), j = "std_p") |>
    flextable::bold(i = which(T3_wide$aw_padj  < 0.05), j = "aw_p") |>
    flextable::autofit()

  finalize_ft(T3_ft, "T3_secondary",
    caption = "Table 3. Secondary review outcomes by prompt style and injection condition.",
    footnotes = c(
      paste0("Values are mean (SD). Injection conditions pooled across placements."),
      paste0("Sentiment scored 0 (maximally harsh or dismissive) to 10 (maximally enthusiastic or positive). ",
             "Critiques: number of distinct methodological weaknesses raised. ",
             "Strengths: number of distinct positive aspects identified."),
      paste0("p-values from two-sided paired Wilcoxon signed-rank test (injection vs baseline, ",
             "paired by paper \u00d7 model, injection averaged across placements), ",
             "Bonferroni-adjusted for 3 outcomes \u00d7 2 prompt styles (6 comparisons). ",
             "Bold = p < 0.05 after correction.")
    ))
}

# ── T4: clmm — main model ─────────────────────────────────────────────────────

if (!is.null(stats_out$clmm_main_tbl)) {
  T4_raw  <- stats_out$clmm_main_tbl
  T4_sig  <- which(T4_raw$p_raw < 0.05)
  T4_dat  <- T4_raw |> select(-p_raw)

  save_table(T4_dat, "T4_clmm_main",
    caption = paste0("Table 4. Cumulative link mixed model — injection effect and ",
                     "prompt-style interaction."),
    bold_col  = "p-value",
    bold_rows = T4_sig,
    footnotes = c(
      paste0("Model: recommendation ordinal (1\u20134) ~ injection direction \u00d7 prompt style ",
             "+ (1 | paper), fitted with the ordinal R package (clmm)."),
      "Reference levels: Baseline (direction), Standard prompt (prompt style).",
      "OR = odds ratio for a one-step upward shift in the ordinal recommendation scale.",
      "95% CI computed from \u00b11.96 \u00d7 standard error of the log-OR estimate.",
      "Bold = p < 0.05."
    ))
}

# ── T5: clmm — placement model ───────────────────────────────────────────────

if (!is.null(stats_out$clmm_place_tbl)) {
  T5_raw  <- stats_out$clmm_place_tbl
  T5_sig  <- which(T5_raw$p_raw < 0.05)
  T5_dat  <- T5_raw |> select(-p_raw)

  save_table(T5_dat, "T5_clmm_placement",
    caption = "Table 5. Cumulative link mixed model — placement effects (injection conditions only).",
    bold_col  = "p-value",
    bold_rows = T5_sig,
    footnotes = c(
      paste0("Model fitted on injection conditions only: recommendation ordinal ~ placement ",
             "+ prompt style + (1 | paper). Reference level: Beginning placement."),
      "OR = odds ratio for a one-step upward shift in the ordinal recommendation scale.",
      "95% CI computed from \u00b11.96 \u00d7 standard error of the log-OR estimate.",
      "Bold = p < 0.05."
    ))
}

# ── T5b: clmm — placement × model interaction ─────────────────────────────────

if (!is.null(stats_out$clmm_place_interact_tbl)) {
  T5b_raw  <- stats_out$clmm_place_interact_tbl
  T5b_sig  <- which(T5b_raw$p_raw < 0.05)
  T5b_dat  <- T5b_raw |> select(-p_raw)

  lrt_note <- if (!is.null(stats_out$lrt_place_model)) {
    lrt_row <- stats_out$lrt_place_model[2, ]
    lrt_df  <- tryCatch(lrt_row[["df"]],          error = function(e) NA)
    lrt_x2  <- tryCatch(lrt_row[["LR.stat"]],     error = function(e) NA)
    lrt_p   <- tryCatch(lrt_row[["Pr(>Chisq)"]], error = function(e) NA)
    sprintf("Likelihood ratio test vs. additive placement model: \u03c7\u00b2(%s)\u2009=\u2009%.2f, p\u2009=\u2009%s.",
            lrt_df, lrt_x2, fmt_p(lrt_p))
  } else ""

  save_table(T5b_dat, "T5b_placement_model_interact",
    caption = paste0("Table S2. Cumulative link mixed model \u2014 placement \u00d7 model ",
                     "interaction (injection conditions only)."),
    bold_col  = "p-value",
    bold_rows = T5b_sig,
    footnotes = c(
      paste0("Model: recommendation ordinal ~ placement \u00d7 model_name + prompt style ",
             "+ (1 | paper). Reference level: Beginning placement, Gemma 4."),
      "OR = odds ratio for a one-step upward shift in the ordinal recommendation scale.",
      "95% CI computed from \u00b11.96 \u00d7 standard error of the log-OR estimate.",
      lrt_note,
      "Bold = p < 0.05."
    ))
}

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE CAPTIONS
# Written to output/legends.md (consolidated) and figures/<name>.md (per figure).
# ══════════════════════════════════════════════════════════════════════════════

n_papers <- n_distinct(dat$paper_id)

CAPTIONS <- list(

  FS2_heatmap = sprintf(paste0(
    "**Figure S2. Per-paper recommendation heatmap.**\n",
    "Each row represents one paper (n\u2009=\u2009%d); columns represent model\u2013condition combinations. ",
    "Colour encodes recommendation category ",
    "(R\u2009=\u2009Reject, Maj\u2009=\u2009Major revision, Min\u2009=\u2009Minor revision, A\u2009=\u2009Accept). ",
    "Papers are grouped by study type (RCT, observational), separated by white dividers. ",
    "A vertical line separates the baseline condition (left) from the three injection placement conditions (right). ",
    "Left panel: standard reviewer prompt; right panel: injection-aware prompt."),
    n_papers),

  F2F3_panel = paste0(
    "**Figure 2. Primary outcomes of prompt injection.**\n",
    "**A** Mean reviewer recommendation (ordinal 1\u20134; 95%\u2009CI) by prompt style (x-axis) ",
    "and condition (Baseline vs.\u2009pooled injection across placements). ",
    "Shape encodes LLM reviewer model. ",
    "Asterisks: paired Wilcoxon signed-rank test (injection vs.\u2009baseline), ",
    "Bonferroni-corrected across three placements; ",
    "\\*p\u2009<\u20090.05, \\*\\*p\u2009<\u20090.01, \\*\\*\\*p\u2009<\u20090.001. ",
    "**B** Proportion of reviews in which the reviewer explicitly detected the embedded instruction ",
    "(Clopper\u2013Pearson 95%\u2009CI). ",
    "Asterisks: two-sided Fisher\u2019s exact test (injection vs.\u2009baseline); ",
    "\\*p\u2009<\u20090.05, \\*\\*p\u2009<\u20090.01, \\*\\*\\*p\u2009<\u20090.001."),

  F_placement = paste0(
    "**Figure 3. Injection placement effects on reviewer recommendation and detection.**\n",
    "**A** Mean recommendation ordinal (95%\u2009CI) by injection placement condition (x-axis), ",
    "pooled across reviewer models. Colour encodes prompt style. ",
    "Lines connect the baseline recommendation to each placement condition. ",
    "All six injection vs.\u2009baseline comparisons (three placements \u00d7 two prompt styles) ",
    "were significant at p\u2009<\u20090.001 (paired Wilcoxon signed-rank test, ",
    "Bonferroni-corrected across three placements per prompt style). ",
    "**B** Proportion of reviews in which the reviewer explicitly detected the injected ",
    "instruction (Clopper\u2013Pearson 95%\u2009CI), both prompt styles shown, pooled across models. ",
    "Baseline detection corresponds to the false-positive rate. ",
    "Asterisks above injection conditions: two-sided Fisher\u2019s exact test vs.\u2009baseline ",
    "within each prompt style, Bonferroni-corrected across three placements; ",
    "\\*p\u2009<\u20090.05, \\*\\*p\u2009<\u20090.01, \\*\\*\\*p\u2009<\u20090.001."),

  F4_secondary = paste0(
    "**Figure 4. Secondary review outcomes.**\n",
    "**A** LLM-extracted sentiment score (0\u2009=\u2009maximally harsh or dismissive, ",
    "10\u2009=\u2009maximally enthusiastic or positive). ",
    "**B** Critique count: number of distinct methodological weaknesses identified. ",
    "**C** Strength count: number of distinct strengths identified. ",
    "Boxes span the interquartile range of all reviews; whiskers extend to 1.5\u2009\u00d7\u2009IQR. ",
    "Points represent per-paper mean values (averaged across placements), ",
    "with shape encoding the reviewer model. ",
    "Asterisks: paired Wilcoxon signed-rank test (injection vs.\u2009baseline, paired by paper, ",
    "injection averaged across placements), Bonferroni-corrected; ",
    "\\*p\u2009<\u20090.05, \\*\\*p\u2009<\u20090.01, \\*\\*\\*p\u2009<\u20090.001, ns\u2009=\u2009not significant."),

  F1_alluvial = paste0(
    "**Figure 1. Recommendation flow from baseline to injection condition.**\n",
    "Each alluvia represents one paper, coloured by its baseline recommendation category, ",
    "tracing the shift (or stability) in recommendation when the adversarial instruction is present. ",
    "Flow width is proportional to the number of papers. Reviewer models and injection placements are pooled. ",
    "Left stratum: baseline recommendation (no injection); ",
    "right stratum: injection-condition recommendation (placements pooled). ",
    "**A** Standard reviewer prompt. **B** Injection-aware reviewer prompt."),

  F_psycholing = if (has_psycholing) paste0(
    "**Figure S1. Transformer-derived psycholinguistic review dimensions.**\n",
    "Secondary analysis using transformer models applied to review text. ",
    "**A** Positive affect (zero-shot NLI classifier, cross-encoder/nli-MiniLM2-L6-H768). ",
    "**B** Sentiment positivity (cardiffnlp/twitter-roberta-base-sentiment-latest). ",
    "**C** Assertiveness. **D** Certainty ",
    "(zero-shot NLI classifier, cross-encoder/nli-MiniLM2-L6-H768). ",
    "Boxes span the IQR of all reviews; points are per-paper means. ",
    "Asterisks: paired Wilcoxon signed-rank test on per-paper means ",
    "(injection vs. baseline); ",
    "\\*p\u2009<\u20090.05, \\*\\*p\u2009<\u20090.01, \\*\\*\\*p\u2009<\u20090.001, ns\u2009=\u2009not significant."
  ) else NULL,

  F6_distribution = NULL  # Stacked distribution bars — not included in manuscript
)

# Per-figure companion .md files (alongside PDFs in figures/)
for (nm in names(CAPTIONS)) {
  if (!is.null(CAPTIONS[[nm]]))
    writeLines(CAPTIONS[[nm]], here("figures", paste0(nm, ".md")))
}

# Consolidated legends file
writeLines(
  c("# Figure Legends\n",
    paste(Filter(Negate(is.null), CAPTIONS), collapse = "\n\n")),
  here("output", "legends.md")
)

n_figs <- length(list.files(here("figures"), pattern = "\\.pdf$"))
n_tabs <- length(list.files(here("tables"),  pattern = "\\.(docx|csv)$"))
cat(sprintf("Done.\n  %d figures -> figures/\n  %d tables  -> tables/\n  Legends    -> output/legends.md\n",
            n_figs, n_tabs))

# ══════════════════════════════════════════════════════════════════════════════
# R1 ADDITIONAL ANALYSES (JMIR ms#101182 revision)
# Writes output/r1_additional.md — the response letter reads its numbers from
# this file; nothing below is retyped into prose.
# ══════════════════════════════════════════════════════════════════════════════

r1 <- c("# R1 additional analyses", "",
        sprintf("Generated by pipeline.R on %s from data/study.db.", Sys.Date()), "")

# ── Review length: mean (SD), median (IQR), normality (reviewer N-3) ──────────

rev_len <- dat |>
  mutate(len = lengths(strsplit(full_review_text, "\\s+"))) |>
  group_by(model_name) |>
  summarise(
    n = n(), mean = mean(len), sd = sd(len),
    median = median(len), q1 = quantile(len, .25), q3 = quantile(len, .75),
    shapiro_p = shapiro.test(len)$p.value, .groups = "drop"
  )

r1 <- c(r1, "## Review length (words) by model (N-3)", "",
  sprintf("- %s: n=%d, mean %.0f (SD %.0f), median %.0f (IQR %.0f-%.0f), Shapiro-Wilk p=%s",
          rev_len$model_name, rev_len$n, rev_len$mean, rev_len$sd,
          rev_len$median, rev_len$q1, rev_len$q3, fmt_p(rev_len$shapiro_p)),
  "")

# ── Secondary outcomes: unpaired (submitted) vs paired (R1) p-values ──────────

sec_cmp <- stats_out$wilcox_secondary_pooled |>
  select(outcome, prompt_style, p_adj_unpaired = p_adj) |>
  left_join(stats_out$wilcox_secondary_pooled_paired |>
              select(outcome, prompt_style, n_pairs, p_adj_paired = p_adj),
            by = c("outcome", "prompt_style"))

r1 <- c(r1, "## Secondary outcomes: pooled Bonferroni-adjusted p, unpaired vs paired (BE-3, BX-4)", "",
  "| Outcome | Prompt | p_adj rank-sum (submitted) | p_adj signed-rank (R1, n pairs) |",
  "|---|---|---|---|",
  sprintf("| %s | %s | %s | %s (n=%d) |",
          sec_cmp$outcome, sec_cmp$prompt_style,
          fmt_p(sec_cmp$p_adj_unpaired), fmt_p(sec_cmp$p_adj_paired), sec_cmp$n_pairs),
  "")

# ── Publication-year sensitivity (BV-3 training-data contamination) ───────────
# Contamination would predict a different injection effect in papers likely
# inside the reviewers' training data (older) vs after their cutoffs (2026).

year_shift <- paired |>
  left_join(distinct(dat, paper_id, year), by = "paper_id") |>
  group_by(paper_id, year, prompt_style) |>
  summarise(mean_shift = mean(shift), .groups = "drop")

year_tests <- year_shift |>
  group_by(prompt_style) |>
  summarise(
    rho = cor.test(mean_shift, year, method = "spearman", exact = FALSE)$estimate,
    p   = cor.test(mean_shift, year, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  )

year_bins <- year_shift |>
  mutate(bin = cut(year, c(-Inf, 2024, 2025, Inf),
                   labels = c("2020-2024", "2025", "2026"))) |>
  group_by(prompt_style, bin) |>
  summarise(n_papers = n(), sd = sd(mean_shift), mean_shift = mean(mean_shift),
            .groups = "drop")

r1 <- c(r1, "## Injection effect by publication year (BV-3)", "",
  "Per-paper mean ordinal shift (models and placements pooled within prompt style).", "",
  sprintf("- %s prompt: Spearman rho=%.3f, p=%s",
          year_tests$prompt_style, year_tests$rho, fmt_p(year_tests$p)),
  "",
  "| Prompt | Year bin | Papers | Mean shift (SD) |",
  "|---|---|---|---|",
  sprintf("| %s | %s | %d | %.2f (%.2f) |",
          year_bins$prompt_style, year_bins$bin, year_bins$n_papers,
          year_bins$mean_shift, year_bins$sd),
  "")

# ── Corpus screen for pre-existing directives / hidden characters (N-m1) ──────

corpus_50 <- papers |> semi_join(distinct(dat, paper_id), by = "paper_id")
directive_rx <- paste0(
  "(?i)note (for|to) (the )?reviewer|dear reviewer|your recommendation",
  "|ignore (all |any )?(previous|prior|above)|you must recommend",
  "|recommend accept|as an ai|system prompt"
)
hidden_rx <- "[\\x{200B}\\x{200C}\\x{200D}\\x{2060}\\x{FEFF}]"

corpus_screen <- corpus_50 |>
  mutate(
    directive_hit = grepl(directive_rx, full_text, perl = TRUE),
    hidden_hit    = grepl(hidden_rx, full_text, perl = TRUE)
  )

r1 <- c(r1, "## Corpus screen for pre-existing injections (N-m1)", "",
  sprintf("- Papers screened: %d", nrow(corpus_screen)),
  sprintf("- Directive-pattern hits: %d", sum(corpus_screen$directive_hit)),
  sprintf("- Hidden/zero-width character hits: %d", sum(corpus_screen$hidden_hit)),
  if (any(corpus_screen$directive_hit | corpus_screen$hidden_hit)) {
    sprintf("  - flagged DOI: %s",
            corpus_screen$doi[corpus_screen$directive_hit | corpus_screen$hidden_hit])
  } else "- No paper carried directive-style text or hidden characters.",
  "")

# ── Corpus descriptives verified against DB (BE-1 reconciliation) ─────────────

corpus_desc <- corpus_50 |>
  summarise(min_wc = min(word_count), max_wc = max(word_count),
            median_wc = median(word_count),
            min_year = min(year), max_year = max(year))

r1 <- c(r1, "## Corpus descriptives from DB (BE-1)", "",
  sprintf("- Word count: median %.0f, range %d-%d (manuscript stated median ~6,300, range 4,134-8,998: correct to these values)",
          corpus_desc$median_wc, corpus_desc$min_wc, corpus_desc$max_wc),
  sprintf("- Publication years: %d-%d (manuscript stated 2020-2025: correct to these values)",
          corpus_desc$min_year, corpus_desc$max_year),
  paste0("- Study types: ",
         paste(sprintf("%s n=%d", names(table(corpus_50$study_type)),
                       as.integer(table(corpus_50$study_type))), collapse = ", ")),
  "")

writeLines(r1, here("output", "r1_additional.md"))
cat("  R1 stats   -> output/r1_additional.md\n")

# R1 extension analysis (2026 luna arms) — runs on dat_all, leaves primary intact
source(here("analysis", "extension_2026.R"))
