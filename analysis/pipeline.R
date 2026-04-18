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
         injection_ordinal = recommendation_ordinal, injection_rec = recommendation)

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

# Recommendation distribution as formatted text string
fmt_rec_dist <- function(recs_vec) {
  labs <- c(REJECT = "Reject", MAJOR_REVISION = "Major",
            MINOR_REVISION = "Minor", ACCEPT = "Accept")
  counts <- table(factor(as.character(recs_vec),
                         levels = names(labs)))
  n_total <- sum(counts)
  parts <- character(0)
  for (cat in names(labs)) {
    if (!is.na(counts[cat]) && counts[cat] > 0)
      parts <- c(parts,
                 sprintf("%s %d (%d%%)", labs[cat], counts[cat],
                         round(100 * counts[cat] / n_total)))
  }
  paste(parts, collapse = "; ")
}

# ══════════════════════════════════════════════════════════════════════════════
# FIGURES
# ══════════════════════════════════════════════════════════════════════════════

# ── F1: Per-paper tile chart ──────────────────────────────────────────────────

paper_order <- dat |>
  distinct(paper_id, study_type) |>
  arrange(study_type, paper_id) |>
  mutate(row_idx = row_number())

group_ends <- paper_order |>
  group_by(study_type) |>
  summarise(end_idx = max(row_idx), .groups = "drop") |>
  filter(end_idx < max(paper_order$row_idx))

study_labels <- paper_order |>
  group_by(study_type) |>
  summarise(mid_idx = mean(row_idx), .groups = "drop") |>
  mutate(study_type = recode(study_type,
    rct = "RCT", systematic_review = "Systematic\nreview",
    observational = "Observational"))

tile_dat <- dat |>
  filter(!is.na(recommendation)) |>
  left_join(paper_order, by = c("paper_id", "study_type")) |>
  mutate(prompt_long     = fct_prompt_long(as.character(prompt_style)),
         model_f         = fct_model(model_name),
         condition_label = fct_condition(as.character(direction), as.character(placement)),
         rec_label       = REC_SHORT[as.character(recommendation)])

F1 <- tile_dat |>
  ggplot(aes(x = condition_label, y = row_idx, fill = recommendation)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_vline(xintercept = 1.5, color = "grey30", linewidth = 0.9) +
  geom_hline(data = group_ends, aes(yintercept = end_idx + 0.5),
             color = "white", linewidth = 1.2, inherit.aes = FALSE) +
  facet_grid(model_f ~ prompt_long) +
  scale_fill_manual(values = REC_COLORS, labels = REC_LABELS) +
  scale_x_discrete(position = "top",
                   labels = c(Baseline  = "Baseline",
                              Beginning = "Injection\n(beginning)",
                              Middle    = "Injection\n(middle)",
                              End       = "Injection\n(end)")) +
  scale_y_continuous(breaks = study_labels$mid_idx,
                     labels = study_labels$study_type, expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(linewidth = 0))) +
  theme(axis.text.y = element_text(size = 8, color = "#555555"),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(), legend.position = "bottom",
        panel.spacing.x = unit(1.2, "lines"))

save_fig(F1, "FS2_tiles", width = 7, height = 8)

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

wilcox_pooled <- bind_rows(lapply(unique(dat$model_name), function(mn) {
  bind_rows(lapply(c("standard", "aware"), function(ps) {
    bl  <- dat |> filter(model_name == mn, prompt_style == ps, direction == "baseline",
                         !is.na(recommendation_ordinal)) |> pull(recommendation_ordinal)
    inj <- dat |> filter(model_name == mn, prompt_style == ps, direction == "favorable",
                         !is.na(recommendation_ordinal)) |> pull(recommendation_ordinal)
    if (length(bl) < 3 || length(inj) < 3) return(NULL)
    test <- wilcox.test(inj, bl, exact = FALSE)
    data.frame(model_f = fct_model(mn), prompt_f = fct_prompt(ps),
               p_raw = test$p.value, stars = pstar(test$p.value),
               stringsAsFactors = FALSE)
  }))
}))

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

# Fisher exact test: injection vs baseline detection rate per model × prompt.
det_stars_f3 <- bind_rows(lapply(unique(dat$model_name), function(mn) {
  bind_rows(lapply(c("standard", "aware"), function(ps) {
    bl  <- dat |> filter(model_name == mn, prompt_style == ps,
                         direction == "baseline",  !is.na(detected_injection))
    inj <- dat |> filter(model_name == mn, prompt_style == ps,
                         direction == "favorable", !is.na(detected_injection))
    if (nrow(bl) < 3 || nrow(inj) < 3) return(NULL)
    tbl  <- matrix(
      c(sum(inj$detected_injection),  nrow(inj) - sum(inj$detected_injection),
        sum(bl$detected_injection),   nrow(bl)  - sum(bl$detected_injection)),
      nrow = 2, byrow = TRUE)
    test <- fisher.test(tbl)
    data.frame(model_f = fct_model(mn), prompt_f = fct_prompt(ps),
               stars = pstar(test$p.value), stringsAsFactors = FALSE)
  }))
}))

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

# Pooled Wilcoxon across models (pair by paper_id × model_name)
wilcox_place_pooled <- bind_rows(lapply(c("standard", "aware"), function(ps) {
  bl_df <- dat |>
    filter(prompt_style == ps, direction == "baseline",
           !is.na(recommendation_ordinal)) |>
    select(paper_id, model_name, bl_ord = recommendation_ordinal)
  bind_rows(lapply(c("beginning", "middle", "end"), function(pl) {
    inj_df <- dat |>
      filter(prompt_style == ps, direction == "favorable",
             placement == pl, !is.na(recommendation_ordinal)) |>
      select(paper_id, model_name, inj_ord = recommendation_ordinal)
    paired_df <- inner_join(bl_df, inj_df, by = c("paper_id", "model_name"))
    if (nrow(paired_df) < 3) return(NULL)
    test <- wilcox.test(paired_df$inj_ord, paired_df$bl_ord,
                        paired = TRUE, exact = FALSE)
    data.frame(prompt_style = ps, placement = pl, p_raw = test$p.value)
  }))
})) |>
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

star_place <- wilcox_place_pooled |>
  mutate(
    prompt_long = fct_prompt_long(prompt_style),
    condition_p = fct_condition("favorable", placement)
  ) |>
  select(prompt_long, condition_p, stars)

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

# Fisher's exact test: each injection placement vs. baseline, per prompt style
fisher_det_place <- bind_rows(lapply(
  levels(det_place_agg$prompt_long),
  function(pl_long) {
    bl_row <- det_place_agg |>
      filter(as.character(prompt_long) == pl_long,
             as.character(condition_p) == "Baseline")
    bind_rows(lapply(c("Beginning", "Middle", "End"), function(cond) {
      inj_row <- det_place_agg |>
        filter(as.character(prompt_long) == pl_long,
               as.character(condition_p) == cond)
      if (nrow(inj_row) == 0 || nrow(bl_row) == 0) return(NULL)
      mat <- matrix(
        c(inj_row$detected, inj_row$n - inj_row$detected,
          bl_row$detected,  bl_row$n  - bl_row$detected),
        nrow = 2
      )
      p <- tryCatch(fisher.test(mat)$p.value, error = function(e) NA_real_)
      data.frame(
        prompt_long = factor(pl_long, levels = levels(det_place_agg$prompt_long)),
        condition_p = factor(cond, levels = levels(det_place_agg$condition_p)),
        p_raw = p, stringsAsFactors = FALSE
      )
    }))
  }
)) |>
  mutate(p_adj = p.adjust(p_raw, "bonferroni"), stars = pstar(p_adj))

star_det_place <- det_place_agg |>
  left_join(fisher_det_place, by = c("prompt_long", "condition_p")) |>
  filter(condition_p != "Baseline", replace_na(stars, "") != "")

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

secondary_long <- dat |>
  mutate(prompt_f  = fct_prompt(as.character(prompt_style)),
         model_f   = fct_model(model_name),
         dir_label = factor(ifelse(direction == "baseline", "Baseline", "Injection"),
                            levels = c("Baseline", "Injection"))) |>
  pivot_longer(cols = all_of(secondary_cols),
               names_to = "metric", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(metric    = factor(metric,
                            levels = secondary_cols,
                            labels = secondary_labels[secondary_cols]),
         is_count  = metric %in% c("Critiques (n)", "Strengths (n)"))

# Per-paper means for dot overlay (paper is the unit of analysis in clmm;
# injection conditions averaged across placements to avoid triplication).
secondary_paper_means <- dat |>
  mutate(prompt_f  = fct_prompt(as.character(prompt_style)),
         model_f   = fct_model(model_name),
         dir_label = factor(ifelse(direction == "baseline", "Baseline", "Injection"),
                            levels = c("Baseline", "Injection"))) |>
  group_by(paper_id, prompt_f, model_f, dir_label) |>
  summarise(across(all_of(secondary_cols), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  pivot_longer(cols = all_of(secondary_cols), names_to = "metric", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(metric   = factor(metric, levels = secondary_cols,
                           labels = secondary_labels[secondary_cols]),
         is_count = metric %in% c("Critiques (n)", "Strengths (n)"))

dodge_w <- 0.55

# y_pos: range-relative offset so it works for both counts and probabilities.
# scale_y_continuous below adds 18 % headroom above max to guarantee clearance.
metric_ymax <- secondary_long |>
  group_by(metric) |>
  summarise(
    y_pos = max(value, na.rm = TRUE) + 0.10 * diff(range(value, na.rm = TRUE)),
    .groups = "drop"
  )

# DEPRECATED, USED TO GO WITH THE BOXPLOT AND POINTS VERSION
# sig_f4 <- secondary_long |>
#   group_by(prompt_f, metric) |>
#   summarise(
#     pval  = wilcox.test(value[dir_label == "Baseline"],
#                         value[dir_label == "Injection"],
#                         exact = FALSE)$p.value,
#     .groups = "drop"
#   ) |>
#   left_join(metric_ymax, by = "metric") |>
#   mutate(label = case_when(pval < 0.001 ~ "***", pval < 0.01 ~ "**",
#                            pval < 0.05 ~ "*", TRUE ~ "ns"),
#          x_mid = as.numeric(prompt_f))

sig_f4 <- secondary_long |>
  group_by(prompt_f, metric) |>
  summarise(
    pval = wilcox.test(
      value[dir_label == "Baseline"],
      value[dir_label == "Injection"],
      exact = FALSE
    )$p.value,
    mean_inj = mean(value[dir_label == "Injection"], na.rm = TRUE),
    se_inj = sd(value[dir_label == "Injection"], na.rm = TRUE) /
      sqrt(sum(dir_label == "Injection")),
    .groups = "drop"
  ) |>
  mutate(
    label = case_when(
      pval < 0.001 ~ "***",
      pval < 0.01 ~ "**",
      pval < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    x_mid = as.numeric(prompt_f)
  )

f4_titles <- c(
  sentiment_score = "Sentiment (0\u201310)",
  critique_count  = "Critiques",
  strength_count  = "Strengths"
)

make_f4_panel <- function(mdat, mdat_pts, msig, title) {
  is_cnt <- isTRUE(unique(mdat$is_count)[1])
  ylab   <- if (is_cnt) "Count (n)" else "Score (0\u20131)"
  jpos   <- position_jitterdodge(
    jitter.width  = 0.14,
    dodge.width   = dodge_w,
    jitter.height = if (is_cnt) 0.15 else 0
  )

  ggplot(mdat, aes(x = prompt_f, y = value, fill = dir_label)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.45, linewidth = 0.4,
                 position = position_dodge(dodge_w)) +
    geom_point(data = mdat_pts, aes(color = dir_label, shape = model_f),
               position = jpos, size = 1.5, alpha = 0.55) +
    geom_text(data = msig, aes(x = x_mid, y = y_pos, label = label),
              inherit.aes = FALSE, size = 3.8, color = "#333333", vjust = 0) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
    scale_fill_manual(values = c(Baseline = "#BBBBBB", Injection = "#2166AC")) +
    scale_color_manual(values = c(Baseline = "#888888", Injection = "#2166AC"),
                       guide = "none") +
    scale_shape_manual(values = c("Gemma 4 (26B)" = 16, "GPT o4-mini" = 17,
                                  "Claude Sonnet" = 15)) +
    labs(x = NULL, y = ylab, fill = "Condition", shape = "Model", title = title)
}

# DEPRECATED, OLD BOXPLOT + POINTS VERSION
# make_f4_panel <- function(mdat, mdat_pts, msig, title) {
#   is_cnt <- isTRUE(unique(mdat$is_count)[1])
#   ylab   <- if (is_cnt) "Count (n)" else "Score (0\u201310)"

#   agg <- mdat |>
#     group_by(prompt_f, dir_label) |>
#     summarise(
#       mean_val = mean(value, na.rm = TRUE),
#       se       = sd(value, na.rm = TRUE) / sqrt(n()),
#       .groups  = "drop"
#     ) |>
#     mutate(ci_lo = mean_val - 1.96 * se,
#            ci_hi = mean_val + 1.96 * se)

#   ggplot(agg, aes(x = as.integer(prompt_f), y = mean_val,
#                   ymin = ci_lo, ymax = ci_hi,
#                   color = dir_label,
#                   group = dir_label)) +
#     geom_pointrange(position = position_dodge(width = 0.4),
#                     linewidth = 0.6, size = 0.45) +
#     geom_text(data = msig, aes(x = x_mid, y = y_pos, label = label),
#               inherit.aes = FALSE, size = 3.8, color = "#333333", vjust = 0) +
#     scale_x_continuous(breaks = seq_len(nlevels(agg$prompt_f)),
#                        labels = levels(agg$prompt_f)) +
#     scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
#     scale_color_manual(values = c(Baseline = "#888888", Injection = "#2166AC")) +
#     labs(x = NULL, y = ylab, color = "Condition", title = title)
# }

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
  left_join(msig |> select(prompt_f, label), by = "prompt_f") |>
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

  psycholing_long <- dat |>
    mutate(prompt_f  = fct_prompt(as.character(prompt_style)),
           model_f   = fct_model(model_name),
           dir_label = factor(ifelse(direction == "baseline", "Baseline", "Injection"),
                              levels = c("Baseline", "Injection"))) |>
    pivot_longer(cols = all_of(psycholing_cols_present),
                 names_to = "metric", values_to = "value") |>
    filter(!is.na(value)) |>
    mutate(metric   = factor(metric,
                             levels = names(psycholing_labels),
                             labels = psycholing_labels[names(psycholing_labels)]),
           is_count = FALSE)

  psycholing_paper_means <- dat |>
    mutate(prompt_f  = fct_prompt(as.character(prompt_style)),
           model_f   = fct_model(model_name),
           dir_label = factor(ifelse(direction == "baseline", "Baseline", "Injection"),
                              levels = c("Baseline", "Injection"))) |>
    group_by(paper_id, prompt_f, model_f, dir_label) |>
    summarise(across(all_of(psycholing_cols_present), \(x) mean(x, na.rm = TRUE)),
              .groups = "drop") |>
    pivot_longer(cols = all_of(psycholing_cols_present),
                 names_to = "metric", values_to = "value") |>
    filter(!is.na(value)) |>
    mutate(metric   = factor(metric,
                             levels = names(psycholing_labels),
                             labels = psycholing_labels[names(psycholing_labels)]),
           is_count = FALSE)

  psy_ymax <- psycholing_long |>
    group_by(metric) |>
    summarise(y_pos = max(value, na.rm = TRUE) +
                0.10 * diff(range(value, na.rm = TRUE)), .groups = "drop")

  sig_psy <- psycholing_long |>
    group_by(prompt_f, metric) |>
    summarise(
      pval = wilcox.test(value[dir_label == "Baseline"],
                         value[dir_label == "Injection"],
                         exact = FALSE)$p.value,
      .groups = "drop"
    ) |>
    left_join(psy_ymax, by = "metric") |>
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

  # Two-line labels keep text within stratum boxes
  REC_LABELS_2L <- c(
    REJECT         = "Reject",
    MAJOR_REVISION = "Major\nrev.",
    MINOR_REVISION = "Minor\nrev.",
    ACCEPT         = "Accept"
  )

  alluvial_dat <- paired |>
    count(prompt_long, baseline_rec, injection_rec) |>
    mutate(baseline_rec  = factor(baseline_rec,  levels = names(REC_COLORS)),
           injection_rec = factor(injection_rec, levels = names(REC_COLORS)))

  make_alluvial_panel <- function(dat, title) {
    dat |>
      ggplot(aes(axis1 = baseline_rec, axis2 = injection_rec, y = n)) +
      geom_alluvium(aes(fill = baseline_rec), alpha = 0.60, width = 0.32, knot.pos = 1/3) +
      geom_stratum(aes(fill = after_stat(stratum)), width = 0.32,
                   color = "white", linewidth = 0.7) +
      geom_text(stat = "stratum",
                aes(label = after_stat(ifelse(n >= 15,
                  dplyr::case_when(
                    as.character(stratum) == "REJECT"         ~ "Reject",
                    as.character(stratum) == "MAJOR_REVISION" ~ "Major\nrev.",
                    as.character(stratum) == "MINOR_REVISION" ~ "Minor\nrev.",
                    as.character(stratum) == "ACCEPT"         ~ "Accept",
                    TRUE ~ ""
                  ), ""))),
                size = 2.5, color = "white", fontface = "bold", lineheight = 0.85) +
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

.scalar_stats <- tibble::tibble(
  key   = c("n_papers", "n_reviews", "n_extractions_with_rec", "pct_extractions_complete",
            "baseline_mean_overall"),
  value = c(
    as.numeric(n_distinct(dat$paper_id)),
    as.numeric(nrow(dat)),
    as.numeric(sum(!is.na(dat$recommendation_ordinal))),
    round(100 * mean(!is.na(dat$recommendation_ordinal)), 1),
    round(mean(dat$recommendation_ordinal[dat$direction == "baseline"], na.rm = TRUE), 2)
  )
)

writeLines(R.version.string,
           here("output", "r_version.txt"))

.corpus_counts <- dat |>
  distinct(paper_id, study_type) |>
  dplyr::count(study_type) |>
  mutate(key = paste0("n_papers_", study_type)) |>
  select(key, value = n) |>
  mutate(value = as.numeric(value))

.det_rates <- dat |>
  group_by(prompt_style, placement) |>
  summarise(
    n        = n(),
    detected = sum(detected_injection, na.rm = TRUE),
    pct      = round(100 * mean(detected_injection, na.rm = TRUE), 1),
    .groups  = "drop"
  ) |>
  mutate(key = sprintf("detection_pct_%s_%s", prompt_style, placement)) |>
  select(key, value = pct) |>
  mutate(value = as.numeric(value))

.bl_means <- dat |>
  filter(direction == "baseline", !is.na(recommendation_ordinal)) |>
  group_by(model_name) |>
  summarise(mean_ord = round(mean(recommendation_ordinal), 2), .groups = "drop") |>
  mutate(key = paste0("baseline_mean_", gsub("[^a-zA-Z0-9]", "_", model_name))) |>
  select(key, value = mean_ord) |>
  mutate(value = as.numeric(value))

.alluvial_stats <- paired |>
  group_by(prompt_f) |>
  summarise(
    n_total  = n(),
    pct_up   = round(100 * mean(shift > 0), 1),
    pct_same = round(100 * mean(shift == 0), 1),
    pct_down = round(100 * mean(shift < 0), 1),
    .groups  = "drop"
  ) |>
  mutate(ps = ifelse(as.character(prompt_f) == "Standard", "std", "aw")) |>
  pivot_longer(cols = c(n_total, pct_up, pct_same, pct_down),
               names_to = "metric", values_to = "value") |>
  mutate(key = paste0("alluvial_", ps, "_", metric)) |>
  select(key, value) |>
  mutate(value = as.numeric(value))

# Most common transition per prompt style
.alluvial_top <- paired |>
  group_by(prompt_f, baseline_rec, injection_rec) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(prompt_f) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    ps = ifelse(as.character(prompt_f) == "Standard", "std", "aw"),
    pct = round(100 * n / sum(.alluvial_stats$value[.alluvial_stats$key == paste0("alluvial_", ps, "_n_total")]), 1)
  ) |>
  pivot_longer(cols = c(n, pct), names_to = "metric", values_to = "value") |>
  mutate(
    key = paste0("alluvial_", ps, "_top_transition_", metric),
    value = as.numeric(value)
  ) |>
  select(key, value)

# Residual injection OR under aware prompt (exp(beta_inj + beta_interaction))
.residual_OR_stat <- if (!is.null(stats_out$clmm_main)) {
  cf <- summary(stats_out$clmm_main)$coefficients
  beta_inj   <- cf["directionfavorable", "Estimate"]
  beta_inter <- cf["directionfavorable:prompt_styleaware", "Estimate"]
  tibble::tibble(key = "residual_OR_injection_under_aware",
                 value = round(exp(beta_inj + beta_inter), 2))
} else {
  tibble::tibble(key = "residual_OR_injection_under_aware", value = NA_real_)
}

# LRT placement × model interaction
.lrt_stat <- if (!is.null(stats_out$lrt_place_model)) {
  lrt_row <- stats_out$lrt_place_model[2, ]
  tibble::tibble(
    key   = c("lrt_placement_model_chisq", "lrt_placement_model_df",
              "lrt_placement_model_p"),
    value = c(
      round(tryCatch(lrt_row[["LR.stat"]], error = function(e) NA_real_), 2),
      as.numeric(tryCatch(lrt_row[["df"]],           error = function(e) NA_real_)),
      as.numeric(tryCatch(lrt_row[["Pr(>Chisq)"]],   error = function(e) NA_real_))
    )
  )
} else {
  tibble::tibble(
    key   = c("lrt_placement_model_chisq", "lrt_placement_model_df",
              "lrt_placement_model_p"),
    value = NA_real_
  )
}

text_stats <- bind_rows(
  .scalar_stats, .corpus_counts, .det_rates, .bl_means,
  .alluvial_stats, .alluvial_top, .residual_OR_stat, .lrt_stat
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

# ── T2: Primary results ───────────────────────────────────────────────────────
# One row per prompt_style × condition; Wilcoxon p-values joined from stats_out.

build_primary_tbl <- function(ps_label, ps_code, include_detection = FALSE) {
  rows <- dat |>
    filter(prompt_style == ps_code, !is.na(recommendation)) |>
    mutate(condition = fct_condition(as.character(direction), as.character(placement))) |>
    group_by(condition) |>
    summarise(
      n_reviews   = n(),
      rec_dist    = fmt_rec_dist(recommendation),
      mean_ord    = mean(recommendation_ordinal, na.rm = TRUE),
      sd_ord      = sd(recommendation_ordinal,   na.rm = TRUE),
      delta       = mean(recommendation_ordinal, na.rm = TRUE) - baseline_mean,
      det_n       = sum(detected_injection, na.rm = TRUE),
      det_pct     = round(100 * mean(detected_injection, na.rm = TRUE)),
      .groups     = "drop"
    ) |>
    mutate(
      `N`                     = n_reviews,
      `Recommendation, n (%)`  = rec_dist,
      `Mean ordinal (SD)`      = sprintf("%.1f (%.1f)", mean_ord, sd_ord),
      `Shift vs baseline`      = ifelse(condition == "Baseline", "\u2014",
                                        sprintf("%+.2f", delta))
    )

  # Join p-values — one row per placement (take most conservative across models)
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
    select(condition, p_fmt, stars)

  rows <- left_join(rows, pvals, by = "condition") |>
    mutate(`p-value` = ifelse(condition == "Baseline", "\u2014",
                              ifelse(is.na(p_fmt), "—", p_fmt)))

  out <- rows |> select(Condition = condition, N, `Recommendation, n (%)`,
                         `Mean ordinal (SD)`, `Shift vs baseline`, `p-value`)

  if (include_detection) {
    out <- rows |>
      mutate(`Detection, n (%)` = ifelse(condition == "Baseline",
               sprintf("%d (%d%%)", det_n, det_pct),
               sprintf("%d (%d%%)", det_n, det_pct))) |>
      select(Condition = condition, N, `Recommendation, n (%)`,
             `Mean ordinal (SD)`, `Shift vs baseline`,
             `Detection, n (%)`, `p-value`)
  }
  out
}

T2a <- build_primary_tbl("Standard", "standard", include_detection = FALSE)
T2b <- build_primary_tbl("Injection-aware", "aware",   include_detection = TRUE)

# Rows with significant Bonferroni-adjusted p-values (exclude baseline "—")
sig_rows_t2 <- function(tbl) {
  pv <- tbl[["p-value"]]
  which(pv == "<0.001" | (!is.na(suppressWarnings(as.numeric(pv))) &
                           suppressWarnings(as.numeric(pv)) < 0.05))
}

save_table(T2a, "T2a_standard",
  caption = "Table 2a. Reviewer recommendations by injection condition — standard prompt.",
  hline_after = 1L,
  bold_col  = "p-value",
  bold_rows = sig_rows_t2(T2a),
  footnotes = c(
    "Ordinal scale: 1 = Reject, 2 = Major revision, 3 = Minor revision, 4 = Accept.",
    "Shift vs baseline = difference in mean ordinal score relative to the no-injection condition.",
    paste0("p-values from two-sided Wilcoxon rank-sum test (injection vs no-injection baseline); ",
           "Bonferroni correction applied for three placement comparisons (adjusted threshold p < 0.017). ",
           "Bold = statistically significant after correction.")
  ))

save_table(T2b, "T2b_aware",
  caption = "Table 2b. Reviewer recommendations by injection condition — injection-aware prompt.",
  hline_after = 1L,
  bold_col  = "p-value",
  bold_rows = sig_rows_t2(T2b),
  footnotes = c(
    "Ordinal scale: 1 = Reject, 2 = Major revision, 3 = Minor revision, 4 = Accept.",
    "Shift vs baseline = difference in mean ordinal score relative to the no-injection condition.",
    "Detection: proportion of reviews in which the reviewer explicitly identified the presence of embedded instructions.",
    paste0("p-values from two-sided Wilcoxon rank-sum test (injection vs no-injection baseline); ",
           "Bonferroni correction applied for three placement comparisons (adjusted threshold p < 0.017). ",
           "Bold = statistically significant after correction.")
  ))

# ── T2 combined: side-by-side standard | aware (main publication table) ───────

compact_side <- function(ps_code) {
  rows <- dat |>
    filter(prompt_style == ps_code, !is.na(recommendation)) |>
    mutate(condition = fct_condition(as.character(direction),
                                     as.character(placement))) |>
    group_by(condition) |>
    summarise(
      n_reviews = n(),
      mean_ord  = mean(recommendation_ordinal, na.rm = TRUE),
      sd_ord    = sd(recommendation_ordinal,   na.rm = TRUE),
      delta     = mean(recommendation_ordinal, na.rm = TRUE) - baseline_mean,
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

T2a_cpt <- compact_side("standard")
T2b_cpt <- compact_side("aware")

T2_combined <- T2a_cpt |>
  select(-det) |>
  rename(std_N = n_reviews, std_mean = mean_sd,
         std_shift = shift,  std_p    = p_val) |>
  left_join(
    T2b_cpt |>
      rename(aw_N  = n_reviews, aw_mean  = mean_sd,
             aw_shift = shift,  aw_det   = det,   aw_p = p_val),
    by = "condition"
  )

write.csv(T2_combined, here("tables", "T2_primary.csv"), row.names = FALSE)

sig_std_comb <- which(T2_combined$std_p == "<0.001" |
  (!is.na(suppressWarnings(as.numeric(T2_combined$std_p))) &
    suppressWarnings(as.numeric(T2_combined$std_p)) < 0.05))
sig_aw_comb  <- which(T2_combined$aw_p  == "<0.001" |
  (!is.na(suppressWarnings(as.numeric(T2_combined$aw_p))) &
    suppressWarnings(as.numeric(T2_combined$aw_p))  < 0.05))

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

sec_cells <- dat |>
  filter(!is.na(sentiment_score)) |>
  mutate(dir_label = ifelse(direction == "baseline", "Baseline", "Injection")) |>
  group_by(prompt_style, dir_label) |>
  summarise(
    sent = sprintf("%.1f (%.1f)", mean(sentiment_score), sd(sentiment_score)),
    crit = sprintf("%.1f (%.1f)", mean(critique_count),  sd(critique_count)),
    str  = sprintf("%.1f (%.1f)", mean(strength_count),  sd(strength_count)),
    .groups = "drop"
  )

gcell3 <- function(ps, dir, col) {
  r <- sec_cells |> filter(prompt_style == ps, dir_label == dir)
  if (nrow(r) == 0) "—" else r[[col]][1]
}
gpval3 <- function(ps, out) {
  r <- stats_out$wilcox_secondary |> filter(prompt_style == ps, outcome == out)
  if (nrow(r) == 0) "—" else r$p_fmt[1]
}
gpadj3 <- function(ps, out) {
  r <- stats_out$wilcox_secondary |> filter(prompt_style == ps, outcome == out)
  if (nrow(r) == 0) 1 else r$p_adj[1]
}

T3_wide <- data.frame(
  Outcome  = c("Sentiment (0\u201310)", "Critiques, n", "Strengths, n"),
  std_bl   = c(gcell3("standard","Baseline","sent"), gcell3("standard","Baseline","crit"), gcell3("standard","Baseline","str")),
  std_inj  = c(gcell3("standard","Injection","sent"), gcell3("standard","Injection","crit"), gcell3("standard","Injection","str")),
  std_p    = c(gpval3("standard","sentiment_score"), gpval3("standard","critique_count"), gpval3("standard","strength_count")),
  std_padj = c(gpadj3("standard","sentiment_score"), gpadj3("standard","critique_count"), gpadj3("standard","strength_count")),
  aw_bl    = c(gcell3("aware","Baseline","sent"), gcell3("aware","Baseline","crit"), gcell3("aware","Baseline","str")),
  aw_inj   = c(gcell3("aware","Injection","sent"), gcell3("aware","Injection","crit"), gcell3("aware","Injection","str")),
  aw_p     = c(gpval3("aware","sentiment_score"), gpval3("aware","critique_count"), gpval3("aware","strength_count")),
  aw_padj  = c(gpadj3("aware","sentiment_score"), gpadj3("aware","critique_count"), gpadj3("aware","strength_count")),
  stringsAsFactors = FALSE
)

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
      paste0("p-values from two-sided Wilcoxon rank-sum test (injection vs baseline, pooled), ",
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

n_papers    <- n_distinct(dat$paper_id)
n_extracted <- sum(!is.na(dat$recommendation))
models_run  <- paste(unique(fct_model(dat$model_name)), collapse = ", ")

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
    "**Figure 2. Primary outcomes of adversarial prompt injection.**\n",
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
    "Asterisks: unpaired Wilcoxon rank-sum test (injection vs.\u2009baseline, placements pooled); ",
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
    "Asterisks: unpaired Wilcoxon rank-sum test; ",
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
