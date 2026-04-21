suppressPackageStartupMessages(library(ordinal))
suppressPackageStartupMessages(library(dplyr))

# ── Helpers ───────────────────────────────────────────────────────────────────

fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "—",
    p < 0.001 ~ "<0.001",
    TRUE      ~ sprintf("%.3f", p)
  )
}

pstar <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

models_in_data <- unique(as.character(dat$model_name))

# ── 1. Primary outcome: Wilcoxon rank-sum ─────────────────────────────────────
# Injection vs baseline within each model × prompt_style × placement.
# Bonferroni correction per model × prompt_style (3 comparisons each).

wilcox_primary <- bind_rows(lapply(models_in_data, function(mn) {
  bind_rows(lapply(c("standard", "aware"), function(ps) {
    bl_df <- dat |>
      filter(model_name == mn, prompt_style == ps, direction == "baseline",
             !is.na(recommendation_ordinal)) |>
      select(paper_id, bl_ord = recommendation_ordinal)
    bind_rows(lapply(c("beginning", "middle", "end"), function(pl) {
      inj_df <- dat |>
        filter(model_name == mn, prompt_style == ps, direction == "favorable",
               placement == pl, !is.na(recommendation_ordinal)) |>
        select(paper_id, inj_ord = recommendation_ordinal)
      # Inner join aligns pairs by paper; paired Wilcoxon on matched reviews
      paired_df <- inner_join(bl_df, inj_df, by = "paper_id")
      if (nrow(paired_df) < 3) return(NULL)
      test <- wilcox.test(paired_df$inj_ord, paired_df$bl_ord,
                          paired = TRUE, exact = FALSE, alternative = "two.sided")
      data.frame(model_name = mn, prompt_style = ps, placement = pl,
                 n_pairs = nrow(paired_df),
                 V = as.numeric(test$statistic), p_raw = test$p.value,
                 stringsAsFactors = FALSE)
    }))
  }))
})) |>
  group_by(model_name, prompt_style) |>
  mutate(p_adj = p.adjust(p_raw, method = "bonferroni")) |>
  ungroup() |>
  mutate(p_fmt = fmt_p(p_adj), stars = pstar(p_adj))

# ── 2. Secondary outcomes: Wilcoxon rank-sum ──────────────────────────────────
# Pooled injection vs baseline within each model × prompt_style.
# Bonferroni correction across 3 outcomes × 2 prompt_styles × n_models tests.

wilcox_secondary <- bind_rows(lapply(
  c("sentiment_score", "critique_count", "strength_count"),
  function(outcome) {
    bind_rows(lapply(models_in_data, function(mn) {
      bind_rows(lapply(c("standard", "aware"), function(ps) {
        bl  <- dat |> filter(model_name == mn, prompt_style == ps,
                             direction == "baseline", !is.na(.data[[outcome]])) |>
                      pull(!!sym(outcome))
        inj <- dat |> filter(model_name == mn, prompt_style == ps,
                             direction == "favorable", !is.na(.data[[outcome]])) |>
                      pull(!!sym(outcome))
        if (length(inj) < 3 || length(bl) < 3) return(NULL)
        test <- wilcox.test(inj, bl, exact = FALSE, alternative = "two.sided")
        data.frame(outcome = outcome, model_name = mn, prompt_style = ps,
                   n_inj = length(inj), n_bl = length(bl),
                   W = as.numeric(test$statistic), p_raw = test$p.value,
                   stringsAsFactors = FALSE)
      }))
    }))
  }
)) |>
  mutate(p_adj = p.adjust(p_raw, method = "bonferroni"),
         p_fmt = fmt_p(p_adj), stars = pstar(p_adj))

wilcox_secondary_pooled <- bind_rows(lapply(
  c("sentiment_score", "critique_count", "strength_count"),
  function(outcome) {
      bind_rows(lapply(c("standard", "aware"), function(ps) {
        bl <- dat |>
          filter(
            prompt_style == ps,
            direction == "baseline",
            !is.na(.data[[outcome]])
          ) |>
          pull(!!sym(outcome))
        inj <- dat |>
          filter(
            prompt_style == ps,
            direction == "favorable",
            !is.na(.data[[outcome]])
          ) |>
          pull(!!sym(outcome))
        if (length(inj) < 3 || length(bl) < 3) {
          return(NULL)
        }
        test <- wilcox.test(inj, bl, exact = FALSE, alternative = "two.sided")
        data.frame(
          outcome = outcome,
          prompt_style = ps,
          n_inj = length(inj),
          n_bl = length(bl),
          W = as.numeric(test$statistic),
          p_raw = test$p.value,
          stringsAsFactors = FALSE
        )
      }))
  }
)) |>
  mutate(
    p_adj = p.adjust(p_raw, method = "bonferroni"),
    p_fmt = fmt_p(p_adj),
    stars = pstar(p_adj)
  )

# ── 3. clmm — primary model (H1 and H4) ──────────────────────────────────────
# model_name added as fixed effect when >1 model present.

clmm_dat <- dat |>
  filter(!is.na(recommendation_ordinal)) |>
  mutate(
    rec_ord      = factor(recommendation_ordinal, levels = 1:4, ordered = TRUE),
    direction    = relevel(factor(direction),    ref = "baseline"),
    prompt_style = relevel(factor(prompt_style), ref = "standard"),
    model_name   = factor(model_name)
  )

clmm_main_formula <- if (length(models_in_data) > 1) {
  rec_ord ~ direction * prompt_style + model_name + (1 | paper_id)
} else {
  rec_ord ~ direction * prompt_style + (1 | paper_id)
}

clmm_main <- tryCatch(
  ordinal::clmm(clmm_main_formula, data = clmm_dat),
  error = function(e) { message("[clmm_main] ", conditionMessage(e)); NULL }
)

# ── 4. clmm — placement model (H2), injection conditions only ─────────────────

clmm_place_dat <- dat |>
  filter(!is.na(recommendation_ordinal), direction == "favorable") |>
  mutate(
    rec_ord      = factor(recommendation_ordinal, levels = 1:4, ordered = TRUE),
    prompt_style = relevel(factor(prompt_style), ref = "standard"),
    placement    = factor(as.character(placement),
                          levels = c("beginning", "middle", "end")),
    model_name   = factor(model_name)
  )

clmm_place_formula <- if (length(models_in_data) > 1) {
  rec_ord ~ placement + prompt_style + model_name + (1 | paper_id)
} else {
  rec_ord ~ placement + prompt_style + (1 | paper_id)
}

clmm_place <- tryCatch(
  ordinal::clmm(clmm_place_formula, data = clmm_place_dat),
  error = function(e) { message("[clmm_place] ", conditionMessage(e)); NULL }
)

# ── 5. Format clmm tables ─────────────────────────────────────────────────────

fmt_clmm <- function(fit, term_labels) {
  if (is.null(fit)) return(NULL)
  cf <- summary(fit)$coefficients
  loc <- cf[!grepl("\\|", rownames(cf)), , drop = FALSE]
  df <- data.frame(
    term    = rownames(loc),
    OR      = exp(loc[, "Estimate"]),
    ci_lo   = exp(loc[, "Estimate"] - 1.96 * loc[, "Std. Error"]),
    ci_hi   = exp(loc[, "Estimate"] + 1.96 * loc[, "Std. Error"]),
    p_raw   = loc[, "Pr(>|z|)"],
    stringsAsFactors = FALSE, row.names = NULL
  ) |>
    mutate(
      Term          = dplyr::recode(term, !!!term_labels),
      `OR (95% CI)` = sprintf("%.2f (%.2f\u2013%.2f)", OR, ci_lo, ci_hi),
      `p-value`     = fmt_p(p_raw)
    ) |>
    select(Term, `OR (95% CI)`, `p-value`, p_raw)
  df
}

MAIN_LABELS <- c(
  directionfavorable                    = "Injection vs Baseline (standard prompt)",
  prompt_styleaware                     = "Injection-aware vs Standard prompt (baseline)",
  "directionfavorable:prompt_styleaware"= "Injection \u00d7 Aware prompt (interaction)",
  "model_nameo4-mini"                   = "GPT o4-mini vs Gemma 4 (reference)"
)

PLACE_LABELS <- c(
  placementmiddle    = "Middle vs Beginning",
  placementend       = "End vs Beginning",
  prompt_styleaware  = "Injection-aware vs Standard prompt",
  "model_nameo4-mini"= "GPT o4-mini vs Gemma 4 (reference)"
)

clmm_main_tbl  <- fmt_clmm(clmm_main,  MAIN_LABELS)
clmm_place_tbl <- fmt_clmm(clmm_place, PLACE_LABELS)

# ── 6. clmm — placement × model interaction (LRT vs additive model) ───────────
# Tests whether the placement effect on recommendation differs between models.

clmm_place_interact     <- NULL
lrt_place_model         <- NULL
clmm_place_interact_tbl <- NULL

if (length(models_in_data) > 1 && !is.null(clmm_place)) {
  PLACE_LABELS_INTERACT <- c(
    PLACE_LABELS,
    "placementmiddle:model_nameo4-mini" = "Middle \u00d7 GPT o4-mini",
    "placementend:model_nameo4-mini"    = "End \u00d7 GPT o4-mini"
  )

  clmm_place_interact <- tryCatch(
    ordinal::clmm(
      rec_ord ~ placement * model_name + prompt_style + (1 | paper_id),
      data = clmm_place_dat
    ),
    error = function(e) { message("[clmm_pmi] ", conditionMessage(e)); NULL }
  )

  lrt_place_model <- tryCatch(
    anova(clmm_place, clmm_place_interact),
    error = function(e) { message("[lrt_place_model] ", conditionMessage(e)); NULL }
  )

  clmm_place_interact_tbl <- fmt_clmm(clmm_place_interact, PLACE_LABELS_INTERACT)
}

# ── Export ────────────────────────────────────────────────────────────────────

stats_out <- list(
  wilcox_primary          = wilcox_primary,
  wilcox_secondary        = wilcox_secondary,
  wilcox_secondary_pooled = wilcox_secondary_pooled,
  clmm_main               = clmm_main,
  clmm_place              = clmm_place,
  clmm_main_tbl           = clmm_main_tbl,
  clmm_place_tbl          = clmm_place_tbl,
  clmm_place_interact     = clmm_place_interact,
  clmm_place_interact_tbl = clmm_place_interact_tbl,
  lrt_place_model         = lrt_place_model
)

lrt_msg <- if (!is.null(lrt_place_model)) {
  lrt_row <- lrt_place_model[2, ]   # second row = the more complex model
  # ordinal::anova columns: no.par, AIC, logLik, LR.stat, df, Pr(>Chisq)
  lrt_df <- tryCatch(lrt_row[["df"]], error = function(e) NA)
  lrt_x2 <- tryCatch(lrt_row[["LR.stat"]], error = function(e) NA)
  lrt_p  <- tryCatch(lrt_row[["Pr(>Chisq)"]], error = function(e) NA)
  sprintf("placement\u00d7model LRT: \u03c7\u00b2(%s)=%.2f, p=%s",
          lrt_df, lrt_x2, fmt_p(lrt_p))
} else "placement\u00d7model LRT: n/a"

message(sprintf("[stats] Wilcoxon primary: %d tests | secondary: %d tests | clmm: %s | models: %s | %s",
  nrow(wilcox_primary),
  nrow(wilcox_secondary),
  ifelse(!is.null(clmm_main), "OK", "FAILED"),
  paste(models_in_data, collapse = ", "),
  lrt_msg))
