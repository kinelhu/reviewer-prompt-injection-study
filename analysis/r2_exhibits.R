# JMIR R2 figures, read from the output of r2_analysis.R. Palette and theme from analysis/theme.R.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(ggplot2)})
source("analysis/theme.R")
SCOPE <- Sys.getenv("R2_SCOPE", "all"); DET <- Sys.getenv("R2_DET", "extractor"); OUT <- file.path("output", if (SCOPE != "all") "r2_test" else if (DET == "detector") "r2_detector" else "r2"); FIG <- file.path(OUT, "figures"); dir.create(FIG, showWarnings = FALSE)
rd <- function(f) suppressMessages(read_csv(file.path(OUT, f), show_col_types = FALSE))
LLMS <- c("Gemma 4 26B", "GPT o4-mini", "GPT-5.6-luna", "Claude Opus 5", "Qwen 3.8 27B")
pr <- function(x) factor(ifelse(x == "standard", "Standard", "Aware"), c("Standard", "Aware"))
sv <- function(p, n, w, h) { ggsave(file.path(FIG, paste0(n, ".pdf")), p, width = w, height = h); ggsave(file.path(FIG, paste0(n, ".png")), p, width = w, height = h, dpi = 300, bg = "white") }
col <- scale_color_manual(values = PROMPT_COLORS, name = "Prompt"); shp <- scale_shape_manual(values = c(Standard = 16, Aware = 17), name = "Prompt")
dg <- position_dodge(-.55); th <- theme_study() + theme(legend.position = "bottom", axis.title.y = element_blank(), strip.text.y = element_text(angle = 0, hjust = 0))
sh <- rd("shift_by_arm.csv"); ov <- rd("overall.csv"); det <- rd("detection.csv"); lv <- c(LLMS[LLMS %in% sh$llm], "Across models")
ylab <- function(x) factor(x, rev(lv))
a <- sh |> filter(main) |> transmute(row = llm, prompt, est, lo, hi, panel = "A  Recommendation shift (ordinal steps)")
a <- bind_rows(a, ov |> transmute(row = "Across models", prompt, est, lo, hi, panel = "A  Recommendation shift (ordinal steps)"))
b <- det |> filter(main, cond == "injected") |> transmute(row = llm, prompt, est = pct, lo, hi, panel = "B  Injection flagged (% of injected reviews)")
b <- bind_rows(b, rd("overall_detection.csv") |> transmute(row = "Across models", prompt, est = pct, lo, hi, panel = "B  Injection flagged (% of injected reviews)"))
f1 <- bind_rows(a, b) |> mutate(row = ylab(row), prompt = pr(prompt)) |> ggplot(aes(est, row, colour = prompt, shape = prompt)) +
  geom_vline(data = data.frame(panel = "A  Recommendation shift (ordinal steps)", x = 0), aes(xintercept = x), linetype = "dashed", colour = "grey45", linewidth = .4, inherit.aes = FALSE) +
  geom_linerange(aes(xmin = lo, xmax = hi), position = dg, linewidth = .6) + geom_point(position = dg, size = 2.2) + facet_wrap(~panel, scales = "free_x") + col + shp + labs(x = NULL) + th
sv(f1, "figure1_main", 8.6, 3.9)
dist <- rd("distribution.csv") |> filter(main) |> mutate(rec = factor(recname, c("ACCEPT", "MINOR_REVISION", "MAJOR_REVISION", "REJECT")), cond = factor(ifelse(cond == "baseline", "Baseline", "Injected"), c("Injected", "Baseline")), llm = factor(llm, LLMS), prompt = factor(ifelse(prompt == "standard", "Standard prompt", "Injection-aware prompt"), c("Standard prompt", "Injection-aware prompt"))) |> group_by(llm, prompt, cond) |> mutate(p = n / sum(n)) |> ungroup()
f2 <- ggplot(dist, aes(p, cond, fill = rec)) + geom_col(width = .78, colour = "white", linewidth = .4) +
  geom_text(aes(label = ifelse(p >= .07, n, ""), group = rec, colour = rec == "ACCEPT"), position = position_stack(vjust = .5), size = 2.8, show.legend = FALSE) +
  facet_grid(llm ~ prompt) + scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "grey15")) + scale_x_continuous(labels = scales::percent, breaks = c(0, .5, 1), expand = c(0, 0)) +
  scale_fill_manual(values = REC_COLORS, breaks = c("REJECT", "MAJOR_REVISION", "MINOR_REVISION", "ACCEPT"), labels = c("Reject", "Major revision", "Minor revision", "Accept"), name = NULL, drop = FALSE) + labs(x = NULL) + th + theme(panel.spacing.x = unit(26, "pt"))
sv(f2, "figure2_distribution", 8.6, 1.3 + 1.05 * length(unique(dist$llm)))
pl <- rd("placement.csv") |> filter(main) |> mutate(llm = factor(llm, LLMS), prompt = pr(prompt), placement = factor(tools::toTitleCase(placement), c("Beginning", "Middle", "End")))
long <- bind_rows(pl |> transmute(llm, prompt, placement, est, lo, hi, measure = "Shift (ordinal steps)"), pl |> transmute(llm, prompt, placement, est = det_pct, lo = det_lo, hi = det_hi, measure = "Flagged (%)")) |> mutate(measure = factor(measure, c("Shift (ordinal steps)", "Flagged (%)")))
f3 <- ggplot(long, aes(placement, est, colour = prompt, shape = prompt)) + geom_hline(data = data.frame(measure = factor("Shift (ordinal steps)", levels(long$measure)), y = 0), aes(yintercept = y), linetype = "dashed", colour = "grey45", linewidth = .4, inherit.aes = FALSE) + geom_linerange(aes(ymin = lo, ymax = hi), position = position_dodge(.4), linewidth = .6) + geom_point(position = position_dodge(.4), size = 2) +
  facet_grid(measure ~ llm, scales = "free_y", switch = "y") + col + shp + labs(x = NULL, y = NULL) + theme_study() + theme(legend.position = "bottom", strip.placement = "outside", panel.spacing.x = unit(10, "pt"), axis.text.x = element_text(size = rel(.85)))
sv(f3, "figure3_placement", 10.2, 4.6)
se <- rd("secondary.csv") |> filter(main) |> mutate(llm = factor(llm, rev(LLMS)), prompt = pr(prompt), outcome = factor(tools::toTitleCase(outcome), c("Sentiment", "Critique", "Strength")))
# Arrows are offset by hand: position_dodge moves the start of a segment but not its end, which slants them.
yoff <- function(llm, prompt) as.numeric(llm) + ifelse(prompt == "Standard", .17, -.17)
ysc <- function(lv) scale_y_continuous(breaks = seq_along(lv), labels = lv)
f4 <- ggplot(se, aes(y = yoff(llm, prompt), colour = prompt)) + geom_segment(aes(x = baseline, xend = injected, yend = yoff(llm, prompt)), arrow = arrow(length = unit(4, "pt")), linewidth = .6) +
  geom_point(aes(x = baseline), size = 1.4) + facet_wrap(~outcome, scales = "free_x") + ysc(levels(se$llm)) + col + labs(x = "Mean per paper, baseline to injected") + th + theme(legend.key.width = unit(32, "pt"))
sv(f4, "figure4_secondary", 8.6, 3.4)
rc <- rd("reasoning.csv") |> mutate(llm = factor(llm, LLMS), prompt = pr(prompt)) |> pivot_longer(c(off_est, on_est), names_to = "s", values_to = "est") |> mutate(s = factor(ifelse(s == "off_est", "Off", "On"), c("Off", "On")))
f5 <- ggplot(rc, aes(s, est, colour = prompt, group = prompt, shape = prompt)) + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45", linewidth = .4) + geom_line(linewidth = .6) + geom_point(size = 2.4) + facet_wrap(~llm) + col + shp + labs(x = "Reasoning", y = "Recommendation shift (ordinal steps)") + theme_study() + theme(legend.position = "bottom")
sv(f5, "figure5_reasoning", 7.6, 3.4)
# Supplement: per-paper heatmap and transformer battery, per LLM
pp <- rd("per_paper.csv") |> filter(main) |> mutate(llm = factor(llm, LLMS), prompt = factor(ifelse(prompt == "standard", "Standard prompt", "Injection-aware prompt"), c("Standard prompt", "Injection-aware prompt")), cond = factor(ifelse(direction == "baseline", "Baseline", tools::toTitleCase(placement)), c("Baseline", "Beginning", "Middle", "End")), rec = factor(recname, c("REJECT", "MAJOR_REVISION", "MINOR_REVISION", "ACCEPT")), paper = factor(paper_id))
s1 <- ggplot(pp, aes(cond, paper, fill = rec)) + geom_tile(colour = "white", linewidth = .15) + facet_grid(llm ~ prompt) + scale_fill_manual(values = REC_COLORS, labels = c(REJECT = "Reject", MAJOR_REVISION = "Major revision", MINOR_REVISION = "Minor revision", ACCEPT = "Accept"), name = NULL, drop = FALSE) + labs(x = NULL, y = "Paper") + theme_study() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "bottom", panel.grid = element_blank(), strip.text.y = element_text(angle = 0, hjust = 0))
sv(s1, "figureS_heatmap", 8.6, 11)
trf <- rd("transformer.csv") |> filter(main) |> mutate(llm = factor(llm, rev(LLMS)), prompt = pr(prompt), dimension = factor(c(sent_positive = "Sentiment positivity", assertiveness = "Assertiveness", certainty = "Certainty", positive_affect = "Positive affect")[dimension], c("Sentiment positivity", "Assertiveness", "Certainty", "Positive affect")))
s2 <- ggplot(trf, aes(y = yoff(llm, prompt), colour = prompt)) + geom_segment(aes(x = baseline, xend = injected, yend = yoff(llm, prompt)), arrow = arrow(length = unit(4, "pt")), linewidth = .6) + geom_point(aes(x = baseline), size = 1.4) + facet_wrap(~dimension, scales = "free_x", nrow = 1) + ysc(levels(trf$llm)) + col + labs(x = "Mean score per paper, baseline to injected") + th
sv(s2, "figureS_transformer", 10, 3.2)
cat("figures written to", FIG, "\n")
