# JMIR R2 analysis, following paper/R2/plan/analysis-plan.md and plan/analysis-plan-amendments.md (frozen 2026-11-06).
# Writes output/<dir>/values.csv and tables; exhibits come from r2_exhibits.R.
#   Rscript analysis/r2_analysis.R            all arms; stops unless every arm is fully extracted
#   R2_SCOPE=analysed Rscript analysis/r2_analysis.R   test on the four arms analysed in R1
# Seed 20261106 governs the single joint cluster bootstrap below, and nothing else resamples.
suppressMessages({library(DBI); library(RSQLite); library(dplyr); library(tidyr); library(readr)})
SCOPE <- Sys.getenv("R2_SCOPE", "all")
DET <- Sys.getenv("R2_DET", "extractor")   # Deviation 1: extractor detection. "detector" writes a comparison run to its own folder
OUT <- file.path("output", if (SCOPE != "all") "r2_test" else if (DET == "detector") "r2_detector" else "r2"); dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
B <- 10000L
ARMS <- tribble(~arm, ~llm, ~setting, ~main, ~contrast,
  "gemma4:26b", "Gemma 4 26B", "Default", TRUE, NA,
  "o4-mini", "GPT o4-mini", "Medium", TRUE, NA,
  "gpt-5.6-luna", "GPT-5.6-luna", "Off", FALSE, "off",
  "gpt-5.6-luna-medium", "GPT-5.6-luna", "Medium", TRUE, "on",
  "claude-opus-5-off", "Claude Opus 5", "Off", FALSE, "off",
  "claude-opus-5-on", "Claude Opus 5", "Adaptive", TRUE, "on",
  "qwen3.8-27b-off", "Qwen 3.8 27B", "Off", FALSE, "off",
  "qwen3.8-27b-on", "Qwen 3.8 27B", "Medium", TRUE, "on")
if (SCOPE != "all") ARMS <- filter(ARMS, arm %in% c("gemma4:26b", "o4-mini", "gpt-5.6-luna", "gpt-5.6-luna-medium"))
con <- dbConnect(SQLite(), "data/study.db", flags = SQLITE_RO)
qs <- paste0("'", ARMS$arm, "'", collapse = ",")
d <- dbGetQuery(con, sprintf("SELECT r.review_id, r.paper_id, r.model_name arm, r.full_review_text txt, c.prompt_style prompt,
  c.direction, c.placement, e.recommendation_ordinal rec, e.recommendation recname, e.detected_injection det,
  e.sentiment_score sentiment, e.critique_count critique, e.strength_count strength
  FROM reviews r JOIN conditions c USING(condition_id) LEFT JOIN extractions e USING(review_id) WHERE r.model_name IN (%s)", qs))
dbDisconnect(con)
d$words <- lengths(strsplit(trimws(d$txt), "\\s+")); d$txt <- NULL
chk <- count(d, arm, extracted = !is.na(rec))
if (nrow(d) != 400L * nrow(ARMS) || any(!chk$extracted))
  stop("Not every arm is complete and extracted: ", paste(chk$arm[!chk$extracted], chk$n[!chk$extracted], collapse = "; "), call. = FALSE)
if (anyNA(d$det)) stop("Missing detection values", call. = FALSE)
# Comparison run only; see Deviation 1 in paper/R2/plan/analysis-plan-amendments.md.
if (SCOPE == "all" && DET == "detector") {
  fs <- list.files("data/detection_v1", "^shard_.*[.]csv$", full.names = TRUE)
  dv <- bind_rows(lapply(fs, read_csv, show_col_types = FALSE, col_types = cols(.default = "c"))) |> mutate(review_id = as.integer(review_id))
  if (anyDuplicated(dv$review_id) || !setequal(dv$review_id, d$review_id)) stop("Detector output does not cover every review exactly once", call. = FALSE)
  d <- d |> select(-det) |> left_join(transmute(dv, review_id, det = as.integer(state == "IDENTIFIED")), by = "review_id")
  if (anyNA(d$det)) stop("Detector returned no valid state for ", sum(is.na(d$det)), " reviews; adjudicate them", call. = FALSE)
}
incomplete <- d |> filter(is.na(sentiment) | is.na(critique) | is.na(strength)) |> count(arm, name = "n_incomplete_secondary")
papers <- sort(unique(d$paper_id)); stopifnot(length(papers) == 50L)
val <- list(); add <- function(...) val[[length(val) + 1L]] <<- tibble(...)
base <- d |> filter(direction == "baseline") |> select(paper_id, arm, prompt, b = rec)
PR <- d |> filter(direction != "baseline") |> inner_join(base, by = c("paper_id", "arm", "prompt")) |> mutate(s = rec - b)
stopifnot(all(count(PR, arm, prompt)$n == 150L))
pp <- PR |> group_by(paper_id, arm, prompt) |> summarise(s = mean(s), up = mean(rec > b), .groups = "drop")
key <- expand_grid(arm = ARMS$arm, prompt = c("standard", "aware")) |> mutate(id = paste(arm, prompt, sep = "|"))
mat <- function(col, tb = pp) { m <- tb |> mutate(id = paste(arm, prompt, sep = "|")) |> select(paper_id, id, v = all_of(col)) |> pivot_wider(names_from = id, values_from = v) |> arrange(paper_id)
  as.matrix(m[, key$id]) }
S <- mat("s"); U <- mat("up")
pl <- PR |> group_by(paper_id, arm, prompt, placement) |> summarise(s = mean(s), .groups = "drop") |> mutate(id = paste(arm, prompt, placement, sep = "|"))
PL <- pl |> select(paper_id, id, s) |> pivot_wider(names_from = id, values_from = s) |> arrange(paper_id); PLn <- setdiff(names(PL), "paper_id"); PL <- as.matrix(PL[, PLn])
main_ids <- function(p) paste(ARMS$arm[ARMS$main], p, sep = "|")
contr <- ARMS |> filter(!is.na(contrast)) |> select(llm, arm, contrast) |> pivot_wider(names_from = contrast, values_from = arm)
set.seed(20261106)
idx <- matrix(sample.int(50L, 50L * B, replace = TRUE), nrow = 50L)
bcm <- function(M) apply(idx, 2, function(i) colMeans(M[i, , drop = FALSE]))   # one joint resample of papers
BS <- bcm(S); BP <- bcm(PL)
ci <- function(x) as.numeric(quantile(x, c(.025, .975), names = FALSE))
# Every percentage: Clopper-Pearson with papers as the unit (Amendment 5). x may be fractional when a paper contributes several reviews.
cp <- function(k, n, papers = 50L) { x <- k * papers / n; 100 * c(if (x <= 0) 0 else qbeta(.025, x, papers - x + 1), if (x >= papers) 1 else qbeta(.975, x + 1, papers - x)) }
wil <- function(x) { x <- x[x != 0]; if (!length(x)) return(c(V = NA, p = NA)); r <- suppressWarnings(wilcox.test(x, mu = 0, exact = FALSE)); c(V = unname(r$statistic), p = r$p.value) }
# 1. Shift per arm and prompt
res <- key |> rowwise() |> mutate(
  est = mean(S[, id]), lo = ci(BS[id, ])[1], hi = ci(BS[id, ])[2], pct_up = 100 * mean(U[, id]), up_lo = cp(mean(U[, id]), 1)[1], up_hi = cp(mean(U[, id]), 1)[2],
  n_up = sum(PR$rec[PR$arm == arm & PR$prompt == prompt] > PR$b[PR$arm == arm & PR$prompt == prompt]), pairs = 150L,
  n_down = sum(PR$rec[PR$arm == arm & PR$prompt == prompt] < PR$b[PR$arm == arm & PR$prompt == prompt]),
  V = wil(S[, id])["V"], p = wil(S[, id])["p"], collapsed = hi - lo == 0) |> ungroup()
res <- res |> left_join(ARMS, by = "arm") |> group_by(main) |> mutate(p_adj = ifelse(main, pmin(1, p * sum(main & !is.na(p)) ), NA)) |> ungroup()
write_csv(select(res, llm, setting, arm, prompt, main, est, lo, hi, pct_up, up_lo, up_hi, n_up, n_down, pairs, V, p, p_adj, collapsed), file.path(OUT, "shift_by_arm.csv"))
# 2. Overall row: unweighted mean of main arms
ov <- lapply(c("standard", "aware"), function(p) { ids <- main_ids(p); m <- rowMeans(S[, ids]); bm <- colMeans(BS[ids, ]); u <- rowMeans(U[, ids])
  e <- colMeans(S[, ids]); tibble(prompt = p, n_models = length(ids), est = mean(m), lo = ci(bm)[1], hi = ci(bm)[2], min = min(e), max = max(e), pct_up = 100 * mean(u)) })
write_csv(bind_rows(ov), file.path(OUT, "overall.csv"))
# 2b. Overall flag rate (Amendment 8): unweighted mean over main arms of the per-arm percentage of injected reviews flagged.
# Per-paper flag proportions resampled with the same paper draws as above, so no new randomness.
DP <- mat("dp", d |> filter(direction != "baseline") |> group_by(paper_id, arm, prompt) |> summarise(dp = mean(det), .groups = "drop")); BD <- bcm(DP)
ovd <- lapply(c("standard", "aware"), function(p) { ids <- main_ids(p); e <- 100 * colMeans(DP[, ids]); b <- 100 * colMeans(BD[ids, ]); tibble(prompt = p, n_models = length(ids), pct = mean(e), lo = ci(b)[1], hi = ci(b)[2], min = min(e), max = max(e)) })
write_csv(bind_rows(ovd), file.path(OUT, "overall_detection.csv"))
# 3. Aware minus standard, within arm
aw <- ARMS |> rowwise() |> mutate(diff = { i <- paste(arm, c("aware", "standard"), sep = "|"); mean(S[, i[1]] - S[, i[2]]) }, lo = ci(BS[paste(arm, "aware", sep = "|"), ] - BS[paste(arm, "standard", sep = "|"), ])[1], hi = ci(BS[paste(arm, "aware", sep = "|"), ] - BS[paste(arm, "standard", sep = "|"), ])[2], V = wil(S[, paste(arm, "aware", sep = "|")] - S[, paste(arm, "standard", sep = "|")])["V"], p = wil(S[, paste(arm, "aware", sep = "|")] - S[, paste(arm, "standard", sep = "|")])["p"]) |> ungroup() |> group_by(main) |> mutate(p_adj = ifelse(main, pmin(1, p * sum(main & !is.na(p))), NA)) |> ungroup()
write_csv(aw, file.path(OUT, "aware_vs_standard.csv"))
# 4. Placement
plr <- tibble(id = PLn) |> separate(id, c("arm", "prompt", "placement"), sep = "\\|", remove = FALSE) |> rowwise() |> mutate(est = mean(PL[, id]), lo = ci(BP[id, ])[1], hi = ci(BP[id, ])[2], p = wil(PL[, id])["p"]) |> ungroup() |> left_join(ARMS, by = "arm") |> mutate(p_adj = pmin(1, p * 3))
det_pl <- d |> filter(direction != "baseline") |> group_by(arm, prompt, placement) |> summarise(x = sum(det), n = n(), .groups = "drop") |> rowwise() |> mutate(det_pct = 100 * x / n, det_lo = cp(x, n)[1], det_hi = cp(x, n)[2]) |> ungroup()
write_csv(left_join(select(plr, -id), det_pl, by = c("arm", "prompt", "placement")), file.path(OUT, "placement.csv"))
# 5. Detection
dt <- d |> mutate(cond = ifelse(direction == "baseline", "baseline", "injected")) |> group_by(arm, prompt, cond) |> summarise(x = sum(det), n = n(), .groups = "drop") |> rowwise() |> mutate(pct = 100 * x / n, lo = cp(x, n)[1], hi = cp(x, n)[2]) |> ungroup() |> left_join(ARMS, by = "arm")
write_csv(dt, file.path(OUT, "detection.csv"))
# 6. Recommendation distribution
write_csv(d |> mutate(cond = ifelse(direction == "baseline", "baseline", "injected")) |> count(arm, prompt, cond, recname) |> left_join(ARMS, by = "arm"), file.path(OUT, "distribution.csv"))
# 7. Secondary, within LLM; Bonferroni across 3 x 2 x main LLMs (amendment 1)
sec <- d |> mutate(cond = ifelse(direction == "baseline", "b", "i")) |> pivot_longer(c(sentiment, critique, strength), names_to = "outcome", values_to = "v") |> group_by(paper_id, arm, prompt, outcome, cond) |> summarise(v = mean(v, na.rm = TRUE), .groups = "drop") |> pivot_wider(names_from = cond, values_from = v) |> mutate(s = i - b)
secr <- sec |> group_by(arm, prompt, outcome) |> summarise(baseline = mean(b, na.rm = TRUE), baseline_sd = sd(b, na.rm = TRUE), injected = mean(i, na.rm = TRUE), injected_sd = sd(i, na.rm = TRUE), diff = mean(s, na.rm = TRUE), V = wil(na.omit(s))["V"], p = wil(na.omit(s))["p"], .groups = "drop") |> left_join(ARMS, by = "arm")
ntests <- sum(secr$main & !is.na(secr$p)); secr <- secr |> mutate(p_adj = ifelse(main, pmin(1, p * ntests), NA), n_tests = ntests)
write_csv(secr, file.path(OUT, "secondary.csv"))
write_csv(d |> mutate(cond = ifelse(direction == "baseline", "baseline", "injected")) |> group_by(arm, prompt, cond) |> summarise(words_mean = mean(words), words_sd = sd(words), words_median = median(words), .groups = "drop") |> left_join(ARMS, by = "arm"), file.path(OUT, "review_length.csv"))
write_csv(incomplete, file.path(OUT, "incomplete_secondary.csv"))
# 7b. Transformer battery, within LLM; Bonferroni across dimensions x prompts x main LLMs
con2 <- dbConnect(SQLite(), "data/study.db", flags = SQLITE_RO); psy <- dbGetQuery(con2, "SELECT review_id, sent_positive, assertiveness, certainty, positive_affect FROM psycholing"); dbDisconnect(con2)
tr <- d |> select(review_id, paper_id, arm, prompt, direction) |> inner_join(psy, by = "review_id") |> mutate(cond = ifelse(direction == "baseline", "b", "i")) |> pivot_longer(c(sent_positive, assertiveness, certainty, positive_affect), names_to = "dimension", values_to = "v") |> group_by(paper_id, arm, prompt, dimension, cond) |> summarise(v = mean(v), .groups = "drop") |> pivot_wider(names_from = cond, values_from = v) |> mutate(s = i - b)
trr <- tr |> group_by(arm, prompt, dimension) |> summarise(baseline = mean(b), injected = mean(i), diff = mean(s), p = wil(s)["p"], .groups = "drop") |> left_join(ARMS, by = "arm")
nt <- sum(trr$main & !is.na(trr$p)); write_csv(trr |> mutate(p_adj = ifelse(main, pmin(1, p * nt), NA), n_tests = nt), file.path(OUT, "transformer.csv"))
# Per-paper recommendations for the supplement heatmaps
write_csv(d |> select(paper_id, arm, prompt, direction, placement, recname) |> left_join(ARMS, by = "arm"), file.path(OUT, "per_paper.csv"))
# Corpus and design values read by the manuscript
con3 <- dbConnect(SQLite(), "data/study.db", flags = SQLITE_RO); pap <- dbGetQuery(con3, sprintf("SELECT paper_id, study_type, word_count, year FROM papers WHERE paper_id IN (%s)", paste(papers, collapse = ","))); ncond <- dbGetQuery(con3, "SELECT COUNT(*) n FROM conditions")$n; dbDisconnect(con3)
# Publication-year sensitivity, every arm, per-paper shift. Spearman; no resampling. Not estimable where every paper shifted alike.
yrs <- pap$year[match(papers, pap$paper_id)]; stopifnot(!anyNA(yrs), nrow(S) == length(yrs), identical(as.numeric(sort(unique(PR$paper_id))), as.numeric(papers)))
ysn <- key |> rowwise() |> mutate(rho = if (sd(S[, id]) > 0) suppressWarnings(cor.test(S[, id], yrs, method = "spearman", exact = FALSE))$estimate else NA_real_, p = if (sd(S[, id]) > 0) suppressWarnings(cor.test(S[, id], yrs, method = "spearman", exact = FALSE))$p.value else NA_real_) |> ungroup() |> left_join(ARMS, by = "arm") |> mutate(n_year_max = sum(yrs == max(yrs)), year_max = max(yrs)) |> group_by(main) |> mutate(p_adj = ifelse(main, pmin(1, p * sum(main & !is.na(p))), NA)) |> ungroup()
write_csv(select(ysn, llm, setting, arm, prompt, main, rho, p, p_adj, n_year_max, year_max), file.path(OUT, "year_sensitivity.csv"))
write_csv(pap, file.path(OUT, "corpus.csv")); write_csv(tibble(n_papers = length(papers), n_conditions = ncond, n_arms = nrow(ARMS), reviews_per_arm = 400L, n_reviews = nrow(d), bootstrap = B, n_secondary_tests = ntests), file.path(OUT, "design.csv"))
# 7c. Detected vs undetected, aware prompt, injected reviews (Amendment 3). Reuses the bootstrap draw above; no new randomness.
dv <- PR |> filter(prompt == "aware") |> mutate(paper_i = match(paper_id, papers))
dvr <- bind_rows(lapply(ARMS$arm, function(a) { x <- filter(dv, arm == a); y1 <- x[x$det == 1, ]; y0 <- x[x$det == 0, ]; both <- nrow(y1) >= 5 && nrow(y0) >= 5
  sm <- function(y) { s_ <- numeric(50); n_ <- numeric(50); r <- rowsum(y$s, y$paper_i); s_[as.integer(rownames(r))] <- r[, 1]; t_ <- table(factor(y$paper_i, 1:50)); n_[] <- as.numeric(t_); list(s = s_, n = n_) }
  a1 <- sm(y1); a0 <- sm(y0); bd <- if (both) apply(idx, 2, function(i) sum(a1$s[i]) / sum(a1$n[i]) - sum(a0$s[i]) / sum(a0$n[i])) else NA
  tibble(arm = a, n_det = nrow(y1), n_undet = nrow(y0), shift_det = if (nrow(y1)) mean(y1$s) else NA, shift_undet = if (nrow(y0)) mean(y0$s) else NA, up_det = sum(y1$s > 0), up_undet = sum(y0$s > 0), diff = if (both) mean(y1$s) - mean(y0$s) else NA, lo = if (both) ci(na.omit(bd))[1] else NA, hi = if (both) ci(na.omit(bd))[2] else NA,
         p = if (both && (var(c(y1$s, y0$s)) > 0)) suppressWarnings(wilcox.test(y1$s, y0$s, exact = FALSE))$p.value else NA) })) |> left_join(ARMS, by = "arm")
dvr <- dvr |> group_by(main) |> mutate(p_adj = ifelse(main, pmin(1, p * sum(main & !is.na(p))), NA)) |> ungroup()
write_csv(dvr, file.path(OUT, "detected_vs_undetected.csv"))
o2 <- dv |> filter(arm %in% c("gemma4:26b", "o4-mini")) |> group_by(det) |> summarise(shift = mean(s), up = mean(s > 0), n = n(), .groups = "drop"); write_csv(o2, file.path(OUT, "detected_two_model_check.csv"))
# 8. Reasoning contrast, exploratory: on minus off, per paper
rc <- contr |> expand_grid(prompt = c("standard", "aware")) |> rowwise() |> mutate(off_est = mean(S[, paste(off, prompt, sep = "|")]), on_est = mean(S[, paste(on, prompt, sep = "|")]), diff = on_est - off_est,
  lo = ci(BS[paste(on, prompt, sep = "|"), ] - BS[paste(off, prompt, sep = "|"), ])[1], hi = ci(BS[paste(on, prompt, sep = "|"), ] - BS[paste(off, prompt, sep = "|"), ])[2], p = wil(S[, paste(on, prompt, sep = "|")] - S[, paste(off, prompt, sep = "|")])["p"], replicates = prompt == "aware" & diff < 0 & hi < 0) |> ungroup()
write_csv(rc, file.path(OUT, "reasoning.csv"))
tally <- rc |> filter(prompt == "aware", llm != "GPT-5.6-luna"); writeLines(sprintf("%d of %d further models", sum(tally$replicates), nrow(tally)), file.path(OUT, "replication_tally.txt"))
# 9. Two-model CLMM for the supplement, reproduces R1
cl <- d |> filter(arm %in% c("gemma4:26b", "o4-mini")) |> mutate(rec_ord = factor(rec, 1:4, ordered = TRUE), direction = relevel(factor(direction), "baseline"), prompt = relevel(factor(prompt), "standard"), model_name = factor(arm), paper_id = factor(paper_id))
m <- ordinal::clmm(rec_ord ~ direction * prompt + model_name + (1 | paper_id), data = cl); sm <- coef(summary(m)); sm <- sm[!grepl("\\|", rownames(sm)), ]
write_csv(tibble(term = rownames(sm), or = exp(sm[, 1]), lo = exp(sm[, 1] - 1.96 * sm[, 2]), hi = exp(sm[, 1] + 1.96 * sm[, 2]), p = sm[, 4]), file.path(OUT, "clmm_two_model.csv"))
# 10. Why the CLMM cannot lead: all arms, additive and full interaction, both models with Hessians where available
if (SCOPE == "all") {
  al <- d |> mutate(rec_ord = factor(rec, ordered = TRUE), direction = relevel(factor(direction), "baseline"), prompt = relevel(factor(prompt), "standard"), arm_f = factor(arm), paper_id = factor(paper_id))
  m_add <- ordinal::clmm(rec_ord ~ direction * prompt + arm_f + (1 | paper_id), data = al)
  m_int <- suppressWarnings(ordinal::clmm(rec_ord ~ direction * prompt * arm_f + (1 | paper_id), data = al))
  lrt <- as.data.frame(anova(m_add, m_int)); ci_ <- suppressWarnings(coef(summary(m_int))); ci_ <- ci_[!grepl("\\|", rownames(ci_)), ]; se_int <- ci_[, 2]; sa <- coef(summary(m_add)); write_csv(tibble(term = rownames(ci_), est = ci_[, 1], se = ci_[, 2]), file.path(OUT, "clmm_all_arms_interaction.csv"))
  # Per-arm injection OR from the interaction model, by linear contrast. Stable across reference arms and tighter control (checked 2026-11-07).
  bi <- coef(m_int); vi <- vcov(m_int); arms_i <- levels(al$arm_f); ref_i <- arms_i[1]
  con_or <- function(a, aware) { nm <- c("directionfavorable", if (a != ref_i) paste0("directionfavorable:arm_f", a), if (aware) "directionfavorable:promptaware", if (aware && a != ref_i) paste0("directionfavorable:promptaware:arm_f", a)); stopifnot(all(nm %in% names(bi))); e <- sum(bi[nm]); se <- sqrt(sum(vi[nm, nm])); tibble(arm = a, prompt = if (aware) "aware" else "standard", log_or = e, se = se, or = exp(e), lo = exp(e - 1.96 * se), hi = exp(e + 1.96 * se)) }
  write_csv(bind_rows(lapply(arms_i, con_or, FALSE), lapply(arms_i, con_or, TRUE)) |> left_join(ARMS, by = "arm"), file.path(OUT, "clmm_per_arm.csv"))
  write_csv(tibble(arms = nrow(ARMS), lr = lrt$LR.stat[2], df = lrt$df[2], p = lrt$`Pr(>Chisq)`[2], usable_se_interaction = all(is.finite(se_int)), max_abs_est = max(abs(ci_[, 1])), max_se = max(se_int), n_se_over_10 = sum(se_int > 10), n_terms = length(se_int), or_additive = exp(sa["directionfavorable", 1])), file.path(OUT, "clmm_all_arms.csv"))
}
# 11. Placement in the two original models: R1's test of hypothesis 2 across models, retained
pc2 <- d |> filter(arm %in% c("gemma4:26b", "o4-mini"), direction != "baseline") |> mutate(rec_ord = factor(rec, 1:4, ordered = TRUE), prompt = relevel(factor(prompt), "standard"), placement = factor(placement, c("beginning", "middle", "end")), model_name = factor(arm), paper_id = factor(paper_id))
m_pa <- ordinal::clmm(rec_ord ~ placement + prompt + model_name + (1 | paper_id), data = pc2); m_pi <- ordinal::clmm(rec_ord ~ placement * model_name + prompt + (1 | paper_id), data = pc2); lp <- as.data.frame(anova(m_pa, m_pi)); sp <- coef(summary(m_pa)); sp <- sp[!grepl("\\|", rownames(sp)), ]
write_csv(tibble(term = rownames(sp), or = exp(sp[, 1]), lo = exp(sp[, 1] - 1.96 * sp[, 2]), hi = exp(sp[, 1] + 1.96 * sp[, 2]), p = sp[, 4], lrt_chisq = lp$LR.stat[2], lrt_df = lp$df[2], lrt_p = lp$`Pr(>Chisq)`[2]), file.path(OUT, "clmm_placement_two_model.csv"))
# 12. Sentiment item-level correlation and length normality
con4 <- dbConnect(SQLite(), "data/study.db", flags = SQLITE_RO); sp2 <- dbGetQuery(con4, "SELECT review_id, sent_positive FROM psycholing"); dbDisconnect(con4)
cr <- d |> inner_join(sp2, by = "review_id") |> group_by(arm) |> summarise(r = cor(sentiment, sent_positive, use = "complete.obs"), shapiro_p = shapiro.test(words)$p.value, .groups = "drop") |> left_join(ARMS, by = "arm"); write_csv(cr, file.path(OUT, "sentiment_correlation.csv"))
cat("written to", OUT, "\n")
