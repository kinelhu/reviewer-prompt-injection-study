library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(here)

db_path <- here("data", "study.db")
con <- dbConnect(SQLite(), db_path)

# Load all tables
papers      <- dbReadTable(con, "papers")
conditions  <- dbReadTable(con, "conditions")
reviews     <- dbReadTable(con, "reviews")
extractions <- dbReadTable(con, "extractions")

# Psycholinguistic scores (separate track — may not exist yet)
psycholing_available <- dbExistsTable(con, "psycholing") &&
  dbGetQuery(con, "SELECT COUNT(*) AS n FROM psycholing")$n > 0
psycholing <- if (psycholing_available) dbReadTable(con, "psycholing") else NULL

dbDisconnect(con)

# Join into analysis dataset
dat <- extractions |>
  left_join(reviews,     by = "review_id") |>
  left_join(conditions,  by = "condition_id") |>
  left_join(papers,      by = "paper_id") |>
  mutate(
    recommendation = factor(
      recommendation,
      levels = c("REJECT", "MAJOR_REVISION", "MINOR_REVISION", "ACCEPT"),
      ordered = TRUE
    ),
    direction    = factor(direction,    levels = c("baseline", "favorable")),
    placement    = factor(placement,    levels = c("none", "beginning", "middle", "end")),
    length       = factor(length,       levels = c("na", "concise", "verbose")),
    prompt_style = factor(prompt_style, levels = c("standard", "aware")),
    detected_injection = as.logical(detected_injection),
    sentiment_score    = as.integer(sentiment_score),
    critique_count     = as.integer(critique_count),
    strength_count     = as.integer(strength_count)
  )

# Attach psycholinguistic scores when available
if (!is.null(psycholing)) {
  dat <- dat |>
    left_join(
      psycholing |> select(review_id, sent_positive, sent_negative, sent_neutral,
                           assertiveness, certainty, positive_affect),
      by = "review_id"
    )
  cat(sprintf("  Psycholing scores attached: %d / %d reviews\n",
              sum(!is.na(dat$sent_positive)), nrow(dat)))
}

cat(sprintf("Loaded %d extractions across %d papers, %d conditions, %d models\n",
            nrow(dat),
            n_distinct(dat$paper_id),
            n_distinct(dat$condition_id),
            n_distinct(dat$model_name)))
