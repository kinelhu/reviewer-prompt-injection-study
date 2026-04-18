library(ggplot2)

# ── Palettes ──────────────────────────────────────────────────────────────────

REC_COLORS <-
  RColorBrewer::brewer.pal(4, "Blues") |>
  setNames(c("REJECT", "MAJOR_REVISION", "MINOR_REVISION", "ACCEPT"))

#   c(
#   REJECT         = "#C75450",
#   MAJOR_REVISION = "#de672bff",
#   MINOR_REVISION = "#6BAED6",
#   ACCEPT         = "#2D6A4F"
# )


REC_LABELS <- c(
  REJECT         = "Reject",
  MAJOR_REVISION = "Major revision",
  MINOR_REVISION = "Minor revision",
  ACCEPT         = "Accept"
)

# Short labels for tile interiors
REC_SHORT <- c(
  REJECT         = "R",
  MAJOR_REVISION = "Maj",
  MINOR_REVISION = "Min",
  ACCEPT         = "A"
)

PROMPT_COLORS  <- c(Standard = "#2166AC", Aware = "#B2182B")
PROMPT_FILLS   <- c(Standard = "#6BAED6", Aware = "#FC8D59")

SHIFT_COLORS <- c(
  "Shifted up"   = "#2D6A4F",
  "No change"    = "#CCCCCC",
  "Shifted down" = "#C75450"
)

# Condition colours: baseline neutral, placements distinct
PLACE_COLORS <- c(
  "Baseline"  = "#999999",
  "Beginning" = "#1A7837",
  "Middle"    = "#7B2D8B",
  "End"       = "#D97621"
)

# Aggregate condition colours (baseline vs pooled injection)
COND_COLORS <- c("Baseline" = "#999999", "Injection" = "#2166AC")

STUDY_COLORS <- c(
  rct                = "#4D9DE0",
  systematic_review  = "#E15554",
  observational      = "#3BB273"
)

# ── Factor helpers ────────────────────────────────────────────────────────────

fct_model <- function(x) {
  factor(
    dplyr::case_when(
      grepl("gemma",  x, ignore.case = TRUE) ~ "Gemma 4 (26B)",
      grepl("o4",     x, ignore.case = TRUE) ~ "GPT o4-mini",
      grepl("sonnet", x, ignore.case = TRUE) ~ "Claude Sonnet",
      TRUE ~ x
    ),
    levels = c("Gemma 4 (26B)", "GPT o4-mini", "Claude Sonnet")
  )
}

fct_prompt <- function(x) {
  factor(
    ifelse(x == "standard", "Standard", "Aware"),
    levels = c("Standard", "Aware")
  )
}

fct_prompt_long <- function(x) {
  factor(
    ifelse(x == "standard", "Standard prompt", "Injection-aware prompt"),
    levels = c("Standard prompt", "Injection-aware prompt")
  )
}

fct_placement <- function(x) {
  factor(
    dplyr::case_when(
      x == "beginning" ~ "Beginning",
      x == "middle"    ~ "Middle",
      x == "end"       ~ "End",
      TRUE             ~ as.character(x)
    ),
    levels = c("Beginning", "Middle", "End")
  )
}

fct_condition <- function(direction, placement) {
  label <- dplyr::case_when(
    direction == "baseline"  ~ "Baseline",
    placement == "beginning" ~ "Beginning",
    placement == "middle"    ~ "Middle",
    placement == "end"       ~ "End"
  )
  factor(label, levels = c("Baseline", "Beginning", "Middle", "End"))
}

# ── ggplot2 theme ─────────────────────────────────────────────────────────────

theme_study <- function(base_size = 11) {
  theme_linedraw(base_size = base_size) +
  theme(
    # Panels — keep closed box, soften border colour
    panel.border      = element_rect(color = "#555555", linewidth = 0.5, fill = NA),
    panel.grid.major  = element_line(color = "#EEEEEE", linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    axis.line         = element_blank(),   # panel.border handles the frame
    axis.ticks        = element_line(color = "#888888", linewidth = 0.35),

    # Facets — neutral grey, no coloured border
    strip.background  = element_rect(fill = "grey92", color = NA),
    strip.text        = element_text(face = "bold", color = "grey20",
                                     size = base_size - 0.5, margin = margin(4, 4, 4, 4)),

    # Axes
    axis.text         = element_text(color = "#333333", size = base_size - 1),
    axis.title        = element_text(color = "#333333", size = base_size - 0.5),

    # Legend
    legend.position   = "right",
    legend.title      = element_text(face = "bold", size = base_size - 1.5),
    legend.text       = element_text(size = base_size - 1.5),
    legend.key.size   = unit(0.85, "lines"),
    legend.background = element_blank(),
    legend.margin     = margin(0, 0, 0, 6),

    # Margins
    plot.margin       = margin(10, 14, 8, 10),

    # Panel title (labs(title = ...))
    plot.title        = element_text(size = base_size - 0.5, color = "grey25",
                                     face = "plain", hjust = 0.5,
                                     margin = margin(0, 0, 3, 0))
  )
}

theme_set(theme_study())
