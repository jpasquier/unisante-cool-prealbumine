# ╭───────────────────────────────────────────────────────────────────────────╮
# │     Table 1: Preoperative characteristics of the longitudinal cohort      │
# ╰───────────────────────────────────────────────────────────────────────────╯

# Baseline characteristics of the primary longitudinal cohort. Data are
# presented as mean (SD), median [interquartile range] or n (%), as
# appropriate. ALM, appendicular lean mass; ALMI, appendicular lean mass index;
# BMI, body mass index; CRP, C-reactive protein; DXA, dual-energy X-ray
# absorptiometry; FMI, fat mass index; LMI, lean mass index.

library(dplyr)
library(here)
library(knitr)
library(purrr)
library(tibble)
library(writexl)

# Set project root directory
i_am("code/article_v2/table1.R")

output_dir <- here("output", "article_v2")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load data
load(here("data/data_july_2026.rda"))

# Select preoperative data form the longitudinal dataset
po <- filter(lg, `FU-CC` == "0_PO")

# Prepare variables used in the table. CRP and hsCRP contain complementary
# measurements (with one overlapping observation), while DXA masses are stored
# in grams in the source data.
po <- po |>
    mutate(
        `preAlb<0.20` = preAlb < 0.20,
        CRP = coalesce(hsCRP, as.numeric(sub("^<", "", CRP))),
        LMTot = LMTot / 1000,
        FMTot = FMTot / 1000
    )

# Table specification
table_spec <- tribble(
    ~label,                          ~variable,      ~statistic,  ~digits,
    "Age, years",                    "CC_age",       "mean_sd",         1,
    "Height, m",                     "DX04_height",  "mean_sd",         2,
    "Body weight, kg",               "CC_weight",    "mean_sd",         1,
    "BMI, kg/m²",                    "BMIc",         "mean_sd",         1,
    "Prealbumin, g/L",               "preAlb",       "mean_sd",         3,
    "Prealbumin < 0.20 g/L, n (%)",  "preAlb<0.20",  "n_percent",       0,
    "CRP, mg/L",                     "CRP",          "median_iqr",      1,
    "Total lean mass, kg",           "LMTot",        "mean_sd",         1,
    "Lean mass, % of body weight",   "LMTot-pc",     "mean_sd",         1,
    "Total fat mass, kg",            "FMTot",        "mean_sd",         1,
    "Fat mass, % of body weight",    "FMTotpc",      "mean_sd",         1,
    "ALMI, kg/m²",                   "ALMI",         "mean_sd",         2,
    "LMI, kg/m²",                    "LMI",          "mean_sd",         2,
    "FMI, kg/m²",                    "FMI",          "mean_sd",         2,
    "ALM / body weight",             "ALM/weight",   "mean_sd",         3
)

# Format a single summary cell
summarize_cell <- function(x, statistic, digits, n = FALSE) {
    fmt <- paste0("%.", digits, "f")

    cell <- switch(
        statistic,
        mean_sd = sprintf(
            paste0(fmt, " (", fmt, ")"),
            mean(x, na.rm = TRUE),
            sd(x, na.rm = TRUE)
        ),
        median_iqr = {
            q <- quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
            sprintf(paste0(fmt, " [", fmt, "–", fmt, "]"), q[2], q[1], q[3])
        },
        n_percent = sprintf(
            "%d (%.0f)",
            sum(x, na.rm = TRUE),
            100 * mean(x, na.rm = TRUE)
        )
    )

    if (isTRUE(n)) cell <- paste0("[", sum(!is.na(x)), "] ", cell)

    return(cell)
}

# Summarize women, men and the complete cohort
cohorts <- list(Women = filter(po, Gender == "F"),
                Men = filter(po, Gender == "M"),
                All = po)

table1_body <- function(n = FALSE) {
    pmap_dfr(table_spec, function(label, variable, statistic, digits) {
        cells <- map_chr(cohorts, ~ summarize_cell(
            .x[[variable]], statistic, digits, n = n))
        tibble(Characteristic = label, !!!cells)
    })
}

table1 <- function(n = FALSE) {
    bind_rows(
        tibble(
            Characteristic = "n",
            Women = as.character(nrow(cohorts$Women)),
            Men = as.character(nrow(cohorts$Men)),
            All = as.character(nrow(cohorts$All))
        ),
        table1_body(n = n)
    )
}

# Display Table 1
kable(table1(n = TRUE), align = c("l", "c", "c", "c"), row.names = FALSE)

# Save Table 1 as Excel file
write_xlsx(table1(), file.path(output_dir, "table1.xlsx"))

# Session information
sink(file.path(output_dir, "table1_sessionInfo.txt"))
sessionInfo()
sink()

# Caption
"
Baseline characteristics of the primary longitudinal cohort. Data are presented
as mean (SD), median [interquartile range] or n (%), as appropriate. ALM,
appendicular lean mass; ALMI, appendicular lean mass index; BMI, body mass
index; CRP, C-reactive protein; DXA, dual-energy X-ray absorptiometry; FMI, fat
mass index; LMI, lean mass index
" |>
    strwrap() |>
    paste(collapse = " ") |>
    cat(file = file.path(output_dir, "table1_caption.txt"))
