# ╭───────────────────────────────────────────────────────────────────────────╮
# │   Supplementary Table S1. Preoperative characteristics of patients with   │
# │   and without an available 3-year assessment.                             │
# ╰───────────────────────────────────────────────────────────────────────────╯

suppressPackageStartupMessages({
    library(dplyr)
    library(here)
    library(knitr)
    library(purrr)
    library(tibble)
    library(writexl)
})

# Set project root directory
i_am("code/article_v2/tableS1.R")

output_dir <- here("output", "article_v2")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load data
load(here("data/data_july_2026.rda"))

# Identify participants with an available 3-year assessment
subjects_with_3y_dxa <- lg |>
    filter(`FU-CC` == "3_3Y") |>
    pull(`Subject ID`)

# Prepare preoperative data from the longitudinal dataset and create variables
po <- lg |>
    filter(`FU-CC` == "0_PO") |>
    mutate(
        has_3y_dxa = `Subject ID` %in% subjects_with_3y_dxa,
        Women = Gender == "F",
        `preAlb<0.20` = preAlb < 0.20,
        CRP = coalesce(hsCRP, as.numeric(sub("^<", "", CRP))),
        LMTot = LMTot / 1000,
        FMTot = FMTot / 1000
    )

# Row specification. Section rows are retained to reproduce the manuscript.
table_spec <- tribble(
    ~label,                          ~variable,      ~statistic,   ~digits,
    "Women, n (%)",                  "Women",        "n_percent",        0,
    "Age, years",                    "CC_age",       "mean_sd",          1,
    "Height, m",                     "DX04_height",  "mean_sd",          2,
    "Body weight, kg",               "CC_weight",    "mean_sd",          1,
    "BMI, kg/m²",                    "BMIc",         "mean_sd",          1,
    "Prealbumin, g/L",               "preAlb",       "mean_sd",          3,
    "Prealbumin < 0.20 g/L, n (%)",  "preAlb<0.20",  "n_percent",        0,
    "CRP, mg/L",                     "CRP",          "median_iqr",       1,
    "Total lean mass, kg",           "LMTot",        "mean_sd",          1,
    "Lean mass, % of body weight",   "LMTot-pc",     "mean_sd",          1,
    "Total fat mass, kg",            "FMTot",        "mean_sd",          1,
    "Fat mass, % of body weight",    "FMTotpc",      "mean_sd",          1,
    "ALMI, kg/m²",                   "ALMI",         "mean_sd",          2,
    "LMI, kg/m²",                    "LMI",          "mean_sd",          2,
    "FMI, kg/m²",                    "FMI",          "mean_sd",          2,
    "ALM / body weight",             "ALM/weight",   "mean_sd",          3,
) |>
    mutate(
        test = case_when(
            variable == "preAlb<0.20" ~ "fisher",
            statistic == "n_percent" ~ "proportion",
            statistic == "median_iqr" ~ "wilcox",
            statistic == "mean_sd" ~ "welch",
            TRUE ~ NA_character_
        ),
        smd = ifelse(statistic %in% c("mean_sd", "median_iqr"), TRUE, FALSE)
    )

with_3y <- filter(po, has_3y_dxa)
without_3y <- filter(po, !has_3y_dxa)

format_summary <- function(x, statistic, digits) {
    fmt <- paste0("%.", digits, "f")

    switch(
        statistic,
        mean_sd = sprintf(
            paste0(fmt, " (", fmt, ")"),
            mean(x, na.rm = TRUE),
            sd(x, na.rm = TRUE)
        ),
        median_iqr = {
            q <- quantile(x, c(0.25, 0.50, 0.75), na.rm = TRUE)
            sprintf(paste0(fmt, " [", fmt, "–", fmt, "]"), q[2], q[1], q[3])
        },
        n_percent = sprintf(
            "%d (%.0f)",
            sum(x, na.rm = TRUE),
            100 * mean(x, na.rm = TRUE)
        )
    )
}

calculate_p <- function(variable, test) {
    x <- po[[variable]]
    group <- po$has_3y_dxa

    switch(
        test,
        welch = t.test(x ~ group)$p.value,
        wilcox = suppressWarnings(
            wilcox.test(x ~ group, exact = FALSE)$p.value),
        proportion = {
            events <- c(sum(x[group]), sum(x[!group]))
            totals <- c(sum(group), sum(!group))
            prop.test(events, totals, correct = TRUE)$p.value
        },
        fisher = fisher.test(table(group, x))$p.value
    )
}

# Signed standardized mean difference: participants with versus without a
# 3-year DXA assessment. The denominator is the square root of the unweighted
# average of the two group variances.
calculate_smd <- function(variable) {
    x1 <- with_3y[[variable]]
    x0 <- without_3y[[variable]]
    (mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) /
        sqrt((var(x1, na.rm = TRUE) + var(x0, na.rm = TRUE)) / 2)
}

format_p <- function(x) sprintf("%.2f", x)

format_smd <- function(x) {
    value <- sprintf("%+.2f", x)
    gsub("-", "−", value, fixed = TRUE)
}

table_body <- pmap_dfr(table_spec, function(label, variable, statistic,
                                             digits, test, smd) {
    tibble(
        Characteristic = label,
        `With 3-year DXA` = format_summary(
            with_3y[[variable]], statistic, digits
        ),
        `Without 3-year DXA` = format_summary(
            without_3y[[variable]], statistic, digits
        ),
        P = format_p(calculate_p(variable, test)),
        SMD = if (smd) format_smd(calculate_smd(variable)) else "—"
    )
})

tableS1 <- bind_rows(
    tibble(
        Characteristic = "n",
        `With 3-year DXA` = as.character(nrow(with_3y)),
        `Without 3-year DXA` = as.character(nrow(without_3y)),
        P = "—",
        SMD = "—"
    ),
    table_body
)

# Display Supplementary Table S1
kable(
    tableS1,
    align = c("l", "c", "c", "c", "c"),
    row.names = FALSE
)

# Save Supplementary Table S1
write_xlsx(tableS1, file.path(output_dir, "tableS1.xlsx"))

# Caption
c(
    "Preoperative characteristics of participants with and without an available
    3-year DXA assessment in the longitudinal cohort. Data are presented as
    mean (SD), median [interquartile range] or n (%), as appropriate.",
    "P values are from Welch two-sample t-tests for continuous variables, a
    Wilcoxon rank-sum test for C-reactive protein, a continuity-corrected
    two-sample test of proportions for sex, and Fisher's exact test for
    prealbumin <0.20 g/L. SMDs are signed as the group with a 3-year assessment
    minus the group without one; categorical SMDs are not reported.",
    "ALM, appendicular lean mass; ALMI, appendicular lean mass index; BMI, body
    mass index; CRP, C-reactive protein; DXA, dual-energy X-ray absorptiometry;
    FMI, fat mass index; IQR, interquartile range; LMI, lean mass index; SD,
    standard deviation; SMD, standardized mean difference."
) |>
    lapply(\(p) paste(strwrap(p), collapse = " ")) |>
    paste(collapse = "\n\n") |>
    cat(file = file.path(output_dir, "tableS1_caption.txt"))

# Session information
sink(file.path(output_dir, "tableS1_sessionInfo.txt"))
sessionInfo()
sink()
