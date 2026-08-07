# ╭───────────────────────────────────────────────────────────────────────────╮
# │   Table 4. Characteristics of the cross-sectional cohort assessed more    │
# │   than three years after Roux-en-Y gastric bypass.                        │
# ╰───────────────────────────────────────────────────────────────────────────╯

library(dplyr)
library(here)
library(knitr)
library(purrr)
library(tibble)
library(writexl)

# Set project root directory
i_am("code/article_v2/table4.R")

output_dir <- here("output", "article_v2")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load data
load(here("data/data_july_2026.rda"))

# Prepare variables used in the table
cs <- cs |>
    rename_with(~ sub("^DX[0-9]{2}_", "", .x)) |>
    mutate(
        # Calculate follow-up from the DXA assessment date.
        FU_CC_calc = as.numeric(Date - DateBS) / 365.25,
        FU_CC_3to5 = FU_CC_calc >= 3 & FU_CC_calc < 5,
        FU_CC_5to10 = FU_CC_calc >= 5 & FU_CC_calc < 10,
        FU_CC_10plus = FU_CC_calc >= 10,
        # Create a binary variable for prealbumin < 0.20 g/L.
        `preAlb<0.20` = preAlb < 0.20,
        # Prefer the hsCRP-based CRP and use CRP as fallback.
        CRP = coalesce(hsCRP, as.numeric(sub("^<", "", CRP))),
        # Prefer the creatinine-based eGFR and use GFR as fallback. Values
        # reported as >90 or >60 are represented by their stated boundary.
        eGFR = coalesce(
            as.numeric(sub("^>", "", GFRcrt)),
            as.numeric(sub("^>", "", GFR))
        ),
        # Convert DXA masses from grams to kilograms.
        LMTot = LMTot / 1000,
        FMTot = FMTot / 1000
    )


# Table specification
table_spec <- tribble(
    ~label,                             ~variable,       ~statistic,  ~digits,
    "Age, years",                       "age",           "mean_sd",         1,
    "Height, m",                        "height",        "mean_sd",         2,
    "Preoperative body weight, kg",     "weight_pOp",    "mean_sd",         1,
    "Body weight at assessment, kg",    "weight",        "mean_sd",         1,
    "BMI at assessment, kg/m²",         "BMIc",          "mean_sd",         1,
    "Total weight loss, %",             "CC_TWL%",       "mean_sd",         1,
    "{time} Median, years [IQR]",       "FU_CC_calc",    "median_iqr",      1,
    "{time} Range, years",              "FU_CC_calc",    "range",           1,
    "{time} 3 to <5 years, n (%)",      "FU_CC_3to5",    "n_percent",       0,
    "{time} 5 to <10 years, n (%)",     "FU_CC_5to10",   "n_percent",       0,
    "{time} ≥10 years, n (%)",          "FU_CC_10plus",  "n_percent",       0,
    "Prealbumin, g/L",                  "preAlb",        "mean_sd",         3,
    "Prealbumin < 0.20 g/L, n (%)",     "preAlb<0.20",   "n_percent",       0,
    "Albumin, g/L",                     "Albumine",      "mean_sd",         1,
    "CRP, mg/L",                        "CRP",           "median_iqr",      1,
    "eGFR, mL/min/1.73 m²",             "eGFR",          "mean_sd",         0,
    "Total lean mass, kg",              "LMTot",         "mean_sd",         1,
    "Lean mass, % of body weight",      "LMTot-pc",      "mean_sd",         1,
    "Total fat mass, kg",               "FMTot",         "mean_sd",         1,
    "Fat mass, % of body weight",       "FMTotpc",       "mean_sd",         1,
    "ALMI, kg/m²",                      "ALMI",          "mean_sd",         2,
    "LMI, kg/m²",                       "LMI",           "mean_sd",         2,
    "FMI, kg/m²",                       "FMI",           "mean_sd",         2,
    "ALM / body weight",                "ALM/weight",    "mean_sd",         3
) |>
    rowwise() |>
    mutate(
        label = gsub("\\{time\\}", "Time since surgery:", label),
        label = case_when(
            sum(is.na(cs[[variable]])) > 0 ~
                paste0(label, " [n = ", sum(!is.na(cs[[variable]])), "]"),
            TRUE ~ label
        )
    ) |>
    ungroup()

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
        range = {
            r <- range(x, na.rm = TRUE)
            sprintf(paste0(fmt, "–", fmt), r[1], r[2])
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
cohorts <- list(Women = filter(cs, Gender == "F"),
                Men = filter(cs, Gender == "M"),
                All = cs)

table4_body <- function(n = FALSE) {
    pmap_dfr(table_spec, function(label, variable, statistic, digits) {
        cells <- map_chr(cohorts, ~ summarize_cell(
            .x[[variable]], statistic, digits, n = n))
        tibble(Characteristic = label, !!!cells)
    })
}

table4 <- function(n = FALSE) {
    bind_rows(
        tibble(
            Characteristic = "n",
            Women = as.character(nrow(cohorts$Women)),
            Men = as.character(nrow(cohorts$Men)),
            All = as.character(nrow(cohorts$All))
        ),
        table4_body(n = n)
    )
}

# Display Table 4
kable(table4(n = TRUE), align = c("l", "c", "c", "c"), row.names = FALSE)

# Save Table 4 as Excel file
write_xlsx(table4(), file.path(output_dir, "table4.xlsx"))

# Caption
fu_time_range <- range(cs$FU_CC_calc, na.rm = TRUE) |>
    map_chr(~ sprintf("%.1f", .x))

c(
    "Data are mean (SD), except for time since surgery and C-reactive protein,
    reported as median [interquartile range] because of their skewed
    distributions, and categorical variables, reported as n (%%). SD denotes the
    dispersion of individual values, not the precision of the mean. All
    variables were complete except the estimated glomerular filtration rate,
    available for %s patients. C-reactive protein was measured on the same
    blood sample as prealbumin, using either a standard or a high-sensitivity
    assay; values reported as below the limit of detection were treated as such
    and were in all cases well below the 10 mg/L exclusion threshold.",
    "Each patient contributed a single assessment, defined as the most recent
    eligible visit performed more than three years after surgery. Women and men
    are presented separately because both prealbumin concentrations and lean
    mass indices differ by sex; no statistical comparison between sexes is
    reported, as this table is descriptive.",
    "Fifty of these patients also contributed to the longitudinal cohort,
    through earlier assessments performed within the first three years after
    surgery. No dual-energy X-ray absorptiometry examination is shared between
    the two cohorts, so that the observations analysed are distinct; the two
    cohorts are nonetheless not independent samples and the present one should
    not be regarded as an external validation cohort.",
    "Time since surgery ranged from %s to %s years, and this heterogeneity is
    addressed in the analyses by adjustment for time since surgery and by
    stratification into the three intervals shown.",
    "ALM, appendicular lean mass; ALMI, appendicular lean mass index; BMI, body
    mass index; CRP, C-reactive protein; DXA, dual-energy X-ray absorptiometry;
    eGFR, estimated glomerular filtration rate; FMI, fat mass index; IQR,
    interquartile range; LMI, lean mass index; SD, standard deviation."
) |>
    lapply(\(p) paste(strwrap(p), collapse = " ")) |>
    paste(collapse = "\n\n") |>
    sprintf(
        nrow(cs),
        fu_time_range[1],
        fu_time_range[2]
    ) |>
    cat(file = here(output_dir, "table4_caption.txt"))

# Session information
sink(file.path(output_dir, "table4_sessionInfo.txt"))
sessionInfo()
sink()
