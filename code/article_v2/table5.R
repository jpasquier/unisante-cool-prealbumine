# ╭───────────────────────────────────────────────────────────────────────────╮
# │   Table 5, Panel A: Association between prealbumin and body composition   │
# ╰───────────────────────────────────────────────────────────────────────────╯

suppressPackageStartupMessages({
    library(dplyr)
    library(here)
    library(knitr)
    library(lubridate)
    library(purrr)
    library(tibble)
    library(writexl)
})

# Set project root directory
i_am("code/article_v2/table5.R")

output_dir <- here("output", "article_v2")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load data
load(here("data/data_july_2026.rda"))

# Prepare cross-sectional analysis variables
cs <- cs |>
    rename_with(~ sub("^DX[0-9]{2}_", "", .x)) |>
    mutate(
        Gender = factor(Gender, levels = c("F", "M")),
        FU_CC_calc = time_length(interval(DateBS, Date), "years"),
        prealbumin_decrease_005 = -preAlb / 0.05,
        LMTot = LMTot / 1000
    )

# Outcomes and displayed precision
outcome_spec <- tribble(
    ~label,                           ~variable,     ~digits,
    "Total lean mass, kg",           "LMTot",              2,
    "Lean mass, % of body weight",   "LMTot-pc",           2,
    "ALMI, kg/m²",                   "ALMI",               2,
    "LMI, kg/m²",                    "LMI",                2,
    "ALM / body weight",             "ALM/weight",         4,
    "FMI, kg/m²",                    "FMI",                2
)

# Models specification
model_spec <- list(
    "Model 1: adjusted for sex" = "Gender",
    "Model 2: additionally adjusted for age and time since surgery" =
        c("Gender", "age", "FU_CC_calc")
)

# Helpers: format effects, confidence intervals and partial correlations
format_signed <- function(x, digits) {
    value <- sprintf(paste0("%+.", digits, "f"), x)
    gsub("-", "−", value, fixed = TRUE)
}

format_effect_ci <- function(estimate, lower, upper, digits) {
    paste0(
        format_signed(estimate, digits), " (",
        format_signed(lower, digits), " to ",
        format_signed(upper, digits), ")"
    )
}

format_partial_correlation <- function(estimate, lower, upper) {
    format_effect_ci(estimate, lower, upper, digits = 3)
}

# Model per outcome and covariates
analyse_outcome <- function(label, variable, digits, covariates) {
    formula <- reformulate(
        c("prealbumin_decrease_005", covariates),
        response = paste0("`", variable, "`")
    )
    fit <- lm(formula, data = cs)
    coefficient <- summary(fit)$coefficients["prealbumin_decrease_005", ]

    # Difference per 0.05 g/L lower prealbumin
    estimate <- unname(coefficient["Estimate"])
    standard_error <- unname(coefficient["Std. Error"])
    effect_interval <- unname(confint(fit)["prealbumin_decrease_005", ])

    # The partial correlation is reported in the natural prealbumin direction,
    # so its sign is opposite to the coefficient for lower prealbumin.
    t_statistic <- -unname(coefficient["t value"])
    residual_df <- df.residual(fit)
    partial_r <- t_statistic / sqrt(t_statistic^2 + residual_df)

    # Fisher interval for a partial correlation controlling for k covariates.
    n <- nobs(fit)
    k <- length(covariates)
    fisher_se <- 1 / sqrt(n - k - 3)
    partial_interval <- tanh(
        atanh(partial_r) + c(-1, 1) * qnorm(0.975) * fisher_se
    )

    tibble(
        Characteristic = label,
        `Difference per 0.05 g/L lower prealbumin (95% CI)` =
            format_effect_ci(
                estimate, effect_interval[1], effect_interval[2], digits
            ),
        P = sprintf("%.3f", coefficient["Pr(>|t|)"]),
        `Partial correlation (95% CI)` = format_partial_correlation(
            partial_r, partial_interval[1], partial_interval[2]
        ),
        `Excludes |r| = 0.30` = ifelse(
            partial_interval[1] > -0.30 && partial_interval[2] < 0.30,
            "Yes", "No"
        )
    )
}

table5_panel_a <- imap_dfr(model_spec, function(covariates, model_label) {
    model_rows <- pmap_dfr(
        outcome_spec,
        ~ analyse_outcome(..1, ..2, ..3, covariates)
    )

    bind_rows(
        tibble(
            Characteristic = model_label,
            `Difference per 0.05 g/L lower prealbumin (95% CI)` = "",
            P = "",
            `Partial correlation (95% CI)` = "",
            `Excludes |r| = 0.30` = ""
        ),
        model_rows
    )
})

# Display Table 5, panel A
kable(
    table5_panel_a,
    align = c("l", "c", "c", "c", "c"),
    row.names = FALSE
)

# Save Panel A in a workbook that can later receive Panels B and C
write_xlsx(
    list(`Panel A` = table5_panel_a),
    file.path(output_dir, "table5.xlsx")
)

# Session information
sink(file.path(output_dir, "table5_sessionInfo.txt"))
sessionInfo()
sink()
