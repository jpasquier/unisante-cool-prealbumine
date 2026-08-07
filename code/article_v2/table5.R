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
        low_prealbumin = preAlb < 0.20,
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
analyse_outcome <- function(label, variable, digits, covariates, data = cs) {
    formula <- reformulate(
        c("prealbumin_decrease_005", covariates),
        response = paste0("`", variable, "`")
    )
    fit <- lm(formula, data = data)
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

# ╭───────────────────────────────────────────────────────────────────────────╮
# │  Table 5, Panel B: Association with ALMI according to time since surgery  │
# ╰───────────────────────────────────────────────────────────────────────────╯

time_groups <- list(
    `3 to <5 years` = function(x) x >= 3 & x < 5,
    `5 to <10 years` = function(x) x >= 5 & x < 10,
    `≥10 years` = function(x) x >= 10
)

analyse_almi_stratum <- function(group_label, in_group) {
    d <- filter(cs, in_group(FU_CC_calc))

    analyse_outcome(
        label = paste0(group_label, " (n = ", nrow(d), ")"),
        variable = "ALMI",
        digits = 2,
        covariates = c("Gender", "age"),
        data = d
    ) |>
        select(-`Excludes |r| = 0.30`)
}

table5_panel_b <- imap_dfr(
    time_groups,
    ~ analyse_almi_stratum(..2, ..1)
)

# Display Table 5, panel B
kable(
    table5_panel_b,
    align = c("l", "c", "c", "c"),
    row.names = FALSE
)

# ╭───────────────────────────────────────────────────────────────────────────╮
# │ Table 5, Panel C: Body composition by clinical prealbumin threshold  │
# ╰────────────────────────────────────────────────────────────────────────────╯

format_mean_sd <- function(x, digits) {
    sprintf(
        paste0("%.", digits, "f (%.", digits, "f)"),
        mean(x), sd(x)
    )
}

analyse_threshold_groups <- function(label, variable, digits) {
    low <- cs[[variable]][cs$low_prealbumin]
    high <- cs[[variable]][!cs$low_prealbumin]

    formula <- reformulate(
        c("low_prealbumin", "Gender", "age", "FU_CC_calc"),
        response = paste0("`", variable, "`")
    )
    fit <- lm(formula, data = cs)
    coefficient <- summary(fit)$coefficients["low_prealbuminTRUE", ]
    estimate <- unname(coefficient["Estimate"])
    effect_interval <- unname(confint(fit)["low_prealbuminTRUE", ])

    tibble(
        Characteristic = label,
        `Prealbumin < 0.20 g/L, n = 80` = format_mean_sd(low, digits),
        `Prealbumin ≥ 0.20 g/L, n = 230` = format_mean_sd(high, digits),
        `Adjusted difference (95% CI)` = format_effect_ci(
            estimate, effect_interval[1], effect_interval[2], digits
        ),
        P = sprintf("%.3f", coefficient["Pr(>|t|)"])
    )
}

table5_panel_c <- pmap_dfr(
    outcome_spec,
    ~ analyse_threshold_groups(..1, ..2, ..3)
)

# Display Table 5, panel C
kable(
    table5_panel_c,
    align = c("l", "c", "c", "c", "c"),
    row.names = FALSE
)

# Save Panels A, B and C in separate worksheets
write_xlsx(
    list(
        `Panel A` = table5_panel_a,
        `Panel B` = table5_panel_b,
        `Panel C` = table5_panel_c
    ),
    file.path(output_dir, "table5.xlsx")
)

# Session information
sink(file.path(output_dir, "table5_sessionInfo.txt"))
sessionInfo()
sink()
