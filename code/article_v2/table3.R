# ╭───────────────────────────────────────────────────────────────────────────╮
# │          Table 3, Panel A: Detection of excessive lean-mass loss          │
# ╰───────────────────────────────────────────────────────────────────────────╯

suppressPackageStartupMessages({
    library(binom)
    library(dplyr)
    library(here)
    library(knitr)
    library(pROC)
    library(purrr)
    library(writexl)
})

# Set project root directory
i_am("code/article_v2/table3.R")

data_dir <- here("data")
output_dir <- here("output/article_v2")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load data
load(here(data_dir, "data_july_2026.rda"))

# Prepare data for analysis
#   - `%LM_Loss`: "lean mass loss" / "body weight loss" * 100
#   - `LM%MassTotL`: "lean mass loss" / "total DXA mass loss" * 100
lg <- lg |>
    filter(`FU-CC` != "0_PO") |>
    mutate(
        excessive_lean_mass_loss = `%LM_Loss` > 25,
        low_prealbumin = preAlb < 0.20,
        # Larger values of this score mean greater predicted risk.
        prealbumin_risk_score = -preAlb
    )

time_levels <- c(
    `6 months` = "1_6M",
    `1 year` = "2_1Y",
    `3 years` = "3_3Y"
)

# Wilson confidence interval for a binomial proportion
wilson_ci <- function(events, total, conf.level = 0.95) {
    binom.confint(events, total, method = "wilson")[1, c("lower", "upper")]
}

format_estimate_ci <- function(estimate, interval, digits = 2) {
    fmt <- paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)")
    sprintf(fmt, estimate, interval[1], interval[2])
}

# Maximum Youden index
maximum_youden <- function(roc_obj) {
    best <- coords(roc_obj, "best", best.method = "youden",
                   ret = c("sensitivity", "specificity"))
    (best$sensitivity + best$specificity - 1)[1]
}

analyse_assessment <- function(
    time_code = c("1_6M", "2_1Y", "3_3Y"),
    auc_ci_method = c("bootstrap", "delong")
) {
    time_code <- match.arg(time_code)
    auc_ci_method <- match.arg(auc_ci_method)

    d <- filter(lg, `FU-CC` == time_code)
    response <- d$excessive_lean_mass_loss
    test_positive <- d$low_prealbumin
    risk_score <- d$prealbumin_risk_score

    assessed <- nrow(d)
    cases <- sum(response)

    true_positive <- sum(response & test_positive)
    false_positive <- sum(!response & test_positive)
    false_negative <- sum(response & !test_positive)
    true_negative <- sum(!response & !test_positive)

    sensitivity <- true_positive / (true_positive + false_negative)
    specificity <- true_negative / (true_negative + false_positive)
    positive_predictive_value <- true_positive /
        (true_positive + false_positive)
    negative_predictive_value <- true_negative /
        (true_negative + false_negative)

    # The ROC direction is fixed a priori: lower prealbumin means higher risk.
    # Allowing pROC to choose direction automatically would flip AUCs below 0.5
    # and would instead evaluate higher prealbumin as the positive direction.
    roc_fit <- roc(
        response = response,
        predictor = risk_score,
        levels = c(FALSE, TRUE),
        direction = "<",
        quiet = TRUE
    )
    auc_interval <- ci.auc(
        roc_fit,
        method = auc_ci_method,
        boot.n = 10000,
        boot.stratified = TRUE
    )

    # Log-scale confidence interval for the positive likelihood ratio
    positive_likelihood_ratio <- sensitivity / (1 - specificity)
    se_log_lr <- sqrt((1 - sensitivity) / true_positive +
                      specificity / false_positive)
    lr_interval <- exp(
        log(positive_likelihood_ratio) + c(-1, 1) * qnorm(0.975) * se_log_lr
    )

    c(
        `Patients assessed, n` = as.character(assessed),
        `Patients meeting the criterion, n (%)` = sprintf(
            "%d (%.0f)", cases, 100 * cases / assessed
        ),
        `AUC of prealbumin (95% CI)` = format_estimate_ci(
            as.numeric(auc(roc_fit)), auc_interval[c(1, 3)]
        ),
        `Prealbumin < 0.20 g/L` = "",
        `  True positives / false positives` = sprintf(
            "%d / %d", true_positive, false_positive
        ),
        `  False negatives / true negatives` = sprintf(
            "%d / %d", false_negative, true_negative
        ),
        `  Sensitivity (95% CI)` = format_estimate_ci(
            sensitivity,
            wilson_ci(true_positive, true_positive + false_negative)
        ),
        `  Specificity (95% CI)` = format_estimate_ci(
            specificity,
            wilson_ci(true_negative, true_negative + false_positive)
        ),
        `  Positive predictive value (95% CI)` = format_estimate_ci(
            positive_predictive_value,
            wilson_ci(true_positive, true_positive + false_positive)
        ),
        `  Negative predictive value (95% CI)` = format_estimate_ci(
            negative_predictive_value,
            wilson_ci(true_negative, true_negative + false_negative)
        ),
        `  Positive likelihood ratio (95% CI)` = format_estimate_ci(
            positive_likelihood_ratio, lr_interval
        ),
        `  Maximum Youden index across all cut-offs` = sprintf(
            "%.2f", maximum_youden(roc_fit)
        )
    )
}

# A fixed seed makes the bootstrap confidence intervals reproducible.
set.seed(424242)

table3_panel_a <- map(time_levels, analyse_assessment,
                      auc_ci_method = "delong") |>
    as.data.frame(check.names = FALSE) |>
    tibble::rownames_to_column("Characteristic")

# Display Table 3, panel A
kable(table3_panel_a, align = c("l", "c", "c", "c"), row.names = FALSE)

# ╭───────────────────────────────────────────────────────────────────────────╮
# │    Table 3, Panel B:  Association with the preoperative lean mass lost    │
# ╰───────────────────────────────────────────────────────────────────────────╯

# `TLML%` is the lean mass lost since surgery, expressed as a percentage of
# preoperative lean mass. Regressions are fitted separately at each follow-up
# and adjusted for sex. The predictor is scaled so that a one-unit increase
# represents a 0.05 g/L decrease in prealbumin.
format_signed_ci <- function(estimate, lower, upper) {
    value <- sprintf("%+.2f (%+.2f to %+.2f)", estimate, lower, upper)
    gsub("-", "−", value, fixed = TRUE)
}

analyse_continuous_outcome <- function(time_code) {
    d <- lg |>
        filter(`FU-CC` == time_code) |>
        mutate(
            Gender = factor(Gender, levels = c("F", "M")),
            prealbumin_decrease_005 = -preAlb / 0.05
        )

    fit <- lm(`TLML%` ~ prealbumin_decrease_005 + Gender, data = d)
    coefficient <- summary(fit)$coefficients["prealbumin_decrease_005", ]

    # Asymptotic Wald confidence limits. P value is the two-sided t-test
    # reported by the linear model.
    # ╭──────────────────────────────────────────────────────────────────────╮
    # │ ╭──────────────────────────────────────────────────────────────────╮ │
    # │ │ ╭──────────────────────────────────────────────────────────────╮ │ │
    # │ │ │ TODO: Consider using confint(fit, "prealbumin_decrease_005") │ │ │
    # │ │ │ for profile likelihood                                       │ │ │
    # │ │ ╰──────────────────────────────────────────────────────────────╯ │ │
    # │ ╰──────────────────────────────────────────────────────────────────╯ │
    # ╰──────────────────────────────────────────────────────────────────────╯
    estimate <- unname(coefficient["Estimate"])
    standard_error <- unname(coefficient["Std. Error"])
    interval <- estimate + c(-1, 1) * qnorm(0.975) * standard_error

    c(
        `Patients assessed, n` = as.character(nobs(fit)),
        `Lean mass lost, % of preoperative value` = sprintf(
            "%.1f (%.1f)", mean(d$`TLML%`), sd(d$`TLML%`)
        ),
        `Difference per 0.05 g/L lower prealbumin, percentage points (95% CI)` =
            format_signed_ci(estimate, interval[1], interval[2]),
        `P value` = sprintf("%.3f", coefficient["Pr(>|t|)"])
    )
}

table3_panel_b <- map(time_levels, analyse_continuous_outcome) |>
    as.data.frame(check.names = FALSE) |>
    tibble::rownames_to_column("Characteristic")

# Display Table 3, panel B
kable(table3_panel_b, align = c("l", "c", "c", "c"), row.names = FALSE)

# Save both panels in one workbook
write_xlsx(
    list(`Panel A` = table3_panel_a, `Panel B` = table3_panel_b),
    file.path(output_dir, "table3.xlsx")
)

# Session information
sink(file.path(output_dir, "table3_sessionInfo.txt"))
sessionInfo()
sink()
