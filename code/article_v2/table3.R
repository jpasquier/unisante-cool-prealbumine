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

    list(
        table = c(
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
        ),
        roc_fit = roc_fit,
        auc = as.numeric(auc(roc_fit)),
        auc_interval = unname(auc_interval[c(1, 3)]),
        assessed = assessed,
        cases = cases,
        threshold_point = c(
            false_positive_rate = 1 - specificity,
            sensitivity = sensitivity
        )
    )
}

# A fixed seed makes the bootstrap confidence intervals reproducible.
set.seed(424242)

assessment_results <- map(
    time_levels,
    analyse_assessment,
    auc_ci_method = "bootstrap"
)

table3_panel_a <- map(assessment_results, "table") |>
    as.data.frame(check.names = FALSE) |>
    tibble::rownames_to_column("Characteristic")

# Display Table 3, panel A
kable(table3_panel_a, align = c("l", "c", "c", "c"), row.names = FALSE)

# ╭───────────────────────────────────────────────────────────────────────────╮
# │ Figure 1: ROC curves for excessive lean-mass loss                         │
# ╰───────────────────────────────────────────────────────────────────────────╯

draw_figure1 <- function() {
    old_par <- par(
        mfrow = c(1, 3),
        mar = c(4.2, 4.2, 3.0, 1.0),
        oma = c(0, 0, 0, 0),
        mgp = c(2.4, 0.7, 0),
        tcl = -0.3
    )
    on.exit(par(old_par))

    panel_titles <- names(time_levels)

    walk2(assessment_results, seq_along(assessment_results), \(result, i) {
        false_positive_rate <- 1 - result$roc_fit$specificities
        sensitivity <- result$roc_fit$sensitivities
        curve_order <- order(false_positive_rate, sensitivity)

        plot(
            NA,
            xlim = c(0, 1), ylim = c(0, 1), asp = 1,
            xaxs = "i", yaxs = "i",
            xlab = "1 - specificity",
            ylab = if (i == 1) "Sensitivity" else "",
            axes = FALSE
        )
        axis(1, at = seq(0, 1, 0.25), las = 1)
        axis(2, at = seq(0, 1, 0.25), las = 1)
        box()
        abline(a = 0, b = 1, col = "grey60", lty = 2, lwd = 1.5)
        lines(
            false_positive_rate[curve_order],
            sensitivity[curve_order],
            type = "s", lwd = 2.5, col = "#163A5F"
        )

        threshold <- result$threshold_point
        points(
            threshold["false_positive_rate"], threshold["sensitivity"],
            pch = 21, cex = 1.35, lwd = 2,
            bg = "white", col = "#B44C43"
        )
        text(
            threshold["false_positive_rate"],
            threshold["sensitivity"],
            labels = "0.20 g/L", pos = 3, offset = 0.7, cex = 0.9
        )

        title(
            main = paste0(LETTERS[i], "   ", panel_titles[i]),
            adj = 0, font.main = 2
        )
        text(
            0.97, 0.04,
            labels = sprintf(
                "AUC %.2f (%.2f to %.2f)\nn = %d, %d cases",
                result$auc,
                result$auc_interval[1], result$auc_interval[2],
                result$assessed, result$cases
            ),
            adj = c(1, 0), cex = 0.85
        )
    })
}

png(
    file.path(output_dir, "figure1.png"),
    width = 2400, height = 850, res = 200, pointsize = 12
)
draw_figure1()
dev.off()

pdf(
    file.path(output_dir, "figure1.pdf"),
    width = 12, height = 4.25, pointsize = 11, useDingbats = FALSE
)
draw_figure1()
dev.off()

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
    estimate <- unname(coefficient["Estimate"])
    interval <- unname(confint(fit)["prealbumin_decrease_005", ])

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

# Table 3 caption
ranges <- list(
    sensitivity = "Sensitivity",
    ppv = "Positive predictive value",
    plr = "Positive likelihood ratio"
) |>
    map(\(x) {
        table3_panel_a[
            grepl(x, table3_panel_a$Characteristic),
            !grepl("Characteristic", names(table3_panel_a))
        ] |>
            sub(pattern = "\\(.+\\)", replacement = "") |>
            as.numeric() |>
            range()
    })

c(
    "Panel A evaluates prealbumin as a diagnostic test against a clinically
    established criterion of excessive lean mass loss, namely lean mass
    accounting for more than 25%% of the weight lost since surgery. The index
    test was prealbumin, analysed both as a continuous variable, through the
    area under the receiver operating characteristic curve, and dichotomised at
    the clinical threshold of 0.20 g/L. The reference standard was dual-energy
    X-ray absorptiometry performed at the same visit. Confidence intervals for
    the area under the curve were obtained by bootstrap resampling, and Wilson
    intervals were used for proportions.",
    "Panel B analyses the percentage of preoperative lean mass lost as a
    continuous outcome. No dichotomisation was applied, because no threshold
    for this quantity is established in the literature and, as shown in
    Supplementary Figure S1, estimates depend on the threshold chosen. Values
    in the second row are mean (SD) of the outcome; the following rows give the
    linear regression coefficient adjusted for sex, expressed as the additional
    percentage of preoperative lean mass lost for each 0.05 g/L decrease in
    prealbumin.",
    "The area under the curve did not differ from 0.50 at any time point, and
    was numerically below it at 6 months and 1 year, indicating that lower
    prealbumin concentrations were not associated with a greater proportion of
    lean mass in the weight lost. At the clinical threshold of 0.20 g/L,
    sensitivity ranged from %s to %s, so that between three quarters and
    nine tenths of patients meeting the criterion were not identified, and
    positive predictive values ranged from %s to %s. Positive likelihood
    ratios were between %s and %s, whereas a clinically useful test
    requires a value above."
) |>
    lapply(\(p) paste(strwrap(p), collapse = " ")) |>
    paste(collapse = "\n\n") |>
    sprintf(
        ranges$sensitivity[1],
        ranges$sensitivity[2],
        ranges$ppv[1],
        ranges$ppv[2],
        ranges$plr[1],
        ranges$plr[2]
    ) |>
    cat(file = here(output_dir, "table3_caption.txt"))

# Session information
sink(file.path(output_dir, "table3_sessionInfo.txt"))
sessionInfo()
sink()
