# ╭───────────────────────────────────────────────────────────────────────────╮
# │ Supplementary Figure S1: AUC across lean-mass-loss thresholds             │
# ╰───────────────────────────────────────────────────────────────────────────╯

suppressPackageStartupMessages({
    library(dplyr)
    library(here)
    library(pROC)
    library(purrr)
    library(tibble)
})

# Set project root directory
i_am("code/article_v2/figureS1.R")

output_dir <- here("output", "article_v2")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load data
load(here("data", "data_july_2026.rda"))

time_levels <- c(
    `6 months` = "1_6M",
    `1 year` = "2_1Y",
    `3 years` = "3_3Y"
)

thresholds <- 6:20
minimum_cases_for_auc <- 6
sparse_case_limit <- 15

# At each threshold, the outcome is lean mass lost since surgery as a
# percentage of preoperative lean mass (`TLML%`) greater than that threshold.
# The risk score uses prealbumin measured at the corresponding postoperative
# assessment and is oriented a priori so that lower prealbumin means higher
# predicted risk.
analyse_thresholds <- function(time_code, time_label) {
    d <- lg |>
        filter(`FU-CC` == time_code) |>
        transmute(
            lean_mass_lost = `TLML%`,
            prealbumin_risk_score = -preAlb
        ) |>
        filter(if_all(everything(), \(x) !is.na(x)))

    map_dfr(thresholds, \(threshold) {
        response <- d$lean_mass_lost > threshold
        cases <- sum(response)
        non_cases <- sum(!response)

        # Very small groups do not support a meaningful ROC estimate. This
        # affects only the most extreme thresholds at 6 months.
        if (cases < minimum_cases_for_auc ||
            non_cases < minimum_cases_for_auc) {
            return(tibble(
                time = time_label,
                threshold = threshold,
                observations = nrow(d),
                cases = cases,
                auc = NA_real_,
                lower = NA_real_,
                upper = NA_real_
            ))
        }

        roc_fit <- roc(
            response = response,
            predictor = d$prealbumin_risk_score,
            levels = c(FALSE, TRUE),
            direction = "<",
            quiet = TRUE
        )
        auc_interval <- ci.auc(roc_fit, method = "delong")

        tibble(
            time = time_label,
            threshold = threshold,
            observations = nrow(d),
            cases = cases,
            auc = as.numeric(auc(roc_fit)),
            lower = unname(auc_interval[1]),
            upper = unname(auc_interval[3])
        )
    })
}

figureS1_data <- imap_dfr(
    time_levels,
    \(time_code, time_label) analyse_thresholds(time_code, time_label)
)

draw_figureS1 <- function() {
    old_par <- par(
        mfrow = c(1, 3),
        mar = c(4.5, 4.2, 3.0, 1.0),
        oma = c(0, 0, 0, 0),
        mgp = c(2.6, 0.7, 0),
        tcl = -0.3
    )
    on.exit(par(old_par))

    walk2(names(time_levels), seq_along(time_levels), \(time_label, i) {
        panel_data <- filter(figureS1_data, time == time_label)
        plotted_data <- filter(panel_data, !is.na(auc))
        sparse_threshold <- panel_data |>
            filter(cases < sparse_case_limit) |>
            summarise(value = min(threshold)) |>
            pull(value)

        plot(
            NA,
            xlim = range(thresholds), ylim = c(0, 1),
            xaxs = "i", yaxs = "i",
            xlab = "Threshold, % of preoperative\nlean mass lost",
            ylab = if (i == 1) "AUC of prealbumin" else "",
            axes = FALSE
        )
        axis(1, at = seq(6, 20, 2), las = 1)
        axis(2, at = seq(0, 1, 0.1), las = 1)
        box()

        rect(
            sparse_threshold, 0, max(thresholds), 1,
            col = "#EEEEEE", border = NA
        )
        abline(h = 0.5, col = "grey50", lty = 2, lwd = 1.5)
        polygon(
            c(plotted_data$threshold, rev(plotted_data$threshold)),
            c(plotted_data$lower, rev(plotted_data$upper)),
            col = "#B9C7D5A6", border = NA
        )
        lines(
            plotted_data$threshold, plotted_data$auc,
            col = "#163A5F", lwd = 2.5
        )

        title(
            main = paste0(LETTERS[i], "   ", time_label),
            adj = 0, font.main = 2
        )
        text(
            mean(c(sparse_threshold, max(thresholds))), 0.94,
            labels = paste0("< ", sparse_case_limit, " cases"),
            col = "grey35", cex = 0.9
        )
        text(
            max(thresholds) - 0.4, 0.06,
            labels = paste0("n = ", unique(panel_data$observations)),
            adj = c(1, 0), col = "grey30", cex = 0.9
        )
    })
}

# Save raster and vector versions of Supplementary Figure S1.
png(
    file.path(output_dir, "figureS1.png"),
    width = 2400, height = 900, res = 200, pointsize = 12
)
draw_figureS1()
dev.off()

pdf(
    file.path(output_dir, "figureS1.pdf"),
    width = 12, height = 4.5, pointsize = 11, useDingbats = FALSE
)
draw_figureS1()
dev.off()

# Caption
c(
    "Supplementary Figure S1. Discrimination of serum prealbumin for different
    definitions of postoperative lean-mass loss. At each threshold on the
    horizontal axis, cases were defined as participants who had lost more than
    that percentage of their preoperative lean mass. Receiver operating
    characteristic analyses used the prealbumin concentration measured at the
    corresponding assessment and were performed separately at 6 months (A), 1
    year (B), and 3 years (C). Prealbumin was oriented a priori so that lower
    concentrations represented greater predicted risk.",
    "Solid lines show the area under the receiver operating characteristic
    curve (AUC), shaded bands show DeLong 95% confidence intervals, and the
    dashed horizontal line indicates no discrimination (AUC = 0.50). Light
    vertical regions identify thresholds yielding fewer than 15 cases. AUCs
    were not displayed when there were five or fewer cases or non-cases. The
    number in each panel is the number of participants with complete data.",
    "Unlike Table 3 panel A, this sensitivity analysis defines loss relative
    to preoperative lean mass rather than as the proportion of total body
    weight lost. Because no clinical cut-off is established for this outcome,
    the figure illustrates how discrimination changes when different
    data-dependent thresholds are considered; estimates at high thresholds
    should be interpreted cautiously because they are based on few cases.",
    "AUC, area under the receiver operating characteristic curve; CI,
    confidence interval."
) |>
    lapply(\(p) paste(strwrap(p), collapse = " ")) |>
    paste(collapse = "\n\n") |>
    cat(file = file.path(output_dir, "figureS1_caption.txt"))

# Save the plotted estimates and session information for reproducibility.
write.csv(
    figureS1_data,
    file.path(output_dir, "figureS1_data.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
)

sink(file.path(output_dir, "figureS1_sessionInfo.txt"))
sessionInfo()
sink()
