# ╭───────────────────────────────────────────────────────────────────────────╮
# │ Figure 2: Cross-sectional body composition and prealbumin                 │
# ╰───────────────────────────────────────────────────────────────────────────╯

suppressPackageStartupMessages({
    library(dplyr)
    library(here)
    library(purrr)
    library(tibble)
})

# Set project root directory
i_am("code/article_v2/figure2.R")

output_dir <- here("output", "article_v2")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load and prepare the cross-sectional data used in Table 5.
load(here("data", "data_july_2026.rda"))

cs <- cs |>
    rename_with(~ sub("^DX[0-9]{2}_", "", .x)) |>
    mutate(
        Sex = factor(Gender, levels = c("F", "M"),
                     labels = c("Women", "Men"))
    )

outcome_spec <- tribble(
    ~panel, ~variable,     ~y_label,
    "A",    "ALMI",       "ALMI, kg/m²",
    "B",    "ALM/weight", "ALM / body weight"
)

sex_style <- tibble(
    Sex = factor(c("Women", "Men"), levels = levels(cs$Sex)),
    point = c(21, 24),
    line = c(1, 2),
    colour = c("#24557A", "#A34A28")
)

format_signed <- function(x) sprintf("%+.2f", x)

# Calculate the statistics displayed in the panels. These are unadjusted
# Pearson correlations and sex-specific ordinary least-squares regressions.
analyse_group <- function(panel, variable, y_label, sex) {
    d <- cs |>
        filter(Sex == sex) |>
        transmute(prealbumin = preAlb, outcome = .data[[variable]]) |>
        filter(if_all(everything(), \(x) !is.na(x)))

    cor_test <- cor.test(
        d$prealbumin, d$outcome,
        method = "pearson", conf.level = 0.95
    )
    fit <- lm(outcome ~ prealbumin, data = d)

    tibble(
        panel = panel,
        variable = variable,
        y_label = y_label,
        Sex = sex,
        observations = nrow(d),
        correlation = unname(cor_test$estimate),
        lower = unname(cor_test$conf.int[1]),
        upper = unname(cor_test$conf.int[2]),
        intercept = unname(coef(fit)["(Intercept)"]),
        slope = unname(coef(fit)["prealbumin"])
    )
}

figure2_results <- pmap_dfr(outcome_spec, \(panel, variable, y_label) {
    map_dfr(
        levels(cs$Sex),
        \(sex) analyse_group(panel, variable, y_label, sex)
    )
})

draw_figure2 <- function() {
    old_par <- par(no.readonly = TRUE)
    layout(
        matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE),
        widths = c(1, 1), heights = c(6, 1)
    )
    on.exit({
        layout(1)
        par(old_par)
    })
    par(
        mar = c(4.4, 4.5, 2.6, 1.0),
        mgp = c(2.7, 0.8, 0),
        tcl = -0.3
    )

    pmap(outcome_spec, \(panel, variable, y_label) {
        panel_data <- cs |>
            select(Sex, prealbumin = preAlb, outcome = all_of(variable)) |>
            filter(if_all(everything(), \(x) !is.na(x)))
        panel_results <- filter(
            figure2_results,
            .data$panel == .env$panel
        )

        plot(
            panel_data$prealbumin, panel_data$outcome,
            type = "n",
            xlab = "Prealbumin, g/L", ylab = y_label,
            xaxs = "r", yaxs = "r"
        )

        walk(levels(panel_data$Sex), \(sex) {
            style <- filter(sex_style, Sex == sex)
            d <- filter(panel_data, Sex == sex)
            result <- filter(panel_results, Sex == sex)

            points(
                d$prealbumin, d$outcome,
                pch = style$point, cex = 0.75,
                col = adjustcolor(style$colour, alpha.f = 0.72),
                bg = adjustcolor("white", alpha.f = 0.55),
                lwd = 1.2
            )
            x_values <- range(d$prealbumin)
            lines(
                x_values,
                result$intercept + result$slope * x_values,
                col = style$colour, lty = style$line, lwd = 2.5
            )
        })

        title(main = panel, adj = 0, font.main = 2)

        correlation_labels <- panel_results |>
            mutate(
                label = paste0(
                    Sex, "  r = ", format_signed(correlation), " (",
                    format_signed(lower), " to ", format_signed(upper), ")"
                )
            ) |>
            pull(label)

        legend(
            "topleft", legend = correlation_labels,
            bty = "n", text.col = sex_style$colour,
            cex = 0.90, inset = 0.01
        )
    })

    # Shared legend in a dedicated row below the two panels.
    par(mar = c(0, 0, 0, 0))
    plot.new()
    legend(
        "center",
        legend = c(
            paste0("Women (n = ", figure2_results$observations[1], ")"),
            paste0("Men (n = ", figure2_results$observations[2], ")"),
            "Regression, women", "Regression, men"
        ),
        pch = c(sex_style$point, NA, NA),
        pt.bg = c("white", "white", NA, NA),
        col = c(sex_style$colour, sex_style$colour),
        lty = c(NA, NA, sex_style$line),
        lwd = c(NA, NA, 2.5, 2.5),
        horiz = TRUE, bty = "n", cex = 0.9
    )
}

# Save raster and vector versions of Figure 2.
png(
    file.path(output_dir, "figure2.png"),
    width = 2200, height = 1050, res = 200, pointsize = 12
)
draw_figure2()
dev.off()

pdf(
    file.path(output_dir, "figure2.pdf"),
    width = 11, height = 5.25, pointsize = 11, useDingbats = FALSE
)
draw_figure2()
dev.off()

# Caption
c(
    "Figure 2. Cross-sectional associations of prealbumin concentration with
    appendicular lean mass index (ALMI; A) and appendicular lean mass relative
    to body weight (B), stratified by sex. Points represent individual
    participants and lines are sex-specific ordinary least-squares regression
    fits. Pearson correlation coefficients (r) with 95% confidence intervals
    are displayed within each panel.",
    "The regressions and correlations shown here are unadjusted descriptive
    analyses. They illustrate two of the associations examined in Table 5, but
    they are not graphical representations of its adjusted models: Table 5
    panel A assumes a common prealbumin slope across sexes and adjusts for sex,
    with the second model additionally adjusted for age and time since
    surgery.", "ALM, appendicular lean mass; ALMI, appendicular lean mass
    index; CI, confidence interval."
) |>
    lapply(\(p) paste(strwrap(p), collapse = " ")) |>
    paste(collapse = "\n\n") |>
    cat(file = file.path(output_dir, "figure2_caption.txt"))

# Save the displayed estimates and session information for reproducibility.
write.csv(
    figure2_results,
    file.path(output_dir, "figure2_data.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
)

sink(file.path(output_dir, "figure2_sessionInfo.txt"))
sessionInfo()
sink()
