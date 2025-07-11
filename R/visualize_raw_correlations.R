{
    library(dplyr)
    library(forcats)
    library(ggplot2)
    library(ggpubr)
    library(grid)
    library(gridExtra)
    library(here)
    library(readxl)
    library(svglite)
    library(tidyr)
} |>
    suppressMessages()

# Set working directory
i_am("R/visualize_raw_correlations.R") |> suppressMessages()

# Set default ggplot theme
theme_set(theme_bw())

# Get last correlations analyses and preprocessed the results
tbl_cor <- local({
    period_levels <- c("PO", "6M", "1Y", "3Y", "CS", "CS_2YFU")
    path1 <- here("results/analyses_20250528/correlations.xlsx")
    path2 <- here("results/analyses_cs_2yfu_20250703/correlations.xlsx")
    cor1 <- read_xlsx(path1) |>
        mutate(period = sub("FU_end", "CS", period))
    cor2 <- read_xlsx(path2) |>
        mutate(period = sub("FU_END", "CS_2YFU", period))
    bind_rows(cor1, cor2) |>
        relocate(subpop, .after = period) |>
        mutate(
            period = factor(period, period_levels),
            gender = recode(gender, F = "Female", M = "Male", .missing = "All")
        )
})

# Draw a figure of the raw correlations between prealbumin and other biomarkers
fig_raw_correlations <- tbl_cor |>
    filter(variable2 == "preAlb") |>
    mutate(period = fct_recode(period, "CS\n2YFU" = "CS_2YFU")) |>
    ggplot(aes(x = period, y = cor_pearson, color = gender, group = gender)) +
    geom_point(position = position_dodge(width = .5)) +
    geom_errorbar(aes(ymin = cor_pearson_lwr, ymax = cor_pearson_upr),
                  position = position_dodge(width = .5), width = .2) +
    #geom_line(position = position_dodge(width = 0.3))
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(vars(variable1)) +
    labs(x = element_blank(), y = "Pearson correlation", color = "Gender",
         title = "Correlations between prealbumin and other biomarkers") +
    theme(legend.position.inside = c(.75, .15))

# Sample sizes per stratum
tbl_raw_correlations_n <- tbl_cor |>
    filter(variable2 == "preAlb") |>
    select(period, gender, variable1, nobs) |>
    arrange(period) |>
    group_by(period, gender) |>
    summarize(
        nobs_min = min(nobs),
        nobs_max = max(nobs),
        .groups = "drop"
    ) |>
    mutate(nobs = case_when(
        nobs_min == nobs_max ~ as.character(nobs_min),
        TRUE ~ paste0(nobs_min, "-", nobs_max)
    )) |>
    select(!c(nobs_min, nobs_max)) |>
    pivot_wider(values_from = nobs, names_from = period)

# Create table grob for the sample sizes
table_grob <- tbl_raw_correlations_n |>
    tableGrob(rows = NULL, theme = ttheme_minimal())

# Add title to the table
title_grob <- textGrob(
    "Sample sizes",
    gp = gpar(fontsize = 12, fontface = "bold"),
    y = unit(-5, "npc")
)
fig_sample_sizes <- arrangeGrob(
    title_grob,
    table_grob,
    heights = c(0.01, 0.99),
    ncol = 1
)

# Combine the correlation plot and sample size table
combined_fig <- ggarrange(
    fig_raw_correlations,
    fig_sample_sizes,
    ncol = 1,
    heights = c(4, 1)
)

# Export the combined figure
today <- format(Sys.Date(), "%Y%m%d")
output_file <- paste0("visualize_raw_correlations_", today, ".svg")
output_file <- here("results", output_file)
svglite(output_file, width = 12, height = 12 / sqrt(2))
print(combined_fig)
dev.off() |> invisible()
