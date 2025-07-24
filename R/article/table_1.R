{
    library(dplyr)
    library(emmeans)
    library(forcats)
    library(here)
    library(knitr)
    library(lme4)
    library(purrr)
    library(tidyr)
    library(writexl)
} |>
    suppressMessages()

# Set working directory
i_am("R/article/table_1.R") |> suppressMessages()

# Load preprocessed data
load(here("data", "dta.rda"))

# Convert the longitudinal data to long format
lg <- dta$lg %>%
    rename_with(~ sub("^(PO|6M|1Y|3Y)(_)(.+)", "\\1__\\3", .x)) |>
    pivot_longer(
        cols = matches("^(PO|6M|1Y|3Y)__(.+)"),
        names_to = c("period", ".value"),
        names_pattern = "(.+)__(.+)"
    ) |>
    mutate(period = factor(period, levels = c("PO", "6M", "1Y", "3Y")))

# Formaters
m <- function(mean, se, fmt_mean = "%.1f", fmt_se = "%.2f") {
    paste0(sprintf(fmt_mean, mean), " (", sprintf(fmt_se, se), ")")
}
p <- function(pv) ifelse(pv < .001, "<.001", sprintf("%.3f", pv))

# Table 1
table1 <- map_dfr(c("BMIc", "preAlb", "TWL"), function(y) {
    # Linear mixed effect models
    fml <- reformulate("Gender * period + (1 | IPP)", response = y)
    fit <- lmer(fml, data = lg)

    # Marginal means
    fmt_mean <- if (y == "preAlb") "%.2f" else "%.1f"
    fmt_se <- if (y == "preAlb") "%.3f" else "%.2f"
    emm <- bind_rows(
        as_tibble(emmeans(fit, ~ Gender + period)),
        as_tibble(emmeans(fit, ~ period))
    ) |>
        mutate(
            Gender = fct_na_value_to_level(Gender, level = "All"),
            mean = m(emmean, SE, fmt_mean, fmt_se)
        ) |>
        select(period, Gender, mean)

    # Period contrasts
    cst_p <- map_dfr(list(~ period | Gender, ~ period), ~ {
        emmeans(fit, .x) |>
            contrast("pairwise", adjust = "none") |>
            as_tibble()
    }) |>
        filter(grepl("^PO - ", contrast)) |>
        mutate(
            period = sub("^PO - ", "", as.character(contrast)),
            Gender = fct_na_value_to_level(Gender, level = "All"),
            p.value = p(p.value)
        ) |>
        select(period, Gender, p.value)

    # Add period contrast p-values to the marginal means and convert the table
    # to wide format
    emm <- full_join(emm, cst_p, by = c("period", "Gender")) |>
        pivot_wider(
            names_from = period,
            values_from = c(mean, p.value),
            names_glue = "{period}_{.value}"
        ) |>
        select(!any_of("PO_p.value")) |>
        rename_with(~ sub("_mean", "", .x)) |>
        mutate(Gender = fct_relevel(Gender, "All", "F", "M")) |>
        arrange(Gender)

    # Gender contrasts, per period
    cst_g <-  emmeans(fit, ~ Gender | period) |>
        contrast("pairwise", adjust = "none") |>
        as_tibble() |>
        select(period, p.value) |>
        mutate(p.value = p(p.value)) |>
        pivot_wider(names_from = period, values_from = p.value)

    # Add gender contrast p-values to the marginal means
    bind_rows(emm, cst_g) |>
        mutate(
            Gender = fct_na_value_to_level(Gender, level = "(p.value)"),
            parameter = y
        ) |>
        relocate(parameter)
})

# Display table 1
kable(table1)

#  Optional: Save table 1 to an Excel file
if (FALSE) {
    write_xlsx(table1, "~/table_1.xlsx")
}
