{
    library(dplyr)
    library(here)
    library(knitr)
    library(purrr)
    library(tidyr)
} |>
    suppressMessages()

# Set working directory
i_am("R/article/results.R") |> suppressMessages()

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

# Cross-sectional data
cs <- dta$cs |>
    rename_with(~ sub("^DX[0-9]{2}_", "", .x))

# Formaters
m <- function(x, fmt = "%.1f") {
    x <- x[!is.na(x)]
    paste0("[", length(x), "] ", sprintf(fmt, mean(x)), " (",
           sprintf(fmt, sd(x)), ")")
}
p <- function(pv) ifelse(pv < .001, "<.001", sprintf("%.3f", pv))
f <- function(x, fmt = "%.2f") sprintf(fmt, x)

# Cross-sectional patient caracteristics
bind_rows(
    cs |>
        group_by(Gender) |>
        summarise(
            Age = m(age),
            BMI = m(BMIc),
            TWL = m(TWL)
        ),
    cs |>
        summarise(
            Age = m(age),
            BMI = m(BMIc),
            TWL = m(TWL)
        )
) |>
    kable()

# Correlations between ALMI/LMI and prealbumin in the cross-sectional data
map_dfr(1:2, function(k) {
    map_dfr(c("ALMI", "LMI"), function(x) {
        y <- "preAlb"
        map_dfr(c("F", "M"), function(g) {
            d <- cs |>
                filter(Gender == g, `FU_CC-calc` >= c(0, 2)[k]) |>
                select(any_of(c(x, y))) |>
                drop_na()
            delta <- qnorm(0.975) / sqrt(nrow(d) - 3)
            r <- cor(d[[x]], d[[y]])
            lwr <- tanh(atanh(r) - delta)
            upr <- tanh(atanh(r) + delta)
            pv <- cor.test(d[[x]], d[[y]])$p.value
            tibble(
                Pop = c("All", "FU_CC-calc >= 2")[k],
                X = x,
                Y = y,
                Gender = g,
                n = nrow(d),
                R = f(r),
                `95% CI` = paste0("[", f(lwr), ", ", f(upr), "]"),
                p = f(pv, "%.3f")
            )
        })
    })
}) |>
    kable()

# Correlations between ALMI / LMI / ALM/weight and prealbumin in the
# longitudinal data
map_dfr(c("F", "M"), function(g) {
    map_dfr(c("6M", "1Y", "3Y"), function(p) {
        X <- c("ALMI", "LMI", "ALM/poids")
        map_dfr(X, function(x) {
            y <- "preAlb"
            d <- lg |>
                filter(Gender == g, period == p) |>
                select(any_of(c(X, y))) |>
                drop_na()
            delta <- qnorm(0.975) / sqrt(nrow(d) - 3)
            r <- cor(d[[x]], d[[y]])
            lwr <- tanh(atanh(r) - delta)
            upr <- tanh(atanh(r) + delta)
            pv <- cor.test(d[[x]], d[[y]])$p.value
            tibble(
                Gender = g,
                Period = p,
                X = x,
                Y = y,
                n = nrow(d),
                R = f(r),
                `95% CI` = paste0("[", f(lwr), ", ", f(upr), "]"),
                p = f(pv)
            )
        })
    })
}) |>
    kable()

# Proportion of patients with prealbumin levels below normal
with(lg, addmargins(table(period, preAlb2)))
(with(lg, prop.table(table(period, preAlb2), 1)) * 100) |> round(1)
