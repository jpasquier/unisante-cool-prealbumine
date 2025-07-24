library(dplyr)
library(here)
library(knitr)
library(purrr)
library(tidyr)
library(writexl)

# Set working directory
i_am("R/article/table_3.R")

# Load preprocessed data
load(here("data/lg.rda"))

# Table 3
m <- function(x, fmt = "%.2f") {
    paste0(sprintf(fmt, mean(x)), " (", sprintf(fmt, sd(x)), ")")
}
table3 <- map_dfr(c("6M", "1Y", "3Y"), function(p) {
    map_dfr(c("F", "M"), function(g) {
        d <- lg[lg$period == p & lg$Gender == g, c("preAlb", "ΔLM")]
        d <- na.omit(d)
        x1 <- d$preAlb[d$`ΔLM` == "<=25%"]
        x2 <- d$preAlb[d$`ΔLM` == ">25%"]
        pv <- sprintf("%.2f", wilcox.test(x1, x2, exact = FALSE)$p.value)
        tibble(
            period = p,
            gender = factor(g, c("F", "M"), c("Women", "Men")),
            `Lean mass loss <= 25%` = m(x1),
            `Lean mass loss > 25%` = m(x2),
            `p-value` = pv,
            n1 = length(x1),
            n2 = length(x2),
            i = ((p == "1Y") + (p == "3Y") * 2) * 3 +
                (g == "F") + (g == "M") * 2
        )
    })
})

# Number of observations
n <-  function(nF, nM) paste0("n = ", nF + nM, " (", nF, "/", nM, ")")
N <- table3 |>
    select(period, gender, n1, n2) |>
    pivot_wider(names_from = gender, values_from = c(n1, n2)) |>
    mutate(
        gender = case_when(
            period == "6M" ~ "Prealbumin at 6 months",
            period == "1Y" ~ "Prealbumin at 1 year",
            period == "3Y" ~ "Prealbumin at 3 years"
        ),
        `Lean mass loss <= 25%` = n(n1_Women, n1_Men),
        `Lean mass loss > 25%` = n(n2_Women, n2_Men),
        i = ((period == "1Y") + (period == "3Y") * 2) * 3
    ) |>
    select(!matches("n(1|2)_(Women|Men)"))

# Finalize table 3
table3 <- bind_rows(table3, N) |>
    arrange(i) |>
    select(` ` = gender, starts_with("Lean mass loss"), `p-value`)

# Display table 3
kable(table3)

#  Optional: Save table 3 to an Excel file
if (FALSE) {
    write_xlsx(table3, "~/table_3.xlsx")
}
