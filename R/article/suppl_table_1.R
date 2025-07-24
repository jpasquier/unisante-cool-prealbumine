{
    library(dplyr)
    library(here)
    library(knitr)
    library(purrr)
    library(tidyr)
    library(writexl)
} |>
    suppressMessages()

# Set working directory
i_am("R/article/suppl_table_1.R") |> suppressMessages()

# Load preprocessed data
load(here("data/lg.rda"))

# Formater
f <- function(x, fmt = "%.2f") sprintf(fmt, x)

# Supplementary table 1
x <-  "FMI"
y <-  "preAlb"
tbl <-  map_dfr(c("FU_end", "6M", "1Y", "3Y"), function(p) {
    map_dfr(c("F", "M"), function(g) {
        d <- lg |>
            filter(period == p, Gender == g) |>
            select(any_of(c(x, y))) |>
            drop_na()
        delta <- qnorm(0.975) / sqrt(nrow(d) - 3)
        r <- cor(d[[x]], d[[y]])
        lwr <- tanh(atanh(r) - delta)
        upr <- tanh(atanh(r) + delta)
        pv <- cor.test(d[[x]], d[[y]])$p.value
        tibble(
            Timepoint = p,
            Sex = g,
            n = nrow(d),
            R = f(r),
            `95% CI` = paste0("[", f(lwr), ", ", f(upr), "]"),
            p = f(pv)
        )
    })
})

# Display supplementary table 1
kable(tbl)

#  Optional: Save supplementary table 1 to an Excel file
if (FALSE) {
    write_xlsx(tbl, "~/suppl_table_1.xlsx")
}
