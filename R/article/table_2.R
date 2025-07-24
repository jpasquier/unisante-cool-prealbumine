library(here)
library(knitr)
library(writexl)

# Set working directory
i_am("R/article/table_2.R")

# Load preprocessed data
load(here("data/lg.rda"))

# Table 2
periods <-  c("FU_end", "6M", "1Y", "3Y")
Y <- c("ALMI", "LMI", "ALM/poids", "LMTot", "FMI")
m <- function(x, fmt = "%.1f", n = FALSE) {
    out <- paste0(sprintf(fmt, mean(x)), " (", sprintf(fmt, sd(x)), ")")
    if (n) out <- paste0("[", length(x), "] ", out)
    return(out)
}
table2 <- function(n = TRUE) do.call(rbind, lapply(periods, function(p) {
    do.call(rbind, lapply(Y, function(y) {
        do.call(rbind, lapply(c("F", "M"), function(g) {
            d <- lg[lg$period == p & lg$Gender == g, c("preAlb2", y)]
            d <- na.omit(d)
            if (y == "LMTot") d[[y]] <- d[[y]] / 1e3
            x1 <- d[[y]][d$preAlb2 == ">=0.2"]
            x2 <- d[[y]][d$preAlb2 == "<0.2"]
            pv <- if (length(x1) >= 2 && length(x2) >= 2) {
                pv <- t.test(x1, x2)$p.value
                fmt_pv <- if (pv < .1) "%.2f" else "%.1f"
                sprintf(fmt_pv, pv)
            } else {
                NA
            }
            fmt <- if (y == "ALM/poids") "%.2f" else "%.1f"
            data.frame(
                period = p,
                parameter = y,
                gender = g,
                `preAlb >= 0.2` = m(x1, fmt, n = n),
                `preAlb < 0.2` = m(x2, fmt, n = n),
                pv = pv,
                check.names = FALSE
            )
        }))
    }))
}))

# Display table 2
kable(table2())

#  Optional: Save table 2 to an Excel file
if (FALSE) {
    write_xlsx(table2(n = FALSE), "~/table_2.xlsx")
    write_xlsx(table2(), "~/table_2_with_N.xlsx")
}
