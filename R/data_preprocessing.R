library(dplyr)
library(here)
library(knitr)
library(parallel)
library(readxl)
library(tidyr)

# Set options
options(mc.cores = detectCores() - 1)

# Working directory
i_am("R/data_preprocessing.R") |> suppressMessages()

# Helper(s)
`%~=%` <- function(x, y) {
    is.na(x) & is.na(y) | !is.na(x) & !is.na(y) & x == y
}

# ── Import and preprocess data ───────────────────────────────────────────────

# Import data
files <- c(lg = "COOL_CC_246_longitudinal_06.05.2025.xlsx",
           lg1 = "COOL_CC_246_longitudinal_06.05.2025[1].xlsx",
           cs = "COOL_CC_577_cross-sectional_06.05.2025.xlsx",
           cs1 = "COOL_CC_577_cross-sectional_06.05.2025[1].xlsx")
dta <- mclapply(files, function(file) {
    file <- here("data-raw", file)
    read_xlsx(file) |> suppressMessages()
})

# Check that common variables across datasets are equal
#   - if yes, merge data sets
#   - else, raise an error
for (s in c("lg", "cs")) {
    s1 <- paste0(s, "1")
    common_variables <- intersect(names(dta[[s]]), names(dta[[s1]]))
    b <- all(sapply(common_variables, function(x) {
        all(dta[[s]][[x]] %~=% dta[[s1]][[x]])
    }))
    if (b) {
        x <- !(names(dta[[s1]]) %in% common_variables)
        dta[[s]] <- cbind(dta[[s]], dta[[s1]][x])
    } else {
        stop(paste("Not all common variables are equal across",
                   s, "datasets."))
    }
}
dta <- dta[c("lg", "cs")]
rm(b, common_variables, s, s1, x)

# Remove 'unit' variables
dta <- mclapply(dta, function(d) d[!grepl("unit$", names(d))])

# Remove empty variables
dta <- mclapply(dta, function(d) d[sapply(d, function(x) any(!is.na(x)))])

# Rename 'Masse totale perdue' and 'Masse maigre perdue' in the longitudinal
# dataset using the same logic as the other variables
dta$lg <- dta$lg %>%
    rename(
        `6M_Masse_totale_perdue` = `Masse totale perdue 6M`,
        `6M_Masse_maigre_perdue` = `Masse maigre perdue 6M`,
        `6M_Masse_maigre_perdue_pct` = `Masse maigre perdue % 6M`,
        `1Y_Masse_totale_perdue` = `Masse totale perdue 1Y`,
        `1Y_Masse_maigre_perdue` = `Masse maigre perdue 1Y`,
        `1Y_Masse_maigre_perdue_pct` = `Masse maigre perdue % 1Y`,
        `3Y_Masse_totale_perdue` = `Masse totale perdue 3Y`,
        `3Y_Masse_maigre_perdue` = `Masse maigre perdue 3Y`,
        `3Y_Masse_maigre_perdue_pct` = `Masse maigre perdue % Y3`
    )

# Rename 'hsCRP' in longitudinal data to '3Y_hsCRP'
dta$lg <- rename(dta$lg, `3Y_hsCRP` = hsCRP)

# Rename '.._CC_TWL%' in longitudinal data to '.._TWL'
dta$lg <- rename_with(dta$lg, ~ sub("CC_TWL%", "TWL", .x))

# Create a table indicating which variables are available in each dataset
var_table <- function() {
    vars <- unique(sub("^(DX[0-9]{2}|PO|6M|1Y|3Y)_", "",
                       c(names(dta$cs), names(dta$lg))))
    check_vars <- function(dataset, period) {
        ds <- tolower(dataset)
        pattern <- switch(period,
                          "BL" = "^DX[0-9]{2}_",
                          "PO" = "^PO_",
                          "6M" = "^6M_",
                          "1Y" = "^1Y_",
                          "3Y" = "^3Y_")
        sapply(vars, function(x) {
            check <- any(sub(pattern, "", names(dta[[ds]])) == x)
            as.numeric(check)
        })
    }
    tbl <- data.frame(
        Variable = vars,
        CS = check_vars("CS", "BL"),
        `LG BL` = check_vars("LG", "BL"),
        `LG PO` = check_vars("LG", "PO"),
        `LG 6M` = check_vars("LG", "6M"),
        `LG 1Y` = check_vars("LG", "1Y"),
        `LG 3Y` = check_vars("LG", "3Y")
    )
    tbl <- arrange(tbl, Variable)
    kable(tbl, row.names = FALSE, format = "simple")
}
if (FALSE) var_table()

# Data preprocessing
dta <- mclapply(dta, function(d) {
    # Convert character variables which contain numeric values to numeric
    for (j in which(!grepl("CRP$", names(d)))) {
        x <- d[[j]]
        if (all(is.na(x) | grepl("^(-)?[0-9]+(\\.[0-9]+)?$", x))) {
            d[[j]] <- as.numeric(x)
        }
    }
    # Gender as factor variable
    d$Gender <- factor(d$Gender, c("F", "M"))
    # preAlb - Groups
    for (x in grep("preAlb", names(d), value = TRUE)) {
        d[[paste0(x, "2")]] <- factor(d[[x]] < .2, c(FALSE, TRUE),
                                      c(">=0.2", "<0.2"))
    }
    d
})

# Data in long format -- Cross-sectional data are analyzed in the same way than
# the longitudinal data. Actually, data are analyzed per-period. So we create a
# dataset in the long format with the following periods:
#   - from the cross-sectional dataset: FU_end
#   - from the longitudinal dataset: PO, 6M, 1Y, 3Y
# The variables are renamed to have a common format:
#   - CS dataset: remove the prefix `DX[0-9]{2}_`
#   - LG dataset: remove the prefix `PO_`, `6M_`, `1Y_`, `3Y_`. The prefix is
#     used to determine the period
lg_lg <- dta$lg %>%
    mutate(
        `6M_ΔpreAlb` = `6M_preAlb` - PO_preAlb,
        `1Y_ΔpreAlb` = `1Y_preAlb` - PO_preAlb,
        `3Y_ΔpreAlb` = `3Y_preAlb` - PO_preAlb
    ) %>%
    rename_with(~ sub("^(PO|6M|1Y|3Y)(_)(.+)", "\\1__\\3", .x)) %>%
    pivot_longer(
        cols = starts_with(c("PO__", "6M__", "1Y__", "3Y__")),
        names_to = c("period", ".value"),
        names_pattern = "(.+)__(.+)"
    ) %>%
    mutate(ΔLM = factor(Masse_maigre_perdue_pct <= 25, c(TRUE, FALSE),
                        c("<=25%", ">25%")))
lg_cs <- dta$cs %>%
    rename_with(~ sub("^DX[0-9]{2}_", "", .x)) %>%
    rename(Date_BS = DateBS) %>%
    mutate(period = "FU_end") %>%
    select(any_of(names(lg_lg)))
lg <- bind_rows(lg_lg, lg_cs)
rm(lg_cs, lg_lg)

# Save preprocessed data
if (!dir.exists(here("data"))) dir.create(here("data"))
save(dta, file = here("data", "dta.rda"), compress = "xz")
save(lg, file = here("data", "lg.rda"), compress = "xz")
