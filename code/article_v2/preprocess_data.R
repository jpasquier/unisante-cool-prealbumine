library(dplyr)
library(here)
library(knitr)
library(purrr)
library(readxl)

# ── Set project root directory ───────────────────────────────────────────────

i_am("code/article_v2/preprocess_data.R")

# ── Data directories and file names ──────────────────────────────────────────

data_dir <- here("data")
raw_data_dir <- here("data-raw")

file_names <- c(
    "1-COOL_CC_178preal_longitudinal_by_subject_30.07.2026.xlsx",
    "2-COOL_CC_178preal_longitudinal_by_CC_30.07.2026.xlsx",
    "3-COOL_CC_prealb_310_cross-sectional_30.07.2026.xlsx"
) |>
    map_chr(~ file.path(raw_data_dir, .x))

# ── Helpers ──────────────────────────────────────────────────────────────────

`%==%` <- function(x, y) {
    is.na(x) & is.na(y) | !is.na(x) & !is.na(y) & x == y
}

str_to_num <- function(dat) {
    for (j in which(map_lgl(dat, ~ class(.x)[1] == "character"))) {
        x <- dat[[j]]
        if (all(is.na(x) | grepl("^(-)?[0-9]+(\\.[0-9]+)?$", x))) {
            dat[[j]] <- as.numeric(x)
        }
    }
    return(dat)
}

str_to_date <- function(dat) {
    re <- "^\\d{2}/\\d{2}/\\d{4}( \\d{2}:\\d{2}:\\d{2})?$"
    for (j in which(map_lgl(dat, ~ class(.x)[1] == "character"))) {
        x <- dat[[j]]
        if (all(is.na(x) | grepl(re, x))) {
            dat[[j]] <- as.Date(x, format = "%d/%m/%Y")
        }
    }
    return(dat)
}


compare_datasets <- function(d1, d2, id_var) {
    common_vars <- setdiff(intersect(names(d1), names(d2)), id_var)
    map_dfr(common_vars, ~ {
        d <- full_join(select(d1, all_of(c(id_var, .x))),
                       select(d2, all_of(c(id_var, .x))),
                       by = id_var)
        x1 <- paste0(.x, ".x")
        x2 <- paste0(.x, ".y")
        tibble(
            variable = .x,
            class1 = class(d[[x1]])[1],
            class2 = class(d[[x2]])[1],
            equal = all(d[[x1]] %==% d[[x2]])
        )
    })
}

# ── Longitudinal data ────────────────────────────────────────────────────────

# By subject (means wide format)
# ...Tansform to long format for comparison with the by CC dataset
lg0 <- map_dfr(c("A:AH", "AI:BW", "BX:DL", "DM:FA"), \(cols) {
    read_excel(file_names[1], range = cell_cols(cols)) |>
        filter(!is.na(`FU-CC`)) |>
        select(!matches("unit$")) |>
        rename_with(~ sub("^(PO|6M|1Y|3Y)_", "", .x)) |>
        rename_with(~ sub("_(PO|6M|1Y|3Y)$", "", .x)) |>
        rename_with(~ sub("^MassTot loss$", "MassTot_Loss" ,.x)) |>
        rename_with(~ sub("^%LM_loss$", "%LM_Loss", .x)) |>
        str_to_num() |>
        str_to_date()
})

# By CC (means long format)
lg <- read_excel(file_names[2]) |>
    select(!matches("unit$")) |>
    rename(
        FMTotpc    = "PO_FMTotpc",
        `LMTot-pc` = "PO_LMTot-pc"
    ) |>
    str_to_num() |>
    str_to_date()

# Check that the longitudinal datasets are consistent
kable(compare_datasets(lg0, lg, c("Subject ID", "FU-CC")))

if (FALSE) {
    full_join(lg0, lg, by = c("Subject ID", "FU-CC")) |>
        select(`Subject ID`, `FU-CC`, starts_with("Id_CC")) |>
        filter(!(Id_CC.x %==% Id_CC.y)) |>
        print(n = Inf)
    full_join(lg0, lg, by = c("Subject ID", "FU-CC")) |>
        select(`Subject ID`, `FU-CC`, starts_with("DX02_Date")) |>
        filter(!(DX02_Date.x %==% DX02_Date.y)) |>
        print(n = Inf)
}

# ── Cross-sectional data ─────────────────────────────────────────────────────

cs <- read_excel(file_names[3]) |>
    select(!matches("unit$")) |>
    rename(Id1 = Id...1, Id2 = Id...9) |>
    str_to_num() |>
    str_to_date()

# Save preprocessed data
save(lg, cs, file = file.path(data_dir, "data_july_2026.rda"), compress = "xz")
