library(dplyr)
library(ggplot2)
library(ggpubr)
library(here)
library(knitr)
library(officer)
library(parallel)
library(readxl)
library(rvg)
library(svglite)
library(tidyr)
library(writexl)

# Set options
options(mc.cores = detectCores() - 1)

# Conflict between officer and readxl
read_xlsx = readxl::read_xlsx

# Working directory
i_am("R/analyses.R")

# Helper(s)
`%~=%` <- function(x, y) {
    is.na(x) & is.na(y) | !is.na(x) & !is.na(y) & x == y
}

# Output directory
outdir <- here(paste0("results/analyses_", format(Sys.Date(), "%Y%m%d")))
if (!dir.exists(outdir)) dir.create(outdir)

# ── Import and preprocess data ───────────────────────────────────────────────

# Import data
files <- c(lg = "COOL_CC_246_longitudinal_06.05.2025.xlsx",
           lg1 = "COOL_CC_246_longitudinal_06.05.2025[1].xlsx",
           cs = "COOL_CC_577_cross-sectional_06.05.2025.xlsx",
           cs1 = "COOL_CC_577_cross-sectional_06.05.2025[1].xlsx")
dta <- mclapply(files, function(file) {
    file <- here("data-raw", file)
    read_xlsx(file)
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

# ── Build the four comparison tables that will be saved to “tables.xlsx” ─────

# Strategy
# ────────
#
# We create four wide data frames that summarise the study outcomes for every
# follow-up period and, where applicable, for each category of the chosen
# grouping variable.  The outer `mclapply()` loops (k = 1:2, l = 1:2) run in
# parallel:
#
#   k = 1 → body-composition endpoints (ALMI, LMI, FMI, TWL, Albumine …)
#           analysed overall and stratified by (l)
#             • pre-albumin category ( ≥0.2 g l⁻¹ vs <0.2 g l⁻¹ )
#             • gender (F / M)
#
#   k = 2 → pre-albumin trajectories (preAlb) analysed overall and stratified
#           by
#             • Δ-lean-mass category ( ≤25 % vs >25 % loss )
#             • gender (F / M)
#
# Periods considered are:
#   • Cross-sectional FU_end plus PO, 6 M, 1 Y, 3 Y for the mass analyses
#   • 6 M, 1 Y, 3 Y for the preAlb analyses
#
# For every period × response variable × subgroup we:
#   1. extract the relevant rows from the long data set `lg`
#   2. calculate n, mean, SD for the full set and for each subgroup
#   3. run a two-sample t-test (with SE and 95 % CI) and a Wilcoxon test
#   4. bind the results row-wise into one table
#
# Output
# ──────

# The nested list returned by the two parallel loops is flattened with
# `unlist(..., recursive = FALSE)` to give a *named* list of four data frames:
#
#   ▸ tbls$mass_preAlb   – body-composition vs pre-albumin category
#   ▸ tbls$mass_gender   – body-composition vs gender
#   ▸ tbls$preAlb_mass   – pre-albumin vs Δ-lean-mass category
#   ▸ tbls$preAlb_gender – pre-albumin vs gender
#
# Finally, the whole list is written to “results/analyses_{date}/tables.xlsx”
# where each data frame appears on its own worksheet.

tbls <- unlist(recursive = FALSE, mclapply(1:2, function(k) {
    if (k == 1) {
        periods <- c("FU_end", "PO", "6M", "1Y", "3Y")
        Y <- c("ALMI", "LMI", "LMTot", "FMI", "TWL", "Albumine", "ALM/poids")
    } else {
        periods <- c("6M", "1Y", "3Y")
        Y <-"preAlb"
    }
    lapply(1:2, function(l) {
        u <- c(c("preAlb2", "ΔLM")[k], "Gender")[l]
        U <- list(list(c(">=0.2", "<0.2"), c("<=25%", ">25%"))[[k]],
                  c("F", "M"))[[l]]
        var.g <- c("Gender", c("preAlb2", "ΔLM")[k])[l]
        G <- list(
                c("", "F", "M"),
                list(
                    c("", ">=0.2", "<0.2"),
                    c("", "<=25%", ">25%")
                )[[k]]
            )[[l]]
        r <- do.call(rbind, lapply(periods, function(p) {
            do.call(rbind, lapply(Y, function(y) {
                if (p == "PO" & y == "TWL") return(NULL)
                do.call(rbind, lapply(G, function(g) {
                    d <- if (g == "") {
                        lg[lg$period == p, c(u, y)]
                    } else {
                        lg[lg$period == p & lg[[var.g]] == g, c(u, y)]
                    }
                    x <- na.omit(d[[y]])
                    x1 <- na.omit(d[d[[u]] %in% U[1], y, drop = TRUE])
                    x2 <- na.omit(d[d[[u]] %in% U[2], y, drop = TRUE])
                    fml <- as.formula(paste0("`", y, "` ~ ", u))
                    if (length(x1) >= 2 & length(x2) >= 2) {
                        z <- t.test(fml, d)
                    } else {
                        z <- list(stderr = NA, conf.int = c(NA, NA),
                                  p.value = NA)
                    }
                    if (length(x1) >= 1 & length(x2) >= 1) {
                        w <- wilcox.test(fml, d, exact = FALSE)$p.value
                    } else {
                        w <- NA
                    }
                    r <- data.frame(
                        period = p,
                        variable = y,
                        GROUP = g,
                        n = length(x),
                        mean = mean(x),
                        sd = sd(x),
                        n_1 = length(x1),
                        mean_1 = mean(x1),
                        sd_1 = sd(x1),
                        n_2 = length(x2),
                        mean_2 = mean(x2),
                        sd_2 = sd(x2),
                        diff = mean(x2) - mean(x1),
                        se = z$stderr,
                        diff_lwr = -z$conf.int[2],
                        diff_upr = -z$conf.int[1],
                        `t-test p-value` = z$p.value,
                        `wilcoxon test p-value` = w,
                        stringsAsFactors = FALSE,
                        check.names = FALSE
                    )
                }))
            }))
        }))
        names(r)[names(r) == "GROUP"] <- c("gender", c("preAlb", "ΔLM")[k])[l]
        names(r) <- sub("_1$", paste0(" ", u, U[1]), names(r))
        names(r) <- sub("_2$", paste0(" ", u, U[2]), names(r))
        attr(r, "Y") <- Y
        attr(r, "periods") <- periods
        r
    })
}))
names(tbls) <- c("mass_preAlb", "mass_gender", "preAlb_mass", "preAlb_gender")
write_xlsx(tbls, file.path(outdir, "tables.xlsx"))

# ── Figures ──────────────────────────────────────────────────────────────────

# Build four forest-style plots—one for each result table stored in `tbls`—that
# visualise the mean differences (with 95 % CIs) between the two categories of
# interest for every follow-up period and outcome:
#
#   • tbls$mass_preAlb   → preAlb < 0.2 vs ≥ 0.2  (body-composition endpoints)
#   • tbls$mass_gender   → Female vs Male         (body-composition endpoints)
#   • tbls$preAlb_mass   → ΔLM ≤ 25 % vs > 25 %   (pre-albumin trajectories)
#   • tbls$preAlb_gender → Female vs Male         (pre-albumin trajectories)
#
# Workflow
# ────────
#
# 1. Loop (in a named `mclapply`) over the four tables.
# 2. For each table:
#      – Recode the appropriate grouping variable into a tidy “GROUP” factor
#        with levels “All”, “Male/Female”, or the relevant clinical categories.
#      – Rescale lean-mass totals (LMTot) by 1 000 for the body-composition
#        figures so that the axis units are sensible.
#      – Convert `variable` and `period` columns into ordered factors so the
#        facets appear in a logical order.
#      – Assemble a forest plot of mean difference ± CI, faceted by period
#        (rows) and outcome variable (columns), with a grey vertical reference
#        line at zero.
# 3. Collect the four resulting `ggplot` objects in a list called `figs`.
# 4. Iterate over that list and write each plot to
#    'results/analyses_{date}/<name>.svg' using svglite.

figs <- mclapply(setNames(1:4, names(tbls)), function(k) {
    d <- tbls[[k]]
    d$GROUP <- if (k %in% c(1, 3)) {
        factor(d$gender, c("M", "F", ""), c("Male", "Female", "All"))
    } else if (k == 2) {
        factor(d$preAlb, c("<0.2", ">=0.2", ""),
               c("preAlb<0.2", "preAlb>=0.2", "All"))
    } else {
        factor(d$ΔLM, c(">25%", "<=25%", ""),
               c("ΔLM>25%", "ΔLM<=25%", "All"))
    }
    if (k %in% 1:2) {
        d[d$variable == "LMTot", c("diff", "diff_lwr", "diff_upr")] <-
            d[d$variable == "LMTot", c("diff", "diff_lwr", "diff_upr")] / 10^3
        d$variable[d$variable == "LMTot"] <- "LMTot / 1000"
    }
    d$variable = factor(d$variable, sub("LMTot", "LMTot / 1000", attr(d, "Y")))
    d$period = factor(d$period, attr(d, "periods"))
    ttl <- c("Mean differences between preAlb>=0.2 and preAlb<0.2",
             "Mean differences between females and males",
             "Mean differences between ΔLM<=25% and ΔLM>25%",
             "Mean differences between females and males")[k]
    select(d, variable, period, GROUP, diff, diff_lwr, diff_upr) %>%
        drop_na(diff) %>%
        ggplot(aes(x = diff, y = GROUP)) +
        geom_point() +
        geom_errorbar(aes(xmin = diff_lwr, xmax = diff_upr), width = .1) +
        geom_vline(aes(xintercept = 0), colour = "grey70") +
        facet_grid(rows = vars(period), cols = vars(variable),
                   scales = "free") +
        labs(x = "Difference", y = 0, title = ttl)
})
for (s in names(figs)) {
    svglite(file.path(outdir, paste0(s, ".svg")))
    print(figs[[s]])
    dev.off()
}
rm(s)

# ── Correlation analyses and visual outputs ──────────────────────────────────
#
# Goal
# ────
#
# Quantify and visualise the relationships between nutritional markers
#   • pre-albumin at follow-up (preAlb)
#   • change in pre-albumin (ΔpreAlb)
#   • serum albumin (Albumine)
# and a set of body-composition or weight-loss variables across every study
# period (FU_end, PO, 6 M, 1 Y, 3 Y), with results reported:
#   ▸ for the whole cohort,
#   ▸ for females only, and
#   ▸ for males only.
#
# Workflow
# ────────
#
# 1. Loop over each Y-variable listed in `Y`.
# 2. Per Y, iterate over the five study periods and the relevant X-variables
#    (special cases handled so that, e.g., Albumine is not correlated with
#    itself, and ΔpreAlb is only compared with lean-mass loss %).
# 3. For every X ~ Y pair and gender stratum:
#      – Extract the non-missing subset (`d`) from the long data set `lg`.
#      – If ≥ 4 observations exist, compute
#          • Pearson r with 95 % CI (Fisher z) and p-value
#          • Spearman ρ with 95 % CI and p-value
#        otherwise fill with NA.
#      – Assemble a results row (`tbl_row`) capturing sample size and all
#        statistics.
#      – Create a publication-styled scatter plot (`fig`) with a regression
#        line and an in-panel annotation of r and p.
#      – Store the row, the plot, and a systematic file name in a list.
# 4. Flatten all lists into `corL`.
#
# Outputs
# ───────
#
# • correlations.xlsx  – an Excel workbook containing the full correlation
#   table (`cor_tbl`), one row per X-Y-period-gender combination.
# • correlations/      – a sub-folder holding, for *every* plot in `corL`,
#     ◦ an SVG graphic  (vector quality for manuscripts) and
#     ◦ a single-slide PowerPoint file, useful when investigators need to
#       tweak or present figures.
#
#   Each figure file name follows the pattern
#     <period>_cor_<X>_<Y>[_<gender>].svg | .pptx
#   ensuring easy traceability back to the numerical results.

Y <- c("preAlb", "ΔpreAlb", "Albumine")
corL <- unlist(recursive = FALSE, lapply(Y, function(y) {
    periods <- c("FU_end", "PO", "6M", "1Y", "3Y")
    X <- c("ALMI", "LMI", "LMTot", "FMI", "TWL", "Albumine", "ALM/poids")
    if (y == "ΔpreAlb") {
        X <- "Masse_maigre_perdue_pct"
    } else if (y == "Albumine") {
        X <- X[X != "Albumine"]
    }
    genders <- c(NA, "F", "M")
    unlist(recursive = FALSE, lapply(periods, function(p) {
        unlist(recursive = FALSE, lapply(X, function(x) {
            r <- lapply(genders, function(g) {
                g2 <- if (is.na(g)) c("F", "M") else g
                d <- lg %>%
                    filter(period == p, Gender %in% g2) %>%
                    select(any_of(c(x, y))) %>%
                    drop_na()
                n <- nrow(d)
                if (n == 0) {
                    return(NULL)
                } else if (n <= 3) {
                    cor_pearson <- NA
                    cor_pearson_lwr <- NA
                    cor_pearson_upr <- NA
                    cor_pearson_pv <- NA
                    cor_spearman <- NA
                    cor_spearman_lwr <- NA
                    cor_spearman_upr <- NA
                    cor_spearman_pv <- NA
                } else {
                    delta <- qnorm(0.975) / sqrt(nrow(d) - 3)
                    cor_pearson <- cor(d[[x]], d[[y]])
                    cor_pearson_lwr <- tanh(atanh(cor_pearson) - delta)
                    cor_pearson_upr <- tanh(atanh(cor_pearson) + delta)
                    cor_pearson_pv <- cor.test(d[[x]], d[[y]])$p.value
                    cor_spearman <- cor(d[[x]], d[[y]], method = "spearman")
                    cor_spearman_lwr <- tanh(atanh(cor_spearman) - delta)
                    cor_spearman_upr <- tanh(atanh(cor_spearman) + delta)
                    cor_spearman_pv <- cor.test(d[[x]], d[[y]],
                                                method = "spearman",
                                                exact = FALSE)$p.value
                }
                tbl_row <- data.frame(
                    period = p,
                    gender = g,
                    variable1 = x,
                    variable2 = y,
                    nobs = nrow(d),
                    cor_pearson = cor_pearson,
                    cor_pearson_lwr = cor_pearson_lwr,
                    cor_pearson_upr = cor_pearson_upr,
                    cor_pearson_pv = cor_pearson_pv,
                    cor_spearman = cor_spearman,
                    cor_spearman_lwr = cor_spearman_lwr,
                    cor_spearman_upr = cor_spearman_upr,
                    cor_spearman_pv = cor_spearman_pv
                )
                a <- paste0("R = ", signif(cor_pearson, 2), ", p = ",
                            signif(cor_pearson_pv, 2))
                fig <- ggplot(d, aes(x = .data[[x]], y = .data[[y]])) +
                    geom_point() +
                    geom_smooth(method = lm, se = FALSE, formula = y ~ x,
                                color = "black") +
                    annotate("text", x = -Inf, y = Inf, hjust = -.2, vjust = 5,
                             label = a) +
                    theme_pubr()
                fig_filename <- c(p, "cor", sub("/", "_", x), y, g) %>%
                    na.omit() %>%
                    paste(collapse = "_") %>%
                    paste0(".svg")
                list(tbl_row = tbl_row, fig = fig, fig_filename = fig_filename)
            })
            r[!sapply(r, is.null)]
        }))
    }))
}))
rm(Y)

# Tables
cor_tbl <- do.call(rbind, lapply(corL, function(z) z$tbl_row))
write_xlsx(cor_tbl, file.path(outdir, "correlations.xlsx"))

# Figures
o <- file.path(outdir, "correlations")
if (!dir.exists(o)) dir.create(o)
for (z in corL) {
    ggsave(file.path(o, z$fig_filename), z$fig)
    doc <- read_pptx()
    doc <- add_slide(doc, 'Title and Content', 'Office Theme')
    anyplot <- dml(ggobj = z$fig)
    doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
    print(doc, target = file.path(o, sub("svg$", "pptx", z$fig_filename)))
}
rm(o, z, doc, anyplot)

# ── Session Info ─────────────────────────────────────────────────────────────

sink(file.path(outdir, "sessionInfo.txt"))
print(sessionInfo(), locale = FALSE)
sink()
