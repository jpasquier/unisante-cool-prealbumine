# ─────────────────────────────────────────────────────────────────────────────
# Re-do the first analysis (R/analyses.R) but only on cross-sectional data and
# patients with at least two years of follow-up (n=427)
# ─────────────────────────────────────────────────────────────────────────────

{
    library(dplyr)
    library(ggplot2)
    library(ggpubr)
    library(here)
    library(knitr)
    library(officer)
    library(parallel)
    library(rvg)
    library(svglite)
    library(tidyr)
    library(writexl)
} |>
    suppressMessages()

# Set options
options(mc.cores = detectCores() - 1)

# Working directory
# i_am("R/analyses.R") |> suppressMessages()
i_am("R/analyses_cs_2yfu.R") |> suppressMessages()

# Helper(s)
`%~=%` <- function(x, y) {
    is.na(x) & is.na(y) | !is.na(x) & !is.na(y) & x == y
}

# Output directory
# outdir <- here(paste0("results/analyses_", format(Sys.Date(), "%Y%m%d")))
outdir <- here(paste0("results/analyses_cs_2yfu_",
                      format(Sys.Date(), "%Y%m%d")))
if (!dir.exists(outdir)) dir.create(outdir)

# Load preprocessed data
load(here("data", "dta.rda"))
#load(here("data", "lg.rda"))

cs <- dta$cs %>%
    rename_with(~ sub("^DX[0-9]{2}_", "", .x)) %>%
    rename(Date_BS = DateBS) %>%
    filter(`FU_CC-calc` >= 2)

# ── Build the four comparison tables that will be saved to “tables.xlsx” ─────

tbls <- lapply(1:2, function(l) {
    Y <- c("ALMI", "LMI", "LMTot", "FMI", "TWL", "Albumine", "ALM/poids",
           "preAlb")
    u <- c("preAlb2", "Gender")[l]
    U <- list(c(">=0.2", "<0.2"), c("F", "M"))[[l]]
    var.g <- c("Gender", "preAlb2")[l]
    G <- list( c("", "F", "M"), c("", ">=0.2", "<0.2"))[[l]]
    r <- do.call(rbind, mclapply(Y, function(y) {
        if (y == "preAlb" && l == 1) return(NULL)
        do.call(rbind, lapply(G, function(g) {
            d <- if (g == "") {
                cs[c(u, y)]
            } else {
                cs[cs[[var.g]] == g, c(u, y)]
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
                period = "FU_END",
                subpop = "FU_CC-calc>=2",
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
    names(r)[names(r) == "GROUP"] <- c("gender", "preAlb")[l]
    names(r) <- sub("_1$", paste0(" ", u, U[1]), names(r))
    names(r) <- sub("_2$", paste0(" ", u, U[2]), names(r))
    attr(r, "Y") <- Y
    r
})
names(tbls) <- c("preAlb", "gender")
write_xlsx(tbls, file.path(outdir, "tables.xlsx"))

# ── Figures ──────────────────────────────────────────────────────────────────

figs <- mclapply(setNames(1:2, names(tbls)), function(k) {
    d <- tbls[[k]]
    d$GROUP <- if (k == 1) {
        factor(d$gender, c("M", "F", ""), c("Male", "Female", "All"))
    } else {
        factor(d$preAlb, c("<0.2", ">=0.2", ""),
               c("preAlb<0.2", "preAlb>=0.2", "All"))
    }
    d[d$variable == "LMTot", c("diff", "diff_lwr", "diff_upr")] <-
        d[d$variable == "LMTot", c("diff", "diff_lwr", "diff_upr")] / 10^3
    d$variable[d$variable == "LMTot"] <- "LMTot / 1000"
    d$variable = factor(d$variable, sub("LMTot", "LMTot / 1000", attr(d, "Y")))
    ttl <- paste("Mean differences between",
                 c("preAlb>=0.2 and preAlb<0.2", "females and males")[k],
                 "(at the end of follow-up)")
    select(d, variable, GROUP, diff, diff_lwr, diff_upr) %>%
        drop_na(diff) %>%
        ggplot(aes(x = diff, y = GROUP)) +
        geom_point() +
        geom_errorbar(aes(xmin = diff_lwr, xmax = diff_upr), width = .1) +
        geom_vline(aes(xintercept = 0), colour = "grey70") +
        facet_wrap(vars(variable), scales = "free") +
        labs(x = "Difference", y = 0, title = ttl, caption = "FU_CC-calc>=2")
})
for (s in names(figs)) {
    svglite(file.path(outdir, paste0(s, ".svg")))
    print(figs[[s]])
    dev.off()
}
rm(s)

# ── Correlation analyses and visual outputs ──────────────────────────────────

Y <- c("preAlb", "Albumine")
corL <- unlist(recursive = FALSE, lapply(Y, function(y) {
    p <- "FU_END"
    X <- c("ALMI", "LMI", "LMTot", "FMI", "TWL", "Albumine", "ALM/poids")
    if (y == "Albumine") {
        X <- X[X != "Albumine"]
    }
    genders <- c(NA, "F", "M")
    unlist(recursive = FALSE, lapply(X, function(x) {
        r <- lapply(genders, function(g) {
            g2 <- if (is.na(g)) c("F", "M") else g
            d <- cs %>%
                filter(Gender %in% g2) %>%
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
                subpop = "FU_CC-calc>=2",
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
                labs(caption = "FU_CC-calc>=2") +
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
rm(Y)

# Tables
cor_tbl <- do.call(rbind, lapply(corL, function(z) z$tbl_row))
write_xlsx(cor_tbl, file.path(outdir, "correlations.xlsx"))

# Figures
o <- file.path(outdir, "correlations")
if (!dir.exists(o)) dir.create(o)
for (z in corL) {
    ggsave(file.path(o, z$fig_filename), z$fig) |> suppressMessages()
    doc <- read_pptx()
    doc <- add_slide(doc, 'Title and Content', 'Office Theme')
    anyplot <- dml(ggobj = z$fig)
    doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
    print(doc, target = file.path(o, sub("svg$", "pptx", z$fig_filename)))
}
rm(o, z, doc, anyplot)

# Illustration of the Simpson Paradox
o <- file.path(outdir, "simpson_paradox_in_correlations.svg")
X <- c("ALMI", "LMI", "ALM/poids")
p <- lapply(X, function(x) {
    ggplot(cs, aes(x = .data[[x]], y = preAlb, color = Gender)) +
        geom_point(aes(color = Gender)) +
        geom_smooth(aes(color = NULL), method = lm, se = FALSE,
                    formula = y ~ x, color = "black") +
        geom_smooth(method = lm, se = FALSE, formula = y ~ x) +
        theme_pubr()
}) %>%
    ggarrange(plotlist = ., nrow = 1, common.legend = TRUE) %>%
    ggsave(o, plot = .) %>%
    suppressMessages()
rm(o, X, p)

# ── Session Info ─────────────────────────────────────────────────────────────

sink(file.path(outdir, "sessionInfo.txt"))
print(sessionInfo(), locale = FALSE)
sink()
