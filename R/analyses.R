library(dplyr)
library(ggplot2)
library(ggpubr)
library(parallel)
library(readxl)
library(svglite)
library(tidyr)
library(writexl)

options(mc.cores = detectCores() - 1) 

# Working directory
setwd("~/Projects/Consultations/Favre Lucie (COOL-Prealbumine)")

# Output directory
outdir <- paste0("results/analyses_", format(Sys.Date(), "%Y%m%d"))
if (!dir.exists(outdir)) dir.create(outdir)

# Import data
f <- c("data-raw/COOL CC prealb All DXA + selection creat & prealbumin.xlsx",
       "data-raw/Timepoints prealb.xlsx")
dta <- list(read_xlsx(f[1], sheet = "Selection eGFR >30 et CRP <30"),
            read_xlsx(f[2], sheet = "Selection eGF>30 et CRP <30"))
rm(f)

# Data preprocessing
dta <- mclapply(dta, function(d) {
  names(d)[names(d) == "Id...1"] <- "Id"
  d <- d[names(d) != "Id...9"]
  # Remove units
  d <- d[!grepl("unit$", names(d))]
  # Remove empty variables
  d <- d[sapply(d, function(x) any(!is.na(x)))]
  # Convert character variables which contain numeric values to numeric
  for (j in which(!grepl("CRP$", names(d)))) {
    x <- d[[j]]
    if (all(is.na(x) | grepl("^(-)?[0-9]+(\\.[0-9]+)?$", x))) {
      d[[j]] <- as.numeric(x)
    }
  }
  if ("6M_CRP" %in% names(d)) d$`6M_CRP` <- as.character(d$`6M_CRP`)
  # Factor variable(s)
  d$Gender <- factor(d$Gender, c("F", "M"))
  # preAlb - Groups
  for (x in grep("preAlb", names(d), value = TRUE)) {
    d[[paste0(x, "2")]] <- factor(d[[x]] < .2, c(FALSE, TRUE),
                                  c(">=0.2", "<0.2"))
  }
  d
})

# Longitudinal data
lg2 <- dta[[2]] %>%
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
  ) %>%
  rename_with(~ sub("^(PO|6M|1Y|3Y)(_)(.+)", "\\1__\\3", .x)) %>%
  pivot_longer(
    cols = starts_with(c("PO__", "6M__", "1Y__", "3Y__")),
    names_to = c("period", ".value"), 
    names_pattern = "(.+)__(.+)"
  ) %>%
  mutate(ΔLM = factor(Masse_maigre_perdue_pct <= 25, c(TRUE, FALSE),
                      c("<=25%", ">25%")))
lg1 <- dta[[1]] %>%
  rename_with(~ sub("^DX[0-9]{2}_", "", .x)) %>%
  rename(Date_BS = DateBS) %>%
  mutate(period = "FU_end") %>%
  select(any_of(names(lg2)))
lg <- bind_rows(lg2, lg1)
rm(lg1, lg2)

# Tables
tbls <- unlist(recursive = FALSE, mclapply(1:2, function(k) {
  if (k == 1) {
    periods <- c("FU_end", "PO", "6M", "1Y", "3Y")
    Y <- c("ALMI", "LMI", "LMTot", "FMI", "TWL", "Albumine")
  } else {
    periods <- c("6M", "1Y", "3Y")
    Y <-"preAlb"
  }
  lapply(1:2, function(l) {
    u <- c(c("preAlb2", "ΔLM")[k], "Gender")[l]
    U <- list(list(c(">=0.2", "<0.2"), c("<=25%", ">25%"))[[k]],
              c("F", "M"))[[l]]
    var.g <- c("Gender", c("preAlb2", "ΔLM")[k])[l]
    G <- list(c("", "F", "M"),
              list(c("", ">=0.2", "<0.2"), c("", "<=25%", ">25%"))[[k]])[[l]]
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
          fml <- as.formula(paste(y, "~", u))
          if (length(x1) >= 2 & length(x2) >= 2) {
            z <- t.test(fml, d)
          } else {
            z <- list(stderr = NA, conf.int = c(NA, NA), p.value = NA)
          }
          w <- wilcox.test(fml, d, exact = FALSE)$p.value
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

# Figures
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
  ggplot(d, aes(x = diff, y = GROUP)) +
    geom_point() +
    geom_errorbar(aes(xmin = diff_lwr, xmax = diff_upr), width = .1) +
    geom_vline(aes(xintercept = 0), colour = "grey70") +
    facet_grid(rows = vars(period), cols = vars(variable), scales = "free") +
    labs(x = "Difference", y = 0, title = ttl)
})
for (s in names(figs)) {
  svglite(file.path(outdir, paste0(s, ".svg")))
  print(figs[[s]])
  dev.off()
}
rm(s)

# Correlations
periods <- c("FU_end", "PO", "6M", "1Y", "3Y")
corL <- unlist(recursive = FALSE, mclapply(periods, function(p) {
  X <- c("ALMI", "LMI", "LMTot", "FMI", "TWL", "Albumine")
  if (p == "PO") X <- X[X != "TWL"]
  lapply(X, function(x) {
    y <- "preAlb"
    d <- na.omit(lg[lg$period == p, c(x, y)])
    delta <- qnorm(0.975) / sqrt(nrow(d) - 3)
    cor_pearson <- cor(d[[x]], d[[y]])
    cor_pearson_lwr <- tanh(atanh(cor_pearson) - delta)
    cor_pearson_upr <- tanh(atanh(cor_pearson) + delta)
    cor_pearson_pv <- cor.test(d[[x]], d[[y]])$p.value
    cor_spearman <- cor(d[[x]], d[[y]], method = "spearman")
    cor_spearman_lwr <- tanh(atanh(cor_spearman) - delta)
    cor_spearman_upr <- tanh(atanh(cor_spearman) + delta)
    cor_spearman_pv <- cor.test(d[[x]], d[[y]], method = "spearman",
                                exact = FALSE)$p.value
    tbl_row <- data.frame(
      period = p,
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
    fig <- ggscatter(d, x, y, add = "reg.line") +
      stat_cor() +
      labs(title = p)
  fig_filename <- paste0(paste(p, "cor", x, y, sep = "_"), ".svg")
    list(tbl_row = tbl_row, fig = fig, fig_filename = fig_filename)
  })
}))
rm(periods)

# Correlations - Table
cor_tbl <- do.call(rbind, lapply(corL, function(z) z$tbl_row))
write_xlsx(cor_tbl, file.path(outdir, "correlations.xlsx"))

# Correlations - Figures
o <- file.path(outdir, "correlations")
if (!dir.exists(o)) dir.create(o)
for (z in corL) {
  svglite(file.path(o, z$fig_filename))
  print(z$fig)
  dev.off()
}
rm(o, z)

# Session Info
sink(file.path(outdir, "sessionInfo.txt"))
print(sessionInfo(), locale = FALSE)
sink()

