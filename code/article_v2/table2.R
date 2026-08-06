# ╭───────────────────────────────────────────────────────────────────────────╮
# │     Table 2: Changes over three years after Roux-en-Y gastric bypass      │
# ╰───────────────────────────────────────────────────────────────────────────╯

suppressPackageStartupMessages({
    library(car)
    library(dplyr)
    library(emmeans)
    library(here)
    library(knitr)
    library(lmerTest)
    library(purrr)
    library(tibble)
    library(tidyr)
    library(writexl)
})

# Set project root directory
i_am("code/article_v2/table2.R")

data_dir <- here("data")
output_dir <- here("output/article_v2")

# Load data
load(here(data_dir, "data_july_2026.rda"))

# Prepare analysis variables
time_levels <- c(
    Preoperative = "0_PO",
    `6 months` = "1_6M",
    `1 year` = "2_1Y",
    `3 years` = "3_3Y"
)
gender_levels <- c(Female = "F", Male = "M")

lg <- lg |>
    rename(Time = `FU-CC`, Sex = Gender, Subject = `Subject ID`) |>
    mutate(
        Time = factor(Time, time_levels, names(time_levels)),
        Sex = factor(Sex, gender_levels, names(gender_levels)),
        `preAlb<0.20` = preAlb < 0.20,
        CRP = coalesce(hsCRP, as.numeric(sub("^<", "", CRP))),
        LMTot = LMTot / 1000,
        FMTot = FMTot / 1000
    )

# Variables and precision
table_spec <- tribble(
    ~label,                         ~variable,     ~digits,
    "Body weight, kg",             "CC_weight",         1,
    "BMI, kg/m²",                  "BMIc",              1,
    "Total weight loss, %",        "TWL%",              1,
    "Prealbumin, g/L",             "preAlb",            3,
    "Total lean mass, kg",         "LMTot",             1,
    "Lean mass, % of body weight", "LMTot-pc",          1,
    "Total fat mass, kg",          "FMTot",             1,
    "Fat mass, % of body weight",  "FMTotpc",           1,
    "ALMI, kg/m²",                 "ALMI",              2,
    "LMI, kg/m²",                  "LMI",               2,
    "FMI, kg/m²",                  "FMI",               2,
    "ALM / body weight",           "ALM/weight",        3
)

time_columns <- names(time_levels)
empty_p_values <- c(Time = "", Sex = "", `Sex × time` = "")
not_applicable <- c(Time = "—", Sex = "—", `Sex × time` = "—")

format_mean_sd <- function(x, digits) {
    fmt <- paste0("%.", digits, "f (%.", digits, "f)")
    sprintf(fmt, mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

format_estimate <- function(estimate, se, digits) {
    fmt <- paste0("%.", digits, "f (%.", digits, "f)")
    sprintf(fmt, estimate, se)
}

format_contrast <- function(estimate, lower, upper, digits) {
    fmt <- paste0("%+.", digits, "f (%+.", digits, "f, %+.", digits, "f)")
    sprintf(fmt, estimate, lower, upper)
}

format_p <- function(x) {
    ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
}

# Sex weights are fixed to the baseline cohort composition
sex_weights <- lg |>
    filter(Time == "Preoperative") |>
    count(Sex) |>
    arrange(Sex) |>
    pull(n)

# Fit one random-intercept model per continuous response
fits <- map(table_spec$variable, function(variable) {
    response <- paste0("`", variable, "`")
    lmer(reformulate(c("Time * Sex", "(1 | Subject)"), response), data = lg)
})
names(fits) <- table_spec$variable

# Joint Wald tests
joint_test <- function(fit, pattern) {
    hypotheses <- names(fixef(fit))[grepl(pattern, names(fixef(fit)))]
    linearHypothesis(fit, hypotheses, test = "Chisq")[2, "Pr(>Chisq)"]
}

# Rows of Table 2
model_rows <- pmap(table_spec, function(label, variable, digits) {
    fit <- fits[[variable]]

    # Observed mean (SD)
    observed <- lg |>
        group_by(Time) |>
        summarize(value = format_mean_sd(.data[[variable]], digits),
                  .groups = "drop") |>
        complete(Time = factor(time_columns, levels = time_columns),
                 fill = list(value = "—")) |>
        arrange(Time) |>
        pull(value)

    p_values <- c(
        Time = joint_test(fit, "Time"),
        Sex = joint_test(fit, "Sex"),
        `Sex × time` = joint_test(fit, "Time.*:Sex")
    ) |>
        format_p()

    observed_row <- c(Characteristic = label,
                      setNames(observed, time_columns), p_values)

    # Marginal means averaged using the baseline sex composition
    emm_grid <- suppressMessages(emmeans(
        fit, ~ Time, weights = sex_weights, lmer.df = "asymptotic"
    ))
    emm <- emm_grid |>
        as.data.frame()
    estimated <- setNames(rep("—", length(time_columns)), time_columns)
    estimated[as.character(emm$Time)] <- map2_chr(
        emm$emmean, emm$SE, ~ format_estimate(.x, .y, digits)
    )
    estimated_row <- c(Characteristic = "  Estimated mean (SE)",
                       estimated, empty_p_values)

    # Contrasts use the first modeled assessment as the reference. For total
    # weight loss this is 6 months because preoperative values are undefined.
    contrast_label <- if (variable == "TWL%") {
        "  Δ vs 6 months (95% CI)"
    } else {
        "  Δ vs preoperative (95% CI)"
    }
    changes <- setNames(rep("—", length(time_columns)), time_columns)
    reference <- as.character(emm$Time[1])
    changes[reference] <- "reference"
    contrasts <- contrast(emm_grid, method = "trt.vs.ctrl", ref = 1) |>
        confint(adjust = "none") |>
        as.data.frame()
    names(contrasts)[names(contrasts) == "asymp.LCL"] <- "lower.CL"
    names(contrasts)[names(contrasts) == "asymp.UCL"] <- "upper.CL"
    contrast_times <- sub(" - .*", "", contrasts$contrast)
    changes[contrast_times] <- pmap_chr(
        contrasts[c("estimate", "lower.CL", "upper.CL")],
        ~ format_contrast(..1, ..2, ..3, digits)
    )
    contrast_row <- c(Characteristic = contrast_label,
                      changes, empty_p_values)

    bind_rows(observed_row, estimated_row, contrast_row)
})

# Add the two descriptive biochemical outcomes after prealbumin
descriptive_rows <- function() {
    low_prealbumin <- lg |>
        group_by(Time) |>
        summarize(
            value = sprintf("%d (%.0f)", sum(`preAlb<0.20`, na.rm = TRUE),
                            100 * mean(`preAlb<0.20`, na.rm = TRUE)),
            .groups = "drop"
        ) |>
        arrange(Time) |>
        pull(value)

    crp <- lg |>
        group_by(Time) |>
        summarize(
            value = {
                q <- quantile(CRP, c(0.25, 0.50, 0.75), na.rm = TRUE)
                sprintf("%.1f [%.1f–%.1f]", q[2], q[1], q[3])
            },
            .groups = "drop"
        ) |>
        arrange(Time) |>
        pull(value)

    bind_rows(
        c(Characteristic = "Prealbumin < 0.20 g/L, n (%)",
          setNames(low_prealbumin, time_columns), not_applicable),
        c(Characteristic = "CRP, mg/L",
          setNames(crp, time_columns), not_applicable)
    )
}

prealbumin_row <- which(table_spec$variable == "preAlb")
table2 <- bind_rows(
    model_rows[seq_len(prealbumin_row)],
    descriptive_rows(),
    model_rows[(prealbumin_row + 1):length(model_rows)]
)

# Add observation counts and display Table 2 (section headings omitted)
observations <- lg |>
    count(Time) |>
    arrange(Time) |>
    pull(n) |>
    as.character()

table2 <- bind_rows(
    c(Characteristic = "Observations, n",
      setNames(observations, time_columns), not_applicable),
    table2
)

# Display Table 2
kable(table2, align = c("l", rep("c", 7)), row.names = FALSE)

# Save Table 2 as Excel file
write_xlsx(table2, here(output_dir, "table2.xlsx"))

# Caption
c(
    "Observed data are reported as mean (SD), except for C-reactive protein,
    which is reported as median [interquartile range], and prealbumin <0.20
    g/L, which is reported as n (%%). Estimated means (SE) and contrasts were
    derived from linear mixed-effects models using all %s observations from %s
    patients. Each model included time as a four-level categorical variable,
    sex, a sex-by-time interaction, and a patient-specific random intercept.
    Contrasts were calculated against the preoperative assessment, except for
    total weight loss, for which 6 months was the reference because
    preoperative values were undefined. Estimated means were averaged over sex
    using weights based on the sex distribution of the preoperative cohort
    (%s%% men).",
    "P values are from joint Wald tests. The time test jointly evaluates the
    time main-effect and sex-by-time interaction coefficients; the sex test
    jointly evaluates the sex main-effect and sex-by-time interaction
    coefficients; and the sex × time test evaluates the interaction
    coefficients only.",
    "Because the number of assessments differed across time points (%s, %s, %s
    and %s observations), observed means were calculated from partly different
    subsets of patients and should not be subtracted directly to estimate
    longitudinal change. The mixed-effects models account for within-patient
    correlation and use all available outcome-specific data. Their
    interpretation relies on correct model specification and a
    missing-at-random assumption.",
    "C-reactive protein and the binary indicator of prealbumin <0.20 g/L were
    presented descriptively and were not included as outcomes in the
    mixed-effects models. Continuous prealbumin concentration was analyzed
    using a mixed-effects model. C-reactive protein was not modeled because its
    distribution was skewed, values were left-censored at the assay detection
    limit, and the sample was truncated by the 10 mg/L inclusion criterion.",
    "ALM, appendicular lean mass; ALMI, appendicular lean mass index; BMI, body
    mass index; CI, confidence interval; CRP, C-reactive protein; FMI, fat mass
    index; LMI, lean mass index; SD, standard deviation; SE, standard error."
) |>
    lapply(\(p) paste(strwrap(p), collapse = " ")) |>
    paste(collapse = "\n\n") |>
    sprintf(
        nrow(lg),
        length(unique(lg$Subject)),
        round(sex_weights[2] / sum(sex_weights) * 100, 1),
        observations[1],
        observations[2],
        observations[3],
        observations[4]
    ) |>
    cat(file = here(output_dir, "table2_caption.txt"))

# Session information
sink(here(output_dir, "table2_sessionInfo.txt"))
sessionInfo()
sink()
