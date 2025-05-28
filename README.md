# Is Prealbumin a Good Biomarker of Lean-Mass Loss After Roux-en-Y Gastric Bypass?

## Study summary

___BASED ON THE FIRST VERSION, TO BE UPDATED___

This single-centre study followed 243 adults (median follow-up ≈ 3 years) who had undergone laparoscopic Roux-en-Y gastric bypass to test whether plasma prealbumin reflects absolute lean mass—or loss of lean mass—measured by DXA. Across the cross-sectional cohort and a longitudinal sub-set (baseline, 6 months, 1 year, 3 years), prealbumin showed **no meaningful correlation** with appendicular lean-mass index, total lean-mass index or fat-mass index, nor did low prealbumin (< 0.20 g L⁻¹) identify participants who lost > 25 % lean mass. The findings suggest that routine prealbumin monitoring is **not a reliable proxy for post-operative protein malnutrition** in this population.

## Analytical workflow (`analyses.R`)

The R script documents every step that produced the numerical and graphical results found in the manuscript:

| Stage                         | Key actions                                                                                                                                                                                                                                                              |
| ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **Data import & cleaning**    | • Reads longitudinal and cross-sectional Excel workbooks from *data-raw/*.<br>• Proceeds to integrity checks, removes unit/empty columns, standardises variable names.                                                                                                   |
| **Descriptive summaries**     | • Builds tables of n, mean ± SD for body-composition indices and prealbumin.<br>• Compares subgroups (gender, prealbumin category, Δ-lean-mass category) with two-sample *t*- and Wilcoxon tests.                                                                        |
| **Group-difference figures**  | • Creates error-bar plots for each period × variable combination and saves SVG + PPTX files.                                                                                                                                                                             |
| **Correlation analysis**      | • For every period, gender and biomarker pair (prealbumin, Δ-prealbumin, albumin vs. ALMI, LMI, FMI) computes Pearson *r* and Spearman ρ with 95 % CIs.<br>• Exports a master `correlations.xlsx` and matching scatter plots with in-panel statistics.                   |
| **Outputs & reproducibility** | • All tables (`tables.xlsx`, `correlations.xlsx`), graphics (`correlations/*.svg`, single-slide `.pptx`) and `sessionInfo.txt` are written to `results/analyses_<date>/`.<br>• Random-seed-free code; parallelisation is used only for speed, not stochastic procedures. |

> **Note:** The repository shares **code only** for documentation; the underlying clinical data are confidential and therefore omitted.
