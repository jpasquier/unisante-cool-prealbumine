# Prealbumin and lean mass after Roux-en-Y gastric bypass

## Study overview

This observational study evaluates whether serum prealbumin is associated with
DXA-derived lean mass and can identify excessive lean-mass loss after Roux-en-Y
gastric bypass. It combines a longitudinal cohort of 178 participants assessed
before surgery and up to 3 years afterwards with a complementary
cross-sectional cohort of 310 participants assessed more than 3 years after
surgery.

## Analysis code

The analyses for the current version of the manuscript are documented in
[`code/article_v2`](code/article_v2):

| Script | Content |
|---|---|
| `preprocess_data.R` | Imports and prepares the longitudinal and cross-sectional source data. |
| `table1.R` | Baseline characteristics of the longitudinal cohort. |
| `table2.R` | Longitudinal changes through 3 years using mixed-effects models. |
| `table3.R` | Diagnostic and regression analyses of lean-mass loss; also produces Figure 1. |
| `table4.R` | Characteristics of the long-term cross-sectional cohort. |
| `table5.R` | Cross-sectional associations between prealbumin and body composition. |
| `tableS1.R` | Comparison of participants with and without a 3-year DXA assessment. |
| `figureS1.R` | AUCs across alternative lean-mass-loss thresholds. |
| `figure2.R` | Sex-specific associations of prealbumin with ALMI and ALM/body weight. |

Generated tables, figures, captions, and session information are written to
`output/article_v2`. The scripts in `code/article_v1` and `code/exploratory`
relate to earlier or exploratory work and are not the reference analyses for
the current manuscript.

## Data availability and reproducibility

The underlying clinical data cannot be made publicly available because of
privacy and ethical restrictions. The scripts are therefore provided publicly
for documentation and methodological transparency only; this repository is not
a standalone reproducibility package for external users.

For authorized study investigators who have access to the source data, the
workflow is fully reproducible. After placing the required source files in
`data-raw`, the analysis datasets and manuscript outputs can be regenerated
with:

```sh
make preprocess_data_2
make article_v2
```
