### Statistical analysis

Continuous variables are presented as mean (standard deviation [SD]), except
for C-reactive protein (CRP) and time since surgery, which are presented as
median [interquartile range] because of their skewed distributions. Categorical
variables are presented as number (percentage). Descriptive results are
reported overall and by sex, as appropriate.

Longitudinal trajectories were analysed using separate linear mixed-effects
models for each continuous anthropometric, biochemical, and body-composition
outcome. Time was modelled as a categorical variable (preoperative, 6 months, 1
year, and 3 years), with fixed effects for time, sex, and their interaction,
and a participant-specific random intercept to account for repeated
measurements. Model-based marginal means at each time point were averaged over
sex using the sex distribution of the preoperative cohort. Contrasts compared
each postoperative assessment with the preoperative assessment; for total
weight loss, for which a preoperative value is undefined, 6 months was used as
the reference. Confidence intervals for these contrasts were not adjusted for
multiplicity. Joint Wald chi-squared tests were used to assess time (all time
and time-by-sex coefficients), sex (the sex and time-by-sex coefficients), and
the time-by-sex interaction (interaction coefficients only). CRP and the binary
indicator of prealbumin <0.20 g/L were summarized descriptively and were not
modelled as longitudinal outcomes.

The ability of serum prealbumin to identify excessive lean-mass loss was
evaluated separately at 6 months, 1 year, and 3 years. Excessive loss was
defined as lean mass accounting for more than 25% of the body weight lost since
surgery. Receiver operating characteristic (ROC) curves used the prealbumin
concentration measured at the corresponding assessment and were oriented a
priori so that lower prealbumin indicated greater predicted risk; thus, an area
under the ROC curve (AUC) below 0.50 indicates an association in the direction
opposite to the prespecified hypothesis. Stratified bootstrap resampling with
10,000 replicates was used to obtain 95% confidence intervals for the AUC. At
the clinical threshold of prealbumin <0.20 g/L, sensitivity, specificity,
positive predictive value, and negative predictive value were reported with
Wilson 95% confidence intervals. The positive likelihood ratio was reported
with a log-scale 95% confidence interval, and the maximum Youden index across
all thresholds was calculated.

As a complementary analysis, lean mass lost since surgery was expressed as a
percentage of preoperative lean mass and analysed as a continuous outcome at
each postoperative time point using linear regression adjusted for sex.
Coefficients were expressed as the difference in percentage points per 0.05 g/L
lower prealbumin concentration. To illustrate the consequences of dichotomising
this outcome without an established clinical cut-off, AUCs were also calculated
across thresholds ranging from 6% to 20% of preoperative lean mass lost. DeLong
95% confidence intervals were used in this analysis; thresholds yielding fewer
than 15 cases were identified, and estimates were not displayed when either
outcome group contained five or fewer participants.

Potential attrition bias at 3 years was assessed by comparing preoperative
characteristics of participants with and without an available 3-year DXA
assessment. Continuous variables were compared using Welch two-sample t-tests,
CRP using the Wilcoxon rank-sum test, sex using a continuity-corrected
two-sample test of proportions, and prealbumin <0.20 g/L using Fisher's exact
test. For continuous variables, signed standardized mean differences were
calculated as the mean among participants with a 3-year assessment minus that
among participants without one, divided by the square root of the unweighted
mean of the two group variances.

Cross-sectional analyses used each participant's most recent eligible
assessment performed more than 3 years after surgery. Associations between
prealbumin and total lean mass, lean mass as a percentage of body weight,
appendicular lean mass index (ALMI), lean mass index, ALM/body weight, and fat
mass index were assessed using ordinary least-squares regression. The first
model adjusted for sex; the second additionally adjusted for age and time since
surgery. Coefficients were expressed as the expected difference in the outcome
per 0.05 g/L lower prealbumin. Partial correlation coefficients, adjusted for
the same covariates, were derived from the corresponding t statistics, with 95%
confidence intervals based on Fisher's z transformation. We also examined
whether these intervals lay entirely between -0.30 and +0.30. The association
between prealbumin and ALMI was additionally estimated within three
postoperative intervals (3 to <5, 5 to <10, and ≥10 years), with adjustment for
sex and age. Body-composition outcomes were also compared between participants
with prealbumin <0.20 and ≥0.20 g/L using linear regression adjusted for sex,
age, and time since surgery. Adjusted differences were defined as the mean in
the lower-prealbumin group minus that in the higher-prealbumin group.
Sex-specific scatterplots, ordinary least-squares regression lines, and
unadjusted Pearson correlations were used for descriptive visualization of the
associations of prealbumin with ALMI and ALM/body weight.

Regression confidence intervals and P values were based on the t distribution
unless otherwise specified. Analyses used available observations for each
outcome; no missing values were imputed. Linear mixed-effects models used all
available repeated measurements under a missing-at-random assumption, whereas
other models used complete cases for the variables included in that analysis.
All tests were two-sided, with P < 0.05 considered statistically significant.
No adjustment for multiple testing was applied. Analyses were performed using R
version 4.6.1 (R Foundation for Statistical Computing, Vienna, Austria),
principally with the lme4, lmerTest, emmeans, car, and pROC packages.
