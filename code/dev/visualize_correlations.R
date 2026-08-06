# Remove na values
lg_clean <- lg %>%
    drop_na(ALMI, preAlb, Gender, period)

# Direct estimate of the partial correlations (same results than in tbl_cor)
lg_clean %>%
    group_by(period, Gender) %>%
    reframe(
        nobs = n(),
        par_cor_object = list(cor.test(ALMI, preAlb)),
        par_cor = par_cor_object[[1]]$estimate,
        par_cor_lwr = par_cor_object[[1]]$conf.int[1],
        par_cor_upr = par_cor_object[[1]]$conf.int[2],
        par_cor_pv = par_cor_object[[1]]$p.value
    ) %>%
    select(-par_cor_object)


# Estimate of the partial correlations with a full fixed effects model (all
# interactions)
fit <- lm(preAlb ~ ALMI * Gender * period, data = lg_clean)
sigma_e <-  summary(fit)$sigma
sd_table <- lg_clean %>%
  group_by(Gender, period) %>%
  summarize(sdX = sd(ALMI), .groups = "drop")
trends <- emtrends(fit, ~ Gender * period, var = "ALMI", infer = c(TRUE, TRUE))
slopes <- as.data.frame(trends) %>%
    rename(
        beta    = ALMI.trend,
        se_beta = SE,
        df      = df,
        t_ratio = t.ratio,
        p_beta  = p.value,
        lwr_beta= lower.CL,
        upr_beta= upper.CL
  )
slopes %>%
    left_join(sd_table, by = c("Gender", "period")) %>%
    mutate(
        cor = beta * sdX / sqrt(beta^2 * sdX^2 + sigma_e^2),
        cor_lwr  = lwr_beta* sdX / sqrt(lwr_beta^2 * sdX^2 + sigma_e^2),
        cor_upr  = upr_beta* sdX / sqrt(upr_beta^2 * sdX^2 + sigma_e^2),
        p_cor    = p_beta
    ) %>%
    select(Gender, period, cor, cor_lwr, cor_upr, p_cor)

# Pooling the periods
fit <- lm(preAlb ~ ALMI * Gender + period, data = lg_clean)
sigma_e <-  summary(fit)$sigma
sd_table <- lg_clean %>%
  group_by(Gender) %>%
  summarize(sdX = sd(ALMI))
trends <- emtrends(fit, ~ Gender, var = "ALMI", infer = c(TRUE, TRUE))
slopes <- as.data.frame(trends) %>%
    rename(
        beta    = ALMI.trend,
        se_beta = SE,
        df      = df,
        t_ratio = t.ratio,
        p_beta  = p.value,
        lwr_beta= lower.CL,
        upr_beta= upper.CL
  )
slopes %>%
    left_join(sd_table, by = "Gender") %>%
    mutate(
        cor = beta * sdX / sqrt(beta^2 * sdX^2 + sigma_e^2),
        cor_lwr  = lwr_beta* sdX / sqrt(lwr_beta^2 * sdX^2 + sigma_e^2),
        cor_upr  = upr_beta* sdX / sqrt(upr_beta^2 * sdX^2 + sigma_e^2),
        p_cor    = p_beta
    ) %>%
    select(Gender, cor, cor_lwr, cor_upr, p_cor)



fit <- lmer(preAlb ~ ALMI * Gender + period + (1 | IPP), data = lg_clean)
sigma_e <-  summary(fit)$sigma
sd_table <- lg_clean %>%
  group_by(Gender) %>%
  summarize(sdX = sd(ALMI))
fitX <- lmer(ALMI ~ period + (1|IPP), data = lg_clean)
lg_clean$resX <- residuals(fitX)
sd_table <- lg_clean %>%
  group_by(Gender) %>%
  summarise(sdX = sd(resX), .groups="drop")
trends <- emtrends(fit, ~ Gender, var = "ALMI", infer = c(TRUE, TRUE))
slopes <- as.data.frame(trends) %>%
    rename(
        beta    = ALMI.trend,
        se_beta = SE,
        df      = df,
        t_ratio = t.ratio,
        p_beta  = p.value,
        lwr_beta= lower.CL,
        upr_beta= upper.CL
  )
slopes %>%
    left_join(sd_table, by = "Gender") %>%
    mutate(
        cor = beta * sdX / sqrt(beta^2 * sdX^2 + sigma_e^2),
        cor_lwr  = lwr_beta* sdX / sqrt(lwr_beta^2 * sdX^2 + sigma_e^2),
        cor_upr  = upr_beta* sdX / sqrt(upr_beta^2 * sdX^2 + sigma_e^2),
        p_cor    = p_beta
    ) %>%
    select(Gender, cor, cor_lwr, cor_upr, p_cor)


fitX <- lmer(ALMI ~ period + (1|IPP), data = lg_clean)
lg_clean$resX <- residuals(fitX)
sd_table <- lg_clean %>%
  group_by(Gender) %>%
  summarise(sdX = sd(resX), .groups="drop")
sd_table
lg_clean <- lg_clean %>%
  group_by(IPP) %>%
  mutate(ALMI_c = ALMI - mean(ALMI)) %>%
  ungroup()
sd_table <- lg_clean %>%
  group_by(Gender) %>%
  summarise(sdX = sd(ALMI_c), .groups="drop")
sd_table

#########################################################

### RM correlation
### See 10.3389/fpsyg.2017.00456 (bakdash2017repeated)

library(rmcorr)

lg_clean <- drop_na(lg, ALMI, preAlb, Gender, period)

# Female patients
rmf <- rmcorr(IPP, ALMI, preAlb,
              data = filter(lg_clean, Gender=="F"))

with(filter(lg_clean, Gender=="F"), cor(ALMI, preAlb))

# Male patients
rmm <- rmcorr(IPP, ALMI, preAlb,
              data = filter(lg_clean, Gender=="M"))
rmm$r; rmm$p; rmm$CI

svglite::svglite("~/tmp.svg")
plot(rmm)
dev.off()

rmm$model

# Store current options for contrasts
op <- options(contrasts = getOption("contrasts"))
# Set contrasts to sum contrasts (for type III sum of sq)
options(contrasts = c("contr.sum", "contr.poly"))
x <- "ALMI"
fml <- formula(paste("preAlb ~ IPP +", x))
fit <- lm(fml, data = filter(lg_clean, Gender == "M"))
# Coeficient of the explicative variable of interest
beta <- coef(fit)[x]
# Anova table (type III sum of sq)
aov_tbl <- drop1(fit, ~ ., test = "F")
SSX <- aov_tbl[x, "Sum of Sq"]
SSR <- aov_tbl["<none>", "RSS"]
r <- sqrt(SSX / (SSX + SSR)) * sign(beta)
r
# n = df.resid + 2
tanh(atanh(r) + c(-1, 1) * qnorm(0.975) / sqrt(fit$df.residual - 1))
aov_tbl[x, "Pr(>F)"]

svglite::svglite("~/tmp.svg", height = 15, width = 15)
lg_clean %>%
  filter(Gender == "M") %>%
  mutate(pred_x = predict(fit, newdata = .)) %>%
  ggplot(aes(x = ALMI, y = preAlb)) +
  geom_point(aes(color = period)) +
  geom_line(aes(y = pred_x)) +
  geom_smooth(method = "lm", se = FALSE, linetype="dashed", color = "grey") +
  facet_wrap(vars(IPP))
dev.off()





options(op)

svglite::svglite("~/tmp.svg")
lg_clean %>%
  filter(Gender == "M") %>%
  ggplot(aes(x = ALMI, y = preAlb, color = IPP)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)
dev.off()

svglite::svglite("~/tmp.svg")
lg_clean %>%
  filter(Gender == "M") %>%
  ggplot(aes(x = ALMI, y = preAlb)) +
  geom_point(aes(color = period)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(vars(IPP))
dev.off()
