# EXAMPLE OF PLOTTING PARITAL EFFECT ("ADJUSTED") OF A SPLINE ON ODDS RATIO SCALE
# Created by Lars Mølgaard Saxhaug (lars.molgaard.saxhaug@ntnu.no)

library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.1
library(mgcv) # Mixed GAM Computation Vehicle with Automatic Smoothness Estimation, CRAN v1.8-38
library(gratia) # Graceful 'ggplot'-Based Graphics and Other Functions for GAMs Fitted Using 'mgcv', CRAN v0.7.0
library(carData) # Companion to Applied Regression Data Sets, CRAN v3.0-5
options(contrasts = c("contr.treatment", "contr.treatment")) # simple treatment contrasts for ordinal variables


# data preparation ----
df <- TitanicSurvival %>%
  mutate(
    sex = as.factor(sex),
    passengerClass = as.ordered(passengerClass),
    survived = as.numeric(as.factor(survived)) - 1
  ) %>%
  drop_na(age)

# model fitting using gam from {mgcv} ----
simpmod <-
  gam(
    survived ~ sex + passengerClass + s(age,
                                        by = sex,
                                        bs = "tp", #thin plate restricted spline
                                        pc = 50),  # pc argument  sets a zero constraint for the smooth function, ie. in this case the reference age for the odds ratio we calculate later
    family = binomial,
    data = df
  )
anova(simpmod)
# smooth estimation and plotting ----
simpmod %>%
  smooth_estimates(
    smooth = "s(age)",
    partial_match = TRUE,
    overall_uncertainty = FALSE
  ) %>% # overall_uncertainty = FALSE leaves out the uncertainty for the intercept
  add_confint() %>% # adds confidence interval
  mutate(across(c(est, ends_with("ci")), exp)) %>%  # exponentiates "est" and the confidence interval variables to go from logOR to OR
  mutate(sex = str_remove(smooth, "s\\(age\\):sex")) %>% # create sex varible
  ggplot(aes(
    x = age,
    y = est,
    ymin = lower_ci,
    ymax = upper_ci,
    fill = sex,
    colour = sex
  )) +
  geom_line() +
  geom_ribbon(alpha = 0.3) +
  geom_vline(xintercept = 50,
             linetype = "dashed",
             size = 1.2) + #reference age
  scale_y_continuous(name = "OR vs age 50") + # title for y-axis
  facet_grid(rows = vars(sex),scales = "free") + # free
  theme_minimal() +
  theme(legend.position = "none") # remove legend
