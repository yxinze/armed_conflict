# load required packages
library(boot)
library(here)
library(dplyr)
library(knitr)
library(kableExtra)

# import data
dat <- read.csv(here("data", "analytical", "finaldata.csv"), header = TRUE)

# ==============================================================================

### For Maternal Mortality
# subset 2017 maternal mortality data
dat2017.mat <- dat |>
  filter(Year == 2017 & !is.na(MatMortality))

# function for calculating median difference
getmeddiff.mat <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$MatMortality, sample_data$armed_conflict, 
                       FUN = function(x) median(x,na.rm = TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

# 1000 bootstrap
set.seed(2024)
bootout.mat <- boot(dat2017.mat, statistic = getmeddiff.mat, 
                    strata = dat2017.mat$armed_conflict, R = 1000)
bootout.mat

# bootstrap confidence intervals
# 1. simple percentile bootstrap CI: (lower = 41.4875, upper = 275.0125)
quantile(bootout.mat$t, probs = c(0.025, 0.975))

# 2. simple basic bootstrap CI: (lower = -22.0125, upper = 211.5125)
2*bootout.mat$t0 - quantile(bootout.mat$t, probs = 0.975)
2*bootout.mat$t0 - quantile(bootout.mat$t, probs = 0.025)

# 3. bias-corrected and accelerated bootstrap CI
boot.ci(boot.out = bootout.mat, conf = 0.95, type = c("basic", "perc", "bca"))

# ==============================================================================

### For Infant Mortality
# subset 2017 infant mortality data
dat2017.inf <- dat |>
  filter(Year == 2017 & !is.na(InfMortality))

# function for calculating median difference
getmeddiff.inf <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$InfMortality, sample_data$armed_conflict, 
                       FUN = function(x) median(x,na.rm = TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

# 1000 bootstrap
set.seed(2024)
bootout.inf <- boot(dat2017.inf, statistic = getmeddiff.inf, 
                    strata = dat2017.inf$armed_conflict, R = 1000)
bootout.inf

# bootstrap confidence intervals
# 1. simple percentile bootstrap CI: (lower = 7.19875, upper = 29.10250)
quantile(bootout.inf$t, probs = c(0.025, 0.975))

# 2. simple basic bootstrap CI: (lower = 10.4975, upper = 32.40125)
2*bootout.inf$t0 - quantile(bootout.inf$t, probs = 0.975)
2*bootout.inf$t0 - quantile(bootout.inf$t, probs = 0.025)

# 3. bias-corrected and accelerated bootstrap CI
boot.ci(boot.out = bootout.inf, conf = 0.95, type = c("basic", "perc", "bca"))

# ==============================================================================

### For Under-5 Mortality
# subset 2017 under-5 mortality data
dat2017.und5 <- dat |>
  filter(Year == 2017 & !is.na(Und5Mortality))

# function for calculating median difference
getmeddiff.und5 <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Und5Mortality, sample_data$armed_conflict, 
                       FUN = function(x) median(x,na.rm = TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

# 1000 bootstrap
set.seed(2024)
bootout.und5 <- boot(dat2017.und5, statistic = getmeddiff.und5, 
                    strata = dat2017.und5$armed_conflict, R = 1000)
bootout.und5

# bootstrap confidence intervals
# 1. simple percentile bootstrap CI: (lower = 8.8500, upper = 44.3525)
quantile(bootout.und5$t, probs = c(0.025, 0.975))

# 2. simple basic bootstrap CI: (lower = 13.4475, upper = 48.95)
2*bootout.und5$t0 - quantile(bootout.und5$t, probs = 0.975)
2*bootout.und5$t0 - quantile(bootout.und5$t, probs = 0.025)

# 3. bias-corrected and accelerated bootstrap CI
boot.ci(boot.out = bootout.und5, conf = 0.95, type = c("basic", "perc", "bca"))

# ==============================================================================

### For Neonatal Mortality
# subset 2017 neonatal mortality data
dat2017.neo <- dat |>
  filter(Year == 2017 & !is.na(NeoMortality))

# function for calculating median difference
getmeddiff.neo <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$NeoMortality, sample_data$armed_conflict, 
                       FUN = function(x) median(x,na.rm = TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

# 1000 bootstrap
set.seed(2024)
bootout.neo <- boot(dat2017.neo, statistic = getmeddiff.neo, 
                     strata = dat2017.neo$armed_conflict, R = 1000)
bootout.neo

# bootstrap confidence intervals
# 1. simple percentile bootstrap CI: (lower = 4.60000, upper = 17.15125)
quantile(bootout.neo$t, probs = c(0.025, 0.975))

# 2. simple basic bootstrap CI: (lower = 6.54875, upper = 19.1)
2*bootout.neo$t0 - quantile(bootout.neo$t, probs = 0.975)
2*bootout.neo$t0 - quantile(bootout.neo$t, probs = 0.025)

# 3. bias-corrected and accelerated bootstrap CI
boot.ci(boot.out = bootout.neo, conf = 0.95, type = c("basic", "perc", "bca"))

# ==============================================================================

# function to extract confidence intervals from boot.ci output
extract_cis <- function(bootout) {
  ci <- boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))
  list(
    basic = c(ci$basic[4], ci$basic[5]),  # basic CI
    percentile = c(ci$perc[4], ci$perc[5]),  # percentile CI
    bca = c(ci$bca[4], ci$bca[5])  # BCa CI
  )
}

# extract CIs for each mortality type
mat_cis <- extract_cis(bootout.mat)
inf_cis <- extract_cis(bootout.inf)
und5_cis <- extract_cis(bootout.und5)
neo_cis <- extract_cis(bootout.neo)

# combine results into a data frame
ci_table <- data.frame(
  Mortality_Type = c("Maternal", "Infant", 
                     "Under-5", "Neonatal"),
  Basic_CI = c(
    sprintf("[%.2f, %.2f]", mat_cis$basic[1], mat_cis$basic[2]),
    sprintf("[%.2f, %.2f]", inf_cis$basic[1], inf_cis$basic[2]),
    sprintf("[%.2f, %.2f]", und5_cis$basic[1], und5_cis$basic[2]),
    sprintf("[%.2f, %.2f]", neo_cis$basic[1], neo_cis$basic[2])
  ),
  Percentile_CI = c(
    sprintf("[%.2f, %.2f]", mat_cis$percentile[1], mat_cis$percentile[2]),
    sprintf("[%.2f, %.2f]", inf_cis$percentile[1], inf_cis$percentile[2]),
    sprintf("[%.2f, %.2f]", und5_cis$percentile[1], und5_cis$percentile[2]),
    sprintf("[%.2f, %.2f]", neo_cis$percentile[1], neo_cis$percentile[2])
  ),
  BCa_CI = c(
    sprintf("[%.2f, %.2f]", mat_cis$bca[1], mat_cis$bca[2]),
    sprintf("[%.2f, %.2f]", inf_cis$bca[1], inf_cis$bca[2]),
    sprintf("[%.2f, %.2f]", und5_cis$bca[1], und5_cis$bca[2]),
    sprintf("[%.2f, %.2f]", neo_cis$bca[1], neo_cis$bca[2])
  )
)

# create a formatted table
ci_table %>%
  kable("html", col.names = c("Mortality", "Basic", "Percentile", "BCa")) %>%
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "hover", "condensed")) %>%
  save_kable(here("results","BootstrapCItable.html"))
View(ci_table)

# ==============================================================================
### Interpretation

## Infant Mortality:
# - Basic CI: The difference is between 10.40 and 32.45, suggesting a 
#   statistically significant increase in infant mortality in conflict-affected 
#   countries.
# - Percentile CI: The interval is 7.15 to 29.20, indicating a significant 
#   increase.
# - BCa CI: The interval is 7.74 to 30.16, further supporting the conclusion.
# Conclusion: All methods consistently indicate that infant mortality is 
# significantly higher in conflict-affected countries in 2017.

## Under-5 Mortality:
# - Basic CI: The difference is between 13.35 and 48.95, indicating a 
#   statistically significant increase in under-5 mortality in conflict-affected 
#   countries.
# - Percentile CI: The interval is 8.85 to 44.45, which also suggests a 
#   significant increase.
# - BCa CI: The interval is 9.45 to 45.80, reinforcing the conclusion.
# Conclusion: All methods agree that under-5 mortality is significantly higher 
# in conflict-affected countries.

## Neonatal Mortality:
# - Basic CI: The difference is between 6.50 and 19.10, indicating a 
#   statistically significant increase in neonatal mortality in conflict
#   -affected countries.
# - Percentile CI: The interval is 4.60 to 17.20, also suggesting a significant 
#   increase.
# - BCa CI: The interval is 4.65 to 17.45, further confirming the result.
# Conclusion: All methods consistently show that neonatal mortality is 
# significantly higher in conflict-affected countries.




