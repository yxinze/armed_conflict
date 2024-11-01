# load required packages
library(here)
library(dplyr)
library(plm)
library(stargazer)

# load dataset
final.data <- read.csv(here("data", "analytical", "finaldata.csv"), header = TRUE)

# log-transform GDP
final.data <- final.data %>%
  mutate(log_gdp1000 = log(gdp1000 + 1)) # avoid invalid log(0)

# define model formula
x <- as.formula(" ~ armed_conflict + log_gdp1000 + OECD + popdens + urban + agedep + male_edu + temp + rainfall1000 + Earthquake + Drought + as.factor(Year)")

# fit models for the 4 mortality using plm()
matmor.mod <- plm(update(x, MatMortality ~ .), data = final.data, 
                  index = c("ISO", "Year"), model = "within")
infmor.mod <- plm(update(x, InfMortality ~ .), data = final.data, 
                  index = c("ISO", "Year"), model = "within")
neomor.mod <- plm(update(x, NeoMortality ~ .), data = final.data, 
                  index = c("ISO", "Year"), model = "within")
und5mor.mod <- plm(update(x, Und5Mortality ~ .), data = final.data, 
                   index = c("ISO", "Year"), model = "within")

summary(matmor.mod)
summary(infmor.mod)
summary(neomor.mod)
summary(und5mor.mod)

# create summary table
stargazer(matmor.mod, infmor.mod, neomor.mod, und5mor.mod, 
          type = "html", out = here("results", "mortality_models.html"))