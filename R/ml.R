# load required packages
library("naniar")
library(dplyr)
library(here)
library("mice")
library("texreg")

# load data
final.data <- read.csv(here("data", "analytical", "finaldata.csv"), 
                       header = TRUE)
final.data <- final.data|>
  mutate(loggdp = log(gdp1000), 
         pctpopdens = popdens/100) # add log gdp, avoid invalid log(0)

# visualize missing data
final.data |> 
  arrange(Year, ISO) |>
  dplyr::select(-country_name, -ISO, -region, -Year) |>
  vis_miss()

# convert ISO to numeric
midata <- final.data |>
  mutate(ISOnum = as.numeric(as.factor(ISO))) |>
  select(-country_name, -ISO, -region, -OECD2023)

# dry run to get meth and pred
mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

# edit meth and pred
meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", 
       "MatMortality", "InfMortality", "NeoMortality", "Und5Mortality", 
       "loggdp", "pctpopdens")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", 
       "MatMortality", "InfMortality", "NeoMortality", "Und5Mortality", 
       "loggdp", "pctpopdens"), "ISOnum"] <- -2

# perform MI with m=10 imputations
mice.multi.out <- mice(midata, seed = 100, m = 10, maxit = 20, 
                       method = meth, predictorMatrix = pred)

# check for convergence
plot(mice.multi.out)

# fit models for the 4 mortality using lm()
matmor.mice <- with(mice.multi.out, 
                    lm(MatMortality ~ -1 + armed_conflict + loggdp + OECD + 
                         pctpopdens + urban + agedep + male_edu + temp + 
                         rainfall1000 + Earthquake + Drought + as.factor(Year)))

infmor.mice <- with(mice.multi.out, 
                    lm(InfMortality ~ -1 + armed_conflict + loggdp + OECD + 
                         pctpopdens + urban + agedep + male_edu + temp + 
                         rainfall1000 + Earthquake + Drought + as.factor(Year)))

neomor.mice <- with(mice.multi.out, 
                    lm(NeoMortality ~ -1 + armed_conflict + loggdp + OECD + 
                         pctpopdens + urban + agedep + male_edu + temp + 
                         rainfall1000 + Earthquake + Drought + as.factor(Year)))

und5mor.mice <- with(mice.multi.out, 
                     lm(Und5Mortality ~ -1 + armed_conflict + loggdp + OECD + 
                          pctpopdens + urban + agedep + male_edu + temp + 
                          rainfall1000 + Earthquake + Drought + as.factor(Year)))

out.matmor <- pool(matmor.mice)
out.infmor <- pool(infmor.mice)
out.neomor <- pool(neomor.mice)
out.und5mor <- pool(und5mor.mice)

# complete case analysis
x <- as.formula(" ~ -1 + armed_conflict + loggdp + OECD + pctpopdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + Earthquake + Drought + 
                  as.factor(ISO) + as.factor(Year)")

matmor.cc <- lm(update.formula(x, MatMortality ~ .), data = final.data)
infmor.cc <- lm(update.formula(x, InfMortality ~ .), data = final.data)
neomor.cc <- lm(update.formula(x, NeoMortality ~ .), data = final.data)
und5mor.cc <- lm(update.formula(x, Und5Mortality ~ .), data = final.data)

tosave <- list(out.matmor, out.infmor, out.neomor, out.und5mor, 
               matmor.cc, infmor.cc, neomor.cc, und5mor.cc)

keepvars <- list("armed_conflict" = "Armed conflict",
                 "loggdp" = "log(GDP)",
                 "OECD" = "OECD",
                 "pctpopdens" = "Population density",
                 "urban" = "Urban",
                 "agedep" = "Age dependency",
                 "male_edu" = "Male education",
                 "temp" = "Average temperature",
                 "rainfall1000" = "Average rainfall")

screenreg(tosave, 
          ci.force = TRUE,
          custom.coef.map = keepvars,
          custom.model.names = c("Mat CC", "Mat MI", "Un5 CC", "Un5 MI", "Inf CC", "Inf MI", "Neo CC", "Neo MI"))

save(tosave, file = here("output", "mi_output.Rdata"))





