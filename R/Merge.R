library(here)
library(dplyr)
library(tidyr)

# import covariates data
covariates <- read.csv(here("original", "covariates.csv"), header = TRUE)
colnames(covariates)[which(names(covariates) == 'year')] <- "Year"

# call R scripts that create the individual data sets
source(here("R", "Mortality_v2.R"))
source(here("R", "disaster.R"))
source(here("R", "ArmedConflict.R"))

full.mort$Year <- as.numeric(full.mort$Year)
disaster.grouped$Year <- as.numeric(disaster.grouped$Year)
armed_conflict$Year <- as.numeric(armed_conflict$Year)
covariates$Year <- as.numeric(covariates$Year)

# the keys to join the data are: ISO and Year
# put all data sets into list
all.dat.list <- list(full.mort, armed_conflict, disaster.grouped)

# merge all data sets in list
all.dat.list |> reduce(full_join, by = c('ISO', 'Year')) -> finaldata0

finaldata <- covariates |>
  left_join(finaldata0, by = c('ISO', 'Year'))

# need to fill in NAs with 0's for armconf1, drought, earthquake
finaldata <- finaldata |>
  mutate(armed_conflict = replace_na(armed_conflict, 0),
         Drought = replace_na(Drought, 0),
         Earthquake = replace_na(Earthquake, 0),
         MatMortality = replace_na(MatMortality, 0),
         InfMortality = replace_na(InfMortality, 0),
         NeoMortality = replace_na(NeoMortality, 0),
         Und5Mortality = replace_na(Und5Mortality, 0))

# write csv file
write.csv(finaldata, file = here("data", "analytical", "finaldata.csv"), 
          row.names = FALSE)