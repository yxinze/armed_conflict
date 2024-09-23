library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(countrycode)

# import mortality data
mat.mort <- read.csv(here("original", "maternalmortality.csv"), header = TRUE) 
inf.mort <- read.csv(here("original", "infantmortality.csv"), header = TRUE) 
neo.mort <- read.csv(here("original", "neonatalmortality.csv"), header = TRUE) 
und5.mort <- read.csv(here("original", "under5mortality.csv"), header = TRUE) 

# create mortality list
mort.list <- list(mat.mort, inf.mort, neo.mort, und5.mort)

# create function that performs cleaning procedures
MortalityClean <- function(x) {
  # create subset
  x.sub <- x %>% 
    select(Country.Name, X2000:X2019)
  # pivot to long format
  x.long <- x.sub %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = c("Year"), 
      values_to = "Mortality"
    )
  # remove X in front of the years
  x.long$Year <- gsub("^X", "", x.long$Year)
  x.long
}

# apply to the four data sets
mort.list.clean <- lapply(mort.list, MortalityClean)

# merge the four data sets
full.mort <- reduce(mort.list.clean, 
                    ~full_join(.x, .y, c("Country.Name", "Year")))

# add ISO-3 country code
ISO <- countrycode(full.mort$Country.Name,
                   origin = "country.name",
                   destination = "iso3c")
full.mort$Country.Name <- ISO # replace Country Name with ISO

# modify column names
colnames(full.mort) <- c("ISO", "Year", 
                         "MatMortality", "InfMortality", 
                         "NeoMortality", "Und5Mortality")


