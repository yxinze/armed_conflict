library(here)
library(dplyr)
library(tidyr)

# import data
mat.mortality <- read.csv(here("original", "maternalmortality.csv"), header = TRUE) 

# 4.a create subset
mat.mortality.sub <- mat.mortality %>% 
  select(Country.Name, X2000:X2019)

# 4.b pivot the dataset to long format
mat.mortality.long <- mat.mortality.sub %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = c("Year"), 
    values_to = "MatMor"
  )

# remove X in front of the years
mat.mortality.long$Year <- gsub("^X", "", mat.mortality.long$Year)