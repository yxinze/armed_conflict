library(here)
library(dplyr)
library(tidyr)

# import data
disaster <- read.csv(here("original", "disaster.csv"), header = TRUE)

# 8.abc filter data and create dummy variables for Earthquake and Drought
disaster.sub <- disaster %>%
  filter(Year %in% c(2000:2019) & Disaster.Type %in% c("Earthquake", "Drought")) %>% 
  select(Year, ISO, Disaster.Type) %>%
  mutate(Earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0), 
         Drought = ifelse(Disaster.Type == "Drought", 1, 0))

# 8.d group duplicated entries
disaster.grouped <- disaster.sub %>% 
  group_by(Year, ISO) %>%
  summarize(
    Drought = max(Drought),
    Earthquake = max(Earthquake),
    .groups = "drop"
  )