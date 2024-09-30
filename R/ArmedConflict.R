library(here)
library(dplyr)
library(tidyr)

# import conflict data
conflict <- read.csv(here("original", "conflictdata.csv"), header = TRUE)

# create data set that presents binary indicator of armed conflict 
# for each country each year
armed_conflict <- conflict %>%
  mutate(next_year = year + 1) %>% # adjust the lagged 1 year
  group_by(ISO, next_year) %>%
  # has armed conflict if <25 battle-related death in the year; 
  # no armed conflict if >=25 battle-related death in the year
  summarize(totdeath = sum(best, na.rm = TRUE), 
            armed_conflict = ifelse(sum(best, na.rm = TRUE) >= 25, 1, 0), 
            .groups = 'drop') 

colnames(armed_conflict) <- c("ISO", "Year", "totdeath", "armed_conflict")
