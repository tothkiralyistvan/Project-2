# Libraries
library(haven)
library(tidyverse)
library(janitor)
library(sjmisc)
library(sjstats)
library(psych)
library(summarytools)
library(skimr)

# Read data
positivity <- read_sav("data")
View(positivity)

# Explore data
View(positivity)
names(positivity)
summary(positivity, digits = 4)

# Let's fix the miscoded variables one by one
# Gender
freq(positivity$gender)

# Verify that gender is numerical
class(positivity$gender)

# Recode 1 outlier to NA
positivity <- positivity %>% 
  mutate(gender = ifelse(gender > 2, NA, gender))

# Age
freq(positivity$new_age)
class(positivity$age)

# Recode empty cells, textual responses, change variable type, recode outliers
positivity <- positivity %>%
  mutate(new_age4 = age) %>% 
  mutate(new_age4 = case_when(
    new_age4 == "tizenhet" ~ "17",
    TRUE ~ as.character(new_age4))) %>%
  mutate(new_age4 = as.numeric(new_age4)) %>% 
  mutate(new_age4 = ifelse(new_age4 < 15 | new_age4 > 92, NA, new_age4))
freq(positivity$new_age4)

positivity <- positivity %>%
  select(-age)

positivity %>% 
  select(pos1, pos2, pos3, pos4, pos5, pos6, pos7, pos8) %>% 
  freq()