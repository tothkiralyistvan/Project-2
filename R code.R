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

# Verify age
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

# Remove the "old" age variable as it's not useful anymore
positivity <- positivity %>%
  select(-age)

# Clean last variable
freq(positivity$age_tri)

positivity <- positivity %>%
  mutate(age_tri = ifelse(age_tri > 3, NA, age_tri))
freq(positivity$age_tri)

# Verify frequencies of items
positivity %>% 
  select(pos1, pos2, pos3, pos4, pos5, pos6, pos7, pos8) %>% 
  freq()

# Recode reverse coded item
positivity <- positivity %>%
  mutate(pos6rev = case_when(
    pos6 == 1 ~ 5,
    pos6 == 2 ~ 4,
    pos6 == 3 ~ 3,
    pos6 == 4 ~ 2,
    pos6 == 5 ~ 1,
    TRUE ~ NA_integer_  # Default value if none of the conditions match
  ))

# Rename average score created in SPSS
positivity <- positivity %>%
  rename(pos_old = positivity)

# Verify
freq(positivity$pos_old)

# Recode outlier values to NAs
positivity <- positivity %>%
  mutate(pos_old = ifelse(pos_old > 5, NA, pos_old))
freq(positivity$pos_old)

# Create var list for averages and alphas
list_posi <- c("pos1", "pos2", "pos3", "pos4", "pos5", "pos6rev", "pos7", "pos8")

# Cronbach's alpha
positivity %>% 
  select(list_posi) %>% 
  alpha()