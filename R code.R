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