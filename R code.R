# Libraries
library(haven)
library(tidyverse)
library(sjmisc)
library(sjstats)
library(psych)
library(summarytools)
library(MplusAutomation)

# Read data
positivity <- read_sav("data")
View(positivity)


############################### Data exploration ###############################
View(positivity)
names(positivity)
summary(positivity)


################################ Data cleaning #################################
# Gender
freq(positivity$gender)

## Verify that gender is numerical
class(positivity$gender)

## Recode outlier value to NA
positivity <- positivity %>% 
  mutate(gender = ifelse(gender > 2, NA, gender))

# Age (continuous)
class(positivity$age)

## Recode textual responses, change variable type, recode outliers
positivity <- positivity %>%
  mutate(new_age4 = age) %>% 
  mutate(new_age4 = case_when(
    new_age4 == "tizenhet" ~ "17",
    TRUE ~ as.character(new_age4))) %>%
  mutate(new_age4 = as.numeric(new_age4)) %>% 
  mutate(new_age4 = ifelse(new_age4 < 15 | new_age4 > 92, NA, new_age4))
freq(positivity$new_age4)

## Remove the "old" age variable as it's not useful anymore
positivity <- positivity %>%
  select(-age)

# Age (3 categories)
freq(positivity$age_tri)

positivity <- positivity %>%
  mutate(age_tri = ifelse(age_tri > 3, NA, age_tri))
freq(positivity$age_tri)


############################ Scale item frequencies ############################
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


############################## Reliability: alpha ##############################
positivity %>% 
  select(all_of(list_posi)) %>% 
  alpha()

# Create new "averages" object in environment
averages <- positivity %>%
  transmute(pos_new = mean_n(select(., all_of(list_posi)), n = .5))

# Create new variable and append to existing dataframe
positivity <- positivity %>%
  mutate(pos_new = mean_n(select(., all_of(list_posi)), n = .5))

# Calculate variable manually (results in listwise deletion)
positivity <- positivity %>% 
  mutate(pos_new_manual = (pos1+pos2+pos3+pos4+pos5+pos6rev+pos7+pos8)/8)


################################# Correlations #################################
# Usual method
positivity %>%
  select(pos_new, pos_old, pos_new_manual) %>% 
  corr.test()

# Newer 'correlation' package from the 'easystats' library
results_corr <- positivity %>%
  select(pos_old, pos_new, pos_new_manual) %>% 
  correlation()

# Print results in tidy format
results_corr

# Print results in correlation matrix
summary(results_corr)


################################# Descriptives #################################
positivity %>% 
  select(pos_new, pos_old, pos_new_manual) %>%
  descr()