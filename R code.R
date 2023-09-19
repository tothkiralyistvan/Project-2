# Libraries
library(haven)
library(tidyverse)
library(sjmisc)
library(sjstats)
library(psych)
library(summarytools)
library(MplusAutomation)

# Read data
positivity <- read_sav(here("location"))
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


################################### Codebook ###################################
install.packages(c("labelled", "sjlabelled"))
library(labelled)

# Create value labels list
value_labels_1_5 <- c("strongly disagree" = 1, "disagree" = 2, "neither agree, nor disagree" = 3, "agree" = 4, "strongly agree" = 5)

# Assign variable labels
positivity_labelled <- positivity %>% 
  set_variable_labels(
    adatfile = "source of data",
    gender = "gender of participant",
    age_tri = "age of participants (categorical)",
    age_tri2 = "age of participants (categorical)",
    pos1 = "I have great faith in the future",
    pos2 = "I am satisfied with my life", 
    pos3 = "Others are generally here for me when I need them",    
    pos4 = "I look forward to the future with hope and enthusiasm", 
    pos5 = "On the whole, I am satisfied with myself", 
    pos6r = "At times, the future seems unclear to me. (reversed from original)", 
    pos7 = "I feel I have many things to be proud of",
    pos8 = "I generally feel confident in myself",
    pos6 = "At times, the future seems unclear to me. (reverse scored)",
    pos_old = "average score calculated in SPSS",       
    new_age4 = "age of participants (continuous)",
    pos6rev = "At times, the future seems unclear to me. (reversed from original)",
    pos_new = "average score calculated in R using at least 50% of responses",
    pos_new_manual = "average score calculated in R using SPSS method (listwise deletion)") %>% 
  set_value_labels(
    pos1 = value_labels_1_5,
    pos2 = value_labels_1_5,
    pos3 = value_labels_1_5,
    pos4 = value_labels_1_5,
    pos5 = value_labels_1_5,
    pos6r = value_labels_1_5,  # reversed item  
    pos7 = value_labels_1_5,
    pos8 = value_labels_1_5,
    pos6 = value_labels_1_5)

# Once everything is done, create dictionary object
dictionary <- generate_dictionary(positivity_labelled)
rm(positivity_labelled)

# Show the labelled dataset visually in the Viewer
# Requires the 'sjPlot' package
library(sjPlot)
positivity_labelled %>% 
  view_df(show.na = TRUE)

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
results_corr <- positivity %>%
  select(pos_new, pos_old, pos_new_manual) %>% 
  corr.test()

# Print results in tidy format
results_corr


################################# Descriptives #################################
positivity %>% 
  select(pos_new, pos_old, pos_new_manual) %>%
  descr()
