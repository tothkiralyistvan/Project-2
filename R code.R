# Libraries
library(haven)
library(tidyverse)
library(janitor)
library(sjmisc)
library(sjstats)
library(psych)
library(summarytools)
library(skimr)

positivity <- read_sav("data")
View(positivity)

# Explore data
View(positivity)
names(positivity)
summary(positivity, digits = 4)