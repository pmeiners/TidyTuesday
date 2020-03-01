# Measels tryouts

library(tidyverse)
library(rvest)
library(psych)



# Import ------------------------------------------------------------------




measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')


# Descriptives ------------------------------------------------------------

wsj %>% select(state, year, county, enroll, mmr) %>% psych::describe()

