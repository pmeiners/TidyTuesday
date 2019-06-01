'
Wine Ratings
Analysis for tidy tuesdays
Paul Meiners
'
library(readr)
library(tidyverse)
theme_set(theme_bw())



wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

names(wine_ratings)[1] <- c("id")


wr_edit <- mutate(wine_ratings, ppp = points/price) %>% 
  filter(!is.na(price))






