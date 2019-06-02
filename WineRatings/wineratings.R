'
Wine Ratings
Analysis for tidy tuesdays
Paul Meiners
'
library(readr)
library(tidyverse)
theme_set(theme_bw())
library(ggrepel) # to move labels away from information in plots




wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

names(wine_ratings)[1] <- c("id")


saveRDS(wine_ratings, file="wine_ratings.rds")

# first we can try to create similar graphs as in the article

wine_ratings %>%
  filter(!is.na(price)) %>% 
  group_by(points) %>% 
  summarise(median = median(price)) %>% 
  ggplot(mapping = aes(x = points, y = median)) +
  geom_col()

# However, there is only a small number of wines available for the very high points sections.

wine_ratings %>%
  filter(!is.na(price)) %>% 
  ggplot(mapping = aes(x = points, y = price, group = points))+
  geom_boxplot()

# The boxplot shows that we have a number of extreme outliers. Which wines are that expensive?



