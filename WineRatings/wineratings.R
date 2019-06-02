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


saveRDS(wine_ratings, file="wine_ratings.rds")

# first we can try to create similar graphs as in the article

wine_ratings %>%
  filter(!is.na(price)) %>% 
  group_by(points) %>% 
  summarise(median = median(price)) %>% 
  ggplot(mapping = aes(x = points, y = median))+
  geom_col()


