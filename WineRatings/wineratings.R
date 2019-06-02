'
Wine Ratings
Analysis for tidy tuesdays
Paul Meiners
'
library(readr)
library(tidyverse)
theme_set(theme_bw())
library(ggrepel) # to move labels away from information in plots
library(ggridges)



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

filter(wine_ratings, price>1300) %>% select(id, country, points, price, taster_name, winery)

# Except for one case, all of these wines have been judged by Roger Voss, a professional wine taster. 
# Ignoring these wines creates a much more understandable graph:

wine_ratings %>%
  filter(!is.na(price), price<1300) %>% 
  ggplot(mapping = aes(x = points, y = price, group = points))+
  geom_boxplot()

# Compare countries and prices for wine:
wine_ratings %>%
  filter(!is.na(price), price<1300) %>% 
  ggplot(data = wine_ratings, mapping = aes(x = price, y = country))+
         geom_density_ridges(alpha = 0.5) +
   scale_x_log10(limits=c(1,500), breaks = scales::log_breaks(n = 10))

# English wine is more expensive, and Ukrainian wine more cheap than average.

# Compare countries and ratings for wine:

wine_ratings %>%
  filter(!is.na(price), price<1300) %>% 
  ggplot(data = wine_ratings, mapping = aes(x = points, y = country))+
  geom_density_ridges(alpha = 0.5) 

# Wines from Ukraine and Peru seem to get less points than in other countries






