'
Global Plastic Waste 
Analysis for tidy tuesdays
Paul Meiners
'

library(tidyverse)
theme_set(theme_minimal())
library(ggrepel)
library(viridis)
# ------------------- Cleaning ------------------ #

# load raw data
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

# combine the three data sets
data_all <- coast_vs_waste %>% 
  left_join(mismanaged_vs_gdp, by = c("Code", "Year")) %>%
  left_join(waste_vs_gdp, by = c("Code", "Year"))

# rename columns
names(data_all)[4:9] <- c("waste_mis_total", "coast_pop", "total_pop", "cntry", "waste_mis_pcap", "gdp_pcap")
names(data_all)[12] <- c("waste_total_pcap")

# select only necessary columns
data_clean <- select(data_all, waste_mis_total:gdp_pcap, waste_total_pcap, Year, Code)

saveRDS(data_clean, file="data_clean.rds")

#--------------------- Exploratory data analysis---------------- #

# Data on total waste exists only for 2010. But the time-series data on population and gdp growth can still be interesting. 
# Countries could be divided into "stalbe, low gdp/pop "fast growing" and "stable high gdp/pop" categories.

ggplot(data=subset(data_clean, !is.na(coast_pop)), aes(x=coast_pop)) + 
  geom_bar(stat="bin")
  # Most countries have no costal population

ggplot(data=subset(data_clean, !is.na(waste_mis_pcap)), aes(x=waste_mis_pcap)) + 
  geom_bar(stat="bin")
  # Also logarithmic distribution, a small number of countries has very high waste per capita.

ggplot(data=subset(data_clean, !is.na(waste_mis_total)), aes(x=waste_mis_total, y=coast_pop)) + 
  geom_point()+
  geom_smooth()
  # total waste is produced by the large countries. 
  # The relationship is however not direct, the toal waste is lower than you would expect from countries with high coastal population.

ggplot(data=subset(data_clean, !is.na(waste_mis_pcap)), aes(x = log1p(waste_mis_pcap), y = log(coast_pop))) + 
  geom_point(aes(color = gdp_pcap), size = 3) +
  geom_text_repel(aes(label=ifelse(log1p(waste_mis_pcap)>0.12,as.character(cntry),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_color_viridis()

  # countries with high waste per capita are mostly Island countries
  # also: Richer countries generally have low per capita waste, high per capita waste countries have all low gdp per capita







