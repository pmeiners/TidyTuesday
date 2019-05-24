'
Global Plastic Waste 
Analysis for tidy tuesdays
Paul Meiners
'

library(tidyverse)

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


ggplot(data = data_clean) +
  geom_histogram(mapping = aes( x= coast_pop), binwidth = 0.5)




