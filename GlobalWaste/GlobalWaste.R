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
data_clean <- select(data_all, waste_mis_total, cntry, waste_mis_pcap, coast_pop, gdp_pcap, waste_total_pcap, total_pop, Year, Code)

data_clean <- mutate(data_clean, gdp = total_pop*gdp_pcap)

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

# Waste pcap and coastal population
ggplot(data=subset(data_clean, !is.na(waste_mis_pcap)), aes(x = log1p(waste_mis_pcap), y = log(coast_pop))) + 
  geom_point(aes(color = gdp_pcap), size = 3) +
  geom_text_repel(aes(label=ifelse(log1p(waste_mis_pcap)>0.12,as.character(cntry),'')),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_color_viridis()

# Waste pcap and total population
ggplot(data=subset(data_clean, !is.na(waste_mis_pcap)), aes(x = log1p(waste_mis_pcap), y = log(total_pop))) + 
  geom_point(aes(color = gdp_pcap), size = 3) +
  geom_text_repel(aes(label=ifelse(log1p(waste_mis_pcap)>0.12,as.character(cntry),'')),
                  box.padding   = 0.35, 
                  point.padding = 0.5,
                  segment.color = 'grey50') +
  scale_color_viridis()

  # countries with high waste per capita are mostly Island countries
  # there is no impact of coastal population vs total population here

ggplot(data=subset(data_clean, !is.na(waste_mis_pcap)), aes(x = log(gdp_pcap), y = log(waste_mis_total))) + 
  geom_point(aes(color = waste_mis_pcap), size = 3) +
  geom_text_repel(aes(label=ifelse(log1p(waste_mis_pcap)>0.12,as.character(cntry),'')),
                  box.padding   = 0.35, 
                  point.padding = 0.5,
                  segment.color = 'grey50') +
  geom_smooth()+
  scale_color_viridis()

  'The countries with high waste per capita are distributed across the spectrum, suggesting other reasons for their high waste levels
  Maybe Island countries are just more affected (per capita) by waste that others produce
  The relationship between gdp and waste is nonlinear. Higher gdp per capita seems to increase total waste for the first half of the distriution.
  For the second half of the distribution, higher gdp per capita is negatively associated with total waste.'


# --- Integrating time effects --- #

# GDP per capita change compared to last year:
data_clean <- data_clean %>%
  filter(!is.na(gdp_pcap)) %>%
  group_by(cntry) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(gdp_pcap_change = (gdp_pcap/lag(gdp_pcap) - 1) * 100)

# Population change compared to last year:
data_clean <- data_clean %>%
  filter(!is.na(total_pop)) %>%
  group_by(cntry) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(total_pop_change = (total_pop/lag(total_pop) - 1) * 100)


# Heatmap of changes:
# Adding missing obs
data_clean <- data_clean %>% 
  group_by(cntry) %>% 
  fill(waste_mis_pcap, waste_mis_total) %>% #default direction down
  fill(waste_mis_pcap, waste_mis_total, .direction = "up")

# sorted by total population change
ggplot(data = data_clean, aes(x = Year, y = reorder(cntry, -waste_mis_pcap), z = total_pop_change)) + 
  geom_tile(aes(fill = total_pop_change)) +
  scale_fill_viridis()

# sorted by GDP change
ggplot(data = data_clean, aes(x = Year, y = reorder(cntry, -waste_mis_pcap), z = gdp_pcap_change)) + 
  geom_tile(aes(fill = gdp_pcap_change)) +
  scale_fill_viridis()



  






