rm(list=ls())
library(tidymodels)
library(RColorBrewer)

# library(showtext)
# font_add_google("Special Elite", family = "special")
# font_add_google("Roboto Slab", family = "Roboto_slab")
# font_add_google("Cabin", family = "Cabin")
# showtext_auto()

df <- read.csv(file='data/european_wholesale_electricity_price_data_hourly.csv',
               header=T, col.names=c("X","Country","ISO3_code","DateTime_UTC","DateTime_Local","Price_EUR"))


list_country <- df %>%
  distinct(Country)

df %>%
  filter(Country=='Switzerland') %>%
  mutate(date=as.POSIXct.default(DateTime_UTC)) %>%
  group_by(date) %>%
  summarise(
    daily_price_eur = mean(Price_EUR),
    daily_price_min = min(Price_EUR),
    daily_price_max = max(Price_EUR)
  ) %>%
  ggplot(aes(x=date, y=daily_price_eur))+geom_line()+geom_smooth()+
  xlab('date')+ylab('Price [Eur/MWH]')

# 2022

df %>%
  filter(Country=='Switzerland') %>%
  mutate(date=as.Date(DateTime_UTC)) %>%
  group_by(date) %>%
  filter(date>='2022-01-01') %>%
  summarise(
    daily_price_eur = mean(Price_EUR),
    daily_price_min = min(Price_EUR),
    daily_price_max = max(Price_EUR)) %>%
  
  ggplot(aes(x=date))+
  geom_ribbon(aes(ymin=daily_price_min, ymax=daily_price_max), fill=brewer.pal(7, "Blues")[3])+
  geom_line(aes(y=daily_price_eur), color=brewer.pal(9, "Blues")[9], size=0.8)+
  stat_peaks(aes(y=daily_price_eur), colour=brewer.pal(8, "Dark2")[7], geom="text",
             hjust=-0.2, vjust=-0.6, ignore_threshold=0.75, position="identity", size=2)+
  
  xlab('Date') + ylab('Price [Euro/MWH]')+
  labs(title = 'Price of electricity in Switzerland (2022)', subtitle = 'Source (Ember-climate)')+
  scale_y_continuous(n.breaks=11) + theme_light()+
  scale_x_date(date_breaks="1 month", date_labels = "%m-%Y")+
  theme(plot.title    = element_text(family = "Cabin", colour = brewer.pal(11, "Spectral")[1], size=15),
        plot.subtitle = element_text(family = "Cabin", colour = brewer.pal(11, "Spectral")[1], size=10),
        axis.title.x  = element_text(family = "Cabin", colour = brewer.pal(11, "Spectral")[11], size=10),
        axis.title.y  = element_text(family = "Cabin", colour = brewer.pal(11, "Spectral")[11], size=10),
        axis.text     = element_text(family = "Cabin"))


# Save plot
ggsave("plot_2022.png",
       width = 1920, height = 1080, dpi = 300, units = "px")

