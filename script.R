library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggdark)

# Reading the big mac data from the TidyVerse GitHub repository
big_mac <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv') %>% 
  mutate(date = floor_date(date, "year")) %>% 
  separate(date, into = c("year", "the_rest"), sep = 4) %>% 
  select(-the_rest) %>% 
  mutate(year = as.numeric(year),
         name = recode(name, "Britain" = "United Kingdom"))

# Reading cardiovascular death data, downloaded from OurWorldinData.org
cardio_death <- read_csv("cardiovascular-disease-death-rates.csv") %>% 
  rename("death_rate" = names(.)[4]) %>% 
  left_join(big_mac, by = c("Year" = "year", "Entity" = "name")) %>% 
  drop_na(death_rate, dollar_price) %>% 
  group_by(Entity) %>% 
  mutate(mean_dollar = mean(dollar_price),
         mean_death = mean(death_rate))

# Plotting mean big mac price and death rate over all years
ggplot(data = distinct(cardio_death, Entity, .keep_all = TRUE), 
       # distinct(...) removed repeated countries. This stops geom_text_repel() 
       # giving points multiple labels, which looked very bad.
       aes(x = mean_dollar, y = mean_death, label = Entity)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_text_repel() +
  dark_theme_gray() +
  scale_x_continuous(breaks = 1:7, labels = 1:7, limits = c(1, 7)) +
  labs(y ="Death from Cardiovasular Disease Per 100,000 people",
       x = "Price of a Big Mac in US Dollars",
       title = "The Price of a Big Mac vs. Death Rate from Cardiovascular Disease",
       subtitle = "Averaged across 2000-2017",
       caption = "Visualisation: David Aikman @ResearchingDave, Data Sources: The Economist (via TidyTuesday) and OurWorldinData.org") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 0, 10)),
        plot.subtitle = element_text(size = 12, face = "italic", margin = margin(0, 0, 10, 0)),
        axis.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(margin = margin(20, 0, 0, 0)),
        plot.margin = unit(c(0, 0.5, 0.1, 0.5), "cm"))

ggsave("mean_plot.png", width = 10, height = 8)

# Plotting death rate and big mac price for each year
ggplot(data = cardio_death, aes(x = dollar_price, y = death_rate)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(Year), ncol = 3) +
  dark_theme_gray() +
  scale_x_continuous(breaks = 1:7, labels = 1:7, limits = c(1, 7)) +
  labs(y = "Death from Cardiovasular Disease Per 100,000 people",
       x = "Price of a Big Mac in US Dollars",
       title = "The Price of a Big Mac vs. Death Rate from Cardiovascular Disease",
       subtitle = "For each of the years 2000-2017",
       caption = "Visualisation: David Aikman @ResearchingDave, Data Sources: The Economist (via TidyTuesday) and OurWorldinData.org") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 0, 10)),
        plot.subtitle = element_text(size = 12, face = "italic", margin = margin(0, 0, 10, 0)),
        axis.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(margin = margin(20, 0, 0, 0)),
        plot.margin = unit(c(0, 0.5, 0.1, 0.5), "cm"))

ggsave("years_plot.png", width = 10, height = 8)
