# Temperature data

# Inspired by circular temperature plots by Cedric Scherer:
# https://gist.github.com/z3tt/a35884da020cbf6788c41a8f16569563

library(tidyverse)
library(weathermetrics)

temperature <- read.csv("Temperature_circles/temp_zurich_oslo.csv") |> 
  janitor::clean_names() |> 
  filter(avg_temperature != -99) |> 
  mutate(date = paste0(year, "-", month, "-", day),
         date = lubridate::ymd(date),
         year = lubridate::year(date),
         yday = lubridate::yday(date),
         month = lubridate::month(date),
         avg_temperature = (avg_temperature-32)/1.8)

## data for plot: selected and html-formatted cities with data from 2011 to 2020
temp_selected_plot <- 
  temperature %>% 
  filter(year > 2010)

## plot
temp_selected_plot %>% 
  ggplot(aes(yday, avg_temperature)) +
  geom_point(aes(color = avg_temperature), size = .3) +
  facet_wrap(~ city) +
  scale_x_continuous(breaks = c(1, 91, 182, 274)) +
  #scale_color_viridis_c(option = "rocket", direction = -1, end = .95) +
  #rcartocolor::scale_color_carto_c()
  scico::scale_color_scico(palette = "vikO", limits = c(-20, 30), direction = 1, begin = .03, end = .97) +
  guides(color = guide_colorsteps(barwidth = unit(20, "lines"), 
                                  barheight = unit(.5, "lines"), 
                                  show.limits = TRUE)) +
  coord_polar(start = 0) + #theta = "y"
  theme_minimal(base_size = 20) +
  theme(panel.grid.minor = element_blank(),
        text = element_text(color = "grey20"), 
        axis.title = element_blank(),
        legend.position = "bottom")



