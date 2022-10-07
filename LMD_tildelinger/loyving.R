# Tildelinger fra Landbruks- og matdepartementet, 2022 og 2023

# Inspirert av 
# https://r-graph-gallery.com/web-lollipop-plot-with-r-mario-kart-64-world-records.html

library(tidyverse)
library(showtext)
library(elementalist) # devtools::install_github("teunbrand/elementalist")

# Colors ----
col_bgr <- "#FFFCFC"
col_text <- "grey40"

# Loading fonts ----
f1 <- "Open Sans"
f2 <- "Open Sans"
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)

showtext_auto()
showtext_opts(dpi = 300)


loyving <- read.csv("LMD_tildelinger/loyving.txt", sep = ";") |> 
  janitor::clean_names() |> 
  mutate_at(c("loyving_2022", "loyving_2023"), str_replace_all, " ", "") |> 
  mutate_at(c("loyving_2022", "loyving_2023"), as.numeric) |> 
  mutate(diff_perc = (loyving_2022 - loyving_2023)/loyving_2022)

loyving$loyving_2023[which(loyving$organisasjon == "NOAH – for dyrs rettigheter")] <- 0.1

point <- scales::format_format(big.mark = " ", 
                               decimal.mark = ".", 
                               scientific = FALSE) 


theme_set(theme_minimal(base_family = f1, base_size = 13))
plot_theme <- theme(# AXIS TITLES AND TEXT
             axis.text.y = element_blank(),
             axis.text.x = element_text(color = col_text),
             axis.title = element_blank(),
             axis.ticks = element_blank(),
             # LEGEND
             legend.title = element_text(color = col_text, 
                                         family = f1, 
                                         angle = 90, 
                                         hjust = .5,
                                         size = 9),
             legend.text = element_text(color = col_text, 
                                        family = f1, 
                                        size = 8),
             legend.position = c(0.31, 0.7), 
             legend.box = "horizontal", # How are legends placed relative to each other in the box?
             legend.box.just = "bottom", # where are legends placed inside the legend box?
             legend.margin = margin(0, 0, 0, 0),
             legend.spacing = unit(1.5, "lines"), # dist. between legends
             # BACKGROUND
             panel.grid.major.x = element_line(color = "grey90"),
             panel.grid.major.y = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(color = col_bgr, fill = col_bgr),
             plot.background = element_rect(color = col_bgr, fill = col_bgr),
             plot.margin = margin(10, 20, 10, 150),
             # TITLES
             plot.title = element_text(
               family = f1, 
               size = 17.45
             ),
             plot.subtitle = element_text(
               family = f1, 
               color = col_text, 
               size = 10.8,
               #lineheight = 1.3, 
               margin = margin(t = 5, b = 30)
             ),
             plot.caption = element_text(
               family = f1, 
               color = col_text, 
               size = 10.5, 
               margin = margin(t = 20, b = 0, r = 15)
             )
)

ggplot(loyving, aes(y = reorder(organisasjon, desc(loyving_2023)))) +
  geom_linerange(aes(xmin = loyving_2022, xmax = loyving_2023,
                     color = diff_perc), size = 2) +
  geom_point(aes(x = loyving_2022, fill = "2022"), color = col_bgr,
             shape = 21, size = 3) +
  geom_point(aes(x = loyving_2023, fill = "2023"), color = col_bgr,
             shape = 21, size = 3) +
  geom_label(aes(x = loyving_2023-100000, 
                label = str_wrap(organisasjon, width = 60)), 
            hjust = 1, size = 2.6, family = f1, 
            color = "grey25", fill = col_bgr, label.size = 0) +
  # colorspace::scale_color_continuous_sequential(
  #   name = "Prosentvis nedgang fra 2022 til 2023",
  #   labels = scales::percent,
  #   palette = "Burg") +
  scale_color_gradientn(
    name = "Prosentvis nedgang fra 2022 til 2023",
    labels = scales::percent,
    colours = c("#6C5B7B","#C06C84","#F67280","#F8B195")
  ) +
  scale_fill_manual(name = "", values = c("grey65", "grey35")) +
  scale_x_continuous(#trans=scales::pseudo_log_trans(base = 10),
                     labels = function(x) paste(point(x), "kr"),
                     breaks = seq(0, 8000000, by = 1000000),
                     expand = c(0.1, 0.1),
                     sec.axis = dup_axis()) + # Add axis ticks and labels both on top and bottom.
  coord_cartesian(clip = 'off') +
  guides(color = guide_colorsteps(barheight = unit(12, "lines"),
                                barwidth = unit(.4, "lines"),
                                show.limits = TRUE,
                                title.position = "left")) +
  labs(title = "Tildelinger fra Landbruks- og matdepartementet, 2022 og 2023")+
  plot_theme

# #BE6B53
ggsave("LMD_tildelinger/loyving.pdf", width = 14, height = 9)
ggsave("LMD_tildelinger/loyving.png", width = 14, height = 9)


ggplot(loyving, aes(y = reorder(organisasjon, desc(loyving_2023)))) +
  geom_linerange(aes(xmin = loyving_2022, xmax = loyving_2023,
                     color = diff_perc), size = 2) +
  geom_point(aes(x = loyving_2022), color = col_bgr, fill = "grey65", 
             shape = 21, size = 2.5) +
  geom_point(aes(x = loyving_2023), color = col_bgr, fill = "grey35", 
             shape = 21, size = 2.5) +
  scale_x_log10() +
  guides(fill = guide_legend(override.aes = list(color = c("#F8766D", NA),
                                                 shape = 21)))+
  theme(axis.text.y = element_blank())

