# Tildelinger fra Landbruks- og matdepartementet, 2022 og 2023

# Inspirert av 
# https://r-graph-gallery.com/web-lollipop-plot-with-r-mario-kart-64-world-records.html

library(tidyverse)
library(showtext)
library(ggtext)

# Colors ----
col_bgr <- "#FFFCFC"
col_text <- "grey40"

# Loading fonts ----
f1 <- "Montserrat"
f2 <- "Patua One"
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)

showtext_auto()
showtext_opts(dpi = 300)


loyving <- read.csv("statsstotte/loyving.txt", sep = ";") |> 
  janitor::clean_names() |> 
  mutate_at(c("loyving_2022", "loyving_2023"), str_replace_all, " ", "") |> 
  mutate_at(c("loyving_2022", "loyving_2023"), as.numeric) |> 
  mutate(diff_perc = (loyving_2022 - loyving_2023)/loyving_2022)

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
             legend.position = c(0.39, 0.61), 
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
             plot.margin = margin(30, 20, 10, 20),
             plot.caption = element_text(family = f1, color = col_text, 
                                         hjust = 1, margin = margin(t = 20))
)

ggplot(loyving, aes(y = reorder(organisasjon, desc(loyving_2023)))) +
  geom_linerange(aes(xmin = loyving_2022, xmax = loyving_2023,
                     color = diff_perc), size = 2) +
  geom_point(aes(x = loyving_2022, fill = "2022"), color = col_bgr,
             shape = 21, size = 3) +
  geom_point(aes(x = loyving_2023, fill = "2023"), color = col_bgr,
             shape = 21, size = 3) +
  geom_label(aes(x = loyving_2023-100000, 
                label = str_wrap(organisasjon, width = 45)), 
            hjust = 1, size = 3, family = f1, 
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
  scale_x_continuous(labels = function(x) paste(point(x), "kr"),
                     breaks = seq(0, 8000000, by = 1000000),
                     expand = c(0.01, 0.01),
                     limits = c(-2*10^6, 8.3*10^6),
                     sec.axis = dup_axis()) + # Add axis ticks and labels both on top and bottom.
  coord_cartesian(clip = 'off') +
  guides(color = guide_colorsteps(barheight = unit(11.5, "lines"),
                                barwidth = unit(.4, "lines"),
                                show.limits = TRUE,
                                title.position = "left")) +
  # Title (using geom_richtext to place it inside the plot)
  geom_richtext(aes(x = 8.3*10^6, y = 27), 
                label = "Tildelinger fra Landbruks- og matdepartementet,<br> <span style = 'color:#a6a6a6;'>2022</span> og <span style = 'color:#595959;'>2023</span>",
                family = f2, fontface = "bold",
                size = 10, hjust = 1, label.color = NA, fill = NA) +
  geom_richtext(
    aes(x = 8.3*10^6, y = 20),
        label = 'Ved publiseringen av statsbudsjettet for 2023 ble det kjent at støtten til organisasjonen<br> **NOAH - for dyrs rettigheter** kuttes helt, mens andre organisasjoner får mindre kutt,<br> og noen fortsatt mottar samme beløp som i 2022.<br><br> Ifølge landbruksdepartementet er formålet med tildelingene dette:<br>"*Bevilgningen skal dekke støtte til organisasjoner på nasjonalt nivå<br> som arbeider innenfor landbruks- og matdepartementets målområde*".<br><br> Her er samtlige organisasjoner som mottar støtte av landbruksdepartementet,<br> og beløpene de har mottatt de siste to årene.',
        family = f1, color = col_text, size = 4, hjust = 1,
        label.color = NA, fill = NA) +
  labs(caption = "Kilde: Landbruks- og matdepartementet  |  Grafikk: Emma Skarstein") +
  plot_theme

ggsave("statsstotte/loyving.png", width = 14, height = 9)
ggsave("statsstotte/loyving.pdf", width = 14, height = 9)



# Log-scale
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
