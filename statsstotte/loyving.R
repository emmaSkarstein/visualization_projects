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
f3 <- "Nunito"
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
font_add_google(name = f3, family = f3)

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
                                         angle = 0, 
                                         hjust = .5,
                                         size = 9),
             legend.text = element_text(color = col_text, 
                                        family = f1, 
                                        size = 8),
             legend.direction = "horizontal",
             legend.position = c(0.8, 0.45), 
             legend.box = "horizontal", # How are legends placed relative to each other in the box?
             legend.box.just = "top", # where are legends placed inside the legend box?
             legend.margin = margin(0, 0, 0, 0),
             legend.spacing = unit(2.5, "lines"), # dist. between legends
             # BACKGROUND
             panel.grid.major.x = element_line(color = "grey90"),
             panel.grid.major.y = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(color = col_bgr, fill = col_bgr),
             plot.background = element_rect(color = col_bgr, fill = col_bgr),
             plot.margin = margin(30, 20, 10, 20),
             plot.caption = element_text(family = f3, color = col_text, 
                                         hjust = 1, margin = margin(t = 20))
)

ggplot(loyving, aes(y = reorder(organisasjon, desc(loyving_2023)))) +
  geom_linerange(aes(xmin = loyving_2022, xmax = loyving_2023,
                     color = diff_perc), size = 1.7) +
  geom_point(aes(x = loyving_2022, fill = "2022"), color = col_bgr,
             shape = 21, size = 3) +
  geom_point(aes(x = loyving_2023, fill = "2023"), color = col_bgr,
             shape = 21, size = 3) +
  geom_label(aes(x = loyving_2023-100000, 
                label = str_wrap(organisasjon, width = 45)), 
            hjust = 1, size = 3, family = f1, 
            color = "grey25", fill = col_bgr, label.size = 0) +
  scale_fill_manual(name = "", values = c("grey75", "grey35")) +
  scale_color_gradientn(
    name = "Prosentvis nedgang fra 2022 til 2023",
    labels = scales::percent,
    colours = c("#fbf5f7","#e3bec9","#cc879b","#b4516d")
  ) +
  scale_x_continuous(labels = function(x) paste(point(x), "kr"),
                     breaks = seq(0, 8000000, by = 1000000),
                     expand = c(0.01, 0.01),
                     limits = c(-2*10^6, 8.3*10^6),
                     sec.axis = dup_axis()) + # Add axis ticks and labels both on top and bottom.
  coord_cartesian(clip = 'off') +
  guides(color = guide_colorsteps(barwidth = unit(11.5, "lines"),
                                barheight = unit(.4, "lines"),
                                show.limits = TRUE,
                                title.position = "top", 
                                order = 2),
         fill = guide_legend(order = 1)) +
  # Title (using geom_richtext to place it inside the plot)
  geom_text(aes(x = 8.3*10^6, y = 27), 
                label = "Kutt i tildelingene fra Landbruks- og\n matdepartementet fra 2022 til 2023",
                family = f2, fontface = "bold", 
                size = 10, hjust = 1, 
                data = data.frame()) +
  geom_textbox(aes(x = 8.3*10^6, y = 20),
        label = 'Ved publiseringen av statsbudsjettet for 2023 ble det kjent at støtten til organisasjonen **NOAH - for dyrs rettigheter** kuttes helt, mens andre organisasjoner får mindre kutt, og noen fortsatt mottar samme beløp som i 2022.<br><br> Ifølge landbruksdepartementet er formålet med tildelingene dette: "*Bevilgningen skal dekke støtte til organisasjoner på nasjonalt nivå som arbeider innenfor landbruks- og matdepartementets målområde*".<br><br> Her er samtlige organisasjoner som mottar støtte av landbruksdepartementet, samt beløpene de har mottatt de siste to årene.',
        family = f3, color = col_text, box.color = NA, size = 3.5, halign = 1, hjust = 1, 
        width = unit(0.47, "npc"),
        fill = NA, data = data.frame()) +
  # Annotation 4H
  annotate("text", x = 7.6*10^6, y = 5,
           family = f3, size = 3, color = col_text, hjust = 1,
             label = str_wrap("4H er en av få organisasjoner som ikke kuttes, sammen med Norges Bygdekvinnelag og Norges Bygdeungdomslag.", 
                              width = 40)) +
  annotate(geom = "curve",
           x = 7.4*10^6, xend = 7.2*10^6, y = 3.5, yend = 2,
           curvature = -.3, color = col_text, size = 0.3,
           arrow = arrow(length = unit(0.1, "inches"))) +
  # Annotation NOAH
  annotate("text", x = 0.9*10^6, y = 29,
           family = f3, size = 3, color = col_text, hjust = 0,
           label = str_wrap("Støtten til NOAH kuttes fra 730 000 til 0 kr.", 
                            width = 50)) +
  annotate(geom = "curve",
           x = .8*10^6, xend = .6*10^6, y = 29, yend = 29.5,
           curvature = -.3, color = col_text, size = 0.3,
           arrow = arrow(length = unit(0.1, "inches"))) +
  # Annotation dyrevernalliansen
  annotate("text", x = 1.1*10^6, y = 13,
           family = f3, size = 3, color = col_text, hjust = 0,
           label = str_wrap("Dyrevernalliansen mottok omtrent samme beløp som NOAH i 2022.", 
                            width = 40)) +
  annotate(geom = "curve",
           x = 1.05*10^6, xend = .85*10^6, y = 12.5, yend = 12,
           curvature = -.3, color = col_text, size = 0.3,
           arrow = arrow(length = unit(0.1, "inches"))) +
  labs(caption = "Kilde: Landbruks- og matdepartementet  |  Grafikk: Emma Skarstein") +
  # Annotation Rovdyrpolitikk
  annotate("text", x = 2.1*10^6, y = 6.5,
           family = f3, size = 3, color = col_text, hjust = 0,
           label = str_wrap("Folkeaksjonen ny rovdyrpolitikk mottar det sjette største beløpet.", 
                            width = 40)) +
  annotate(geom = "curve",
           x = 2.05*10^6, xend = 1.8*10^6, y = 6.5, yend = 6,
           curvature = -.3, color = col_text, size = 0.3,
           arrow = arrow(length = unit(0.1, "inches"))) +
  # Annotation medlemsorganisasjon
  annotate("text", x = 0.7*10^6, y = 20,
           family = f3, size = 3, color = col_text, hjust = 0,
           label = str_wrap("Sandra Borch uttalte i forbindelse med kuttet av NOAH at det skal prioriteres støtte til medlemsorganisasjoner. Likevel er flere av organisasjonene ikke medlemsorganisasjoner.", 
                            width = 50)) +
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
