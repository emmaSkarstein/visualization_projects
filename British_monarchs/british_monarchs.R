# British monarchs
# By Emma Skarstein, September 2022

library(tidyverse)
library(readxl)
library(directlabels)
library(showtext)
library(paletteer) 
library(patchwork)
library(ggpubr)



monarchs <- read_xlsx("British_monarchs/british_monarchs.xlsx")

monarchs <- monarchs %>% 
  mutate(Born = as.Date(Born, "%d.%m.%Y"),
         Reign_start = as.Date(Reign_start, "%d.%m.%Y"),
         Reign_end = as.Date(Reign_end, "%d.%m.%Y"), 
         Death = as.Date(Death, "%d.%m.%Y"),
         Name = as.factor(Name), 
         House = as.factor(House)) %>% 
  mutate(age_at_reign = Reign_start - Born,
         age_reigning = Reign_end - Reign_start,
         age_after_reign = Death - Reign_end,
         age_at_death = Death - Born) %>% 
  mutate(birth_death = paste0(format(Born, "%Y"), " - ", 
                              ifelse(is.na(Death), "present", format(Death, "%Y"))))


house_beginning <- monarchs %>% group_by(House) %>%
  filter(Born == min(Born, na.rm=TRUE)) %>% 
  select(Name, House) %>% ungroup() %>% 
  rename(ybeg = "Name") %>% 
  distinct()
house_beginning$House[2] <- "Windsor"
house_beginning <- house_beginning[-1,]

house_end <- monarchs %>% group_by(House) %>%
  filter(Death == max(Death, na.rm=TRUE)) %>% 
  select(Name, House) %>% ungroup() %>% 
  rename(yend = "Name") %>% 
  distinct()
house_end$House[2] <- "Saxe-Coburg and Gotha"
house_end <- house_end[-3,]

house_labels <- inner_join(house_beginning, house_end, by = "House") %>% 
  mutate(x_loc = c(-25, -30, -24, -24, -24, -24, -24)*365) %>% 
  mutate(text_pos = c("George VI", "Edward VII", "George III", "James II", "Edward VI", "Edward V", "Henry V"))
house_labels$yend[1] <- "Charles III"

# Loading fonts ----
f1 <- "EB Garamond"
f2 <- "Playfair Display"
f_num <- "PT Serif"

font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
font_add_google(name = f_num, family = f_num)
showtext_auto()


# Colors ----
col_bgr <- "#F0F0F0"
col_text <- "grey20"
col_num <- "grey55"
set.seed(872436)           # Set seed
col_house <- sample(c("#7a293b","#c55069","#522570","#255570","#a3364b","#272976","#5a1e59","#35739e"))



# Main plot ----
p_main <- ggplot(monarchs, aes(y = reorder(Name, Reign_start))) +
  # Lines for life-events
  geom_segment(aes(x = 0, xend = age_at_reign, # Lifespan before reign
                   yend = Name), color = col_text, lineend = "round") +
  geom_segment(aes(x = age_reigning + age_at_reign, # Lifespan after reigning
                   xend = age_reigning + age_at_reign + age_after_reign,
                   yend = Name), color = col_text, lineend = "round") + 
  geom_segment(aes(x = age_at_reign, xend = age_reigning + age_at_reign, # Lifespan reigning
                   yend = Name, color = House), size = 2, lineend = "round") + 
  # Birth and death annotations
  geom_text(aes(x = -20, label = birth_death), # Year of birth and death
            hjust = 1.1, vjust = 1.5, size = 2.5, family = f_num, 
            color = "grey40") + 
  # Year annotations
  geom_text(aes(x = age_at_reign, 
                label = format(Reign_start, format = "%Y")), # Year of ascension
            hjust = 0.5, vjust = 1.7, size = 2.3, family = f_num, 
            color = col_num) + 
  geom_text(aes(x = age_reigning + age_at_reign, 
                label = format(Reign_end, format = "%Y")), # Year of descension
            hjust = 0.4, vjust = -0.8, size = 2.3, family = f_num, 
            color = col_num) + 
  # Text and brackets for house names
  geom_text(data = house_labels, aes(x = x_loc, y = text_pos, 
                                     color = House, label = House), 
            hjust = 0.5, alpha = 0.6, size = 4, family = f2, fontface = "bold", 
            angle = 90) +
  geom_segment(data = house_labels, # Vertical lines
               aes(x = x_loc+3*365, xend = x_loc+3*365, 
                   y = ybeg, yend = yend, color = House),
               alpha = 0.6) + 
  # Fun facts
  annotate("text", x = 130*365, y = "Edward IV", label = str_wrap("The throne switched back and forth between the rival houses of York and Lancaster during the Wars of the Roses.", 
                                                          width = 70),
           color = col_text, size = 3, family = f1, hjust = 1) +
  annotate("text", x = 130*365, y = "Henry V", label = str_wrap("Henry VI became king at only 9 months old, following the death of his father, Henry V.",
                                                            width = 70),
           color = col_text, size = 3, family = f1, hjust = 1) +
  annotate("text", x = 130*365, y = "Henry VII", label = str_wrap("Edward V and Edward VI both became kings at a very young age, and both died before they were 20. Perhaps that is why it took almost 300 years before the name was chosen for a future king again.",
                                                                width = 65),
           color = col_text, size = 3, family = f1, hjust = 1) +
  annotate("text", x = 130*365, y = "Mary I", label = str_wrap("Philip of Spain became king upon marrying Mary I, despite not knowing English. He only remained king until she died, four years after their wedding.", 
                                                          width = 65),
           color = col_text, size = 3, family = f1, hjust = 1) +
  annotate("text", x = 130*365, y = "Charles I", label = str_wrap("Years of conflict with the Parliament lead to Charles I being convicted of treason and executed. After his death, England was without a monarch for eleven years, until they eventually decided that the alternative was not better, and made his son, Charles II, the new king.", 
                                                               width = 68),
           color = col_text, size = 3, family = f1, hjust = 1) +
  #annotate("text", x = 130*365, y = "James II", label = str_wrap("Due to Something, James lost the throne in", 
  #                                                        width = 45),
  #         color = col_text, size = 3, family = f1, hjust = 1) +
  annotate("text", x = 130*365, y = "William III", label = str_wrap("Anne became the first monarch of the Kingdom of Great Britain after the political union of England and Scotland in 1707.", 
                                                                 width = 65),
           color = col_text, size = 3, family = f1, hjust = 1) +
  annotate("text", x = 130*365, y = "George III", label = str_wrap("The Kingdom of Great Britain and the Kingdom of Ireland merged into The United Kingdom in 1801.", 
                                                             width = 40),
           color = col_text, size = 3, family = f1, hjust = 1) +
  annotate("text", x = 130*365, y = "George V", label = str_wrap("In 1917, during the First World War, George V changed the name of the British royal house from the German sounding House of Saxe-Coburg and Gotha to the more British House of Windsor.", 
                                                                 width = 50),
           color = col_text, size = 3, family = f1, hjust = 1) +
  # Format x-axis
  scale_x_continuous(expand = c(0,0),
                     labels = function(x) round(x/365), 
                     breaks = seq(0, 100, by = 10)*365) +
  coord_cartesian(xlim = c(0, 130*365), # This focuses the x-axis on the range of interest
                  clip = 'off') +
  # Colors for the houses
  scale_color_manual(values = col_house)  +
  # Titles
  xlab("Age") +
  labs(title = "Monarchs of Britain and England",
       subtitle = str_wrap("Following her death on September 8th 2022, Queen Elizabeth II became the longest reigning monarch of Britain and England, 
                           and King Charles III became the oldest person to become monarch. 
                           A steady increase in lifespan among the royals suggests that these records may both be broken soon, 
                           if only the monarchy survives that long.", 
                           width = 100),
       caption = "Visualization by Emma Skarstein") +
  theme_bw() +
  theme(text = element_text(family = f2, color = col_text, size = 15),
        plot.title = element_text(family = f2,
                                  size = 20,
                                  face = "bold",
                                  color = col_text),
        plot.subtitle = element_text(size = 10, family = f1, margin = margin(b = 50)),
        plot.caption = element_text(size = 8, family = f1, margin = margin(t = 20)),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = f2, face = "bold", size = 14, 
                                    hjust = 0.78),
        axis.text.y = element_text(family = f2, face = "bold", size = 11, 
                                   color = col_text, vjust = -0.1),
        panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = "#EBEBEB"),
        legend.position = "none",
        panel.background = element_rect(color = col_bgr, fill = col_bgr),
        plot.background = element_rect(color = col_bgr, fill = col_bgr),
        plot.margin = margin(30,20,10,50))


# Explanation insert ----
segment_explanation <- data.frame(text = c("Birth", "Beginning of reign", 
                                           "End of reign", "Death"), 
                                  x.pos = c(0, 20, 40, 60))

p_legend <- ggplot() +
  geom_segment(aes(x = 0, xend = 20, # Lifespan before reign
                   y = 0, yend = 0), color = col_text, lineend = "round") +
  geom_segment(aes(x = 40, xend = 60, # Lifespan after reigning
                   y = 0, yend = 0), color = col_text, lineend = "round") +
  geom_segment(aes(x = 20, xend = 40, # Lifespan reigning
                   y = 0, yend = 0), color = "darkred", size = 2, lineend = "round") + 
  
  #geom_segment(data = segment_explanation, aes(x = x.pos, xend = x.pos, 
  #                                             y = -0.2, yend = 0.2)) +
  #scale_y_continuous(limits = c(-10, 10)) +
  geom_text(data = segment_explanation, aes(x = x.pos, y = 0, label = text),
            hjust = 0.4, vjust = 2, size = 2.6, family = f1, 
            color = col_num) +
  theme_void()

# Final plot ----
p_main + inset_element(p_legend, left = -0.029, bottom = 1, right = 0.6, top = 1.1)

ggsave("British monarchs/british_monarchs.pdf", width = 8, height = 11)



# For future reference ----
# This might help to download tables from Wikipedia (but will need a lot of cleaning)
library(httr)
library(XML)

url <- "https://en.wikipedia.org/wiki/List_of_Norwegian_monarchs"

r <- GET(url)

doc <- readHTMLTable(
  doc=content(r, "text"))

king_df <- doc[[6]]



