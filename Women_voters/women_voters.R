# Womens voter registers data
# By Emma Skarstein, January-March 2023

library(readxl)
library(tidyverse)

voters_raw <- read_xlsx("Women_voters/1920-womens-voter-registers-dataset-1-2-6-8-10-11-13.xlsx") 
# Save as csv for blog:
write.csv(voters_raw, "Women_voters/womens-voter-registers.csv")

voters <- voters_raw %>% 
  janitor::clean_names()

occupations_clean <- voters %>% 
  mutate(occupation = tolower(occupation),
         occupation = gsub("-", "", occupation),
         occupation = gsub("\\[|\\]", "", occupation),
         occupation = case_when(
           is.na(occupation) ~ "Not given",
           grepl("(hous.*wife)|(h. ?w)|(at home)|(^married$)|(house.*work)|(^home$)", occupation) ~ "housewife",
           grepl("(telephone)|(tel. operator)|(^.?phone)", occupation) ~ "telephone operator",
           grepl("(^m.*ch.*operat)|(operator machine)", occupation) ~ "machine operator",
           grepl("switch.*board", occupation) ~ "switchboard operator",
           grepl("(dress).*(mak)", occupation) ~ "dressmaker",
           grepl("sten", occupation) ~ "stenographer",
           grepl("boo.*keep", occupation) ~ "bookkeeper",
           grepl("house.*keep", occupation) ~ "housekeeper",
           grepl("book.*binder", occupation) ~ "bookbinder",
           grepl("box.*maker", occupation) ~ "box maker",
           grepl("brush.*maker", occupation) ~ "brush maker",
           grepl("nurse", occupation) ~ "nurse",
           grepl("store.*keeper", occupation) ~ "storekeeper",
           grepl("(( )|^)(hair)", occupation) ~ "hairdresser",
           grepl("telegraph", occupation) ~ "telegrapher",
           grepl("candy", occupation) ~ "candy worker",
           grepl("sales", occupation) ~ "saleslady",
           grepl("laund", occupation) ~ "laundress",
           grepl("fore", occupation) ~ "forelady",
           grepl("(office work)|(office assistant)", occupation) ~ "office work",
           grepl("piano", occupation) ~ "piano teacher",
           grepl("school.*teacher", occupation) ~ "teacher",
           grepl("buyer", occupation) ~ "buyer",
           grepl("compt", occupation) ~ "comptometer operator",
           grepl("secretary", occupation) ~ "secretary",
           grepl("(^sew)|(seamstress)", occupation) ~ "seamstress",
           grepl("(milliner)|(hat)", occupation) ~ "hatmaker",
           grepl("elevator", occupation) ~ "elevator operator",
           grepl("physician", occupation) ~ "physician",
           grepl("clerk", occupation) ~ "clerk",
           TRUE ~ occupation),
         occupation = fct_lump_min(occupation, min = 7)) %>% 
  count(occupation, sort = TRUE) %>% 
  mutate(order = 1:nrow(.))

n_housewives <- occupations_clean[1, 2] %>% as.numeric()
n_housewives_div3 <- round(n_housewives/3)
occupations_clean[1, 2] <- n_housewives_div3

# Is domestic the same as housewife? Assuming housekeeper is not the same, as you might be keeping someone else's house?

n_occs <- nrow(occupations_clean)
bar_width <- 2.2

ggplot(occupations_clean) +
  geom_segment(aes(x = 0, xend = n, 
                   y = reorder(occupation, n), yend = reorder(occupation, n)),
               size = bar_width) +
  # Bottom curve, right side
  geom_curve(aes(x = n_housewives_div3, y = n_occs, xend = n_housewives_div3, yend = n_occs+1), 
                 colour = "black", curvature = 1.4, size = bar_width) +
  # Middle segment
  geom_segment(aes(x = n_housewives_div3, xend = 0, y = n_occs+1, yend = n_occs+1), 
               size = bar_width) +
  # Top curve, left side
  geom_curve(aes(x = 0, y = n_occs+1, xend = 0, yend = n_occs+2), 
             colour = "black", curvature = -1.4, size = bar_width) +
  geom_segment(aes(x = n_housewives_div3, xend = 0, y = n_occs+2, yend = n_occs+2), 
               size = bar_width) +
  # Number labels
  geom_text(aes(x = n+50, y = reorder(occupation, n), label = n), hjust = 0, size = 2) +
  scale_y_discrete(expand = expansion(mult = c(0.01, .05)))

ggsave("Women_voters/women_voters.pdf", width = 6, height = 8)
