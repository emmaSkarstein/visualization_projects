# Norwegian monarchs
# Emma Skarstein, September 2022


library(rvest)
library(tidyverse)
library(janitor)
library(showtext)


# HENTE OG RYDDE DATA ----------------------------------------------------------

# Lokalhistorie-wikien har supernice tabell!
#url <- "https://lokalhistoriewiki.no/wiki/Norges_monarker"
#page <- read_html(url)
# Obtain the piece of the web page that corresponds to the "wikitable" node
#my.element <- html_element(page, ".wikitable")
# Convert the html table element into a data frame
#my.table <- html_table(my.element, fill = TRUE) %>% clean_names()

# Lagre, i tilfelle nettsida skulle endre seg
#write_csv(my.table, "Kongerekka/kongedata_skitten.csv")

my.table <- read.csv("Kongerekka/kongedata_skitten.csv")
konge_tabell <- my.table %>% select(navn, tiltradte, fratradte, fodt, dod) %>% 
  mutate(across(tiltradte:dod, ~ sub(" .*", "", .x)),
         across(tiltradte:dod, ~ as.numeric(.x)))

# Legg til dødsdato for Svein Håkonsson, og korriger fødselsdato
svein <- which(konge_tabell$navn == "Svein Håkonsson ladejarl")
konge_tabell$dod[svein] <- 1016
konge_tabell$fodt[svein] <- 970

# Legg til fødselsdato for Olav den hellige
olav <- which(konge_tabell$navn == "Olav II den hellige")
konge_tabell$fodt[olav] <- 995

# Legg til fødselsdato for Erling Steinvegg
erling <- which(konge_tabell$navn == "Erling Steinvegg")
konge_tabell$fodt[erling] <- 1204 #OBS: Dette er året han tiltrådte, i mangel på fødselsår.

# Legg til fødselsdato for Filippus Simonsson
filippus <- which(konge_tabell$navn == "Filippus Simonsson")
konge_tabell$fodt[filippus] <- 1185

# Rediger fratrådt-dato til Guttorm Sigurdsson så streken vil vises
guttorm_sigurdsson <- which(konge_tabell$navn == "Guttorm Sigurdsson")
konge_tabell$fratradte[guttorm_sigurdsson] <- konge_tabell$tiltradte[guttorm_sigurdsson] + 1

# Rediger fratrådt-dato til Christian Fredrik så streken vil vises
christian_frederik <- which(konge_tabell$navn == "Christian Frederik")
konge_tabell$fratradte[christian_frederik] <- konge_tabell$tiltradte[christian_frederik] + 1

# Legg til "dødsdato" og "fratrådt-dato" til Kong Harald
dd <- 2023
haraldV <- nrow(konge_tabell)
konge_tabell$dod[haraldV] <- dd
konge_tabell$fratradte[haraldV] <- dd

## Legg til historisk periode --------------------------------------------------
konge_tabell$periode <- NA

# Hårfagreætten og Ladejarlene
haarfagreaetten_ladejarlene_start <- which(konge_tabell$navn == "Harald I Hårfagre")
haarfagreaetten_ladejarlene_slutt <- which(konge_tabell$navn == "Magnus V Erlingsson")
konge_tabell$periode[haarfagreaetten_ladejarlene_start:haarfagreaetten_ladejarlene_slutt] <- "Hårfagreætten og Ladejarlene"

# Sverreætten
sverreaetten_start <- which(konge_tabell$navn == "Sverre Sigurdsson")
sverreaetten_slutt <- which(konge_tabell$navn == "Olav IV Håkonsson")
konge_tabell$periode[sverreaetten_start:sverreaetten_slutt] <- "Sverreætten"

# Kalmarunionen
kalmar_start <- which(konge_tabell$navn == "Margrete Valdemarsdatter")
kalmar_slutt <- which(konge_tabell$navn == "Karl I Knutsson Bonde")
konge_tabell$periode[kalmar_start:kalmar_slutt] <- "Kalmarunionen"

# Dansketiden
dansketiden_start <- which(konge_tabell$navn == "Christian I")
dansketiden_slutt <- which(konge_tabell$navn == "Frederik VI")
konge_tabell$periode[dansketiden_start:dansketiden_slutt] <- "Dansketiden"

# 1814
#konge_tabell$periode[which(konge_tabell$navn == "Christian Frederik")] <- "1814"
konge_tabell$periode[which(konge_tabell$navn == "Christian Frederik")] <- "Dansketiden"

# Union med Sverige
sverige_start <- which(konge_tabell$navn == "Karl II")
sverige_slutt <- which(konge_tabell$navn == "Oscar II")
konge_tabell$periode[sverige_start:sverige_slutt] <- "Union med Sverige"

# Selvstendig Norge
selvstendig_start <- which(konge_tabell$navn == "Haakon VII")
konge_tabell$periode[selvstendig_start:nrow(konge_tabell)] <- "Selvstendig Norge"

## Legg til nummer innad i hver periode ----------------------------------------
konge_tabell <- konge_tabell %>% 
  group_by(periode) %>% 
  mutate(nummer = row_number())

# PLOTTING ---------------------------------------------------------------------

## Laste inn fonter ----
f1 <- "Open sans"
f2 <- "Playfair Display"
f_num <- "PT Serif"

font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
font_add_google(name = f_num, family = f_num)
showtext_auto()

## Farger ----
col_bgr <- "grey95"
col_line <- "grey85"

## Lag figur ----
p <- ggplot(konge_tabell, aes(x = tiltradte, y = nummer, group = navn)) +
  geom_linerange(aes(xmin = fodt, xmax = dod),
                 color = "grey70",
                 #lineend = "round", 
                 linewidth = 4,
                 alpha = 0.4) +
  geom_linerange(aes(xmin = tiltradte, xmax = fratradte, color = periode),
                 linewidth = 4,
                 alpha = 0.6) +
  geom_text(aes(x = fratradte, label = navn, group = navn), 
            size = 8, family = f1, color = "#191919",
            hjust = 1) +
  scale_x_continuous(breaks = seq(900, 2023, by = 100),
                     limits = c(850, 2024), 
                     expand = c(0,0)) +
  theme_minimal() +
  theme(text = element_text(family = f1, size = 30),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color = col_line),
        plot.background = element_rect(fill = col_bgr, color = col_bgr),
        panel.background = element_rect(fill = col_bgr, color = col_bgr),
        panel.grid = element_line(color = col_line),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
p
ggsave("Kongerekka/kongerekka.png", height = 5, width = 30)

