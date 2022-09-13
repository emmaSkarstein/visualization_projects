# Norwegian monarchs
# Emma Skarstein, September 2022


library(rvest)
library(tidyverse)
library(janitor)


# Lokalhistorie-wikien har supernice tabell!
url <- "https://lokalhistoriewiki.no/wiki/Norges_monarker"
page <- read_html(url)
# Obtain the piece of the web page that corresponds to the "wikitable" node
my.element <- html_element(page, ".wikitable")
# Convert the html table element into a data frame
my.table <- html_table(my.element, fill = TRUE) %>% clean_names()

# Lagre, i tilfelle nettsida skulle endre seg
#write_csv(my.table, "Kongerekka/kongedata_skitten.csv")

konge_tabell <- my.table %>% select(navn, tiltradte, fratradte, fodt, dod) %>% 
  mutate(across(tiltradte:dod, ~ sub(" .*", "", .x)),
         across(tiltradte:dod, ~ as.numeric(.x)))

# Legg til dødsdato for Svein Håkonsson
svein <- which(konge_tabell$navn == "Svein Håkonsson ladejarl")
konge_tabell$dod[svein] <- 1016

# Legg til fødselsdato for Olav den hellige
olav <- which(konge_tabell$navn == "Olav II den hellige")
konge_tabell$fodt[olav] <- 995

# Legg til fødselsdato for Erling Steinvegg
erling <- which(konge_tabell$navn == "Erling Steinvegg")
#konge_tabell$fodt[erling] <- 

# Legg til fødselsdato for Filippus Simonsson
filippus <- which(konge_tabell$navn == "Filippus Simonsson")
konge_tabell$fodt[filippus] <- 1185






