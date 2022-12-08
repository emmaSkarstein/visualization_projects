# Kattemat

# Sources: 
# https://www.untamedcatfood.com/blogs/nutrition/how-many-calories-should-a-cat-eat
# 
library(tidyverse)

mat <- tibble::tribble(
  ~navn,         ~kcal_kg,  ~type,  ~kattunge,     ~lenke,
  "Feringa Meny Duo", 960,       "våtfor", "adult", "https://www.zooplus.no/shop/katt/vatfor_katt/feringa_vat/feringa_meny_duo/730650",
  "Almo Nature Holistic Sterilised", 3915, "tørrfor", "adult", "https://www.zooplus.no/shop/katt/torrfor_katter/almonature/almonature_spesialfor/530156",
  "Carnilove kitten", 4150, "tørrfor", "kitten", "https://www.musti.no/carnilove-cat-kitten-salmon-turkey", 
  "Feringa Kitten Fjærkre", 4134, "tørrfor", "kitten", "https://www.zooplus.no/shop/katt/torrfor_katter/feringa_toerrfor_katt/feringa_kitten/491349",
  "Bozita kitten", 4030, "tørrfor", "kitten", "https://www.musti.no/bozita-kitten-grain-free-chicken"
)

find_calorie_need <- function(weight_kg, male, spayed, outdoor, age){
  # Start with 45*weight = calorie intake
  base <- 45*weight_kg
  
  # Reduce by 25% if cat is senior (11-14 yrs)
  if(age>=11 & age<=14){
    base <- base*(1-0.25)
  }
  
  # Increase by 10% if cat is geriatric (over 15 yrs)
  if(age>=15){
    base <- base*1.1
  }
  
  # Increase by 20 kcal if cat is outdoor and 10 kcal if cat is male
  base <- base*(1-0.1*spayed) + outdoor*20 + male*10 
  
  return(base)
}

find_calorie_need(weight_kg = 3, male = 0, spayed = 0, outdoor = 0, age = 4)

mat %>% filter(navn == "Feringa Meny Duo") %>% select(kcal_kg) * 0.13





