packages <- FALSE

if (packages){
  install.packages('tidyverse')
  install.pakages('WDI')
  install.packages('wid')
}
library(WDI)
library(tidyverse)
library(wid)

# original test with WDI

gini <- WDI(
  country = c('IN','SA'),
  indicator = "SI.POV.GINI",
  start = 1980,
  end = 2025
)

data_gini <- gini %>% filter(!is.na(SI.POV.GINI)) %>% select(country,year,indicator = SI.POV.GINI)
View(data_gini)

#WDI has no GINI data for Saudia Arabia and Not alot for India so using wid instead:


gini_wid <- download_wid(
  indicators = "gptinc",     
  areas      = c("IN", "SA"),
  perc       = "p0p100",     
  ages       = "992",        
  pop        = "j"           
) %>%
  mutate(country = recode(country, "IN" = "India", "SA" = "Saudi Arabia")) %>%
  select(country, year, gini = value) %>% arrange(gini)

#Arranging so i can later cross refrence the data too see if my graphs correct

view(gini_wid)

# plotting 

ggplot(gini_wid, aes(year, gini, colour = country)) +
  geom_line(linewidth = 1) +
  labs(title = "Gini coefficient (pre-tax income)", x = NULL, y = "Gini",
       caption = "Source: World Inequality Database") +
  theme_minimal()


# Next Step I need to do both weath and income ineqaulity

