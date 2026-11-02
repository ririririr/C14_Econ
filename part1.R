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
  select(country, year, gini = value) %>% 

view(gini_wid)

# plotting 

library(scales)

last_points <- gini_wid %>%
  group_by(country) %>%
  filter(year == max(year))

ggplot(gini_wid %>% filter(year >= 1980),
       aes(year, gini, colour = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = last_points, size = 3) +
  geom_text(data = last_points,
            aes(label = paste0(country, "\n", round(gini, 2))),
            hjust = -0.15, size = 3.8, fontface = "bold", lineheight = 0.9) +
  scale_colour_manual(values = c("India" = "#E07B39", "Saudi Arabia" = "#2A7F62")) +
  scale_x_continuous(breaks = seq(1980, 2025, 5),
                     expand = expansion(mult = c(0.02, 0.15))) +
  scale_y_continuous(limits = c(0.4, 0.8), labels = number_format(accuracy = 0.05)) +
  labs(
    title    = "Income inequality in India and Saudi Arabia",
    subtitle = "Gini coefficient of pre-tax national income, adults, 1980 onwards",
    x = NULL, y = "Gini coefficient",
    caption  = "Source: World Inequality Database (WID.world). Saudi figures are modelled estimates."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 16),
    plot.subtitle      = element_text(colour = "grey40"),
    plot.caption       = element_text(colour = "grey50", hjust = 0),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text          = element_text(colour = "grey30")
