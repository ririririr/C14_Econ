packages <- FALSE

if (packages){
  install.packages('tidyverse')
  install.packages('WDI')
  install.packages('wid')
  install.packages('patchwork')
}
library(WDI)
library(tidyverse)
library(wid)
library(ggplot2)
library(patchwork)
country_colours <- c("India" = "#1f77b4", "Saudi Arabia" = "#2ca02c")

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

plotted <- ggplot(gini_wid, aes(year, gini, colour = country)) +
  geom_line(linewidth = 1) +
  labs(title = "Gini coefficient (pre-tax income)", x = 'Year', y = "Gini Cofficent",
       caption = "Source: World Inequality Database") +
  theme_minimal()

plotted


# Next Step I need to do both weath and income ineqaulity

p9010 <- download_wid(
  indicators = "tptinc",                  # income thresholds
  areas      = c("IN", "SA"),
  perc       = c("p10p100", "p90p100"),   # threshold of p10p100 = P10, of p90p100 = P90
  ages       = "992",
  pop        = "j"
) %>%
  mutate(country = recode(country, "IN" = "India", "SA" = "Saudi Arabia"),
         pctl    = recode(percentile, "p10p100" = "P10", "p90p100" = "P90")) %>%
  select(country, year, pctl, value) %>%
  pivot_wider(names_from = pctl, values_from = value) %>%
  mutate(ratio_90_10 = P90 / P10) %>%
  arrange(country, year)

view(p9010)

p9010_plot <- ggplot(p9010, aes(year, ratio_90_10, colour = country)) +
  geom_line(linewidth = 1) +
  labs(title = "P90/P10 ratio (pre-tax income)", x = "Year", y = "P90 / P10",
       colour = NULL, caption = "Source: World Inequality Database") +
  theme_minimal()

#P9010 shows the minimum income required to be in top 10% in a country compared to the maximum income in the bottom 10%




combined <- (plotted + p9010_plot) +
  plot_layout(guides = "collect") +          # one shared legend instead of two
  plot_annotation(title = "Income inequality: India vs Saudi Arabia",
                  caption = "Source: World Inequality Database") &
  theme(legend.position = "bottom")

combined

# Wealth inequality: top 10% share of net personal wealth

wealth_wid <- download_wid(
  indicators = "shweal",     # share of net personal wealth
  areas      = c("IN", "SA"),
  perc       = "p90p100",    # top 10%
  ages       = "992",        # adults
  pop        = "j"           # equal-split, same as income measures
) %>%
  mutate(country = recode(country, "IN" = "India", "SA" = "Saudi Arabia")) %>%
  select(country, year, top10_wealth = value) %>%
  filter(year >= 1980) %>%
  arrange(country, year)

view(wealth_wid)

# Plotting

wealth_plot <- ggplot(wealth_wid, aes(year, top10_wealth, colour = country)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = country_colours) +
  labs(title = "Top 10% wealth share", x = "Year", y = "Share of total wealth",
       colour = NULL, caption = "Source: World Inequality Database") +
  theme_minimal()

wealth_plot

# All three inequality measures side by side

(plotted + wealth_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Income and wealth inequality: India vs Saudi Arabia",
                  caption = "Source: World Inequality Database") &
  theme(legend.position = "bottom")

ggsave("inequality_part1.png", width = 14, height = 5, dpi = 300)

