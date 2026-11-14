# This is the R code for part 3 TBC

#Use R to assess the correlation between inequality (in income and wealth) and income
#growth. Groups should discuss reasons why ascertaining a causal nexus between these
#variables is difficult. [20 marks]
i
library(tidyverse)
library(wid)
library(ggplot2)
library(patchwork)
library(WDI)

gini_wid <- download_wid(
  indicators = "gptinc",     
  areas      = c("IN", "SA"),
  perc       = "p0p100",     
  ages       = "992",        
  pop        = "j"           
) %>%
  mutate(country = recode(country, "IN" = "India", "SA" = "Saudi Arabia")) %>%
  select(country, year, gini = value) %>% arrange(gini)

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
# 1. Growth: annual GDP per capita growth (%)
growth <- WDI(country = c("IN", "SA"), indicator = c(growth = "NY.GDP.PCAP.KD.ZG"),
              start = 1980, end = 2023) %>%
  transmute(country, year = as.integer(year), growth)

# 2. Wealth inequality: top 10% share of net personal wealth (the only new download)
wealth <- download_wid(indicators = "shweal", areas = c("IN", "SA"),
                       perc = "p90p100", ages = "992", pop = "j") %>%
  transmute(country = recode(country, "IN" = "India", "SA" = "Saudi Arabia"),
            year = as.integer(year), top10_wealth = value)

# 3. Combine with the data you already have
df <- growth %>%
  left_join(gini_wid %>% mutate(year = as.integer(year)), by = c("country", "year")) %>%
  left_join(p9010 %>% select(country, year, ratio_90_10) %>% mutate(year = as.integer(year)),
            by = c("country", "year")) %>%
  left_join(wealth, by = c("country", "year")) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(across(c(gini, ratio_90_10, top10_wealth), lag, .names = "{.col}_lag")) %>%
  ungroup()

view(df)

# 4. Correlation of last year's inequality with this year's growth, per country
df %>%
  group_by(country) %>%
  summarise(
    r_gini        = cor(gini_lag,         growth, use = "complete.obs"),
    r_p90p10      = cor(ratio_90_10_lag,  growth, use = "complete.obs"),
    r_wealth_top10 = cor(top10_wealth_lag, growth, use = "complete.obs")
  )

# Significance (swap the country / variable as needed)
# Run a correlation test for every country × measure combination
cor_results <- df %>%
  pivot_longer(c(gini_lag, ratio_90_10_lag, top10_wealth_lag),
               names_to = "measure", values_to = "inequality") %>%
  filter(!is.na(inequality), !is.na(growth)) %>%
  group_by(country, measure) %>%
  filter(sd(inequality) > 0, n() >3) %>%
  summarise(test = list(cor.test(inequality, growth)), n = n(), .groups = "drop") %>%
  mutate(r     = map_dbl(test, ~ .x$estimate),
         lower = map_dbl(test, ~ .x$conf.int[1]),
         upper = map_dbl(test, ~ .x$conf.int[2]),
         p     = map_dbl(test, ~ .x$p.value),
         measure = recode(measure,
                          gini_lag         = "Income Gini",
                          ratio_90_10_lag  = "Income P90/P10",
                          top10_wealth_lag = "Top 10% wealth share")) %>%
  select(-test)

cor_results   # table to put in your write-up

ggplot(cor_results, aes(x = r, y = measure, colour = country)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  position = position_dodge(width = 0.5), linewidth = 0.8) +
  scale_colour_manual(values = c("India" = "#1f77b4", "Saudi Arabia" = "#2ca02c")) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title = "Correlation between inequality (previous year)\n and GDP per capita",
       subtitle = "Points = Pearson r; bars = 95% confidence interval",
       x = "Correlation coefficient (r)", y = NULL, colour = NULL,
       caption = "Sources: World Inequality Database; World Bank WDI") +
  theme_minimal() +
  theme(legend.position = "bottom")


# if the dot is right of r=0 when inequality was higher growth tended to increases at a higher rate


# The line is the 95% confidence level


#For India, the correlation between lagged income Gini and growth is r = [0.277] (95% CI: [-0.027] to [0.533]). As the interval includes zero, we cannot reject the hypothesis of no relationship between inequality and higher rates of growth.”

# How to calculate the confidence level(if you took fresh data 95% of the time the true r value would lie inbetween this interval 95% of the time)

# transform r: z = arctanh(r). Standard error = 1/(root(n-3)). For 95% confidence level the value inside 2.5% normal distribution = qnorm(0.975) = 1.96
#Interval space = z +- (1.96 x SE)
# this part is why a casual nexus is hard to come by all the data gathered contains an interval that contains r =0 so null hypotheis cant be rejected



#Spearmen is another way to check correlations sp for confidence i will compare the data presented via Spearmens to Pearson

both_results <- df %>%
  pivot_longer(c(gini_lag, ratio_90_10_lag, top10_wealth_lag),
               names_to = "measure", values_to = "inequality") %>%
  filter(!is.na(inequality), !is.na(growth)) %>%
  group_by(country, measure) %>%
  filter(sd(inequality) > 0, n() > 3) %>%            # skip flat series
  summarise(
    n          = n(),
    pearson_r  = cor(inequality, growth, method = "pearson"),
    pearson_p  = cor.test(inequality, growth, method = "pearson")$p.value,
    spearman_rho = cor(inequality, growth, method = "spearman"),
    spearman_p   = cor.test(inequality, growth, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(measure = recode(measure,
                          gini_lag         = "Income Gini",
                          ratio_90_10_lag  = "Income P90/P10",
                          top10_wealth_lag = "Top 10% wealth share"),
         across(where(is.numeric) & !n, ~ round(.x, 3)))

both_results
view(both_results)


#Something like Spearmens results are significant for India so some other factors could be driving the correlation down in pearsons test like the 1991 economic crash.
