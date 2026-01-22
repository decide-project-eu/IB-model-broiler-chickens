
# To compare one variable: introduction time, with all other parameters default values


# ------------------------
# Before starting
# ------------------------

library(dplyr)
library(readxl)
library(purrr) #for accumulate function
library(ggplot2)
library(patchwork)
library(scales)

options(scipen = 999)

# -------------------------
# EMULSION output: different introduction days
# ------------------------

all_scenarios <- read.csv( "output_23_12_25.csv")

df_IB <- all_scenarios %>%
  filter(
    beta == 0.8,
    recovery == 0.0833,
    t_until_bact_inf == 5,
    prop_bact_factor == 1.0,
    duration_reduced_growth == 7.0,  # or 10?
    between(bacterial_recovery_rate, 0.070, 0.072), # 1/14 doesn't work
    condemnation_fraction == 0.25,
    factor_growth == 1
  ) %>%
  select(
    introduction_time, mean_p_prevalence, mean_p_resp_signs, mean_p_secondary, 
    mean_weight_per_bird,
    mean_weight_revenue, min_weight_revenue, max_weight_revenue,
    mean_cum_FI, min_cum_FI, max_cum_FI,
    mean_cond, min_cond, max_cond,
    mean_feed_cost, min_feed_cost, max_feed_cost,
    mean_mort, min_mort, max_mort,
    mean_delta, min_delta, max_delta,
    mean_FCR, min_FCR, max_FCR
  ) %>%
  mutate(
    weight_per_bird_ratio = mean_weight_per_bird / 2.998,
    mean_weight_revenue_diff = mean_weight_revenue - 118857,
    min_weight_revenue_diff = max_weight_revenue - 118857,
    max_weight_revenue_diff = min_weight_revenue - 118857,
    mean_feed_cost_diff = mean_feed_cost - 58305.48,
    min_feed_cost_diff = max_feed_cost - 58305.48,
    max_feed_cost_diff = min_feed_cost - 58305.48,
    mean_diff = mean_delta - 60551.52,
    min_diff = max_delta - 60551.52,
    max_diff = min_delta - 60551.52,
    mean_FCR_ratio = mean_FCR / 1.390
  )

# add healthy (introduction 0), EENMALIG!!
healthy <- c(
  "x", 0, 0, 0,
  2.998, # mean weight per bird
  118857, 118530.6, 118999.5,# weight revenue
  121469.7, 121175.4, 121578.3, # cum_FI,
  0, 0, 0, # condemnation (! set to 0),
  58305.48, NA, NA, # feed cost,
  0.03, NA, NA, # mortality,
  60551.52, 60366.42, 60655.11 # delta
)

df <- rbind(df_IB, healthy)

colors3 <-c(
  "#909090",
  "#00A6D6",  # cyan
  "#1F78B4",  # bright blue
  "#6A3D9A"   # purple
)


# Graphs compared to healthy (difference)

# weight per bird
ggplot(
  aes(x = factor(introduction_time),
      y = mean_weight_per_bird, 
      fill = factor(introduction_time)),
  data = df_IB
) +
  geom_bar(stat = "identity") +
  labs(y = "Mean bird weight", x = "Introduction day") +
  theme_classic(base_size = 20) +
  theme(
    legend.position="none"
  ) +
  scale_fill_manual(values = colors3)

# Missed slaughter weight
ggplot(
  aes(x = factor(introduction_time),
      y = mean_weight_revenue_diff/1000, 
      fill = factor(introduction_time)),
  data = df_IB
) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(
      ymin = min_weight_revenue_diff/1000,
      ymax = max_weight_revenue_diff/1000),
    width=.5
  ) +
  theme_classic(base_size = 16) +
  theme(legend.position="none") +
  labs(y = "Slaughter weight revenue vs. healthy (x 1000 €)", x = "Introduction day") +
  scale_fill_manual(values = colors3) +
  ylim(-25,0)  +
  geom_hline(yintercept = 0, linewidth = 0.6)

# Feed costs
ggplot(
  aes(x = factor(introduction_time),
      y = -mean_feed_cost_diff/1000, 
      fill = factor(introduction_time)),
  data = df_IB
) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(
      ymin = -min_feed_cost_diff/1000,
      ymax = -max_feed_cost_diff/1000),
    width=.5
  ) +
  theme_classic(base_size = 16) +
  theme(legend.position="none") +
  labs(y = "Feed costs vs. healthy (x 1000 €)", x = "Introduction day") +
  scale_fill_manual(values = colors3) +
  ylim(-25, 3) +
  geom_hline(yintercept = 0, linewidth = 0.6)

# Slaughter weight - feed cost
ggplot(
  aes(x = factor(introduction_time),
      y = mean_diff/1000, 
      fill = factor(introduction_time)),
  data = df_IB
) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(
      ymin = min_diff/1000,
      ymax = max_diff/1000),
    width=.5
  ) +
  labs(y = "Margin over feed cost vs. healthy (in € x1000)", x = "Introduction day") +
  theme_classic(base_size = 16) +
  theme(
    legend.position="none"
  ) +
  scale_fill_manual(values = colors3) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  ylim(-25, 0)

# Condemnation (due to IB/sec inf)
ggplot(
  aes(x = factor(introduction_time),
      y = mean_cond, 
      fill = factor(introduction_time)),
  data = df_IB
) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(
      ymin = min_cond,
      ymax = max_cond),
    width=.5
  ) +
  labs(y = "Condemnation d42 due to IB", x = "Introduction day") +
  theme_classic(base_size = 20) +
  theme(
    legend.position="none"
  ) +
  scale_fill_manual(values = colors3) +
  scale_y_continuous(labels = scales::percent)

# Mortality
ggplot(
  aes(x = factor(introduction_time),
      y = mean_mort, 
      fill = factor(introduction_time)),
  data = df_IB
) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(
      ymin = min_mort,
      ymax = max_mort),
    width=.5
  ) +
  labs(y = "Total mortality", x = "Introduction day") +
  theme_classic(base_size = 20) +
  theme(
    legend.position="none"
  ) +
  scale_fill_manual(values = colors3) +
  scale_y_continuous(labels = scales::percent)

# FCR
ggplot(
  aes(x = factor(introduction_time),
      y = mean_FCR, 
      fill = factor(introduction_time)),
  data = df_IB
) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(
      ymin = min_FCR,
      ymax = max_FCR),
    width=.5
  ) +
  labs(y = "FCR", x = "Introduction day") +
  theme_classic(base_size = 20) +
  theme(
    legend.position="none"
  ) +
  scale_fill_manual(values = colors3)
