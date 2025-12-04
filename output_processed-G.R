
# THIS VERSION WORKS WITH THE 'separate_bact_state' SCRIPT


# ------------------------
# PARAMETERS - change here
# ------------------------

# Disease parameters
# TO DO: separate mild and severe infections
lower_DG_mild <- 0.99
lower_DG_severe <- 0.95
lower_DG_bact_subc <- 0.90
lower_DG_bact_clin <- 0.70

higher_FCR_mild <- 1.02 # this is without taking mortality into account,
                   # to calculate feed intake
higher_FCR_severe <- 1.05
higher_FCR_bact_subc <- 1.05
higher_FCR_bact_clin <- 1.1

lower_FI_mild <- lower_DG_mild*higher_FCR_mild
lower_FI_severe <- lower_DG_severe*higher_FCR_severe
lower_FI_bact_subc <- lower_DG_bact_subc*higher_FCR_bact_subc
lower_FI_bact_clin <- lower_DG_bact_clin*higher_FCR_bact_clin


# Economic values
slaughter_price = 1.36  # / kg live weight
feed_price = 0.48 # /kg
antibiotic_price = NA
condemnation_fraction = 0.5   # of secondary infected birds


# ------------------------
# Before starting
# ------------------------

library(dplyr)
library(readxl)
library(purrr) #for accumulate function
library(ggplot2)
library(patchwork)

options(scipen = 999)


# ------------------------
# Production values
# ------------------------

healthy_production <- read_excel("healthy_production.xlsx")

# add disease DG, FI and FCR to the production df
production_values <- healthy_production %>%
  rename(
    FI_healthy = healthy_intake,
    DG_healthy = healthy_DG
  ) %>%
  mutate(
    # put weight at day 0 in column DG of day 0 (for calculations)
    DG_healthy = ifelse(day == 0, 44, DG_healthy),
    DG_mild = ifelse(day == 0, 44, lower_DG_mild * DG_healthy),
    DG_severe = ifelse(day == 0, 44, lower_DG_severe * DG_healthy),
    DG_bact_subc = ifelse(day == 0, 44, lower_DG_bact_subc * DG_healthy),
    DG_bact_clin = ifelse(day == 0, 44, lower_DG_bact_clin * DG_healthy),
    
    # Feed intake is 0 at day 0 and 1
    FI_healthy = ifelse((day == 0 | day == 1), 0, FI_healthy),
    FI_mild = lower_FI_mild * FI_healthy,
    FI_severe = lower_FI_severe * FI_healthy,
    FI_bact_subc = lower_FI_bact_subc * FI_healthy,
    FI_bact_clin = lower_FI_bact_clin * FI_healthy
  )


# ------------------------
# EMULSION output: multiple scenarios
# ------------------------
# NOTE: this is for adaptations in the EMULSION model. Not for parameters
# in this script: DG and FCR.

# each variable that changes between scenarios should get a column

# read all data files and put in a list

# when the files are all in one folder:
#dataFiles <- lapply(Sys.glob("./outputs/batch_results*.csv"), read.csv)

# when each file is in its own folder:
files_to_read = list.files(
  path = "./batch_results",        # directory to search within
  #pattern = ".*(1|2).*csv$", # regex pattern, some explanation below
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
dataFiles = lapply(files_to_read, read.csv)


# loop through all data files
all_scenarios <- list()
scenario <- 1
for(model_output in dataFiles){
  
  df <- model_output %>%
    select(-c(level, agent_id, D, De)) %>%
    rename(
      day = step,
      red_grow_mild = RGi1,
      red_grow_severe = RGi2,
      red_grow_bact_subc = RGbs,
      red_grow_bact_clin = RGbc,
      total_birds = P
    ) %>%
    left_join(production_values) %>%
    group_by(simu_id) %>%
    arrange(simu_id, day) %>%
    mutate(

      normal_growing_birds = NG + NGi1 + NGi2,
      p_normal = normal_growing_birds / total_birds,
      p_mild = red_grow_mild / total_birds,
      p_severe = red_grow_severe / total_birds,
      p_bact_subc = red_grow_bact_subc / total_birds,
      p_bact_clin = red_grow_bact_clin / total_birds,
      p_tot = round(p_normal + p_mild + p_severe + p_bact_subc + p_bact_clin, 0), # to check
      
      #mortality
      #mortality = cum_mortality / 30000,  # think if this is the best version or 'real' cumulative mortality.
      mortality = percentage_mortality_10 / 1000,
      
      # weight
      DG_per_bird = p_normal * DG_healthy + 
        p_mild * DG_mild + 
        p_severe * DG_severe +
        p_bact_subc * DG_bact_subc +
        p_bact_clin * DG_bact_clin,
      weight_per_bird = (accumulate(DG_per_bird, ~ .x + .y)) / 1000, # in kg
      total_weight = weight_per_bird * total_birds, # in kg
      
      # weight of thinned birds
      # proportion diseased is the same in thinned and total flock.
      # so fraction thinned * weight at day 34  seems like a good approximation
      thinned_weight = ifelse(day == 34, weight_per_bird * cum_thinning_total, 0),
      
      # feed intake and FCR
      total_daily_FI = (normal_growing_birds * FI_healthy + 
                          red_grow_mild * FI_mild +
                          red_grow_severe * FI_severe +
                          red_grow_bact_subc * FI_bact_subc +
                          red_grow_bact_clin * FI_bact_clin
      ) / 1000, # in kg
      cum_FI = accumulate(total_daily_FI, ~ .x + .y),
      FCR_daily = cum_FI / (total_weight + thinned_weight) # werkt niet omdat thinned_weight 0 is na dag 34
    )
  
  df_34_summary <- df %>%
    filter(day == 34) %>%
    mutate(
      FCR_d34 = cum_FI / (total_weight + thinned_weight),
      slaughter_price_d34 = slaughter_price * thinned_weight,
      condemned_d34 = condemnation_fraction * percentage_prevalence_secondary
    ) %>%
    select(
      simu_id, thinned_weight, FCR_d34, slaughter_price_d34, condemned_d34
    )
  
  df_42_summary <- df %>%
    filter(day == 42) %>%
    select(-day, -thinned_weight) %>%
    left_join(df_34_summary) %>%
    group_by() %>%
    summarise(
      # identifiers  (mean doesn't do anything because it's a constant value)
      introduction_time = mean(introduction_time),
      beta = mean(beta),
      recovery = mean(recovery),
      t_until_bact_inf = mean(t_until_bact_inf),
      mortality_I2 = mean(mortality_I2),
      mortality_BC = mean(mortality_BC),
      treatment_trigger = mean(treatment_trigger),
      
      # summary of infection values
      mean_p_prevalence = mean(percentage_prevalence)/100,
      mean_p_resp_signs = mean(percentage_respiratory_signs)/100,
      mean_p_secondary = mean(percentage_prevalence_secondary)/100,
      
      # summary of production values
      mean_thinned_weight = mean(thinned_weight),
      mean_total_weight = mean(total_weight),
      min_total_weight = min(total_weight),
      max_total_weight = max(total_weight),
      healthy_weight_per_bird = mean(healthy_weight)/1000, # in kg
      mean_weight_per_bird = mean(weight_per_bird),
      relative_weight = mean_weight_per_bird / healthy_weight_per_bird,
      mean_cum_FI = mean(cum_FI),
      min_cum_FI = min(cum_FI),
      max_cum_FI = max(cum_FI),
      mean_sec_inf = mean(percentage_prevalence_secondary),
      min_sec_inf = min(percentage_prevalence_secondary),
      max_sec_inf = max(percentage_prevalence_secondary),
      mean_mort = mean(mortality),
      min_mort = min(mortality),
      max_mort = max(mortality)
    ) %>%
    mutate(
      scenario = scenario
    )
    
  all_scenarios[[scenario]] <- df_42_summary
  scenario <- scenario + 1
  #rm(df, df_34_summary, df_42_summary)
}

all_scenarios <- bind_rows(all_scenarios)


# test the importance of input variable

x_variables <- c('beta', 'recovery', 't_until_bact_inf', 'introduction_time')
y_variables <- c('mean_p_prevalence', 'mean_p_secondary', 'mean_mort', 'relative_weight')


for(x in x_variables){
  
  plots <- list()
  
  for(y in y_variables){
    
    p <- ggplot(aes(
        x = .data[[x]],
        y = .data[[y]],
        group = factor(introduction_time),
        color = factor(introduction_time)
      ), data = all_scenarios) +
      geom_point(
        size = 3
      ) +
      stat_summary(
        aes(y = .data[[y]], group = 1),
        fun.y = mean,
        colour = "#DB830D",
        geom = "line",
        linewidth = 2,
        group = 1
      ) +
      theme_classic(base_size = 14) +
      theme(
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9)
      ) +
      expand_limits(y = 0) +
      scale_y_continuous(labels = scales::percent)
    
    plots[[y]] <- p
  }
  
  grid <- wrap_plots(plots, ncol = 2)  # from package 'patchwork'
  
  # Show on screen
  print(grid)
}


# ------------------------
# Growth parameters sensitivity analysis
# ------------------------

# Change parameters, loop
# create plots same way as above



# ------------------------
# EMULSION output: single scenario
# ------------------------

counts <- read.csv("./outputs/for_R/counts.csv")

df <- counts %>%
  select(-c(level, agent_id, D, De)) %>%
  rename(
    day = step,
    red_grow_mild = RGi1,
    red_grow_severe = RGi2,
    red_grow_bact_subc = RGbs,
    red_grow_bact_clin = RGbc,
    total_birds = P
  ) %>%
  left_join(production_values) %>%
  group_by(simu_id) %>%
  arrange(simu_id, day) %>%
  mutate(

    normal_growing_birds = NG + NGi1 + NGi2,
    p_normal = normal_growing_birds / total_birds,
    p_mild = red_grow_mild / total_birds,
    p_severe = red_grow_severe / total_birds,
    p_bact_subc = red_grow_bact_subc / total_birds,
    p_bact_clin = red_grow_bact_clin / total_birds,
    p_tot = round(p_normal + p_mild + p_severe + p_bact_subc + p_bact_clin, 0), # to check

    #mortality
    #mortality = cum_mortality / 30000,  # think if this is the best version or 'real' cumulative mortality.
    mortality = percentage_mortality_10 / 1000,
    
    # weight
    DG_per_bird = p_normal * DG_healthy + 
                    p_mild * DG_mild + 
                    p_severe * DG_severe +
                    p_bact_subc * DG_bact_subc +
                    p_bact_clin * DG_bact_clin,
    weight_per_bird = (accumulate(DG_per_bird, ~ .x + .y)) / 1000, # in kg
    total_weight = weight_per_bird * total_birds, # in kg
    
    # weight of thinned birds
      # proportion diseased is the same in thinned and total flock.
      # so fraction thinned * weight at day 34  seems like a good approximation
    thinned_weight = ifelse(day == 34, weight_per_bird * cum_thinning_total, 0),
    
    # feed intake and FCR
    total_daily_FI = (normal_growing_birds * FI_healthy + 
                        red_grow_mild * FI_mild +
                        red_grow_severe * FI_severe +
                        red_grow_bact_subc * FI_bact_subc +
                        red_grow_bact_clin * FI_bact_clin
                        ) / 1000, # in kg
    cum_FI = accumulate(total_daily_FI, ~ .x + .y),
    FCR_daily = cum_FI / (total_weight + thinned_weight) # werkt niet omdat thinned_weight 0 is na dag 34
  ) %>%
  select(
    simu_id,
    day,
    total_birds,
    cum_thinning_total,
    mortality,
    percentage_respiratory_signs,
    percentage_prevalence_secondary,
    healthy_weight,
    healthy_FCR,
    DG_per_bird,
    weight_per_bird,
    total_weight,
    thinned_weight,
    total_daily_FI,
    cum_FI,
    FCR_daily # nog niet goed?
  )

par(mfrow = c(2,2))
plot(df$day, df$weight_per_bird)
plot(df$day, df$DG_per_bird)
plot(df$day, df$total_weight)
plot(df$day, df$percentage_respiratory_signs)

plot(df$day, df$total_daily_FI)
plot(df$day, df$cum_FI)



# OUTPUT PLOTS PER DAY, AVERAGED OVER SIMULATIONS

df_daily_summary <- df %>%
  group_by(day) %>%
  summarise(
    mean_total_weight = mean(total_weight),
    min_total_weight = min(total_weight),
    max_total_weight = max(total_weight),
    
    mean_weight_per_bird = mean(weight_per_bird),
    min_weight_per_bird = min(weight_per_bird),
    max_weight_per_bird = max(weight_per_bird),
    
    mean_FI = mean(total_daily_FI),
    min_FI = min(total_daily_FI),
    max_FI = max(total_daily_FI),
    mean_cum_FI = mean(cum_FI),
    min_cum_FI = min(cum_FI),
    max_cum_FI = max(cum_FI)
  )

ggplot(
  data = df_daily_summary,
  aes(x = day,
      y = mean_total_weight
  )) +
  geom_line(
    linewidth = 1.5,
    color = '#c24a9f'
  ) +
  geom_ribbon(
    aes(ymin = min_total_weight, ymax = max_total_weight),
    alpha = 0.5,
    fill = '#c24a9f'
  )+
  theme_classic() +
  xlab("Day") +
  ylab("Total weight (kg)")


ggplot(
  data = df_summary,
  aes(x = day,
      y = mean_weight_per_bird,
      ymin = min_weight_per_bird,
      ymax = max_weight_per_bird
  )) +
  geom_line(
    linewidth = 2,
    color = '#57a0d6'
  ) +
  geom_ribbon(
    alpha = 0.5,
    fill = '#57a0d6'
  )+
  theme_minimal() +
  xlab("Day") +
  ylab("Bird weight (kg)")


ggplot(
  data = df_summary,
  aes(x = day,
      y = mean_FI/1000,
      ymin = min_FI/1000,
      ymax = max_FI/1000
  )) +
  geom_line(
    linewidth = 1,
    color = '#ffa500'
  ) +
  geom_ribbon(
    alpha = 0.5,
    fill = '#ffa500'
  )+
  theme_minimal() +
  xlab("Day") +
  ylab("Feed intake (x 1000 kg)")


ggplot(
  data = df_summary,
  aes(x = day,
      y = mean_cum_FI/1000,
      ymin = min_cum_FI/1000,
      ymax = max_cum_FI/1000
  )) +
  geom_line(
    linewidth = 1,
    color = '#b56727'
  ) +
  geom_ribbon(
    alpha = 0.5,
    fill = '#b56727'
  )+
  theme_minimal() +
  xlab("Day") +
  ylab("Cumulative feed intake (x 1000 kg)")


### Output at day 42
# with thinning weights from day 34 added
# averaged over simulation rounds

df_34_summary <- df %>%
  filter(day == 34) %>%
  mutate(
    FCR_d34 = cum_FI / (total_weight + thinned_weight),
    #slaughter_price_d34 = slaughter_price * thinned_weight,
    #condemned_d34 = condemnation_fraction * percentage_prevalence_secondary
  ) %>%
  select(
    simu_id, thinned_weight, FCR_d34 #, slaughter_price_d34, condemned_d34
  )

df_42_summary <- df %>%
  filter(day == 42) %>%
  select(-day, -thinned_weight) %>%
  left_join(df_34_summary) %>%
  group_by() %>%
  summarise(
    mean_thinned_weight = mean(thinned_weight),
    mean_total_weight = mean(total_weight),
    min_total_weight = min(total_weight),
    max_total_weight = max(total_weight),
    healthy_weight_per_bird = mean(healthy_weight),
    mean_weight_per_bird = mean(weight_per_bird),
    relative_weight = mean_weight_per_bird / healthy_weight_per_bird,
    mean_cum_FI = mean(cum_FI),
    min_cum_FI = min(cum_FI),
    max_cum_FI = max(cum_FI),
    mean_sec_inf = mean(percentage_prevalence_secondary),
    min_sec_inf = min(percentage_prevalence_secondary),
    max_sec_inf = max(percentage_prevalence_secondary),
    mean_mort = mean(mortality),
    min_mort = min(mortality),
    max_mort = max(mortality)
  ) #%>%
  #mutate(
  #  FCR = mean_cum_FI / mean_total_weight,  # KLOPT NIET WANT GEWICHT VAN UITGELADEN KIPPEN
  #  feed_cost = feed_price * mean_cum_FI,
  #  slaughter_price_d42 = slaughter_price * mean_total_weight,
  #  condemned = condemnation_fraction * mean_sec_inf
  #)

View(df_42_summary)



# ------------------------
# EMULSION output: HEALTHY scenario
# ------------------------

counts <- read.csv("./healthy/counts.csv")

df <- counts %>%
  select(-c(level, agent_id, D, De)) %>%
  rename(
    day = step,
    red_grow_mild = RGi1,
    red_grow_severe = RGi2,
    red_grow_bact_subc = RGbs,
    red_grow_bact_clin = RGbc,
    total_birds = P
  ) %>%
  left_join(production_values) %>%
  group_by(simu_id) %>%
  arrange(simu_id, day) %>%
  mutate(
    
    normal_growing_birds = NG + NGi1 + NGi2,
    p_normal = normal_growing_birds / total_birds,
    p_mild = red_grow_mild / total_birds,
    p_severe = red_grow_severe / total_birds,
    p_bact_subc = red_grow_bact_subc / total_birds,
    p_bact_clin = red_grow_bact_clin / total_birds,
    p_tot = round(p_normal + p_mild + p_severe + p_bact_subc + p_bact_clin, 0), # to check
    
    #mortality
    #mortality = cum_mortality / 30000,  # think if this is the best version or 'real' cumulative mortality.
    mortality = percentage_mortality_10 / 1000,
    
    # weight
    DG_per_bird = p_normal * DG_healthy + 
      p_mild * DG_mild + 
      p_severe * DG_severe +
      p_bact_subc * DG_bact_subc +
      p_bact_clin * DG_bact_clin,
    weight_per_bird = (accumulate(DG_per_bird, ~ .x + .y)) / 1000, # in kg
    total_weight = weight_per_bird * total_birds, # in kg
    
    # weight of thinned birds
    # proportion diseased is the same in thinned and total flock.
    # so fraction thinned * weight at day 34  seems like a good approximation
    thinned_weight = ifelse(day == 34, weight_per_bird * cum_thinning_total, 0),
    
    # feed intake and FCR
    total_daily_FI = (normal_growing_birds * FI_healthy + 
                        red_grow_mild * FI_mild +
                        red_grow_severe * FI_severe +
                        red_grow_bact_subc * FI_bact_subc +
                        red_grow_bact_clin * FI_bact_clin
    ) / 1000, # in kg
    cum_FI = accumulate(total_daily_FI, ~ .x + .y),
    FCR_daily = cum_FI / (total_weight + thinned_weight) # werkt niet omdat thinned_weight 0 is na dag 34
  ) %>%
  select(
    simu_id,
    day,
    total_birds,
    cum_thinning_total,
    mortality,
    percentage_respiratory_signs,
    percentage_prevalence_secondary,
    healthy_weight,
    healthy_FCR,
    DG_per_bird,
    weight_per_bird,
    total_weight,
    thinned_weight,
    total_daily_FI,
    cum_FI,
    FCR_daily # not good yet?
  )

### Output at day 42
# with thinning weights from day 34 added
# averaged over simulation rounds

df_34_summary <- df %>%
  filter(day == 34) %>%
  mutate(
    FCR_d34 = cum_FI / (total_weight + thinned_weight),
    #slaughter_price_d34 = slaughter_price * thinned_weight,
    #condemned_d34 = condemnation_fraction * percentage_prevalence_secondary
  ) %>%
  select(
    simu_id, thinned_weight, FCR_d34 #, slaughter_price_d34, condemned_d34
  )

df_42_summary <- df %>%
  filter(day == 42) %>%
  select(-day, -thinned_weight) %>%
  left_join(df_34_summary) %>%
  group_by() %>%
  summarise(
    mean_thinned_weight = mean(thinned_weight),
    mean_total_weight = mean(total_weight),
    min_total_weight = min(total_weight),
    max_total_weight = max(total_weight),
    healthy_weight_per_bird = mean(healthy_weight),
    mean_weight_per_bird = mean(weight_per_bird),
    relative_weight = mean_weight_per_bird / healthy_weight_per_bird,
    mean_cum_FI = mean(cum_FI),
    min_cum_FI = min(cum_FI),
    max_cum_FI = max(cum_FI),
    mean_sec_inf = mean(percentage_prevalence_secondary),
    min_sec_inf = min(percentage_prevalence_secondary),
    max_sec_inf = max(percentage_prevalence_secondary),
    mean_mort = mean(mortality),
    min_mort = min(mortality),
    max_mort = max(mortality)
  ) #%>%
#mutate(
#  FCR = mean_cum_FI / mean_total_weight,  # KLOPT NIET WANT GEWICHT VAN UITGELADEN KIPPEN
#  feed_cost = feed_price * mean_cum_FI,
#  slaughter_price_d42 = slaughter_price * mean_total_weight,
#  condemned = condemnation_fraction * mean_sec_inf
#)

View(df_42_summary)
