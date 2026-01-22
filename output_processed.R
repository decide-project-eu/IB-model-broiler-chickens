
# THIS VERSION WORKS WITH THE 'separate_bact_state' SCRIPT!


# ------------------------
# PARAMETERS - change here
# ------------------------

# Economic values
slaughter_price = 1.36  # / kg live weight
feed_price = 0.48 # /kg
antibiotic_price = NA


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


# ------------------------
# Production values
# ------------------------

healthy_production <- read_excel("healthy_production.xlsx")

# add disease DG, FI and FCR to the production df
healthy_production <- healthy_production %>%
  rename(
    FI_healthy = healthy_intake,
    DG_healthy = healthy_DG
  ) %>%
  mutate(
    # put weight at day 0 in column DG of day 0 (for calculations)
    DG_healthy = ifelse(day == 0, 44, DG_healthy),
    
    # Feed intake is 0 at day 0 and 1
    FI_healthy = ifelse((day == 0 | day == 1), 0, FI_healthy)
  )

DOA <- 0.0008

# ------------------------
# EMULSION output: multiple scenarios
# ------------------------

# read all data files and put in a list

# when the files are all in one folder:
#dataFiles <- lapply(Sys.glob("./outputs/batch_results*.csv"), read.csv)

# when each file is in its own folder:
files_to_read = list.files(
  path = "./batch_results_23_12_25",        # directory to search within
  #pattern = ".*(1|2).*csv$", # regex pattern, some explanation below
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)

# ! this takes +- 1 min with 900 files
dataFiles = lapply(files_to_read, read.csv)


# loop through all data files and the parameter values of FCR, DG, condemnation
all_scenarios <- list()

scenario <- 1
for(factor in c(0.5, 1, 2)){
  
  # calculate new DG and FI
  lower_DG_IB <- 1 - factor * 0.05
  lower_DG_B <- 1 - factor * 0.20
  higher_FCR_IB <- 1 + factor * 0.03 # this is without taking mortality into account, to calculate feed intake
  higher_FCR_B <- 1 + factor * 0.07
  lower_FI_IB <- lower_DG_IB * higher_FCR_IB
  lower_FI_B <- lower_DG_B * higher_FCR_B
  
  production_values <- healthy_production %>%
    mutate(
      # DG and FCR of diseased animals
      DG_IB = ifelse(day == 0, 44, lower_DG_IB * DG_healthy),
      DG_B = ifelse(day == 0, 44, lower_DG_B * DG_healthy),
      FI_IB = lower_FI_IB * FI_healthy,
      FI_B = lower_FI_B * FI_healthy
    )
  
  
  for(model_output in dataFiles){
    
    df <- model_output %>%
      select(-c(level, agent_id, D, De)) %>%
      rename(
        day = step,
        red_grow_IB = RGi,
        red_grow_B = RGb,
        total_birds = P
      ) %>%
      left_join(production_values) %>%
      group_by(simu_id) %>%
      arrange(simu_id, day) %>%
      mutate(
        # proportions
        normal_growing_birds = NG + NGi,
        p_normal = normal_growing_birds / total_birds,
        p_IB = red_grow_IB / total_birds,
        p_B = red_grow_B / total_birds,
        p_tot = round(p_normal + p_IB + p_B, 0), # to check
        
        #mortality
        mortality = percentage_mortality_10 / 1000,
        
        # weight
        DG_per_bird = p_normal * DG_healthy + 
          p_IB * DG_IB + 
          p_B * DG_B,
        weight_per_bird = (accumulate(DG_per_bird, ~ .x + .y)) / 1000, # in kg
        total_weight = weight_per_bird * total_birds, # in kg
        
        # weight of thinned birds
        # proportion diseased is the same in thinned and total flock.
        # so fraction thinned * weight at day 34  seems like a good approximation
        thinned_weight = ifelse(day >= 34, weight_per_bird * cum_thinning_total, 0),
        
        # feed intake and FCR
        total_daily_FI = (normal_growing_birds * FI_healthy + 
                            red_grow_IB * FI_IB +
                            red_grow_B * FI_B) / 1000, # in kg
        cum_FI = accumulate(total_daily_FI, ~ .x + .y),
        FCR_daily = cum_FI / (total_weight + thinned_weight)
      )
    
    for(condemnation_fraction in c(0.05, 0.25, 0.55)){
    
      df_34_summary <- df %>%
        filter(day == 34) %>%
        mutate(
          FCR_d34 = cum_FI / (total_weight + thinned_weight),
          delivered_weight_d34 = thinned_weight - (DOA * thinned_weight),
          condemned_d34 = condemnation_fraction * percentage_prevalence_secondary / 100,
          slaughter_weight_d34 = delivered_weight_d34 - (condemned_d34 * delivered_weight_d34)
        ) %>%
        select(
          simu_id, thinned_weight, FCR_d34, condemned_d34, slaughter_weight_d34
        )
      
      df_42_summary <- df %>%
        filter(day == 42) %>%
        select(-day, -thinned_weight) %>%
        left_join(df_34_summary) %>%
        mutate(
          condemned_d42 = condemnation_fraction * percentage_prevalence_secondary / 100,
          delivered_weight_d42 = total_weight - (DOA * total_weight),
          slaughter_weight_d42 = delivered_weight_d42 - (condemned_d42 * delivered_weight_d42), # condemnations already subtracted
          slaughter_weight_total = slaughter_weight_d34 + slaughter_weight_d42,
          weight_revenue = slaughter_weight_total * slaughter_price,
          feed_cost = cum_FI * feed_price,
          delta = weight_revenue - feed_cost
        ) %>%
        group_by() %>%
        summarise(
          # identifiers  (mean doesn't do anything because it's a constant value)
          introduction_time = mean(introduction_time),
          beta = mean(beta),
          recovery = mean(recovery),
          t_until_bact_inf = mean(t_until_bact_inf),
          prop_bact_factor = mean(prop_bact_factor),
          duration_reduced_growth = mean(duration_reduced_growth),
          bacterial_recovery_rate = mean(bacterial_recovery_rate),
          mortality_I = mean(mortality_I), #not varied
          mortality_B = mean(mortality_B), #not varied
          treatment_trigger = mean(treatment_trigger), #not varied
          condemnation_fraction = condemnation_fraction,
          factor_growth = factor,
          
          # summary of infection values
          mean_p_prevalence = mean(percentage_prevalence)/100,
          min_p_prevalence = min(percentage_prevalence)/100,
          max_p_prevalence = max(percentage_prevalence)/100,
          mean_p_resp_signs = mean(percentage_respiratory_signs)/100,
          min_p_resp_signs = min(percentage_respiratory_signs)/100,
          max_p_resp_signs = max(percentage_respiratory_signs)/100,
          mean_p_secondary = mean(percentage_prevalence_secondary)/100,
          min_p_secondary = min(percentage_prevalence_secondary)/100,
          max_p_secondary = max(percentage_prevalence_secondary)/100,
          
          # summary of production values
          mean_thinned_weight = mean(thinned_weight),
          mean_end_weight = mean(total_weight),
          min_end_weight = min(total_weight),
          max_end_weight = max(total_weight),
          healthy_weight_per_bird = mean(healthy_weight)/1000, # in kg
          mean_weight_per_bird = mean(weight_per_bird),
          relative_weight = mean_weight_per_bird / healthy_weight_per_bird,
          mean_weight_revenue = mean(weight_revenue),
          min_weight_revenue = min(weight_revenue),
          max_weight_revenue = max(weight_revenue),
          mean_cum_FI = mean(cum_FI),
          min_cum_FI = min(cum_FI),
          max_cum_FI = max(cum_FI),
          mean_FCR = mean(FCR_daily), # day 42!
          min_FCR = min(FCR_daily),
          max_FCR = max(FCR_daily),
          mean_delta = mean(delta),
          min_delta = min(delta),
          max_delta = max(delta),
          mean_cond = mean(condemned_d42), # day 42!
          min_cond = min(condemned_d42),
          max_cond = max(condemned_d42),
          mean_feed_cost = mean(feed_cost),
          min_feed_cost = min(feed_cost),
          max_feed_cost = max(feed_cost),
          mean_mort = mean(mortality),
          min_mort = min(mortality),
          max_mort = max(mortality)
        ) %>%
        mutate(
          scenario = scenario,
          treatment_cost = 0
        )
      
      all_scenarios[[scenario]] <- df_42_summary
      scenario <- scenario + 1
      #rm(df, df_34_summary, df_42_summary)
    }
  }
}

all_scenarios <- bind_rows(all_scenarios)

# save
write.csv(all_scenarios, "output_23_12_25.csv", row.names = FALSE)




#### plot ####


all_scenarios <- read.csv( "output_23_12_25.csv")

# test the effect of changing input parameter values

x_variables <- c(
  'beta', 
  'recovery', 
  't_until_bact_inf',
  'prop_bact_factor',
  'duration_reduced_growth',
  'bacterial_recovery_rate',
  'condemnation_fraction',
  'factor_growth'
  )

y_variables <- c(
  'mean_p_prevalence',
  'mean_p_secondary',
  'mean_mort',
  'relative_weight',
  'condemnation',
  'mean_profit'
  )

bounded_vars <- c(
  "mean_p_prevalence",
  "mean_p_secondary",
  "condemnation"
)

percent_vars <- c(
  "mean_mort",
  "relative_weight"
)


colors1 <- c('#51C3CC', '#FFCA99', '#CC5800')
colors2 <- c("#1B9E77", "#D95F02", "#7570B3")
colors3 <-c(
  "#00A6D6",  # cyan
  "#1F78B4",  # bright blue
  "#6A3D9A"   # purple
)


for(x in x_variables){

  
  plots <- list()
  
  for(y in y_variables){
    
    # change y-axis label for certain variable
    y_label <- ifelse(
      y == "mean_profit",
      "Price (weight - feed)",
      ifelse(
        y == "mean_p_prevalence",
        "p_IB",
        ifelse(
          y == "mean_p_secondary",
          "p_bact",
          ifelse(
            y == "mean_mort",
            "Mortality",
            y)
        )
      )
    )
    
    summary_df <- all_scenarios %>%
      group_by(introduction_time, .data[[x]]) %>%
      summarize(
        mean = mean(.data[[y]]),
        min = min(.data[[y]]),  # this is min of the mean of 20 iterations
        max = max(.data[[y]])
      )
    
    fileloc <- paste0('output_summary_', y, '.csv')
    write.csv(as.data.frame(summary_df), fileloc, row.names = TRUE)
    
    p <- ggplot(
      aes(
        x = .data[[x]],
        y = mean,
        group = factor(introduction_time),
        color = factor(introduction_time)
      ),
      data = summary_df) +
      geom_ribbon(
        aes(
          ymin = min,
          ymax = max,
          fill = factor(introduction_time)
        ),
        alpha = 0.1,
        color = NA
      ) +
      geom_line(
        aes(
          y = mean,
          color = factor(introduction_time)
        ),
        linewidth = 1.5) +
      theme_classic(base_size = 14) +
      theme(
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9)
      ) +
      #expand_limits(y = 0) +
      labs(y = y_label, x = x) +
      scale_fill_manual(values = colors3, name = "Introduction time") +
      scale_color_manual(values = colors3, name = "Introduction time")
    
    # set y axis limits for percentage variables
    if (y %in% bounded_vars) {
      p <- p + scale_y_continuous(
        limits = c(0, 1),
        labels = label_percent(accuracy = 1)
        )
    }
    
    if (y %in% percent_vars) {
      p <- p + scale_y_continuous(
        labels = label_percent(accuracy = 1)
      )
    }
    
    plots[[y]] <- p
  }
  
  grid <- wrap_plots(plots, ncol = 3) +  # from package 'patchwork'
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.text  = element_text(size = 14),
      legend.title = element_text(size = 14)
  )
  
  ggsave(
    filename = paste0("plot_", x, ".png"),
    plot = grid,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  # Show on screen
  print(grid)
}




# ------------------------
# EMULSION output: HEALTHY scenario
# ------------------------

counts <- read.csv("./healthy/counts.csv")

df_h <- counts %>%
  select(-c(level, agent_id, D, De)) %>%
  rename(
    day = step,
    total_birds = P
  ) %>%
  left_join(production_values) %>%
  group_by(simu_id) %>%
  arrange(simu_id, day) %>%
  mutate(
    # weight
    DG_per_bird = DG_healthy,
    weight_per_bird = (accumulate(DG_per_bird, ~ .x + .y)) / 1000, # in kg
    total_weight = weight_per_bird * total_birds, # in kg
    
    # weight of thinned birds
    # proportion diseased is the same in thinned and total flock.
    # so fraction thinned * weight at day 34  seems like a good approximation
    thinned_weight = ifelse(day >= 34, weight_per_bird * cum_thinning_total, 0),
    
    # feed intake and FCR
    total_daily_FI = (total_birds * FI_healthy) / 1000, # in kg
    cum_FI = accumulate(total_daily_FI, ~ .x + .y),
    FCR_daily = cum_FI / (total_weight + thinned_weight)
  ) %>%
  select(
    simu_id,
    day,
    total_birds,
    cum_thinning_total,
    healthy_weight,
    healthy_FCR,
    DG_per_bird,
    weight_per_bird,
    total_weight,
    thinned_weight,
    total_daily_FI,
    cum_FI,
    FCR_daily
  )

### Output at day 42
# with thinning weights from day 34 added
# averaged over simulation rounds

df_34_summary <- df_h %>%
  filter(day == 34) %>%
  mutate(
    FCR_d34 = cum_FI / (total_weight + thinned_weight),
    #slaughter_price_d34 = slaughter_price * thinned_weight,
    #condemned_d34 = condemnation_fraction * percentage_prevalence_secondary
  ) %>%
  select(
    simu_id, FCR_d34 #, slaughter_price_d34, condemned_d34
  )

df_42_healthy <- df_h %>%
  filter(day == 42) %>%
  select(-day) %>%
  left_join(df_34_summary) %>%
  group_by() %>%
  mutate(
    total_weight_slaughtered = thinned_weight + total_weight,
    weight_revenue = slaughter_price * total_weight_slaughtered,
    feed_cost = feed_price * cum_FI,
    delta = weight_revenue - feed_cost
  ) %>%
  summarise(
    mean_thinned_weight = mean(thinned_weight),
    mean_end_weight = mean(total_weight),
    mean_weight_per_bird = mean(weight_per_bird),
    mean_total_weight = mean(total_weight_slaughtered),
    min_total_weight = min(total_weight_slaughtered),
    max_total_weight = max(total_weight_slaughtered),
    mean_weight_revenue = mean(weight_revenue),
    min_weight_revenue = min(weight_revenue),
    max_weight_revenue = max(weight_revenue),
    mean_cum_FI = mean(cum_FI),
    min_cum_FI = min(cum_FI),
    max_cum_FI = max(cum_FI),
    mean_delta = mean(delta),
    min_delta = min(delta),
    max_delta = max(delta),
    mean_FCR = mean(FCR_daily)
  ) %>%
mutate(
  FCR = mean_cum_FI / mean_total_weight
)

View(df_42_healthy)
