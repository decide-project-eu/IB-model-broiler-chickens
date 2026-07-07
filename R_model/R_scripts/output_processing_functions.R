# interpret_output takes a dataframe with multiple simulations (rounds or also
# different scenarios) with one row per step (day)

# TO ADD:
# antibiotics: how much treated (% of simulations), mean-min-max costs
# early slaughter: how many times done (% of simulations), which day


interpret_output <- function(
    df,
    growth_factor,
    production_info
    ){
  
  df <- df %>%
    select(-c(level, agent_id, De)) %>%
    rename(
      day = step,
      red_grow_IB = RGi,
      red_grow_B = RGb,
      total_birds = P
    ) %>%
    left_join(production_info)
  
  # Change growth parameters based on growth factor (for sensitivity analysis)
  if(growth_factor == "low"){
    df <- df %>%
      mutate(
        DG_IB == DG_IB_low,
        DG_B == DG_B_low,
        FI_IB == FI_IB_low,
        FI_B == FI_B_low
      )
  }
  else if(growth_factor == "high"){
    df <- df %>%
      mutate(
        DG_IB == DG_IB_high,
        DG_B == DG_B_high,
        FI_IB == FI_IB_high,
        FI_B == FI_B_high
      )
  }
  
   df_clean <- df %>%
    group_by(simu_id) %>%  #for accumulate function of weights
    arrange(simu_id, day) %>%
    mutate(
      # proportions
      normal_growing_birds = NG + NGi + NGr,
      p_normal = normal_growing_birds / total_birds,
      p_IB = red_grow_IB / total_birds,
      p_B = red_grow_B / total_birds,
      p_tot = round(p_normal + p_IB + p_B, 0), # to check
      p_lesions = (B + RL) / total_birds,
      p_no_lesions = (total_birds - (B + RL)) / total_birds,
      
      #mortality
      mortality = percentage_mortality / 100,
      
      #resp signs
      resp_signs = percentage_total_resp_signs / 100,
      peak_resp_signs = max(resp_signs),
      
      # weight
      DG_per_bird = p_normal * DG_healthy + 
        p_IB * DG_IB + 
        p_B * DG_B,
      weight_per_bird = (accumulate(DG_per_bird, ~ .x + .y)) / 1000, # in kg
      total_weight = weight_per_bird * total_birds, # in kg
      
      # weight of thinned birds
      # proportion diseased is the same in thinned and total flock.
      # so fraction thinned * weight at day 34 is a good approximation.
      # This is 0 before thinning.
      thinned_weight = weight_per_bird * cum_thinning,
      
      # feed intake and FCR
      total_daily_FI = (normal_growing_birds * FI_healthy + 
        red_grow_IB * FI_IB +
        red_grow_B * FI_B)
        / 1000, # in kg
      cum_FI = accumulate(total_daily_FI, ~ .x + .y),
      FCR_daily = cum_FI / (total_weight + thinned_weight),
        # this one should be evaluated at thinning and end
      
      #antibiotics
      treatment_start = as.integer(
        not_yet_treated == 0 & lag(not_yet_treated, default = 1) == 1
        )
    ) %>%
    select(-not_yet_treated)
  
  df_clean
}


get_production_values <- function(
    df,
    DOA,
    condemnation_base,
    condemnation_B,
    slaughter_price,
    feed_price,
    ab_price_per_chick,
    ab_price_constant,
    vaccine_price_per_chick,
    n_birds
    ){
  
  # Get summary statistics (min, mean, max) of outputs, all runs taken together

  # Get outputs at day of thinning
  thinning_summary <- df %>%
    # select only day of thinning for each simulation
    group_by(simu_id) %>%
    filter(day == last(thinning_time)) %>%  #last = value stored at slaughter day
    # calculate outputs
    mutate(
      thinning_day = first(day),
      FCR_thinning = FCR_daily,
      delivered_weight_thinning = thinned_weight - (DOA * thinned_weight),
      condemned_thinning = condemnation_B * p_lesions + 
        condemnation_base * p_no_lesions,
      n_condemned_thinning = condemned_thinning * cum_thinning,
      slaughter_weight_thinning = (1 - condemned_thinning) * delivered_weight_thinning,
    ) %>%
    rename(
      n_thinned = cum_thinning,
      weight_per_bird_thinning = weight_per_bird,
    ) %>%
    ungroup() %>%
    select(
      simu_id, 
      thinning_day,
      thinned_weight, 
      FCR_thinning, 
      condemned_thinning,
      n_condemned_thinning,
      slaughter_weight_thinning,
      n_thinned,
      weight_per_bird_thinning
    )
  
  # Then get treatment information from day of treatment
  summary_treated_flocks <- df %>%
    filter(treatment_start == 1) %>%
    group_by(simu_id) %>%
    summarise(
      treatment_day = first(day),
      treated = 1,
      treated_flock_size = first(total_birds),
      treated_kg = first(total_weight),
      treated_days = 3, 
      treatment_cost = ab_price_per_chick * treated_flock_size +
        ab_price_constant
    ) %>%
    ungroup()
  
  # Make a df with all simu_ids, otherwise left_join gives error when
  # summary_treated_flocks is empty
  treatment_summary <- df %>%
    distinct(simu_id) %>%
    mutate(
      treatment_day = as.integer(NA),
      treated = 0,
      treated_flock_size = 0,
      treated_kg = 0,
      treated_days = 0, 
      treatment_cost = 0
    ) %>%
    rows_update(summary_treated_flocks, by = "simu_id")
  
  # Then summarise the output at slaughter time,
  # adding output from thinning and treatment days
  end_total <- df %>%
    # select only the last day of each simulation
    arrange(simu_id, day) %>%
    group_by(simu_id) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    # add thinning and treatment summary
    select(-thinned_weight) %>%
    left_join(thinning_summary, by = "simu_id") %>%
    left_join(treatment_summary, by = "simu_id") %>%
    # calculate outputs of slaughter and total (thinning + slaughter)
    mutate(
      condemned_end = condemnation_B * p_lesions + condemnation_base * p_no_lesions,
      n_condemned_end = condemned_end * total_birds,
      condemned_total = (n_condemned_thinning + n_condemned_end) / (n_thinned + total_birds),
      delivered_weight_end = total_weight - (DOA * total_weight),
      slaughter_weight_end = (1 - condemned_end) * delivered_weight_end,
      slaughter_weight_total = slaughter_weight_thinning + 
        slaughter_weight_end,
      weight_revenue = slaughter_weight_total * slaughter_price,
      feed_cost = cum_FI * feed_price,
      vaccination_cost = ifelse(
        vaccination_scenario == 0,
        0,
        vaccine_price_per_chick * n_birds
        ),
      avg_BW = (thinned_weight + total_weight) / (n_thinned + total_birds),
      delta = weight_revenue - feed_cost - treatment_cost - vaccination_cost,
      DDD_year = treated_days / day * 365,   #day is slaughter day
      
      early_thinning_triggered = as.integer(thinning_day < 34),
      early_slaughter_triggered = as.integer(day < 42)
    )
  
  end_total
  
}
  

get_production_summary <- function(df, scenario_id){
  
  # works on the output of get_production_values to summarise to one line
  # per scenario, with mean, min and max values of selected outputs
  
  end_summary <- df %>%
    group_by() %>%
    summarise(
      # Identifiers of scenario and used parameters
      scenario = scenario_id,
      n = n(),
      vaccination_scenario = first(vaccination_scenario),
      introduction_time = first(introduction_time),
      early_slaughter = first(early_slaughter),
      beta = first(beta),
      recovery = first(recovery),
      #prop_clinical = first(prop_clinical),
      t_until_bact_inf = first(t_until_bact_inf),
      prop_bact_factor = first(prop_bact_factor),
      #time_before_reduced_growth = first(time_before_reduced_growth),
      duration_reduced_growth = first(duration_reduced_growth),
      length_B = first(length_B),
      mortality_I = first(mortality_I), #not varied
      mortality_B = first(mortality_B), #not varied
      treatment_trigger = first(treatment_trigger), #not varied
      condemnation_B = condemnation_B,
      growth_factor = growth_factor,
      slaughter_price = slaughter_price, #not varied
      feed_price = feed_price, #not varied
      ab_price_per_chick = ab_price_per_chick, #not varied
      ab_price_constant = ab_price_constant, #not varied
      vaccine_price = vaccine_price_per_chick,
      
      # summary of infection values
      mean_p_prevalence = mean(percentage_prevalence)/100,
      min_p_prevalence = min(percentage_prevalence)/100,
      max_p_prevalence = max(percentage_prevalence)/100,
      mean_p_resp_signs = mean(resp_signs),
      min_p_resp_signs = min(resp_signs),
      max_p_resp_signs = max(resp_signs),
      mean_p_secondary = mean(percentage_prevalence_secondary)/100,
      min_p_secondary = min(percentage_prevalence_secondary)/100,
      max_p_secondary = max(percentage_prevalence_secondary)/100,
      mean_peak_resp_signs = mean(peak_resp_signs),
      min_peak_resp_signs = min(peak_resp_signs),
      max_peak_resp_signs = max(peak_resp_signs),
      mean_cum_clin_IB = mean(cum_I),
      min_cum_clin_IB = min(cum_I),
      max_cum_clin_IB = max(cum_I),
      mean_cum_secondary = mean(cum_B),
      min_cum_secondary = min(cum_B),
      max_cum_secondary = max(cum_B),
      
      # summary of production values
      mean_thinned_weight = mean(thinned_weight),
      mean_end_weight = mean(total_weight),
      min_end_weight = min(total_weight),
      max_end_weight = max(total_weight),
      mean_avg_BW = mean(avg_BW), # this includes thinning weights!
      min_avg_BW = min(avg_BW),
      max_avg_BW = max(avg_BW),
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
      mean_cond = mean(condemned_total), # includes thinning!
      min_cond = min(condemned_total),
      max_cond = max(condemned_total),
      mean_feed_cost = mean(feed_cost),
      min_feed_cost = min(feed_cost),
      max_feed_cost = max(feed_cost),
      mean_mort = mean(mortality),
      min_mort = min(mortality),
      max_mort = max(mortality),
      
      early_thinning_triggered = mean(thinning_day < 34),
      early_slaughter_triggered = mean(day < 42),
      
      #vaccination
      vaccination_cost = mean(vaccination_cost),
      
      #antibiotics
      mean_treatment_cost = mean(treatment_cost),
      min_treatment_cost = min(treatment_cost),
      max_treatment_cost = max(treatment_cost),
      fraction_treatment_triggered = mean(treatment_triggered),
      fraction_treated = mean(treated),
      median_treatment_day = median(treatment_day),
      min_treatment_day = min(treatment_day),
      max_treatment_day = max(treatment_day),
      mean_kg_treated = mean(treated_kg),
      min_kg_treated = min(treated_kg),
      max_kg_treated = max(treated_kg),
      mean_DDD_year = mean(DDD_year),
      min_DDD_year = min(DDD_year),
      max_DDD_year = max(DDD_year),
    )
  
  end_summary
}
  

check_scenarios <- function(df){
  
  # Get an overview of simulations in the dataset
  
  scenario_check <- df %>%
  summarise(
    n_simulations = n(),
    n_rounds = first(n),
    n_rounds_constant = ifelse(n_distinct(n) > 1, 0, 1),
    vaccination_scenario = list(unique(vaccination_scenario)),
    introduction_times = list(unique(introduction_time)),
    early_slaughter = list(unique(early_slaughter)),
    prop_bact_factor = list(unique(prop_bact_factor))
  )
  View(scenario_check)
}

#######################################
#########     TABLES   ################
#######################################

make_publication_table <- function(df,
                                   digits = 3) {
  
  df <- df %>%
    select(
      "vaccination_scenario","introduction_time",
      "early_slaughter", "prop_bact_factor",
      "mean_delta",
      "mean_weight_per_bird",
      "mean_mort",
      "mean_cond",
      "mean_DDD_year"
    ) %>%
    arrange(
      vaccination_scenario,
      introduction_time, 
      early_slaughter,
      prop_bact_factor
    )
  
  df
}


##################################
#            PLOTS               #
##################################

