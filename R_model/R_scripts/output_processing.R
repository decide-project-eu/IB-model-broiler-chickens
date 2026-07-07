
# This is for comparing different scenarios and for sensitivity analysis


# ------------------------
# Before starting
# ------------------------

setwd("C:/Users/4243692/models/ibv-model")

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(tidyr)
  library(stringr)
  library(purrr) #for accumulate function
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(flextable)
  library(officer)
})

source('R_model/R_scripts/output_processing_functions.R')
source('R_model/R_scripts/config_functions.R')

options(scipen = 999)

# load data with growth/feed intake of healthy and diseased birds
# (overwrite this when changing impact disease on growth)
production_info <- read.csv("R_model/data/production_values.csv")


# ------------------------------------------------------
# PARAMETERS AND FILE DIRECTORIES - change in config.yaml
# ------------------------------------------------------

config <- ReadAndValidateConfig("R_model/config/config.yaml")

DOA                   <- config$R_model$DOA
condemnation_base     <- config$R_model$condemnation_base
condemnation_B        <- config$R_model$condemnation_B
slaughter_price       <- config$R_model$slaughter_price
feed_price            <- config$R_model$feed_price
ab_price_per_chick    <- config$R_model$ab_price_per_chick
ab_price_constant     <- config$R_model$ab_price_constant
vaccine_price_per_chick <- config$R_model$vaccine_price_per_chick
growth_factor         <- config$R_model$growth_factor
n_birds               <- config$R_model$n_birds

input_dir             <- config$project$input_dir
output_dir            <- config$project$output_dir

sens_input_dir        <- config$project$sens_input_dir
sens_output_dir       <- config$project$sens_output_dir

sens_condemnation     <- config$R_sensitivity$sens_condemnation
sens_growth_factor    <- config$R_sensitivity$sens_growth_factor
sens_slaughter_price  <- config$R_sensitivity$sens_slaughter_price
sens_feed_price       <- config$R_sensitivity$sens_feed_price
sens_ab_price_per_chick <- config$R_sensitivity$sens_ab_price_per_chick

# treated_days is always 3 because this is used for the days when treatment
# is allowed in the EMULSION model.


# ------------------------------------------------------
# EMULSION output: multiple scenarios
# ------------------------------------------------------

files_to_read = list.files(
  path = input_dir,  # directory to search within
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)

print(paste('Number of files:', length(files_to_read)))

dataFiles = lapply(files_to_read, read.csv)


# loop through all data files
daily_all_scenarios <- list()
all_scenarios <- list()
all_scenarios_summarised <- list()
scenario <- 1
for(model_output in dataFiles){
  
  # skip simulations with wrong introduction time
  if(model_output$introduction_time[1] == 7){
    next
  }
  
  df <- interpret_output(model_output, growth_factor, production_info)
  production_values <- get_production_values(
    df,
    DOA = DOA,
    condemnation_base = condemnation_base,
    condemnation_B = condemnation_B,
    slaughter_price = slaughter_price,
    feed_price = feed_price,
    ab_price_per_chick = ab_price_per_chick,
    ab_price_constant = ab_price_constant,
    vaccine_price_per_chick = vaccine_price_per_chick,
    n_birds = n_birds
    )
  end_summary <- get_production_summary(production_values, scenario)
  daily_all_scenarios[[scenario]] <- df
  all_scenarios[[scenario]] <- production_values
  all_scenarios_summarised[[scenario]] <- end_summary
  scenario <- scenario + 1
}
daily_all_scenarios <- bind_rows(daily_all_scenarios)
all_scenarios <- bind_rows(all_scenarios)
all_scenarios_summarised <- bind_rows(all_scenarios_summarised)

# provide a summary of different scenarios
check_scenarios(all_scenarios_summarised)

# save output
dir_path <- paste0(output_dir, "/output_", Sys.Date())
dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

filename_all_days <- file.path(dir_path, "output_all_days.csv")
write.csv(daily_all_scenarios, filename_all_days, row.names = FALSE)
filename <- file.path(dir_path, "output.csv")
write.csv(all_scenarios, filename, row.names = FALSE)
filename_summary <- file.path(dir_path, "output_summarised.csv")
write.csv(all_scenarios_summarised, filename_summary, row.names = FALSE)

# ! note: daily mortality is one day late.


########################
# Sensitivity analysis #
########################

# 1) process and summarise outputs from EMULSION sensitivity runs
# 2) change condemnation, performance and prices in default model


# 1) process and summarise outputs from EMULSION sensitivity runs

files_to_read_sens = list.files(
  path = sens_input_dir,  # directory to search within
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)
print(paste('Number of files:', length(files_to_read_sens)))
dataFiles_sens = lapply(files_to_read_sens, read.csv)

# loop through all data files
daily_all_scenarios_sens <- list()
all_scenarios_sens <- list()
sens_summarised <- list()
scenario <- 1
for(model_output in dataFiles_sens){
  df <- interpret_output(model_output, growth_factor, production_info)
  production_values <- get_production_values(
    df,
    DOA = DOA,
    condemnation_base = condemnation_base,
    condemnation_B = condemnation_B,
    slaughter_price = slaughter_price,
    feed_price = feed_price,
    ab_price_per_chick = ab_price_per_chick,
    ab_price_constant = ab_price_constant,
    vaccine_price_per_chick = vaccine_price_per_chick,
    n_birds = n_birds
  )
  end_summary <- get_production_summary(production_values, scenario)
  daily_all_scenarios_sens[[scenario]] <- df
  all_scenarios_sens[[scenario]] <- production_values
  sens_summarised[[scenario]] <- end_summary
  scenario <- scenario + 1
}
all_scenarios_summarised_sens1 <- bind_rows(sens_summarised)

# provide a summary of different scenarios
check_scenarios(all_scenarios_summarised_sens1)



# 2) change condemnation, performance and prices here

# get default model output
# run loop with condemnation, performance and prices adapted

default_scenario <- daily_all_scenarios %>%
  filter(
    vaccination_scenario == 0,
    introduction_time == 14,
    early_slaughter == 0,
    prop_bact_factor == 1
  )

#search for 'raw' default model (for growth factor change)
for(model_output in dataFiles){
  
  # skip simulations with wrong introduction time
  if(model_output$introduction_time[1] == 14){
    if(model_output$vaccination_scenario[1] == 0){
      if(model_output$early_slaughter[1] == 0){
        if(model_output$prop_bact_factor[1] == 1){
          default_model_output <- model_output
        }
      }
    }
  }
}


sens_summarised <- list()
for(condemnation_B in sens_condemnation){
  production_values <- get_production_values(
    default_scenario,
    DOA = DOA,
    condemnation_base = condemnation_base,
    condemnation_B = condemnation_B,
    slaughter_price = slaughter_price,
    feed_price = feed_price,
    ab_price_per_chick = ab_price_per_chick,
    ab_price_constant = ab_price_constant,
    vaccine_price_per_chick = vaccine_price_per_chick,
    n_birds = n_birds
  )
  end_summary <- get_production_summary(production_values, scenario)
  sens_summarised[[scenario]] <- end_summary
  scenario <- scenario + 1
}

condemnation_B        <- config$R_model$condemnation_B

for(growth_factor in sens_growth_factor){
  # for this one, interpret_output needs to be run
  df <- interpret_output(default_model_output, growth_factor, production_info)
  production_values <- get_production_values(
    df,
    DOA = DOA,
    condemnation_base = condemnation_base,
    condemnation_B = condemnation_B,
    slaughter_price = slaughter_price,
    feed_price = feed_price,
    ab_price_per_chick = ab_price_per_chick,
    ab_price_constant = ab_price_constant,
    vaccine_price_per_chick = vaccine_price_per_chick,
    n_birds = n_birds
  )
  end_summary <- get_production_summary(production_values, scenario)
  sens_summarised[[scenario]] <- end_summary
  scenario <- scenario + 1
}

growth_factor <- "middle"

for(slaughter_price in sens_slaughter_price){
  production_values <- get_production_values(
    default_scenario,
    DOA = DOA,
    condemnation_base = condemnation_base,
    condemnation_B = condemnation_B,
    slaughter_price = slaughter_price,
    feed_price = feed_price,
    ab_price_per_chick = ab_price_per_chick,
    ab_price_constant = ab_price_constant,
    vaccine_price_per_chick = vaccine_price_per_chick,
    n_birds = n_birds
  )
  end_summary <- get_production_summary(production_values, scenario)
  sens_summarised[[scenario]] <- end_summary
  scenario <- scenario + 1
}

slaughter_price       <- config$R_model$slaughter_price

for(feed_price in sens_feed_price){
  production_values <- get_production_values(
    default_scenario,
    DOA = DOA,
    condemnation_base = condemnation_base,
    condemnation_B = condemnation_B,
    slaughter_price = slaughter_price,
    feed_price = feed_price,
    ab_price_per_chick = ab_price_per_chick,
    ab_price_constant = ab_price_constant,
    vaccine_price_per_chick = vaccine_price_per_chick,
    n_birds = n_birds
  )
  end_summary <- get_production_summary(production_values, scenario)
  sens_summarised[[scenario]] <- end_summary
  scenario <- scenario + 1
}

feed_price       <- config$R_model$feed_price

for(ab_price_per_chick in sens_ab_price_per_chick){
  production_values <- get_production_values(
    default_scenario,
    DOA = DOA,
    condemnation_base = condemnation_base,
    condemnation_B = condemnation_B,
    slaughter_price = slaughter_price,
    feed_price = feed_price,
    ab_price_per_chick = ab_price_per_chick,
    ab_price_constant = ab_price_constant,
    vaccine_price_per_chick = vaccine_price_per_chick,
    n_birds = n_birds
  )
  end_summary <- get_production_summary(production_values, scenario)
  sens_summarised[[scenario]] <- end_summary
  scenario <- scenario + 1
}

all_scenarios_summarised_sens2 <- bind_rows(sens_summarised)
all_scenarios_summarised_sens <- bind_rows(
  all_scenarios_summarised_sens1,
  all_scenarios_summarised_sens2
)


# save output
dir_path <- paste0(sens_output_dir, "/sensitivity_", Sys.Date())
dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

filename_summary <- file.path(dir_path, "sens_output_summarised.csv")
write.csv(all_scenarios_summarised_sens, filename_summary, row.names = FALSE)



