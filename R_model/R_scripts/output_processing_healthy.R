# ------------------------
# Before starting
# ------------------------

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

healthy_input         <- config$project$healthy_input
output_dir            <- config$project$healthy_dir

# treated_days is always 3 because this is used for the days when treatment
# is allowed in the EMULSION model.


# ------------------------------------------------------
# EMULSION output: healthy scenario
# ------------------------------------------------------

df <- read.csv(healthy_input)
df$scenario <- 'healthy'
df <- interpret_output(df, growth_factor, production_info)
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
end_summary <- get_production_summary(production_values, 'healthy')

# save output
write.csv(df, "R_model/data/healthy_scenario/output_all_days.csv", row.names = FALSE)
write.csv(production_values, "R_model/data/healthy_scenario/output.csv", row.names = FALSE)
write.csv(end_summary, "R_model/data/healthy_scenario/output_summarised.csv", row.names = FALSE)


# ! note: daily mortality is one day late.
