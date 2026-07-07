library(dplyr)
library(readxl)
setwd("C:/Users/4243692/models/ibv-model")

source('R_model/R_scripts/config_functions.R')

# ------------------------------------------------------
# PARAMETERS - change in config.yaml
# ------------------------------------------------------

config <- ReadAndValidateConfig("R_model/config/config.yaml")

DG_change_IB <- config$R_model$DG_change_IB
DG_change_B <- config$R_model$DG_change_B
FCR_change_IB <- config$R_model$FCR_change_IB
FCR_change_B <- config$R_model$FCR_change_B

lower_DG_IB <- 1 + DG_change_IB  # value is negative
lower_DG_B <- 1 + DG_change_B
higher_FCR_IB <- 1 + FCR_change_IB # this is without taking mortality into account, to calculate feed intake
higher_FCR_B <- 1 + FCR_change_B

# values for sensitivity analysis
sens_growth_factor <- config$R_sensitivity$sens_growth_factor

lower_DG_IB_low <- 1 + sens_growth_factor[[1]] * DG_change_IB
lower_DG_IB_high <- 1 + sens_growth_factor[[2]] * DG_change_IB
lower_DG_B_low <- 1 + sens_growth_factor[[1]] * DG_change_B
lower_DG_B_high <- 1 + sens_growth_factor[[2]] * DG_change_B
higher_FCR_IB_low <- 1 + sens_growth_factor[[1]] * FCR_change_IB
higher_FCR_IB_high <- 1 + sens_growth_factor[[2]] * FCR_change_IB
higher_FCR_B_low <- 1 + sens_growth_factor[[1]] * FCR_change_B
higher_FCR_B_high <- 1 + sens_growth_factor[[2]] * FCR_change_B

# Use DG and FCR to calculate FI
lower_FI_IB <- lower_DG_IB * higher_FCR_IB
lower_FI_B <- lower_DG_B * higher_FCR_B
lower_FI_IB_low <- lower_DG_IB_low * higher_FCR_IB_low
lower_FI_B_low <- lower_DG_B_low * higher_FCR_B_low
lower_FI_IB_high <- lower_DG_IB_high * higher_FCR_IB_high
lower_FI_B_high <- lower_DG_B_high * higher_FCR_B_high


#------------------------
# Production values
# ------------------------

healthy_production <- read_excel("R_model/data/healthy_production.xlsx")

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

production_values <- healthy_production %>%
  mutate(
    # DG and FCR of diseased animals
    DG_IB = ifelse(day == 0, 44, lower_DG_IB * DG_healthy),
    DG_B = ifelse(day == 0, 44, lower_DG_B * DG_healthy),
    FI_IB = lower_FI_IB * FI_healthy,
    FI_B = lower_FI_B * FI_healthy,
    
    # for sensitivity analysis
    DG_IB_low = ifelse(day == 0, 44, lower_DG_IB_low * DG_healthy),
    DG_B_low = ifelse(day == 0, 44, lower_DG_B_low * DG_healthy),
    DG_IB_high = ifelse(day == 0, 44, lower_DG_IB_high * DG_healthy),
    DG_B_high = ifelse(day == 0, 44, lower_DG_B_high * DG_healthy),
    FI_IB_low = lower_FI_IB_low * FI_healthy,
    FI_B_low = lower_FI_B_low * FI_healthy,
    FI_IB_high = lower_FI_IB_high * FI_healthy,
    FI_B_high = lower_FI_B_high * FI_healthy
  )

write.csv(production_values, "R_model/data/production_values.csv", row.names = FALSE)
