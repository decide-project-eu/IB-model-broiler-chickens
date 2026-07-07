# Validate that a config entry is a single non-missing value of the expected
# type before more specific checks run.
CheckScalar <- function(value, name, mode = c("numeric", "integer", "character", "logical")) {
  mode <- match.arg(mode)
  if (length(value) != 1 || is.list(value) || any(is.na(value))) {
    stop(sprintf("Config field '%s' must be a scalar %s.", name, mode))
  }
  
  if (mode == "numeric" && !is.numeric(value)) {
    stop(sprintf("Config field '%s' must be numeric.", name))
  }
  if (mode == "integer" && (!is.numeric(value) || abs(value - round(value)) > .Machine$double.eps^0.5)) {
    stop(sprintf("Config field '%s' must be an integer.", name))
  }
  if (mode == "character" && (!is.character(value) || !nzchar(value))) {
    stop(sprintf("Config field '%s' must be a non-empty string.", name))
  }
  if (mode == "logical" && !is.logical(value)) {
    stop(sprintf("Config field '%s' must be TRUE or FALSE.", name))
  }
  
  invisible(TRUE)
}

# Enforce numeric bounds after type validation so error messages stay focused on
# the first invalid assumption.
CheckRange <- function(value, name, min_value = NULL, max_value = NULL) {
  if (!is.null(min_value) && value < min_value) {
    stop(sprintf("Config field '%s' must be >= %s.", name, min_value))
  }
  if (!is.null(max_value) && value > max_value) {
    stop(sprintf("Config field '%s' must be <= %s.", name, max_value))
  }
  
  invisible(TRUE)
}

# Validate that a config entry is a list or vector of non-missing value of the expected
# type before more specific checks run. Convert lists into vectors.
CheckVector <- function(value, name, mode = c("numeric", "integer", "character", "logical"),
                        min_length = 1, allow_na = FALSE) {
  mode <- match.arg(mode)
  
  # Convert YAML list → atomic vector
  if (is.list(value)) {
    value <- unlist(value, use.names = FALSE)
  }
  
  if (length(value) < min_length) {
    stop(sprintf("Config field '%s' must have at least %d elements.", name, min_length))
  }
  
  if (!allow_na && any(is.na(value))) {
    stop(sprintf("Config field '%s' must not contain NA values.", name))
  }
  
  if (mode == "numeric" && !is.numeric(value)) {
    stop(sprintf("Config field '%s' must be numeric.", name))
  }
  if (mode == "integer" && (!is.numeric(value) || any(abs(value - round(value)) > .Machine$double.eps^0.5))) {
    stop(sprintf("Config field '%s' must contain integers.", name))
  }
  if (mode == "character" && !is.character(value)) {
    stop(sprintf("Config field '%s' must be character.", name))
  }
  if (mode == "logical" && !is.logical(value)) {
    stop(sprintf("Config field '%s' must be logical.", name))
  }
  
  # return a vector instead of a list
  return(value)
}

ReadAndValidateConfig <- function(config_path) {
  config <- yaml::read_yaml(config_path)
  
  required_sections <- c("project", "R_model", "R_sensitivity")
  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0) {
    stop("Missing config sections: ", paste(missing_sections, collapse = ", "))
  }
  
  CheckScalar(config$project$input_dir, "project.input_dir", "character")
  CheckScalar(config$project$output_dir, "project.output_dir", "character")
  CheckScalar(config$project$sens_input_dir, "project.sens_input_dir", "character")
  CheckScalar(config$project$sens_output_dir, "project.sens_output_dir", "character")
  
  CheckRange(config$R_model$DOA, "R_model.DOA", max_value = 1)
  CheckRange(config$R_model$condemnation_base, "R_model.condemnation_base", max_value = 1)
  CheckRange(config$R_model$condemnation_B, "R_model.condemnation_B", max_value = 1)
  CheckScalar(config$R_model$slaughter_price, "R_model.slaughter_price", "numeric")
  CheckScalar(config$R_model$feed_price, "R_model.feed_price", "numeric")
  CheckScalar(config$R_model$ab_price_per_chick, "R_model.ab_price_per_chick", "numeric")
  CheckScalar(config$R_model$ab_price_constant, "R_model.ab_price_constant", "numeric")
  CheckScalar(config$R_model$growth_factor, "R_model.growth_factor", "character")
  CheckRange(config$R_model$DG_change_IB, "R_model.DG_change_IB", max_value = 0)
  CheckRange(config$R_model$DG_change_B, "R_model.DG_change_B", max_value = 0)
  CheckRange(config$R_model$FCR_change_IB, "R_model.FCR_change_IB", min_value = 0)
  CheckRange(config$R_model$FCR_change_B, "R_model.FCR_change_B", min_value = 0)
  
  CheckVector(config$R_sensitivity$sens_growth_factor,
              "R_sensitivity.sens_growth_factor", "character", min_length = 1)
  CheckVector(config$R_sensitivity$sens_condemnation,
              "R_sensitivity.sens_condemnation", "numeric", min_length = 1)
  
  config
}