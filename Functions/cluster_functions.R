# MCMC_parameters
# outcome_variable <- "tot_all"
# model_covariates <- "method"
# income_strata_subset <- "LIC/LMIC"
# random_study_effect <- TRUE

# Function to Run Negative Binomial Random Effects Model for Total Contacts Made
total_contacts <- function(MCMC_parameters, outcome_variable, model_covariates, income_strata_subset = NULL, 
                           random_study_effect = TRUE, data, adapt_delta = 0.9) {
  
  # Checking MCMC Parameters Are Specified Correctly
  mcmc_parameter_names <- c("iterations", "burnin", "chains", "cores")
  if (sum(names(MCMC_parameters) %in% mcmc_parameter_names) != length(mcmc_parameter_names)) {
    stop("Incorrect MCMC parameters specified. Must include: iterations, burnin, chains and cores")
  }
  
  # Checking Outcome Variable Specified Correctly 
  outcome_names <- c("tot_all_inc_miss", "tot_all", "tot_home", "tot_work", "tot_school", "tot_other")
  if (length(outcome_variable) != 1) {
    stop("Must specify only 1 outcome variable")
  }
  if (!(outcome_variable %in% outcome_names)) {
    stop("Incorrect outcome variable specified. Must be: tot_all_inc_miss, tot_all, tot_home, tot_work, tot_school or tot_other")
  }
  
  # Checking Model Covariates Are Specified Correctly
  all_covariates <- c("age3cat", "gender", "weekday", "hh_size", "student", "employment", "method", "part_age")
  for (i in seq_along(model_covariates)) {
    if (!(model_covariates[i] %in% all_covariates)) {
      stop("Incorrect covariates specified. Must be from: age3cat, gender, weekday, hh_size, student, employment, part_age and/or method")
    }
  }
  
  # Checking Subset Parameters Are Specified Correctly
  all_subsets <- c("LIC/LMIC", "UMIC", "HIC")
  if (!is.null(income_strata_subset)) {
    for (i in seq_along(income_strata_subset)) {
      if(!(income_strata_subset[i] %in% all_subsets)) {
        stop("Incorrect subset parameters specified. Must be from: LIC/LMIC, UMIC and/or HIC.")
      }
    }
  }

  # Preparing and Subsetting Data
  if (length(income_strata_subset) == 1) {
    input_data <- data %>%  
      filter(income == income_strata_subset)
  } else if (length(income_strata_subset) == 2) {
    input_data <- data %>%  
      filter(income == income_strata_subset[1] | income == income_strata_subset[2])
  } else if (length(income_strata_subset) == 3) {
    input_data <- data %>%  
      filter(income == income_strata_subset[1] | income == income_strata_subset[2] | income == income_strata_subset[3])
  } else if (is.null(income_strata_subset)) {
    input_data <- data
  }

  # Specifying the Model for BRMS Based On Functions Inputs 
  model_specification <- paste0(outcome_variable, " ~ ")
  for (i in 1:length(model_covariates)) {
    if (i != length(model_covariates)) {
      model_specification <- paste(model_specification, model_covariates[i], " + ") 
    } else {
      model_specification <- paste0(model_specification, model_covariates[i], " ")
      if (random_study_effect) {
        model_specification <- paste0(model_specification, "+ (1|study)")
      }
    }
  }
  
  # Running the Model 
  set.seed(68120)
  obj <- brms:: brm(formula = model_specification,
                    data = input_data,
                    family = negbinomial(),
                    iter = MCMC_parameters$iterations,
                    warmup = MCMC_parameters$burnin,
                    chains = MCMC_parameters$chains,
                    cores = MCMC_parameters$cores,
                    control = list(adapt_delta = adapt_delta), 
                    refresh = 0)
  
  # Collating Model Diagnostics
  nuts_output <- nuts_params(obj)
  divergent <- sum(nuts_output[nuts_output$Parameter == "divergent__", "Value"] == 1)
  rhat <- rhat(obj)
  neff_ratio <- neff_ratio(obj)
  diagnostics <- list(overall_nuts_output = nuts_output, divergent = divergent, rhat = rhat, neff_ratio = neff_ratio)

  # Saving the Model Output
  if (is.null(income_strata_subset)) {
    file_name <- "total_"
  } else {
    if (income_strata_subset == "LIC/LMIC") {
      file_name <- paste0("total_LIC_LMIC_")
    } else {
      file_name <- paste0("total_", income_strata_subset, "_")
    }
  }
  for (i in 1:length(model_covariates)) {
    file_name <- paste0(file_name, model_covariates[i], "_")
  }
  file_name <- paste0(file_name, MCMC_parameters$iterations, "iter_", MCMC_parameters$chains, "chains_", Sys.Date())
  to_save <- list(fitting_output = obj, diagnostics = diagnostics)
  if (length(model_covariates) == 1) {
    saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Univariate/", file_name, ".rds"))
  } else {
    saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Multivariate/", file_name, ".rds"))
  }
}

# Function to Run Multinomial Random Effects Model for Proportion of Contacts Made In Each Location
location_contact <- function(MCMC_parameters, model_covariates, income_strata_subset = NULL, random_study_effect = TRUE, data, adapt_delta = 0.9) {
  
  # Checking MCMC Parameters Are Specified Correctly
  mcmc_parameter_names <- c("iterations", "burnin", "chains", "cores")
  if (sum(names(MCMC_parameters) %in% mcmc_parameter_names) != length(mcmc_parameter_names)) {
    stop("Incorrect MCMC parameters specified. Must include: iterations, burning, chains and cores")
  }
  
  # Checking Model Covariates Are Specified Correctly
  all_covariates <- c("age3cat", "gender", "weekday", "hh_size", "student", "employment", "method", "part_age")
  for (i in seq_along(model_covariates)) {
    if (!(model_covariates[i] %in% all_covariates)) {
      stop("Incorrect covariates specified. Must be from: age3cat, gender, weekday, hh_size, student, employment, part_age and/or method")
    }
  }
  
  # Checking Subset Parameters Are Specified Correctly
  all_subsets <- c("LIC/LMIC", "UMIC", "HIC")
  if (!is.null(income_strata_subset)) {
    for (i in seq_along(income_strata_subset)) {
      if(!(income_strata_subset[i] %in% all_subsets)) {
        stop("Incorrect subset parameters specified. Must be from: LIC/LMIC, UMIC and/or HIC.")
      }
    }
  }
  
  # Preparing and Subsetting Data
  if (length(income_strata_subset) == 1) {
    input_data <- data %>%  
      filter(income == income_strata_subset)
  } else if (length(income_strata_subset) == 2) {
    input_data <- data %>%  
      filter(income == income_strata_subset[1] | income == income_strata_subset[2])
  } else if (length(income_strata_subset) == 3) {
    input_data <- data %>%  
      filter(income == income_strata_subset[1] | income == income_strata_subset[2] | income == income_strata_subset[3])
  } else if (is.null(income_strata_subset)) {
    input_data <- data
  }
  input_data$y <- with(input_data, cbind(tot_home, tot_school, tot_work, tot_other))
  input_data$tot_all[input_data$tot_all == 0] <- NA   ### removes 0s
  
  # Specifying the Model for BRMS Based On Functions Inputs 
  model_specification <- "y|trials(tot_all) ~ "
  for (i in 1:length(model_covariates)) {
    if (i != length(model_covariates)) {
      model_specification <- paste(model_specification, model_covariates[i], " + ") 
    } else {
      model_specification <- paste0(model_specification, model_covariates[i], " ")
      if (random_study_effect) {
        model_specification <- paste0(model_specification, "+ (1|study)")
      }
    }
  }  

  # Running the Model 
  set.seed(68120)
  obj <- brm(formula = model_specification,
             data = input_data,
             family = multinomial(),
             iter = MCMC_parameters$iterations,
             warmup = MCMC_parameters$burnin,
             chains = MCMC_parameters$chains,
             cores = MCMC_parameters$cores,
             control = list(adapt_delta = adapt_delta), 
             refresh = 0)
  
  # Collating Model Diagnostics
  nuts_output <- nuts_params(obj)
  divergent <- sum(nuts_output[nuts_output$Parameter == "divergent__", "Value"] == 1)
  rhat <- rhat(obj)
  neff_ratio <- neff_ratio(obj)
  diagnostics <- list(overall_nuts_output = nuts_output, divergent = divergent, rhat = rhat, neff_ratio = neff_ratio)
  
  # Saving the Model Output 
  if (is.null(income_strata_subset)) {
    file_name <- "location_"
  } else {
    if (income_strata_subset == "LIC/LMIC") {
      file_name <- paste0("location_LIC_LMIC_")
    } else {
      file_name <- paste0("location_", income_strata_subset, "_")
    }
  }
  for (i in 1:length(model_covariates)) {
    file_name <- paste0(file_name, model_covariates[i], "_")
  }
  to_save <- list(fitting_output = obj, diagnostics = diagnostics)
  file_name <- paste0(file_name, MCMC_parameters$iterations, "iter_", MCMC_parameters$chains, "chains_", Sys.Date())
  if (length(model_covariates) == 1) {
    saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Univariate/", file_name, ".rds"))
  } else {
    saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Multivariate/", file_name, ".rds"))
  }
}


# Function to Run Bernoulli Random Effects Model for Whether Contact Was Physical Or Not
physical_contact <- function(MCMC_parameters, model_covariates, income_strata_subset = NULL, random_study_effect = TRUE, data, adapt_delta = 0.9) {
  
  # Checking MCMC Parameters Are Specified Correctly
  mcmc_parameter_names <- c("iterations", "burnin", "chains", "cores")
  if (sum(names(MCMC_parameters) %in% mcmc_parameter_names) != length(mcmc_parameter_names)) {
    stop("Incorrect MCMC parameters specified. Must include: iterations, burning, chains and cores")
  }
  
  # Checking Model Covariates Are Specified Correctly
  all_covariates <- c("age3cat", "gender", "weekday", "hh_size", "student", "employment", "method", "part_age")
  for (i in seq_along(model_covariates)) {
    if (!(model_covariates[i] %in% all_covariates)) {
      stop("Incorrect covariates specified. Must be from: age3cat, gender, weekday, hh_size, student, employment, part_age and/or method")
    }
  }
  
  # Checking Subset Parameters Are Specified Correctly
  all_subsets <- c("LIC/LMIC", "UMIC", "HIC")
  if (!is.null(income_strata_subset)) {
    for (i in seq_along(income_strata_subset)) {
      if(!(income_strata_subset[i] %in% all_subsets)) {
        stop("Incorrect subset parameters specified. Must be from: LIC/LMIC, UMIC and/or HIC.")
      }
    }
  }
  
  # Preparing and Subsetting Data
  if (length(income_strata_subset) == 1) {
    input_data <- data %>%  
      filter(income == income_strata_subset)
  } else if (length(income_strata_subset) == 2) {
    input_data <- data %>%  
      filter(income == income_strata_subset[1] | income == income_strata_subset[2])
  } else if (length(income_strata_subset) == 3) {
    input_data <- data %>%  
      filter(income == income_strata_subset[1] | income == income_strata_subset[2] | income == income_strata_subset[3])
  } else if (is.null(income_strata_subset)) {
    input_data <- data
  }
  input_data$tot_phys_recorded <- input_data$tot_phys + input_data$tot_nonphys
  input_data$tot_phys_recorded[input_data$tot_phys_recorded == 0] <- NA   ### removes 0s
  
  # Specifying the Model for BRMS Based On Functions Inputs 
  model_specification <- "tot_phys|trials(tot_phys_recorded) ~ "
  for (i in 1:length(model_covariates)) {
    if (i != length(model_covariates)) {
      model_specification <- paste(model_specification, model_covariates[i], " + ") 
    } else {
      model_specification <- paste0(model_specification, model_covariates[i], " ")
      if (random_study_effect) {
        model_specification <- paste0(model_specification, "+ (1|study)")
      }
    }
  }  
  
  # Running the Model 
  set.seed(68120)
  obj <- brm(formula = model_specification,
             data = input_data,
             family = binomial(),
             iter = MCMC_parameters$iterations,
             warmup = MCMC_parameters$burnin,
             chains = MCMC_parameters$chains,
             cores = MCMC_parameters$cores,
             control = list(adapt_delta = adapt_delta),
             refresh = 0)
  
  # Collating Model Diagnostics
  nuts_output <- nuts_params(obj)
  divergent <- sum(nuts_output[nuts_output$Parameter == "divergent__", "Value"] == 1)
  rhat <- rhat(obj)
  neff_ratio <- neff_ratio(obj)
  diagnostics <- list(overall_nuts_output = nuts_output, divergent = divergent, rhat = rhat, neff_ratio = neff_ratio)
  
  # Saving the Model Output 
  if (is.null(income_strata_subset)) {
    file_name <- "physical_"
  } else {
    if (income_strata_subset == "LIC/LMIC") {
      file_name <- paste0("physical_LIC_LMIC_")
    } else {
      file_name <- paste0("physical_", income_strata_subset, "_")
    }
  }
  for (i in 1:length(model_covariates)) {
    file_name <- paste0(file_name, model_covariates[i], "_")
  }
  to_save <- list(fitting_output = obj, diagnostics = diagnostics)
  file_name <- paste0(file_name, MCMC_parameters$iterations, "iter_", MCMC_parameters$chains, "chains_", Sys.Date())
  if (length(model_covariates) == 1) {
    saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Univariate/", file_name, ".rds"))
  } else {
    saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Multivariate/", file_name, ".rds"))
  }  
}

# Function to Run Bernoulli Random Effects Model for Duration of Contact
duration_contact <- function(MCMC_parameters, model_covariates, income_strata_subset = NULL, random_study_effect = TRUE, data, adapt_delta = 0.9) {
  
  # Checking MCMC Parameters Are Specified Correctly
  mcmc_parameter_names <- c("iterations", "burnin", "chains", "cores")
  if (sum(names(MCMC_parameters) %in% mcmc_parameter_names) != length(mcmc_parameter_names)) {
    stop("Incorrect MCMC parameters specified. Must include: iterations, burning, chains and cores")
  }
  
  # Checking Model Covariates Are Specified Correctly
  all_covariates <- c("age3cat", "gender", "weekday", "hh_size", "student", "employment", "method", "part_age")
  for (i in seq_along(model_covariates)) {
    if (!(model_covariates[i] %in% all_covariates)) {
      stop("Incorrect covariates specified. Must be from: age3cat, gender, weekday, hh_size, student, employment, part_age and/or method")
    }
  }
  
  # Checking Subset Parameters Are Specified Correctly
  all_subsets <- c("LIC/LMIC", "UMIC", "HIC")
  if (!is.null(income_strata_subset)) {
    for (i in seq_along(income_strata_subset)) {
      if(!(income_strata_subset[i] %in% all_subsets)) {
        stop("Incorrect subset parameters specified. Must be from: LIC/LMIC, UMIC and/or HIC.")
      }
    }
  }
  
  # Preparing and Subsetting Data
  if (length(income_strata_subset) == 1) {
    input_data <- data %>%  
      filter(income == income_strata_subset)
  } else if (length(income_strata_subset) == 2) {
    input_data <- data %>%  
      filter(income == income_strata_subset[1] | income == income_strata_subset[2])
  } else if (length(income_strata_subset) == 3) {
    input_data <- data %>%  
      filter(income == income_strata_subset[1] | income == income_strata_subset[2] | income == income_strata_subset[3])
  } else if (is.null(income_strata_subset)) {
    input_data <- data
  }
  input_data$tot_dur_recorded <- input_data$tot_dur_under_1hr + input_data$tot_dur_1hr_plus
  input_data$tot_dur_recorded[input_data$tot_dur_recorded == 0] <- NA   ### removes 0s
  
  # Specifying the Model for BRMS Based On Functions Inputs 
  model_specification <- "tot_dur_1hr_plus|trials(tot_dur_recorded) ~ "
  for (i in 1:length(model_covariates)) {
    if (i != length(model_covariates)) {
      model_specification <- paste(model_specification, model_covariates[i], " + ") 
    } else {
      model_specification <- paste0(model_specification, model_covariates[i], " ")
      if (random_study_effect) {
        model_specification <- paste0(model_specification, "+ (1|study)")
      }
    }
  }  

  # Running the Model 
  set.seed(68120)
  obj <- brm(formula = model_specification,
             data = input_data,
             family = binomial(),
             iter = MCMC_parameters$iterations,
             warmup = MCMC_parameters$burnin,
             chains = MCMC_parameters$chains,
             cores = MCMC_parameters$cores,
             control = list(adapt_delta = adapt_delta),
             refresh = 0)
  
  # Collating Model Diagnostics
  nuts_output <- nuts_params(obj)
  divergent <- sum(nuts_output[nuts_output$Parameter == "divergent__", "Value"] == 1)
  rhat <- rhat(obj)
  neff_ratio <- neff_ratio(obj)
  diagnostics <- list(overall_nuts_output = nuts_output, divergent = divergent, rhat = rhat, neff_ratio = neff_ratio)
  
  # Saving the Model Output 
  if (is.null(income_strata_subset)) {
    file_name <- "duration_"
  } else {
    if (income_strata_subset == "LIC/LMIC") {
      file_name <- paste0("duration_LIC_LMIC_")
    } else {
      file_name <- paste0("duration_", income_strata_subset, "_")
    }
  }
  for (i in 1:length(model_covariates)) {
    file_name <- paste0(file_name, model_covariates[i], "_")
  }
  to_save <- list(fitting_output = obj, diagnostics = diagnostics)
  file_name <- paste0(file_name, MCMC_parameters$iterations, "iter_", MCMC_parameters$chains, "chains_", Sys.Date())
  if (length(model_covariates) == 1) {
    saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Univariate/", file_name, ".rds"))
  } else {
    saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Multivariate/", file_name, ".rds"))
  }  
}
