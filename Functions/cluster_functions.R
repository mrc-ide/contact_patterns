# MCMC_parameters <- list(iterations = 100, burnin = 30, chains = 1, cores = 1)
# model_covariates <- c("age3cat", "gender")
# income_strata_subset <- "LIC/LMIC"
# random_study_effect <- TRUE
# data <- data
# weighted <- TRUE
# adapt_delta <- 0.95

# Function to Run Negative Binomial Random Effects Model for Total Contacts Made
total_contacts <- function(MCMC_parameters, outcome_variable, model_covariates, income_strata_subset = NULL, 
                           random_study_effect = TRUE, data, weighted = FALSE, adapt_delta = 0.95) {
  
  # Checking MCMC Parameters Are Specified Correctly
  mcmc_parameter_names <- c("iterations", "burnin", "chains", "cores")
  if (sum(names(MCMC_parameters) %in% mcmc_parameter_names) != length(mcmc_parameter_names)) {
    stop("Incorrect MCMC parameters specified. Must include: iterations, burnin, chains and cores")
  }
  
  # Checking Outcome Variable Specified Correctly 
  outcome_names <- c("tot_contacts", "tot_contacts_no_add")
  if (length(outcome_variable) != 1) {
    stop("Must specify only 1 outcome variable")
  }
  if (!(outcome_variable %in% outcome_names)) {
    stop("Incorrect outcome variable specified. Must be: tot_contacts or tot_contacts_no_add")
  }
  
  # Checking Model Covariates Are Specified Correctly and Then Filtering NAs Out of Dataset and Calculating Weights
  all_covariates <- c("age3cat", "gender", "weekday", "hh_size", "student", "employment", "method", "part_age")
  for (i in seq_along(model_covariates)) {
    if (!(model_covariates[i] %in% all_covariates)) {
      stop("Incorrect covariates specified. Must be from: age3cat (or part_age), gender, weekday, hh_size, student, employment and/or method")
    }
  }
  filtered <- data 
  for (i in 1:length(model_covariates)) {
    group_var <- rlang::sym(model_covariates[i])
    temp <- filtered %>%
      filter(!is.na(!!group_var))
    filtered <- temp
  }
  data <- filtered

  # Generating Survey Weights 
  total_income_strata <- data %>%
    group_by(income) %>%
    summarise(total_income_participants = n())
  study_size <- data %>%
    group_by(income, study) %>%
    summarise(total_study_participants = n())
  number_studies <- data %>%
    group_by(income) %>%
    summarise(unique_studies = length(unique(study)))
  data <- data %>%
    left_join(total_income_strata, by = "income") %>%
    left_join(study_size, by = c("income", "study")) %>%
    left_join(number_studies, by = "income") %>%
    mutate(average_study_size = round(total_income_participants/unique_studies, 0),
           weight = average_study_size * (1/total_study_participants))
  
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
  if (weighted) {
    model_specification <- paste0(outcome_variable, "|weights(weight) ~ ")
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
  } else {
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
  }

  
  # Running the Model 
  set.seed(68120)
  obj <- brms:: brm(formula = model_specification,
                    data = input_data,
                    family = brms::negbinomial(),
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
    if (outcome_variable == "tot_contacts") {
      if (income_strata_subset == "LIC/LMIC") {
        file_name <- paste0("total_LIC_LMIC_")
      } else {
        file_name <- paste0("total_", income_strata_subset, "_")
      }
    } else {
      if (income_strata_subset == "LIC/LMIC") {
        file_name <- paste0("totalnoadd_LIC_LMIC_")
      } else {
        file_name <- paste0("totalnoadd_", income_strata_subset, "_")
      }
    }

  }
  for (i in 1:length(model_covariates)) {
    file_name <- paste0(file_name, model_covariates[i], "_")
  }
  file_name <- paste0(file_name, MCMC_parameters$iterations, "iter_", MCMC_parameters$chains, "chains_", Sys.Date())
  to_save <- list(fitting_output = obj, diagnostics = diagnostics)
  if (length(model_covariates) == 1) {
    if (weighted) {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Univariable/Weighted/weighted_", file_name, ".rds"))
    } else {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Univariable/Unweighted/", file_name, ".rds"))
    }  } else {
    if (weighted) {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Multivariable/Weighted/weighted_", file_name, ".rds"))
    } else {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Multivariable/Unweighted", file_name, ".rds"))
    }
  }
}

# Function to Run Bernoulli Random Effects Model for Whether Contact Was Physical Or Not
physical_contact <- function(MCMC_parameters, model_covariates, income_strata_subset = NULL, 
                             random_study_effect = TRUE, data, weighted = FALSE, adapt_delta = 0.95) {
  
  # Checking MCMC Parameters Are Specified Correctly
  mcmc_parameter_names <- c("iterations", "burnin", "chains", "cores")
  if (sum(names(MCMC_parameters) %in% mcmc_parameter_names) != length(mcmc_parameter_names)) {
    stop("Incorrect MCMC parameters specified. Must include: iterations, burning, chains and cores")
  }
  
  # Checking Model Covariates Are Specified Correctly and Then Filtering NAs Out of Dataset and Calculating Weights
  all_covariates <- c("age3cat", "gender", "weekday", "hh_size", "student", "employment", "method", "part_age")
  for (i in seq_along(model_covariates)) {
    if (!(model_covariates[i] %in% all_covariates)) {
      stop("Incorrect covariates specified. Must be from: age3cat, gender, weekday, hh_size, student, employment, part_age and/or method")
    }
  }
  filtered <- data 
  for (i in 1:length(model_covariates)) {
    group_var <- rlang::sym(model_covariates[i])
    temp <- filtered %>%
      filter(!is.na(!!group_var))
    filtered <- temp
  }
  data <- filtered
  
  # Filtering Out Individuals With No Physicality Data
  data$tot_phys_recorded <- data$tot_phys + data$tot_nonphys
  data$tot_phys_recorded[data$tot_phys_recorded == 0] <- NA   ### removes 0s
  data <- data %>%
    filter(!is.na(tot_phys_recorded))
  
  # Generating Survey Weights 
  total_income_strata <- data %>%
    group_by(income) %>%
    summarise(total_income_participants = n())
  study_size <- data %>%
    group_by(income, study) %>%
    summarise(total_study_participants = n())
  number_studies <- data %>%
    group_by(income) %>%
    summarise(unique_studies = length(unique(study)))
  data <- data %>%
    left_join(total_income_strata, by = "income") %>%
    left_join(study_size, by = c("income", "study")) %>%
    left_join(number_studies, by = "income") %>%
    mutate(average_study_size = round(total_income_participants/unique_studies, 0),
           weight = average_study_size * (1/total_study_participants))

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
  if (weighted) {
    model_specification <- "tot_phys|trials(tot_phys_recorded) + weights(weight) ~ "
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
  } else {
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
  }

  
  # Running the Model 
  set.seed(68120)
  obj <- brms::brm(formula = model_specification,
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
    if (weighted) {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Univariable/Weighted/weighted_", file_name, ".rds"))
    } else {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Univariable/Unweighted/", file_name, ".rds"))
    }
  } else {
    if (weighted) {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Multivariable/Weighted/weighted_", file_name, ".rds"))
    } else {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Multivariable/Unweighted/", file_name, ".rds"))
    }
  }  
}

# Function to Run Bernoulli Random Effects Model for Duration of Contact
duration_contact <- function(MCMC_parameters, model_covariates, income_strata_subset = NULL, 
                             random_study_effect = TRUE, data, weighted = FALSE, adapt_delta = 0.95) {
  
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
  filtered <- data 
  for (i in 1:length(model_covariates)) {
    group_var <- rlang::sym(model_covariates[i])
    temp <- filtered %>%
      filter(!is.na(!!group_var))
    filtered <- temp
  }
  data <- filtered
  
  # Filtering Out Individuals With No Duration Data 
  table(data$tot_dur_1hr_plus, useNA = "ifany")
  table(data$tot_dur_under_1hr, useNA = "ifany")
  sum((!is.na(data$tot_dur_1hr_plus) & data$tot_dur_1hr_plus == 0) & (!is.na(data$tot_dur_under_1hr) & data$tot_dur_under_1hr == 0))
  
  data$tot_dur_recorded <- data$tot_dur_under_1hr + data$tot_dur_1hr_plus
  data$tot_dur_recorded[data$tot_dur_recorded == 0] <- NA   ### removes 0s
  data <- data %>%
    filter(!is.na(tot_dur_recorded))
  
  # Generating Survey Weights
  total_income_strata <- data %>%
    group_by(income) %>%
    summarise(total_income_participants = n())
  study_size <- data %>%
    group_by(income, study) %>%
    summarise(total_study_participants = n())
  number_studies <- data %>%
    group_by(income) %>%
    summarise(unique_studies = length(unique(study)))
  data <- data %>%
    left_join(total_income_strata, by = "income") %>%
    left_join(study_size, by = c("income", "study")) %>%
    left_join(number_studies, by = "income") %>%
    mutate(average_study_size = round(total_income_participants/unique_studies, 0),
           weight = average_study_size * (1/total_study_participants))
  
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
  if (weighted) {
    model_specification <- "tot_dur_1hr_plus|trials(tot_dur_recorded) + weights(weight) ~ "
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
  } else {
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
  }

  # Running the Model 
  set.seed(68120)
  obj <- brms::brm(formula = model_specification,
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
    if (weighted) {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Univariable/Weighted/weighted_", file_name, ".rds"))
    } else {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Univariable/Unweighted/", file_name, ".rds"))
    }
  } else {
    if (weighted) {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Multivariable/Weighted/weighted_", file_name, ".rds"))
    } else {
      saveRDS(to_save, file = paste0("N:/Charlie/Contact_Matrix_Work/contact_patterns/Outputs/Multivariable/Unweighted", file_name, ".rds"))
    }
  }  
}
