# Basic Checks and Results Accessing for the Binomial Duration Output
example_total <- readRDS(file = "N:/Charlie/contact_matrix_work/Results/total_LIC_LMIC_hh_size_5000iter_3chains_2020-10-30.rds")
example_total$fitting_output
pairs(example_total$fitting_output, off_diag_args = list(size = 1/5, alpha = 1/5))
plot(example_total$fitting_output)
brms::conditional_effects(example_total$fitting_output)
chains <- brms::posterior_samples(example_total$fitting_output)
brms::pp_check(example_total$fitting_output, nsamples = 15)
brms::pp_check(example_total$fitting_output, type = "violin_grouped", group = "hh_size", nsamples = 10)
brms::pp_check(example_total$fitting_output, type = "error_hist", nsamples = 10)
brms::pp_check(example_total$fitting_output, type = "scatter_avg", nsamples = 100)
brms::pp_check(example_total$fitting_output, type = "stat_2d", nsamples = 100)
brms::pp_check(example_total$fitting_output, type = "rootogram", nsamples = 100)

# Basic Checks and Results Accessing for the Binomial Duration Output
example_duration <- readRDS(file = "N:/Charlie/contact_matrix_work/Results/duration_LIC_LMIC_age3cat__5000iter_3chains_2020-10-30.rds")
example_duration$fitting_output
pairs(example_duration$fitting_output, off_diag_args = list(size = 1/5, alpha = 1/5))
plot(example_duration$fitting_output)
brms::conditional_effects(example_duration$fitting_output)
chains <- brms::posterior_samples(example_duration$fitting_output)
brms::pp_check(example_duration$fitting_output)
brms::pp_check(example_duration$fitting_output, type = "violin_grouped", group = "age3cat", nsamples = 1000)

# Basic Checks and Results Accessing for the Binomial Physical Output
example_physical <- readRDS(file = "N:/Charlie/contact_matrix_work/Results/physical_LIC_LMIC_age3cat__5000iter_3chains_2020-10-30.rds")
example_physical$fitting_output
pairs(example_physical$fitting_output, off_diag_args = list(size = 1/5, alpha = 1/5))
plot(example_physical$fitting_output)
brms::conditional_effects(example_physical$fitting_output)
chains <- brms::posterior_samples(example_physical$fitting_output)
brms::pp_check(example_physical$fitting_output)

# Basic Checks and Results Accessing for the Multinomial Output
example_location <- readRDS(file = "N:/Charlie/contact_matrix_work/Results/location_LIC_LMIC_age3cat__5000iter_3chains_2020-10-31.rds")
example_location$fitting_output
pairs(example_location$fitting_output, off_diag_args = list(size = 1/5, alpha = 1/5))
plot(example_location$fitting_output)
brms::conditional_effects(example_location$fitting_output, categorical = TRUE)
chains <- brms::posterior_samples(example_location$fitting_output)

data <- example_location$fitting_output$data$age3cat
data_total <- example_location$fitting_output$data$tot_all
data_home <- example_location$fitting_output$data$y[, "tot_home"]
data_school <- example_location$fitting_output$data$y[, "tot_school"]
data_work <- example_location$fitting_output$data$y[, "tot_work"]
data_other <- example_location$fitting_output$data$y[, "tot_other"]
study <- example_location$fitting_output$data$study

names <- colnames(chains)
subset_chains <- chains[sample(dim(chains)[1], size = 500), ]
prop_matrix <- matrix(nrow = dim(subset_chains)[1], ncol = 4)
for (i in 1:dim(subset_chains)[1]) {
  pars <- subset_chains[i, ]
  ind_matrix <- matrix(nrow = length(data), ncol = 4)
  for (j in 1:length(data)) {
    study_name <- study[j]
    study_spec_covs <- names[grep(study_name, names)]
    if (data[j] == 1) {
      school <- pars$b_mutotschool_Intercept + pars[, study_spec_covs[1]]
      work <- pars$b_mutotwork_Intercept + pars[, study_spec_covs[2]]
      other <- pars$b_mutotother_Intercept + pars[, study_spec_covs[3]]
    } else {
      age3cat_covs <- names[grep(paste0("age3cat", data[j]), names)]
      school <- pars$b_mutotschool_Intercept + pars[, age3cat_covs[1]] + pars[, study_spec_covs[1]]
      work <- pars$b_mutotwork_Intercept + pars[, age3cat_covs[2]] + pars[, study_spec_covs[2]]
      other <- pars$b_mutotother_Intercept + pars[, age3cat_covs[3]] + pars[, study_spec_covs[3]]
    }
    school_prob_init <- exp(school)
    work_prob_init <- exp(work)
    other_prob_init <- exp(other)
    total_prob <- school_prob_init + work_prob_init + other_prob_init
    
    school_prob <- school_prob_init/(1 + total_prob)
    work_prob <- work_prob_init/(1 + total_prob)
    other_prob <- other_prob_init/(1 + total_prob)
    home_prob <- 1 - (school_prob + work_prob + other_prob)
    
    x <- rmultinom(n = 1, size = data_total[j], prob = c(home_prob, school_prob, work_prob, other_prob))
    ind_matrix[j, ] <- t(x)/sum(x)

  }
  
  prop_matrix[i, ] <- apply(ind_matrix, 2, mean)
  
  if(i %% 100 == 0) {
    print(i)
  }
  
}

empirical_prop <- matrix(nrow = length(example_location$fitting_output$data$tot_all), ncol = 4)
for (i in 1:length(example_location$fitting_output$data$tot_all)) {
  empirical_prop[i, ] <- example_location$fitting_output$data$y[i, ]/example_location$fitting_output$data$tot_all[i]
}
apply(empirical_prop, 2, mean)
apply(prop_matrix[1:500, ], 2, mean)

