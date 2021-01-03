# Load Required Libaries
library(tidyverse)

# Setting Up Cluster - Creating a Context and Configuring the Queue
setwd("N:/")
loc <- didehpc::path_mapping("location", "N:", "//fi--didenas5/malaria", "N:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster = "fi--didemrchnb", parallel = FALSE, rtools = TRUE, cores = 2)
packages <- c("rstan", "brms", "tidyverse", "gdata")
sources <- c("N:/Charlie/contact_matrix_work/cluster_functions.R")
context_name <- paste0("N:/Contact_Matrix_Work/STAN_Tester/context_", Sys.Date())
ctx <- context::context_save(path = context_name, sources = sources, packages = packages)
run <- didehpc::queue_didehpc(ctx, config = config)

# Loading In Contact Survey Data
data <- read.csv("N:/Charlie/contact_matrix_work/combined_participant_lvl_all.csv") %>%
  mutate(tot_all_inc_miss = as.numeric(tot_all_inc_miss),
         age3cat = as.factor(age3cat),
         hh_size = ifelse(hh_size == "6+", 6, hh_size),
         hh_size = as.numeric(hh_size),
         hh_size = case_when(hh_size == 1 ~ "1", hh_size == 2 ~ "2",
                             hh_size == 3 ~ "3", hh_size == 4 ~ "4", 
                             hh_size == 5 ~ "5", hh_size >= 6 ~ "6+", TRUE ~ NA_character_),
         hh_size = as.factor(hh_size),
         tot_all = as.numeric(tot_all),
         tot_home = as.numeric(tot_home),
         tot_other = as.numeric(tot_other),
         tot_school = as.numeric(tot_school),
         tot_work = as.numeric(tot_work),
         gender = case_when(part_gender == "Female" ~ 0, part_gender == "Male" ~ 1, TRUE ~ NA_real_),
         employment = as.factor(employment),
         student = as.factor(student),
         study = as.factor(study),
         income = factor(income, levels = c("LIC/LMIC", "UMIC", "HIC")),
         weekday = as.factor(weekday))

table(data$gender, data$part_gender, useNA = "ifany") # fine
table(data$age3cat, useNA = "ifany") # fine
table(data$weekday, data$dayofweek, useNA = "ifany") # fine
table(data$hh_size, useNA = "ifany") # fine (consider going to hh_size 10+ and then excluding the 42 people living hh_size "6+")
table(data$student, useNA = "ifany") # fine
table(data$age3cat, data$student, useNA = "ifany") # additional check on student, fine
table(data$employment, useNA = "ifany") # fine
table(data$employment, data$age3cat, useNA = "ifany") # additional check on employment, fine 
table(data$method, useNA = "ifany") # need to put the "Online" into one of "Diary" or "Interview"
table(data$study, useNA = "ifany") # fine

table(data$study, data$age3cat, useNA = "ifany")
table(data$study, data$gender, useNA = "ifany")
table(data$study, data$weekday, useNA = "ifany")
table(data$study, data$hh_size, useNA = "ifany")
table(data$study, data$student, useNA = "ifany")
table(data$study, data$employment, useNA = "ifany")
table(data$study, data$method, useNA = "ifany")


# Summary of all available clusters and checking various tasks
run$cluster_load(nodes = FALSE)
run$task_list()
run$task_times()

# MCMC Parameters
MCMC_parameters <- list(iterations = 5000, burnin = 2500, chains = 3, cores = 3)

# Fitting Total Contacts Made 
total_LIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "age3cat", "LIC/LMIC", TRUE, data))
total_LIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "gender", "LIC/LMIC", TRUE, data))
total_LIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "weekday", "LIC/LMIC", TRUE, data))
total_LIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "hh_size", "LIC/LMIC", TRUE, data))
total_LIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "student", "LIC/LMIC", TRUE, data))
total_LIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "employment", "LIC/LMIC", TRUE, data))
total_LIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "method", "LIC/LMIC", TRUE, data))

total_UMIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "age3cat", "UMIC", TRUE, data))
total_UMIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "gender", "UMIC", TRUE, data))
total_UMIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "weekday", "UMIC", TRUE, data))
total_UMIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "hh_size", "UMIC", TRUE, data))
total_UMIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "student", "UMIC", TRUE, data))
total_UMIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "employment", "UMIC", TRUE, data))
total_UMIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "method", "UMIC", TRUE, data))

total_HIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "age3cat", "HIC", TRUE, data))
total_HIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "gender", "HIC", TRUE, data))
total_HIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "weekday", "HIC", TRUE, data))
total_HIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "hh_size", "HIC", TRUE, data))
total_HIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "student", "HIC", TRUE, data))
total_HIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "employment", "HIC", TRUE, data))
total_HIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all_inc_miss", "method", "HIC", TRUE, data))

# Fitting Location of Contacts
location_LIC_age <- run$enqueue(location_contact(MCMC_parameters, "age3cat", "LIC/LMIC", TRUE, data))
location_LIC_gender <- run$enqueue(location_contact(MCMC_parameters, "gender", "LIC/LMIC", TRUE, data))
location_LIC_weekday <- run$enqueue(location_contact(MCMC_parameters, "weekday", "LIC/LMIC", TRUE, data))
location_LIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, "hh_size", "LIC/LMIC", TRUE, data))
location_LIC_student <- run$enqueue(location_contact(MCMC_parameters, "student", "LIC/LMIC", TRUE, data))
location_LIC_employment <- run$enqueue(location_contact(MCMC_parameters, "employment", "LIC/LMIC", TRUE, data))
location_LIC_method <- run$enqueue(location_contact(MCMC_parameters, "method", "LIC/LMIC", TRUE, data))

location_UMIC_age <- run$enqueue(location_contact(MCMC_parameters, "age3cat", "UMIC", TRUE, data))
location_UMIC_gender <- run$enqueue(location_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
location_UMIC_weekday <- run$enqueue(location_contact(MCMC_parameters, "weekday", "UMIC", TRUE, data))
location_UMIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, "hh_size", "UMIC", TRUE, data))
location_UMIC_student <- run$enqueue(location_contact(MCMC_parameters, "student", "UMIC", TRUE, data))
location_UMIC_employment <- run$enqueue(location_contact(MCMC_parameters, "employment", "UMIC", TRUE, data))
location_UMIC_method <- run$enqueue(location_contact(MCMC_parameters, "method", "UMIC", TRUE, data))

location_HIC_age <- run$enqueue(location_contact(MCMC_parameters, "age3cat", "HIC", TRUE, data))
location_HIC_gender <- run$enqueue(location_contact(MCMC_parameters, "gender", "HIC", TRUE, data))
location_HIC_weekday <- run$enqueue(location_contact(MCMC_parameters, "weekday", "HIC", TRUE, data))
location_HIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, "hh_size", "HIC", TRUE, data))
location_HIC_student <- run$enqueue(location_contact(MCMC_parameters, "student", "HIC", TRUE, data))
location_HIC_employment <- run$enqueue(location_contact(MCMC_parameters, "employment", "HIC", TRUE, data))
location_HIC_method <- run$enqueue(location_contact(MCMC_parameters, "method", "HIC", TRUE, data)) # only has 1 level (diary) so not possible to do

# Fitting Whether Contacts Were Physical or Not
physical_LIC_age <- run$enqueue(physical_contact(MCMC_parameters, "age3cat", "LIC/LMIC", TRUE, data))
physical_LIC_gender <- run$enqueue(physical_contact(MCMC_parameters, "gender", "LIC/LMIC", TRUE, data))
physical_LIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, "weekday", "LIC/LMIC", TRUE, data))
physical_LIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, "hh_size", "LIC/LMIC", TRUE, data))
physical_LIC_student <- run$enqueue(physical_contact(MCMC_parameters, "student", "LIC/LMIC", TRUE, data))
physical_LIC_employment <- run$enqueue(physical_contact(MCMC_parameters, "employment", "LIC/LMIC", TRUE, data))
physical_LIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "LIC/LMIC", TRUE, data))

physical_UMIC_age <- run$enqueue(physical_contact(MCMC_parameters, "age3cat", "UMIC", TRUE, data))
physical_UMIC_gender <- run$enqueue(physical_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
physical_UMIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, "weekday", "UMIC", TRUE, data))
physical_UMIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, "hh_size", "UMIC", TRUE, data))
physical_UMIC_student <- run$enqueue(physical_contact(MCMC_parameters, "student", "UMIC", TRUE, data))
physical_UMIC_employment <- run$enqueue(physical_contact(MCMC_parameters, "employment", "UMIC", TRUE, data))
physical_UMIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "UMIC", TRUE, data))

physical_HIC_age <- run$enqueue(physical_contact(MCMC_parameters, "age3cat", "HIC", TRUE, data))
physical_HIC_gender <- run$enqueue(physical_contact(MCMC_parameters, "gender", "HIC", TRUE, data))
physical_HIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, "weekday", "HIC", TRUE, data))
physical_HIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, "hh_size", "HIC", TRUE, data))
physical_HIC_student <- run$enqueue(physical_contact(MCMC_parameters, "student", "HIC", TRUE, data))
physical_HIC_employment <- run$enqueue(physical_contact(MCMC_parameters, "employment", "HIC", TRUE, data))
physical_HIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "HIC", TRUE, data))

# Fitting Duration of Contacts 
duration_LIC_age <- run$enqueue(duration_contact(MCMC_parameters, "age3cat", "LIC/LMIC", TRUE, data))
duration_LIC_gender <- run$enqueue(duration_contact(MCMC_parameters, "gender", "LIC/LMIC", TRUE, data))
duration_LIC_weekday <- run$enqueue(duration_contact(MCMC_parameters, "weekday", "LIC/LMIC", TRUE, data))
duration_LIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, "hh_size", "LIC/LMIC", TRUE, data))
duration_LIC_student <- run$enqueue(duration_contact(MCMC_parameters, "student", "LIC/LMIC", TRUE, data))
duration_LIC_employment <- run$enqueue(duration_contact(MCMC_parameters, "employment", "LIC/LMIC", TRUE, data))
duration_LIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "LIC/LMIC", TRUE, data))

duration_UMIC_age <- run$enqueue(duration_contact(MCMC_parameters, "age3cat", "UMIC", TRUE, data))
duration_UMIC_gender <- run$enqueue(duration_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
duration_UMIC_weekday <- run$enqueue(duration_contact(MCMC_parameters, "weekday", "UMIC", TRUE, data))
duration_UMIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, "hh_size", "UMIC", TRUE, data))
duration_UMIC_student <- run$enqueue(duration_contact(MCMC_parameters, "student", "UMIC", TRUE, data))
duration_UMIC_employment <- run$enqueue(duration_contact(MCMC_parameters, "employment", "UMIC", TRUE, data))
duration_UMIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "UMIC", TRUE, data))

duration_HIC_age <- run$enqueue(duration_contact(MCMC_parameters, "age3cat", "HIC", TRUE, data))
duration_HIC_gender <- run$enqueue(duration_contact(MCMC_parameters, "gender", "HIC", TRUE, data))
duration_HIC_weekday <- run$enqueue(duration_contact(MCMC_parameters, "weekday", "HIC", TRUE, data))
duration_HIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, "hh_size", "HIC", TRUE, data))
duration_HIC_student <- run$enqueue(duration_contact(MCMC_parameters, "student", "HIC", TRUE, data))
duration_HIC_employment <- run$enqueue(duration_contact(MCMC_parameters, "employment", "HIC", TRUE, data))
duration_HIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "HIC", TRUE, data))


total_LIC_age$status()
total_LIC_gender$status()
total_LIC_weekday$status()

run$unsubmit(duration_HIC_method$id)


duration_LIC_age$status()

test2 <- run$enqueue(location_contact(MCMC_parameters = MCMC_parameters,  
                                      model_covariates = "age3cat", income_strata_subset = NULL, 
                                      random_study_effect = TRUE, data = data))








test$status()
test2$status()

result <- readRDS(file = "N:/Charlie/contact_matrix_work/Results/location_UMIC_weekday__50iter_1chains_2020-10-30.rds")
result$fitting_output

# Basic Checks and Results Accessing for the Negative Binomial
pairs(result$fitting_output, off_diag_args = list(size = 1/5, alpha = 1/5))
plot(result$fitting_output)
conditional_effects(result$fitting_output)
chains <- brms::posterior_samples(result$fitting_output)
pp_check <- brms::pp_check(result$fitting_output)

# Basic Checks and Results Accessing for the Multinomial
pairs(result$fitting_output, off_diag_args = list(size = 1/5, alpha = 1/5))
plot(result$fitting_output)
conditional_effects(result$fitting_output)
chains <- brms::posterior_samples(result$fitting_output)

subset_data <- data[1:10, ] 
names <- colnames(x)
study <- subset_data$study[1]
study_spec_covs <- names[grep(study, names)]

school <- mean(x$b_mutotschool_Intercept) + mean(x$b_mutotschool_age3cat) * subset_data$age3cat + mean(x[, study_spec_covs[1]])
work <- mean(x$b_mutotwork_Intercept) + mean(x$b_mutotwork_age3cat) * subset_data$age3cat + mean(x[, study_spec_covs[2]])
other <- mean(x$b_mutotother_Intercept) + mean(x$b_mutotother_age3cat) * subset_data$age3cat + mean(x[, study_spec_covs[3]])

school_prob_init <- exp(school)
work_prob_init <- exp(work)
other_prob_init <- exp(other)
total_prob <- school_prob_init + work_prob_init + other_prob_init

school_prob <- school_prob_init/(1 + total_prob)
work_prob <- work_prob_init/(1 + total_prob)
other_prob <- other_prob_init/(1 + total_prob)
home_prob <- 1 - (school_prob + work_prob + other_prob)

rmultinom(n = 1, size = subset_data$tot_all[1], prob = c(home_prob[1], school_prob[1], work_prob[1], other_prob[1]))

school_prob + work_prob + other_prob + home_prob


subset_data$prop_school
subset_data$prop_other
subset_data$prop_work
subset_data$prop_home


colnames(x)





