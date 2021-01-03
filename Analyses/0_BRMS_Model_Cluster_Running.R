# Load Required Libaries
library(tidyverse)

# Setting Up Cluster - Creating a Context and Configuring the Queue
setwd("N:/")
loc <- didehpc::path_mapping("location", "N:", "//fi--didenas5/malaria", "N:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster = "fi--didemrchnb", parallel = FALSE, rtools = TRUE, cores = 2)
packages <- c("rstan", "brms", "tidyverse", "gdata")
sources <- c("N:/Charlie/contact_matrix_work/contact_patterns/Functions/cluster_functions.R")
context_name <- paste0("N:/contact_matrix_work/context_", Sys.Date())
ctx <- context::context_save(path = context_name, sources = sources, packages = packages)
run <- didehpc::queue_didehpc(ctx, config = config)

# Loading In Contact Survey Data
data <- read.csv("N:/Charlie/contact_matrix_work/contact_patterns/Data/combined_participant_lvl_all.csv") %>%
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
MCMC_parameters <- list(iterations = 7000, burnin = 3000, chains = 2, cores = 2)

# Fitting Total Contacts Made 
total_LIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "age3cat", "LIC/LMIC", TRUE, data))
total_LIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "gender", "LIC/LMIC", TRUE, data))
total_LIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "weekday", "LIC/LMIC", TRUE, data))
total_LIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "hh_size", "LIC/LMIC", TRUE, data))
total_LIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "student", "LIC/LMIC", TRUE, data))
total_LIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "employment", "LIC/LMIC", TRUE, data))
total_LIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "method", "LIC/LMIC", TRUE, data))

total_UMIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "age3cat", "UMIC", TRUE, data))
total_UMIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "gender", "UMIC", TRUE, data))
total_UMIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "weekday", "UMIC", TRUE, data))
total_UMIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "hh_size", "UMIC", TRUE, data))
total_UMIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "student", "UMIC", TRUE, data))
total_UMIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "employment", "UMIC", TRUE, data))
total_UMIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "method", "UMIC", TRUE, data))


total_HIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "age3cat", "HIC", TRUE, data))
total_HIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "gender", "HIC", TRUE, data))
total_HIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "weekday", "HIC", TRUE, data))
total_HIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "hh_size", "HIC", TRUE, data))
total_HIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "student", "HIC", TRUE, data))
total_HIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "employment", "HIC", TRUE, data))
total_HIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "method", "HIC", TRUE, data))

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

# Checking the Running
total_LIC_age$status()
total_LIC_gender$status()
total_LIC_weekday$status()
total_LIC_hhsize$status()
total_LIC_student$status()
total_LIC_employment$status()
total_LIC_method$status()
total_UMIC_age$status()
total_UMIC_gender$status()
total_UMIC_weekday$status()
total_UMIC_hhsize$status() 
total_UMIC_student$status()
total_UMIC_employment$status()
total_UMIC_method$status()
total_HIC_age$status()
total_HIC_gender$status()
total_HIC_weekday$status()
total_HIC_hhsize$status()
total_HIC_student$status()
total_HIC_employment$status()
total_HIC_method$status()
location_LIC_age$status()
location_LIC_gender$status()
location_LIC_weekday$status()
location_LIC_hhsize$status()
location_LIC_student$status()
location_LIC_employment$status()
location_LIC_method$status()

location_UMIC_age$status()
location_UMIC_age$log()

location_UMIC_gender$status()
location_UMIC_weekday$status()
location_UMIC_hhsize$status()
location_UMIC_student$status()
location_UMIC_employment$status()
location_UMIC_method$status()
location_HIC_age$status()
location_HIC_gender$status()
location_HIC_weekday$status()
location_HIC_hhsize$status()
location_HIC_student$status()
location_HIC_employment$status()
location_HIC_method$status()
