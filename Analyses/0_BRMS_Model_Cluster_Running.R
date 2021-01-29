# Load Required Libaries
library(tidyverse)

# Setting Up Cluster - Creating a Context and Configuring the Queue
setwd("N:/")
loc <- didehpc::path_mapping("location", "N:", "//fi--didenas5/malaria", "N:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster = "fi--didemrchnb", 
                                  parallel = FALSE, rtools = TRUE, cores = 2)
sources <- c("N:/Charlie/contact_matrix_work/contact_patterns/Functions/cluster_functions.R")
context_name <- paste0("N:/contact_matrix_work/context_", Sys.Date())
# DONT FORGET TO REPLACE WITH PETE'S STANHEADERS ONCE THE CONTEXT IS CREATED AND WORKING PROPERLY
ctx <- context::context_save(context_name,
                             sources = sources,
                             packages = c("rstan", "brms", "magrittr", "tidyverse"))
run <- didehpc::queue_didehpc(ctx, config = config, initialise = TRUE)
run$config
x <-run$enqueue(sessionInfo())
x$result()

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
         weekday = as.factor(weekday),
         method = ifelse(method == "diary", "Diary", method),
         method = ifelse(method == "Online", "Interview", method))

student_data <- data %>%
  filter(!is.na(part_age) & part_age >= 5 & part_age <= 18)
employment_data <- data %>%
  filter(!is.na(part_age) & part_age > 18)

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
MCMC_parameters <- list(iterations = 10, burnin = 5, chains = 2, cores = 2)
physical_contact(MCMC_parameters, "weekday", "LIC/LMIC", TRUE, data)
# total_contacts(MCMC_parameters, "tot_all", "age3cat", "HIC", TRUE, data)
# total_contacts(MCMC_parameters, "tot_all", "age3cat", "LIC/LMIC", TRUE, data)
MCMC_parameters <- list(iterations = 10000, burnin = 3000, chains = 2, cores = 2)

# Fitting Total Contacts Made 
uni_total_LIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "age3cat", "LIC/LMIC", TRUE, data))
uni_total_LIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "gender", "LIC/LMIC", TRUE, data))
uni_total_LIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "method", "LIC/LMIC", TRUE, data))
uni_total_LIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "weekday", "LIC/LMIC", TRUE, data))
uni_total_LIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "hh_size", "LIC/LMIC", TRUE, data))
uni_total_LIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "student", "LIC/LMIC", TRUE, data))
uni_total_LIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "employment", "LIC/LMIC", TRUE, data))
mv_total_LIC_base <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender"), "LIC/LMIC", TRUE, data))
mv_total_LIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data))
mv_total_LIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data))
mv_total_LIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "method"), "LIC/LMIC", TRUE, data))
mv_total_LIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data)) 
mv_total_LIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data)) 

uni_total_UMIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "age3cat", "UMIC", TRUE, data))
uni_total_UMIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "gender", "UMIC", TRUE, data))
uni_total_UMIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "method", "UMIC", TRUE, data))
uni_total_UMIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "weekday", "UMIC", TRUE, data))
uni_total_UMIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "hh_size", "UMIC", TRUE, data))
uni_total_UMIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "student", "UMIC", TRUE, data))
uni_total_UMIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "employment", "UMIC", TRUE, data))
mv_total_UMIC_base <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender"), "UMIC", TRUE, data))
mv_total_UMIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "weekday"), "UMIC", TRUE, data))
mv_total_UMIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data))
mv_total_UMIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "method"), "UMIC", TRUE, data))
mv_total_UMIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("part_age", "gender", "student"), "UMIC", TRUE, student_data)) 
mv_total_UMIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data)) 

uni_total_HIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "age3cat", "HIC", TRUE, data))
uni_total_HIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "gender", "HIC", TRUE, data))
uni_total_HIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "method", "HIC", TRUE, data))
uni_total_HIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "weekday", "HIC", TRUE, data))
uni_total_HIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "hh_size", "HIC", TRUE, data))
uni_total_HIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "student", "HIC", TRUE, data))
uni_total_HIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "employment", "HIC", TRUE, data))
mv_total_HIC_base <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender"), "HIC", TRUE, data))
mv_total_HIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "weekday"), "HIC", TRUE, data))
mv_total_HIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "hh_size"), "HIC", TRUE, data))
mv_total_HIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "method"), "HIC", TRUE, data))
mv_total_HIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("part_age", "gender", "student"), "HIC", TRUE, student_data)) 
mv_total_HIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("part_age", "gender", "employment"), "HIC", TRUE, employment_data))

# Fitting Location of Contacts
uni_location_LIC_age <- run$enqueue(location_contact(MCMC_parameters, "age3cat", "LIC/LMIC", TRUE, data))
uni_location_LIC_gender <- run$enqueue(location_contact(MCMC_parameters, "gender", "LIC/LMIC", TRUE, data))
uni_location_LIC_method <- run$enqueue(location_contact(MCMC_parameters, "method", "LIC/LMIC", TRUE, data))
uni_location_LIC_weekday <- run$enqueue(location_contact(MCMC_parameters, "weekday", "LIC/LMIC", TRUE, data))
uni_location_LIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, "hh_size", "LIC/LMIC", TRUE, data))
uni_location_LIC_student <- run$enqueue(location_contact(MCMC_parameters, "student", "LIC/LMIC", TRUE, data))
uni_location_LIC_employment <- run$enqueue(location_contact(MCMC_parameters, "employment", "LIC/LMIC", TRUE, data))
mv_location_LIC_base <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data))
mv_location_LIC_weekday <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data))
mv_location_LIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data))
mv_location_LIC_method <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", TRUE, data))
mv_location_LIC_student <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data))
mv_location_LIC_employment <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data))

uni_location_UMIC_age <- run$enqueue(location_contact(MCMC_parameters, "age3cat", "UMIC", TRUE, data))
uni_location_UMIC_gender <- run$enqueue(location_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
uni_location_UMIC_weekday <- run$enqueue(location_contact(MCMC_parameters, "weekday", "UMIC", TRUE, data))
uni_location_UMIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, "hh_size", "UMIC", TRUE, data))
uni_location_UMIC_student <- run$enqueue(location_contact(MCMC_parameters, "student", "UMIC", TRUE, data))
uni_location_UMIC_employment <- run$enqueue(location_contact(MCMC_parameters, "employment", "UMIC", TRUE, data))
uni_location_UMIC_method <- run$enqueue(location_contact(MCMC_parameters, "method", "UMIC", TRUE, data))
mv_location_UMIC_base <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data))
mv_location_UMIC_weekday <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "UMIC", TRUE, data))
mv_location_UMIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data))
mv_location_UMIC_method <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", TRUE, data))
mv_location_UMIC_student <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data))
mv_location_UMIC_employment <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data))

uni_location_HIC_age <- run$enqueue(location_contact(MCMC_parameters, "age3cat", "HIC", TRUE, data))
uni_location_HIC_gender <- run$enqueue(location_contact(MCMC_parameters, "gender", "HIC", TRUE, data))
uni_location_HIC_method <- run$enqueue(location_contact(MCMC_parameters, "method", "HIC", TRUE, data)) # only has 1 level (diary) so not possible to do
uni_location_HIC_weekday <- run$enqueue(location_contact(MCMC_parameters, "weekday", "HIC", TRUE, data))
uni_location_HIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, "hh_size", "HIC", TRUE, data))
uni_location_HIC_student <- run$enqueue(location_contact(MCMC_parameters, "student", "HIC", TRUE, data))
uni_location_HIC_employment <- run$enqueue(location_contact(MCMC_parameters, "employment", "HIC", TRUE, data))
mv_location_HIC_base <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data))
mv_location_HIC_weekday <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "HIC", TRUE, data))
mv_location_HIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data))
mv_location_HIC_method <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", TRUE, data))
mv_location_HIC_student <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data))
mv_location_HIC_employment <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data))

# Fitting Whether Contacts Were Physical or Not
uni_physical_LIC_age <- run$enqueue(physical_contact(MCMC_parameters, "age3cat", "LIC/LMIC", TRUE, data))
uni_physical_LIC_gender <- run$enqueue(physical_contact(MCMC_parameters, "gender", "LIC/LMIC", TRUE, data))
uni_physical_LIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "LIC/LMIC", TRUE, data))
uni_physical_LIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, "weekday", "LIC/LMIC", TRUE, data))
uni_physical_LIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, "hh_size", "LIC/LMIC", TRUE, data))
uni_physical_LIC_student <- run$enqueue(physical_contact(MCMC_parameters, "student", "LIC/LMIC", TRUE, data))
uni_physical_LIC_employment <- run$enqueue(physical_contact(MCMC_parameters, "employment", "LIC/LMIC", TRUE, data))
mv_physical_LIC_base <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data))
mv_physical_LIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data))
mv_physical_LIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data))
mv_physical_LIC_method <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", TRUE, data))
mv_physical_LIC_student <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data))
mv_physical_LIC_employment <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data))

uni_physical_UMIC_age <- run$enqueue(physical_contact(MCMC_parameters, "age3cat", "UMIC", TRUE, data))
uni_physical_UMIC_gender <- run$enqueue(physical_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
uni_physical_UMIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "UMIC", TRUE, data))
uni_physical_UMIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, "weekday", "UMIC", TRUE, data))
uni_physical_UMIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, "hh_size", "UMIC", TRUE, data))
uni_physical_UMIC_student <- run$enqueue(physical_contact(MCMC_parameters, "student", "UMIC", TRUE, data))
uni_physical_UMIC_employment <- run$enqueue(physical_contact(MCMC_parameters, "employment", "UMIC", TRUE, data))
mv_physical_UMIC_base <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data))
mv_physical_UMIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "UMIC", TRUE, data))
mv_physical_UMIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data))
mv_physical_UMIC_method <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", TRUE, data))
mv_physical_UMIC_student <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data))
mv_physical_UMIC_employment <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data))

uni_physical_HIC_age <- run$enqueue(physical_contact(MCMC_parameters, "age3cat", "HIC", TRUE, data))
uni_physical_HIC_gender <- run$enqueue(physical_contact(MCMC_parameters, "gender", "HIC", TRUE, data))
uni_physical_HIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "HIC", TRUE, data))
uni_physical_HIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, "weekday", "HIC", TRUE, data))
uni_physical_HIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, "hh_size", "HIC", TRUE, data))
uni_physical_HIC_student <- run$enqueue(physical_contact(MCMC_parameters, "student", "HIC", TRUE, data))
uni_physical_HIC_employment <- run$enqueue(physical_contact(MCMC_parameters, "employment", "HIC", TRUE, data))
mv_physical_HIC_base <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data))
mv_physical_HIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "HIC", TRUE, data))
mv_physical_HIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data))
mv_physical_HIC_method <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", TRUE, data))
mv_physical_HIC_student <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data))
mv_physical_HIC_employment <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data))

# Fitting Duration of Contacts 
uni_duration_LIC_age <- run$enqueue(duration_contact(MCMC_parameters, "age3cat", "LIC/LMIC", TRUE, data))
uni_duration_LIC_gender <- run$enqueue(duration_contact(MCMC_parameters, "gender", "LIC/LMIC", TRUE, data))
uni_duration_LIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "LIC/LMIC", TRUE, data))
uni_duration_LIC_weekday <- run$enqueue(duration_contact(MCMC_parameters, "weekday", "LIC/LMIC", TRUE, data))
uni_duration_LIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, "hh_size", "LIC/LMIC", TRUE, data))
uni_duration_LIC_student <- run$enqueue(duration_contact(MCMC_parameters, "student", "LIC/LMIC", TRUE, data))
uni_duration_LIC_employment <- run$enqueue(duration_contact(MCMC_parameters, "employment", "LIC/LMIC", TRUE, data))
mv_duration_LIC_age <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data))
mv_duration_LIC_weekday <- run$enqueue(duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data))
mv_duration_LIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data))
mv_duration_LIC_method <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", TRUE, data))
mv_duration_LIC_student <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data))
mv_duration_LIC_employment <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data))

uni_duration_UMIC_age <- run$enqueue(duration_contact(MCMC_parameters, "age3cat", "UMIC", TRUE, data))
uni_duration_UMIC_gender <- run$enqueue(duration_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
uni_duration_UMIC_weekday <- run$enqueue(duration_contact(MCMC_parameters, "weekday", "UMIC", TRUE, data))
uni_duration_UMIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, "hh_size", "UMIC", TRUE, data))
uni_duration_UMIC_student <- run$enqueue(duration_contact(MCMC_parameters, "student", "UMIC", TRUE, data))
uni_duration_UMIC_employment <- run$enqueue(duration_contact(MCMC_parameters, "employment", "UMIC", TRUE, data))
uni_duration_UMIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "UMIC", TRUE, data))
mv_duration_UMIC_age <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data))
mv_duration_UMIC_weekday <- run$enqueue(duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "UMIC", TRUE, data))
mv_duration_UMIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data))
mv_duration_UMIC_method <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", TRUE, data))
mv_duration_UMIC_student <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data))
mv_duration_UMIC_employment <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data))

uni_duration_HIC_age <- run$enqueue(duration_contact(MCMC_parameters, "age3cat", "HIC", TRUE, data))
uni_duration_HIC_gender <- run$enqueue(duration_contact(MCMC_parameters, "gender", "HIC", TRUE, data))
uni_duration_HIC_weekday <- run$enqueue(duration_contact(MCMC_parameters, "weekday", "HIC", TRUE, data))
uni_duration_HIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, "hh_size", "HIC", TRUE, data))
uni_duration_HIC_student <- run$enqueue(duration_contact(MCMC_parameters, "student", "HIC", TRUE, data))
uni_duration_HIC_employment <- run$enqueue(duration_contact(MCMC_parameters, "employment", "HIC", TRUE, data))
uni_duration_HIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "HIC", TRUE, data))
mv_duration_HIC_age <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data))
mv_duration_HIC_weekday <- run$enqueue(duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "HIC", TRUE, data))
mv_duration_HIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data))
mv_duration_HIC_method <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", TRUE, data))
mv_duration_HIC_student <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data))
mv_duration_HIC_employment <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data))

# Checking the Running
uni_total_LIC_age$status()
uni_total_LIC_gender$status()
uni_total_LIC_weekday$status()
uni_total_LIC_hhsize$status()
uni_total_LIC_student$status()
uni_total_LIC_employment$status()
uni_total_LIC_method$status()
mv_total_LIC_base$status()
mv_total_LIC_weekday$status()
mv_total_LIC_hhsize$status()
mv_total_LIC_method$status()
mv_total_LIC_student$status()
mv_total_LIC_employment$status()

uni_total_UMIC_age$status()
uni_total_UMIC_gender$status()
uni_total_UMIC_weekday$status()
uni_total_UMIC_hhsize$status()
uni_total_UMIC_student$status()
uni_total_UMIC_employment$status()
uni_total_UMIC_method$status()
mv_total_UMIC_base$status()
mv_total_UMIC_weekday$status()
mv_total_UMIC_hhsize$status()
mv_total_UMIC_method$status()
mv_total_UMIC_student$status()
mv_total_UMIC_employment$status()

uni_total_HIC_age$status()
uni_total_HIC_gender$status()
uni_total_HIC_weekday$status()
uni_total_HIC_hhsize$status()
uni_total_HIC_student$status()
uni_total_HIC_employment$status()
uni_total_HIC_method$status()
mv_total_HIC_base$status()
mv_total_HIC_weekday$status()
mv_total_HIC_hhsize$status()
mv_total_HIC_method$status()
mv_total_HIC_student$status()
mv_total_HIC_employment$status()

uni_location_LIC_age$status()
uni_location_LIC_gender$status() 
uni_location_LIC_method$status()
uni_location_LIC_weekday$status()
uni_location_LIC_hhsize$status()
uni_location_LIC_student$status()
uni_location_LIC_employment$status()
mv_location_LIC_base$status()
mv_location_LIC_weekday$status()
mv_location_LIC_hhsize$status()
mv_location_LIC_method$status()
mv_location_LIC_student$status()
mv_location_LIC_employment$status()

uni_location_UMIC_age$status()
uni_location_UMIC_gender$status()
uni_location_UMIC_weekday$status()
uni_location_UMIC_hhsize$status()
uni_location_UMIC_student$status()
uni_location_UMIC_employment$status()
uni_location_UMIC_method$status()
mv_location_UMIC_base$status()
mv_location_UMIC_weekday$status()
mv_location_UMIC_hhsize$status()
mv_location_UMIC_method$status()
mv_location_UMIC_student$status()
mv_location_UMIC_employment$status()

uni_location_HIC_age$status()
uni_location_HIC_gender$status()
uni_location_HIC_method$status()
uni_location_HIC_weekday$status()
uni_location_HIC_hhsize$status()
uni_location_HIC_student$status()
uni_location_HIC_employment$status()
mv_location_HIC_base$status()
mv_location_HIC_weekday$status()
mv_location_HIC_hhsize$status()
mv_location_HIC_method$status()
mv_location_HIC_student$status()
mv_location_HIC_employment$status()

uni_physical_LIC_age$status()
uni_physical_LIC_gender$status()
uni_physical_LIC_method$status()
uni_physical_LIC_weekday$status()
uni_physical_LIC_hhsize$status()
uni_physical_LIC_student$status()
uni_physical_LIC_employment$status()
mv_physical_LIC_base$status()
mv_physical_LIC_weekday$status()
mv_physical_LIC_hhsize$status()
mv_physical_LIC_method$status()
mv_physical_LIC_student$status()
mv_physical_LIC_employment$status()


physical_UMIC_age$status()
physical_UMIC_gender$status()
physical_UMIC_weekday$status()
physical_UMIC_hhsize$status()
physical_UMIC_student$status()
physical_UMIC_employment$status()
physical_UMIC_method$status()
physical_HIC_age$status()
physical_HIC_gender$status()
physical_HIC_weekday$status()
physical_HIC_hhsize$status()
physical_HIC_student$status()
physical_HIC_employment$status()
physical_HIC_method$status() # failure because they're all one type

duration_LIC_age$status()
duration_LIC_gender$status()
duration_LIC_weekday$status()
duration_LIC_hhsize$status()
duration_LIC_student$status()
duration_LIC_employment$status()
duration_LIC_method$status()
duration_UMIC_age$status()
duration_UMIC_gender$status()
duration_UMIC_weekday$status()
duration_UMIC_hhsize$status()
duration_UMIC_student$status()
duration_UMIC_employment$status()
duration_UMIC_method$status()
duration_HIC_age$status()
duration_HIC_gender$status()
duration_HIC_weekday$status()
duration_HIC_hhsize$status()
duration_HIC_student$status()
duration_HIC_employment$status()
duration_HIC_method$status()

