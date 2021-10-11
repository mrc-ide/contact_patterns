# Load Required Libaries
library(tidyverse)

# Setting Up Cluster - Creating a Context and Configuring the Queue
setwd("N:/")
loc <- didehpc::path_mapping("location", "N:", "//fi--didenas5/malaria", "N:")
config <- didehpc::didehpc_config(shares = loc, use_rrq = FALSE, cluster = "fi--didemrchnb",
                                  parallel = FALSE, cores = 2, template = "32Core")
sources <- c("N:/Charlie/contact_matrix_work/contact_patterns/Functions/cluster_functions.R")
context_name <- paste0("N:/contact_matrix_work/ydk_time4new_newnew_context_", Sys.Date())
ctx <- context::context_save(context_name,
                             sources = sources,
                             packages = c("rstan", "brms", "magrittr", "tidyverse", "pkgdepends", "BH", "RcppEigen"))
run <- didehpc::queue_didehpc(ctx, config = config, initialise = TRUE)
run$config
run$cluster_load()
x <- run$enqueue(sessionInfo())
x$result()

# Loading In Contact Survey Data
data <- read.csv("N:/Charlie/contact_matrix_work/contact_patterns/Data/combined_participant_lvl_final.csv") %>%
  mutate(age3cat = as.factor(age3cat),
         hh_size = ifelse(hh_size == "6+", 6, hh_size),
         hh_size = as.numeric(hh_size),
         hh_size = case_when(hh_size == 1 ~ "1", hh_size == 2 ~ "2",
                             hh_size == 3 ~ "3", hh_size == 4 ~ "4", 
                             hh_size == 5 ~ "5", hh_size >= 6 ~ "6+", TRUE ~ NA_character_),
         hh_size = as.factor(hh_size),
         tot_contacts = as.numeric(tot_contacts),
         tot_contacts_no_add = as.numeric(tot_contacts_no_add),
         gender = case_when(part_gender == "Female" ~ 0, part_gender == "Male" ~ 1, TRUE ~ NA_real_),
         employment = as.factor(employment),
         student = as.factor(student),
         study = as.factor(study),
         income = factor(income, levels = c("LIC/LMIC", "UMIC", "HIC")),
         weekday = as.factor(weekday),
         method = ifelse(method == "diary", "Diary", method),
         method = ifelse(method == "Online", "Interview", method))
student_data <- data %>%
  filter(!is.na(aged_5_to_18) & aged_5_to_18 == 1)
employment_data <- data %>%
  filter(!is.na(aged_over_18) & aged_over_18 == 1)

# Summary of all available clusters and checking various tasks
run$cluster_load(nodes = FALSE)
run$task_list()
run$task_times()

# MCMC Parameters
MCMC_parameters <- list(iterations = 10000, burnin = 3000, chains = 2, cores = 2)
# MCMC_parameters <- list(iterations = 100, burnin = 30, chains = 1, cores = 1)
# x <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "LIC/LMIC", TRUE, data, TRUE)

#######################################################################################
#                                                                                     #
#    Analysis 1: Total Contacts (With Additional Work/Group/Other Contacts)           #
#                                                                                     #
#######################################################################################
mv_total_LIC_base <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "LIC/LMIC", TRUE, data))
mv_total_LIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data))
mv_total_LIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data))
mv_total_LIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data))
mv_total_LIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data)) 
mv_total_LIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data)) 

mv_total_UMIC_base <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "UMIC", TRUE, data))
mv_total_UMIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "UMIC", FALSE, data))
mv_total_UMIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "UMIC", TRUE, data))
mv_total_UMIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data))
mv_total_UMIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "UMIC", TRUE, student_data)) 
mv_total_UMIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data)) 

mv_total_HIC_base <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "HIC", TRUE, data))
mv_total_HIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "HIC", FALSE, data))
mv_total_HIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "HIC", TRUE, data))
mv_total_HIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "HIC", TRUE, data))
mv_total_HIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "HIC", TRUE, student_data)) 
mv_total_HIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "HIC", TRUE, employment_data))

##########################################################################################
#                                                                                        #
# Analysis 1.5: Total Contacts (With Additional Work/Group/Other Contacts) AND WEIGHTING #
#                                                                                        #
##########################################################################################
mv_total_LIC_base_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "LIC/LMIC", TRUE, data, TRUE))
mv_total_LIC_method_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data, TRUE))
mv_total_LIC_weekday_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data, TRUE))
mv_total_LIC_hhsize_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data, TRUE))
mv_total_LIC_student_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data, TRUE)) 
mv_total_LIC_employment_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data, TRUE)) 

mv_total_UMIC_base_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "UMIC", TRUE, data, TRUE))
mv_total_UMIC_method_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "UMIC", FALSE, data, TRUE))
mv_total_UMIC_weekday_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "UMIC", TRUE, data, TRUE))
mv_total_UMIC_hhsize_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data, TRUE))
mv_total_UMIC_student_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "UMIC", TRUE, student_data, TRUE)) 
mv_total_UMIC_employment_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data, TRUE)) 

mv_total_HIC_base_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "HIC", TRUE, data, TRUE))
mv_total_HIC_method_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "HIC", FALSE, data, TRUE))
mv_total_HIC_weekday_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "HIC", TRUE, data, TRUE))
mv_total_HIC_hhsize_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "HIC", TRUE, data, TRUE))
mv_total_HIC_student_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "HIC", TRUE, student_data, TRUE)) 
mv_total_HIC_employment_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "HIC", TRUE, employment_data, TRUE))

#######################################################################################
#                                                                                     #
#    Analysis 2: Total Contacts (Without Additional Work/Group/Other Contacts)        #
#                                                                                     #
#######################################################################################
mv_total_noadd_LIC_base <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender"), "LIC/LMIC", TRUE, data))
mv_total_noadd_LIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data))
mv_total_noadd_LIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data))
mv_total_noadd_LIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data))
mv_total_noadd_LIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data)) 
mv_total_noadd_LIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data)) 

mv_total_noadd_UMIC_base <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender"), "UMIC", TRUE, data))
mv_total_noadd_UMIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "method"), "UMIC", FALSE, data))
mv_total_noadd_UMIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "weekday"), "UMIC", TRUE, data))
mv_total_noadd_UMIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data))
mv_total_noadd_UMIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "student"), "UMIC", TRUE, student_data)) 
mv_total_noadd_UMIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data)) 

mv_total_noadd_HIC_base <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender"), "HIC", TRUE, data))
mv_total_noadd_HIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "method"), "HIC", FALSE, data))
mv_total_noadd_HIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "weekday"), "HIC", TRUE, data))
mv_total_noadd_HIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "hh_size"), "HIC", TRUE, data))
mv_total_noadd_HIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "student"), "HIC", TRUE, student_data)) 
mv_total_noadd_HIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "employment"), "HIC", TRUE, employment_data))

#######################################################################################
#                                                                                     #
#                     Analysis 3: Physicality of Contacts                             #
#                                                                                     #
#######################################################################################
mv_physical_LIC_base <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data))
mv_physical_LIC_method <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data))
mv_physical_LIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data))
mv_physical_LIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data))
mv_physical_LIC_student <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data))
mv_physical_LIC_employment <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data))

mv_physical_UMIC_base <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data))
mv_physical_UMIC_method <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", FALSE, data))
mv_physical_UMIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "UMIC", TRUE, data))
mv_physical_UMIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data))
mv_physical_UMIC_student <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data))
mv_physical_UMIC_employment <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data))

mv_physical_HIC_base <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data))
mv_physical_HIC_method <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data))
mv_physical_HIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "HIC", TRUE, data))
mv_physical_HIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data))
mv_physical_HIC_student <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data))
mv_physical_HIC_employment <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data))

#######################################################################################
#                                                                                     #
#                Analysis 3.5: Physicality of Contacts (Weighted)                     #
#                                                                                     #
#######################################################################################
mv_physical_LIC_base_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data, TRUE))
mv_physical_LIC_method_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data, TRUE))
mv_physical_LIC_weekday_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data, TRUE))
mv_physical_LIC_hhsize_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data, TRUE))
mv_physical_LIC_student_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data, TRUE))
mv_physical_LIC_employment_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data, TRUE))

mv_physical_UMIC_base_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data, TRUE))
mv_physical_UMIC_method_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", FALSE, data, TRUE))
mv_physical_UMIC_weekday_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "UMIC", TRUE, data, TRUE))
mv_physical_UMIC_hhsize_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data, TRUE))
mv_physical_UMIC_student_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data, TRUE))
mv_physical_UMIC_employment_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data, TRUE))

mv_physical_HIC_base_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data, TRUE))
mv_physical_HIC_method_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data, TRUE))
mv_physical_HIC_weekday_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "HIC", TRUE, data, TRUE))
mv_physical_HIC_hhsize_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data, TRUE))
mv_physical_HIC_student_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data, TRUE))
mv_physical_HIC_employment_weighted <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data, TRUE))

#######################################################################################
#                                                                                     #
#                     Analysis 4: Duration of Contacts                                #
#                                                                                     #
#######################################################################################
mv_duration_LIC_base <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data))
mv_duration_LIC_method <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data))
mv_duration_LIC_weekday <- run$enqueue(duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data))
mv_duration_LIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data))
mv_duration_LIC_student <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data))
mv_duration_LIC_employment <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data))

mv_duration_UMIC_base <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data))
mv_duration_UMIC_method <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", FALSE, data))
mv_duration_UMIC_weekday <- run$enqueue(duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "UMIC", TRUE, data))
mv_duration_UMIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data))
mv_duration_UMIC_student <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data))
mv_duration_UMIC_employment <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data))

mv_duration_HIC_base <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data))
mv_duration_HIC_method <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data))
mv_duration_HIC_weekday <- run$enqueue(duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "HIC", TRUE, data))
mv_duration_HIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data))
mv_duration_HIC_student <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data))
mv_duration_HIC_employment <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data))

#######################################################################################
#                                                                                     #
#                  Analysis 4.5: Duration of Contacts (Weighted)                      #
#                                                                                     #
#######################################################################################
mv_duration_LIC_base_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data, TRUE))
mv_duration_LIC_method_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data, TRUE))
mv_duration_LIC_weekday_weighted <- run$enqueue(duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data, TRUE))
mv_duration_LIC_hhsize_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data, TRUE))
mv_duration_LIC_student_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data, TRUE))
mv_duration_LIC_employment_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data, TRUE))

mv_duration_UMIC_base_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data, TRUE))
mv_duration_UMIC_method_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", FALSE, data, TRUE))
mv_duration_UMIC_weekday_weighted <- run$enqueue(duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "UMIC", TRUE, data, TRUE))
mv_duration_UMIC_hhsize_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data, TRUE))
mv_duration_UMIC_student_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data, TRUE))
mv_duration_UMIC_employment_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data, TRUE))

mv_duration_HIC_base_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data, TRUE))
mv_duration_HIC_method_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data, TRUE))
mv_duration_HIC_weekday_weighted <- run$enqueue(duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "HIC", TRUE, data, TRUE))
mv_duration_HIC_hhsize_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data, TRUE))
mv_duration_HIC_student_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data, TRUE))
mv_duration_HIC_employment_weighted <- run$enqueue(duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data, TRUE))

# table(data$gender, useNA = "ifany")
# table(data$income, data$study)
# table(hic$method, hic$study)
# table(data$aged_5_to_18, useNA = "ifany")
# table(data$age3cat, data$aged_5_to_18, useNA = "ifany")
# table(data$aged_over_18, useNA = "ifany")
# table(data$age3cat, data$aged_over_18, useNA = "ifany")
# table(data$gender, data$part_gender, useNA = "ifany") # fine
# table(data$age3cat, useNA = "ifany") # fine
# table(data$weekday, data$weekday, useNA = "ifany") # fine
# table(data$hh_size, useNA = "ifany") # fine (consider going to hh_size 10+ and then excluding the 42 people living hh_size "6+")
# table(data$student, useNA = "ifany") # fine
# table(data$age3cat, data$student, useNA = "ifany") # additional check on student, fine
# table(data$employment, useNA = "ifany") # fine
# table(data$employment, data$age3cat, useNA = "ifany") # additional check on employment, fine 
# table(data$method, useNA = "ifany") # need to put the "Online" into one of "Diary" or "Interview"
# table(data$study, useNA = "ifany") # fine
# 
# table(data$study, data$age3cat, useNA = "ifany")
# table(data$study, data$gender, useNA = "ifany")
# table(data$study, data$weekday, useNA = "ifany")
# table(data$study, data$hh_size, useNA = "ifany")
# table(data$study, data$student, useNA = "ifany")
# table(data$study, data$employment, useNA = "ifany")
# table(data$study, data$method, useNA = "ifany")
