# Load Required Libaries
library(tidyverse); library(rstan); library(brms); library(magrittr); library(here)

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

# Sourcing Required Functions
source("Functions/cluster_functions.R")

# MCMC Parameters
MCMC_parameters <- list(iterations = 10000, burnin = 3000, chains = 2, cores = 2)
# MCMC_parameters <- list(iterations = 100, burnin = 30, chains = 1, cores = 1)

#######################################################################################
#                                                                                     #
#    Analysis 1: Total Contacts (With Additional Work/Group/Other Contacts)           #
#                                                                                     #
#######################################################################################
mv_total_LIC_base <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "LIC/LMIC", TRUE, data)
mv_total_LIC_method <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data)
mv_total_LIC_weekday <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data)
mv_total_LIC_hhsize <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data)
mv_total_LIC_student <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data) 
mv_total_LIC_employment <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data) 

mv_total_UMIC_base <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "UMIC", TRUE, data)
mv_total_UMIC_method <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "UMIC", FALSE, data)
mv_total_UMIC_weekday <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "UMIC", TRUE, data)
mv_total_UMIC_hhsize <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data)
mv_total_UMIC_student <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "UMIC", TRUE, student_data) 
mv_total_UMIC_employment <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data) 

mv_total_HIC_base <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "HIC", TRUE, data)
mv_total_HIC_method <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "HIC", FALSE, data)
mv_total_HIC_weekday <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "HIC", TRUE, data)
mv_total_HIC_hhsize <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "HIC", TRUE, data)
mv_total_HIC_student <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "HIC", TRUE, student_data) 
mv_total_HIC_employment <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "HIC", TRUE, employment_data)

##########################################################################################
#                                                                                        #
# Analysis 1.5: Total Contacts (With Additional Work/Group/Other Contacts) AND WEIGHTING #
#                                                                                        #
##########################################################################################
mv_total_LIC_base_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "LIC/LMIC", TRUE, data, TRUE)
mv_total_LIC_method_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data, TRUE)
mv_total_LIC_weekday_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data, TRUE)
mv_total_LIC_hhsize_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data, TRUE)
mv_total_LIC_student_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data, TRUE) 
mv_total_LIC_employment_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data, TRUE) 

mv_total_UMIC_base_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "UMIC", TRUE, data, TRUE)
mv_total_UMIC_method_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "UMIC", FALSE, data, TRUE)
mv_total_UMIC_weekday_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "UMIC", TRUE, data, TRUE)
mv_total_UMIC_hhsize_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data, TRUE)
mv_total_UMIC_student_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "UMIC", TRUE, student_data, TRUE) 
mv_total_UMIC_employment_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data, TRUE) 

mv_total_HIC_base_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender"), "HIC", TRUE, data, TRUE)
mv_total_HIC_method_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "method"), "HIC", FALSE, data, TRUE)
mv_total_HIC_weekday_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "weekday"), "HIC", TRUE, data, TRUE)
mv_total_HIC_hhsize_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("age3cat", "gender", "hh_size"), "HIC", TRUE, data, TRUE)
mv_total_HIC_student_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "student"), "HIC", TRUE, student_data, TRUE) 
mv_total_HIC_employment_weighted <- total_contacts(MCMC_parameters, "tot_contacts", c("part_age", "gender", "employment"), "HIC", TRUE, employment_data, TRUE)

#######################################################################################
#                                                                                     #
#    Analysis 2: Total Contacts (Without Additional Work/Group/Other Contacts)        #
#                                                                                     #
#######################################################################################
mv_total_noadd_LIC_base <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender"), "LIC/LMIC", TRUE, data)
mv_total_noadd_LIC_method <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data)
mv_total_noadd_LIC_weekday <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data)
mv_total_noadd_LIC_hhsize <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data)
mv_total_noadd_LIC_student <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data) 
mv_total_noadd_LIC_employment <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data) 

mv_total_noadd_UMIC_base <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender"), "UMIC", TRUE, data)
mv_total_noadd_UMIC_method <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "method"), "UMIC", FALSE, data)
mv_total_noadd_UMIC_weekday <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "weekday"), "UMIC", TRUE, data)
mv_total_noadd_UMIC_hhsize <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data)
mv_total_noadd_UMIC_student <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "student"), "UMIC", TRUE, student_data) 
mv_total_noadd_UMIC_employment <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data) 

mv_total_noadd_HIC_base <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender"), "HIC", TRUE, data)
mv_total_noadd_HIC_method <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "method"), "HIC", FALSE, data)
mv_total_noadd_HIC_weekday <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "weekday"), "HIC", TRUE, data)
mv_total_noadd_HIC_hhsize <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("age3cat", "gender", "hh_size"), "HIC", TRUE, data)
mv_total_noadd_HIC_student <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "student"), "HIC", TRUE, student_data) 
mv_total_noadd_HIC_employment <- total_contacts(MCMC_parameters, "tot_contacts_no_add", c("part_age", "gender", "employment"), "HIC", TRUE, employment_data)

#######################################################################################
#                                                                                     #
#                     Analysis 3: Physicality of Contacts                             #
#                                                                                     #
#######################################################################################
mv_physical_LIC_base <- physical_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data)
mv_physical_LIC_method <- physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data)
mv_physical_LIC_weekday <- physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data)
mv_physical_LIC_hhsize <- physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data)
mv_physical_LIC_student <- physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data)
mv_physical_LIC_employment <- physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data)

mv_physical_UMIC_base <- physical_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data)
mv_physical_UMIC_method <- physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", FALSE, data)
mv_physical_UMIC_weekday <- physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "UMIC", TRUE, data)
mv_physical_UMIC_hhsize <- physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data)
mv_physical_UMIC_student <- physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data)
mv_physical_UMIC_employment <- physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data)

mv_physical_HIC_base <- physical_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data)
mv_physical_HIC_method <- physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data)
mv_physical_HIC_weekday <- physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "HIC", TRUE, data)
mv_physical_HIC_hhsize <- physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data)
mv_physical_HIC_student <- physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data)
mv_physical_HIC_employment <- physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data)

#######################################################################################
#                                                                                     #
#                Analysis 3.5: Physicality of Contacts (Weighted)                     #
#                                                                                     #
#######################################################################################
mv_physical_LIC_base_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data, TRUE)
mv_physical_LIC_method_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data, TRUE)
mv_physical_LIC_weekday_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data, TRUE)
mv_physical_LIC_hhsize_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data, TRUE)
mv_physical_LIC_student_weighted <- physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data, TRUE)
mv_physical_LIC_employment_weighted <- physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data, TRUE)

mv_physical_UMIC_base_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data, TRUE)
mv_physical_UMIC_method_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", FALSE, data, TRUE)
mv_physical_UMIC_weekday_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "UMIC", TRUE, data, TRUE)
mv_physical_UMIC_hhsize_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data, TRUE)
mv_physical_UMIC_student_weighted <- physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data, TRUE)
mv_physical_UMIC_employment_weighted <- physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data, TRUE)

mv_physical_HIC_base_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data, TRUE)
mv_physical_HIC_method_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data, TRUE)
mv_physical_HIC_weekday_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "HIC", TRUE, data, TRUE)
mv_physical_HIC_hhsize_weighted <- physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data, TRUE)
mv_physical_HIC_student_weighted <- physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data, TRUE)
mv_physical_HIC_employment_weighted <- physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data, TRUE)

#######################################################################################
#                                                                                     #
#                     Analysis 4: Duration of Contacts                                #
#                                                                                     #
#######################################################################################
mv_duration_LIC_base <- duration_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data)
mv_duration_LIC_method <- duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data)
mv_duration_LIC_weekday <- duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data)
mv_duration_LIC_hhsize <- duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data)
mv_duration_LIC_student <- duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data)
mv_duration_LIC_employment <- duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data)

mv_duration_UMIC_base <- duration_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data)
mv_duration_UMIC_method <- duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", FALSE, data)
mv_duration_UMIC_weekday <- duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "UMIC", TRUE, data)
mv_duration_UMIC_hhsize <- duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data)
mv_duration_UMIC_student <- duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data)
mv_duration_UMIC_employment <- duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data)

mv_duration_HIC_base <- duration_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data)
mv_duration_HIC_method <- duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data)
mv_duration_HIC_weekday <- duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "HIC", TRUE, data)
mv_duration_HIC_hhsize <- duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data)
mv_duration_HIC_student <- duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data)
mv_duration_HIC_employment <- duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data)

#######################################################################################
#                                                                                     #
#                  Analysis 4.5: Duration of Contacts (Weighted)                      #
#                                                                                     #
#######################################################################################
mv_duration_LIC_base_weighted <- duration_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data, TRUE)
mv_duration_LIC_method_weighted <- duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data, TRUE)
mv_duration_LIC_weekday_weighted <- duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data, TRUE)
mv_duration_LIC_hhsize_weighted <- duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data, TRUE)
mv_duration_LIC_student_weighted <- duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data, TRUE)
mv_duration_LIC_employment_weighted <- duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data, TRUE)

mv_duration_UMIC_base_weighted <- duration_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data, TRUE)
mv_duration_UMIC_method_weighted <- duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", FALSE, data, TRUE)
mv_duration_UMIC_weekday_weighted <- duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "UMIC", TRUE, data, TRUE)
mv_duration_UMIC_hhsize_weighted <- duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data, TRUE)
mv_duration_UMIC_student_weighted <- duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data, TRUE)
mv_duration_UMIC_employment_weighted <- duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data, TRUE)

mv_duration_HIC_base_weighted <- duration_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data, TRUE)
mv_duration_HIC_method_weighted <- duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data, TRUE)
mv_duration_HIC_weekday_weighted <- duration_contact(MCMC_parameters,  c("age3cat", "gender", "weekday"), "HIC", TRUE, data, TRUE)
mv_duration_HIC_hhsize_weighted <- duration_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data, TRUE)
mv_duration_HIC_student_weighted <- duration_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data, TRUE)
mv_duration_HIC_employment_weighted <- duration_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data, TRUE)
