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
# Multivariate Analyses (Adjusting for Age or Gender)
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

########################################################################################
#                                                                                      #
# Analysis 1: Total Contacts (With Additional Work/Group/Other Contacts) AND WEIGHTING #
#                                                                                      #
########################################################################################
# Multivariate Analyses (Adjusting for Age or Gender)
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

mv_total_LIC_base_weighted$status()
mv_total_LIC_method_weighted$status()
mv_total_LIC_weekday_weighted$status()
mv_total_LIC_hhsize_weighted$status()
mv_total_LIC_student_weighted$status()
mv_total_LIC_employment_weighted$status()

mv_total_UMIC_base_weighted$status()
mv_total_UMIC_method_weighted$status()
mv_total_UMIC_weekday_weighted$status()
mv_total_UMIC_hhsize_weighted$status()
mv_total_UMIC_student_weighted$status()
mv_total_UMIC_employment_weighted$status()

mv_total_HIC_base_weighted$status()
mv_total_HIC_method_weighted$status()
mv_total_HIC_weekday_weighted$status()
mv_total_HIC_hhsize_weighted$status()
mv_total_HIC_student_weighted$status()
mv_total_HIC_employment_weighted$status()

#######################################################################################
#                                                                                     #
#    Analysis 2: Total Contacts (Without Additional Work/Group/Other Contacts)        #
#                                                                                     #
#######################################################################################
# Multivariate Analyses (Adjusting for Age or Gender)
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

# Multivariate Analyses (Adjusting for Age or Gender)
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
#                     Analysis 4: Duration of Contacts                                #
#                                                                                     #
#######################################################################################
# Multivariate Analyses (Adjusting for Age or Gender)
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

# Checking the Running
# uni_total_LIC_age$status()
# uni_total_LIC_gender$status()
# uni_total_LIC_weekday$status()
# uni_total_LIC_hhsize$status()
# uni_total_LIC_student$status()
# uni_total_LIC_employment$status()
# uni_total_LIC_method$status()
# mv_total_LIC_base$status()
# mv_total_LIC_weekday$status()
# mv_total_LIC_hhsize$status()
# mv_total_LIC_method$status()
# mv_total_LIC_student$status()
# mv_total_LIC_employment$status()
# 
# uni_total_UMIC_age$status()
# uni_total_UMIC_gender$status()
# uni_total_UMIC_weekday$status()
# uni_total_UMIC_hhsize$status()
# uni_total_UMIC_student$status()
# uni_total_UMIC_employment$status()
# uni_total_UMIC_method$status()
# mv_total_UMIC_base$status()
# mv_total_UMIC_weekday$status()
# mv_total_UMIC_hhsize$status()
# mv_total_UMIC_method$status()
# mv_total_UMIC_student$status()
# mv_total_UMIC_employment$status()
# 
# uni_total_HIC_age$status()
# uni_total_HIC_gender$status()
# uni_total_HIC_weekday$status()
# uni_total_HIC_hhsize$status()
# uni_total_HIC_student$status()
# uni_total_HIC_employment$status()
# uni_total_HIC_method$status()
# mv_total_HIC_base$status()
# mv_total_HIC_weekday$status()
# mv_total_HIC_hhsize$status()
# mv_total_HIC_method$status()
# mv_total_HIC_student$status()
# mv_total_HIC_employment$status()
# 
# uni_location_LIC_age$status()
# uni_location_LIC_gender$status() 
# uni_location_LIC_method$status()
# uni_location_LIC_weekday$status()
# uni_location_LIC_hhsize$status()
# uni_location_LIC_student$status()
# uni_location_LIC_employment$status()
# mv_location_LIC_base$status()
# mv_location_LIC_weekday$status()
# mv_location_LIC_hhsize$status()
# mv_location_LIC_method$status()
# mv_location_LIC_student$status()
# mv_location_LIC_employment$status()
# 
# uni_location_UMIC_age$status()
# uni_location_UMIC_gender$status()
# uni_location_UMIC_weekday$status()
# uni_location_UMIC_hhsize$status()
# uni_location_UMIC_student$status()
# uni_location_UMIC_employment$status()
# uni_location_UMIC_method$status()
# mv_location_UMIC_base$status()
# mv_location_UMIC_weekday$status()
# mv_location_UMIC_hhsize$status()
# mv_location_UMIC_method$status()
# mv_location_UMIC_student$status()
# mv_location_UMIC_employment$status()
# 
# uni_location_HIC_age$status()
# uni_location_HIC_gender$status()
# uni_location_HIC_method$status()
# uni_location_HIC_weekday$status()
# uni_location_HIC_hhsize$status()
# uni_location_HIC_student$status()
# uni_location_HIC_employment$status()
# mv_location_HIC_base$status()
# mv_location_HIC_weekday$status()
# mv_location_HIC_hhsize$status()
# mv_location_HIC_method$status()
# mv_location_HIC_student$status()
# mv_location_HIC_employment$status()
# 
# uni_physical_LIC_age$status()
# uni_physical_LIC_gender$status()
# uni_physical_LIC_method$status()
# uni_physical_LIC_weekday$status()
# uni_physical_LIC_hhsize$status()
# uni_physical_LIC_student$status()
# uni_physical_LIC_employment$status()
# mv_physical_LIC_base$status()
# mv_physical_LIC_weekday$status()
# mv_physical_LIC_hhsize$status()
# mv_physical_LIC_method$status()
# mv_physical_LIC_student$status()
# mv_physical_LIC_employment$status()
# 
# uni_physical_UMIC_age$status()
# uni_physical_UMIC_gender$status()
# uni_physical_UMIC_method$status()
# uni_physical_UMIC_weekday$status()
# uni_physical_UMIC_hhsize$status()
# uni_physical_UMIC_student$status()
# uni_physical_UMIC_employment$status()
# mv_physical_UMIC_base$status()
# mv_physical_UMIC_weekday$status()
# mv_physical_UMIC_hhsize$status()
# mv_physical_UMIC_method$status()
# mv_physical_UMIC_student$status()
# mv_physical_UMIC_employment$status()
# 
# uni_physical_HIC_age$status()
# uni_physical_HIC_gender$status() 
# uni_physical_HIC_method$status() # failure because they're all one type
# uni_physical_HIC_weekday$status()
# uni_physical_HIC_hhsize$status()
# uni_physical_HIC_student$status()
# uni_physical_HIC_employment$status()
# mv_physical_HIC_base$status()
# mv_physical_HIC_weekday$status()
# mv_physical_HIC_hhsize$status()
# mv_physical_HIC_method$status() # failure because they're all one type
# mv_physical_HIC_student$status()
# mv_physical_HIC_employment$status()
# 
# uni_duration_LIC_age$status()
# uni_duration_LIC_gender$status()
# uni_duration_LIC_method$status()
# uni_duration_LIC_weekday$status()
# uni_duration_LIC_hhsize$status()
# uni_duration_LIC_student$status()
# uni_duration_LIC_employment$status()
# mv_duration_LIC_base$status()
# mv_duration_LIC_weekday$status()
# mv_duration_LIC_hhsize$status()
# mv_duration_LIC_method$status()
# mv_duration_LIC_student$status()
# mv_duration_LIC_employment$status()
# 
# uni_duration_UMIC_age$status()
# uni_duration_UMIC_gender$status()
# uni_duration_UMIC_method$status()
# uni_duration_UMIC_weekday$status()
# uni_duration_UMIC_hhsize$status()
# uni_duration_UMIC_student$status()
# uni_duration_UMIC_employment$status()
# mv_duration_UMIC_base$status()
# mv_duration_UMIC_weekday$status()
# mv_duration_UMIC_hhsize$status()
# mv_duration_UMIC_method$status()
# mv_duration_UMIC_student$status()
# mv_duration_UMIC_employment$status()
# 
# uni_duration_HIC_age$status()
# uni_duration_HIC_gender$status()
# uni_duration_HIC_method$status()
# uni_duration_HIC_weekday$status()
# uni_duration_HIC_hhsize$status()
# uni_duration_HIC_student$status()
# uni_duration_HIC_employment$status()
# mv_duration_HIC_base$status()
# mv_duration_HIC_weekday$status()
# mv_duration_HIC_hhsize$status()
# mv_duration_HIC_method$status()
# mv_duration_HIC_student$status()
# mv_duration_HIC_employment$status()




# # Extra Runs of Method Without the Random Study Effect
# uni_total_LIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "method", "LIC/LMIC", FALSE, data))
# mv_total_LIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data))
# uni_total_UMIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "method", "UMIC", FALSE, data))
# mv_total_UMIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "method"), "UMIC", FALSE, data))
# uni_total_HIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", "method", "HIC", FALSE, data))
# mv_total_HIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_all", c("age3cat", "gender", "method"), "HIC", FALSE, data))
# 
# table(run$task_status())
# 
# uni_physical_LIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "LIC/LMIC", FALSE, data))
# mv_physical_LIC_method <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data))
# uni_physical_UMIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "UMIC", FALSE, data))
# mv_physical_UMIC_method <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", FALSE, data))
# uni_physical_HIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "HIC", FALSE, data))
# mv_physical_HIC_method <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data))
# 
# table(run$task_status())
# 
# uni_duration_LIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "LIC/LMIC", FALSE, data))
# mv_duration_LIC_method <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", FALSE, data))
# uni_duration_UMIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "UMIC", FALSE, data))
# mv_duration_UMIC_method <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", FALSE, data))
# uni_duration_HIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "HIC", FALSE, data))
# mv_duration_HIC_method <- run$enqueue(duration_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data))
# 
# table(run$task_status())
# 
# uni_total_LIC_method$status()
# mv_total_LIC_method$status()
# uni_total_UMIC_method$status()
# mv_total_UMIC_method$status()
# uni_total_HIC_method$status()
# mv_total_HIC_method$status()
# uni_physical_LIC_method$status()
# mv_physical_LIC_method$status()
# uni_physical_UMIC_method$status()
# mv_physical_UMIC_method$status()
# uni_physical_HIC_method$status() 
# mv_physical_HIC_method$status()
# uni_duration_LIC_method$status() 
# mv_duration_LIC_method$status()
# uni_duration_UMIC_method$status()
# mv_duration_UMIC_method$status() 
# uni_duration_HIC_method$status() 
# mv_duration_HIC_method$status()

# Fitting Location of Contacts
# uni_location_LIC_age <- run$enqueue(location_contact(MCMC_parameters, "age3cat", "LIC/LMIC", TRUE, data))
# uni_location_LIC_gender <- run$enqueue(location_contact(MCMC_parameters, "gender", "LIC/LMIC", TRUE, data))
# uni_location_LIC_method <- run$enqueue(location_contact(MCMC_parameters, "method", "LIC/LMIC", TRUE, data))
# uni_location_LIC_weekday <- run$enqueue(location_contact(MCMC_parameters, "weekday", "LIC/LMIC", TRUE, data))
# uni_location_LIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, "hh_size", "LIC/LMIC", TRUE, data))
# uni_location_LIC_student <- run$enqueue(location_contact(MCMC_parameters, "student", "LIC/LMIC", TRUE, data))
# uni_location_LIC_employment <- run$enqueue(location_contact(MCMC_parameters, "employment", "LIC/LMIC", TRUE, data))
# mv_location_LIC_base <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender"), "LIC/LMIC", TRUE, data))
# mv_location_LIC_weekday <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "LIC/LMIC", TRUE, data))
# mv_location_LIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "LIC/LMIC", TRUE, data))
# mv_location_LIC_method <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "method"), "LIC/LMIC", TRUE, data))
# mv_location_LIC_student <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "student"), "LIC/LMIC", TRUE, student_data))
# mv_location_LIC_employment <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "employment"), "LIC/LMIC", TRUE, employment_data))
# 
# uni_location_UMIC_age <- run$enqueue(location_contact(MCMC_parameters, "age3cat", "UMIC", TRUE, data))
# uni_location_UMIC_gender <- run$enqueue(location_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
# uni_location_UMIC_weekday <- run$enqueue(location_contact(MCMC_parameters, "weekday", "UMIC", TRUE, data))
# uni_location_UMIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, "hh_size", "UMIC", TRUE, data))
# uni_location_UMIC_student <- run$enqueue(location_contact(MCMC_parameters, "student", "UMIC", TRUE, data))
# uni_location_UMIC_employment <- run$enqueue(location_contact(MCMC_parameters, "employment", "UMIC", TRUE, data))
# uni_location_UMIC_method <- run$enqueue(location_contact(MCMC_parameters, "method", "UMIC", TRUE, data))
# mv_location_UMIC_base <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender"), "UMIC", TRUE, data))
# mv_location_UMIC_weekday <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "UMIC", TRUE, data))
# mv_location_UMIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data))
# mv_location_UMIC_method <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "method"), "UMIC", TRUE, data))
# mv_location_UMIC_student <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data))
# mv_location_UMIC_employment <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data))
# 
# uni_location_HIC_age <- run$enqueue(location_contact(MCMC_parameters, "age3cat", "HIC", TRUE, data))
# uni_location_HIC_gender <- run$enqueue(location_contact(MCMC_parameters, "gender", "HIC", TRUE, data))
# uni_location_HIC_method <- run$enqueue(location_contact(MCMC_parameters, "method", "HIC", TRUE, data)) # only has 1 level (diary) so not possible to do
# uni_location_HIC_weekday <- run$enqueue(location_contact(MCMC_parameters, "weekday", "HIC", TRUE, data))
# uni_location_HIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, "hh_size", "HIC", TRUE, data))
# uni_location_HIC_student <- run$enqueue(location_contact(MCMC_parameters, "student", "HIC", TRUE, data))
# uni_location_HIC_employment <- run$enqueue(location_contact(MCMC_parameters, "employment", "HIC", TRUE, data))
# mv_location_HIC_base <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender"), "HIC", TRUE, data))
# mv_location_HIC_weekday <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "weekday"), "HIC", TRUE, data))
# mv_location_HIC_hhsize <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "HIC", TRUE, data))
# mv_location_HIC_method <- run$enqueue(location_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", TRUE, data))
# mv_location_HIC_student <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data))
# mv_location_HIC_employment <- run$enqueue(location_contact(MCMC_parameters, c("part_age", "gender", "employment"), "HIC", TRUE, employment_data))

# uni_total_UMIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "gender", "UMIC", TRUE, data))
# uni_total_UMIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "student", "UMIC", TRUE, student_data))
# uni_total_noadd_LIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "age3cat", "LIC/LMIC", TRUE, data))
# uni_total_noadd_UMIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "hh_size", "UMIC", TRUE, data))
# uni_physical_UMIC_gender <- run$enqueue(physical_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
# uni_physical_UMIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "UMIC", FALSE, data))
# uni_physical_UMIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, "weekday", "UMIC", TRUE, data))
# uni_duration_LIC_student <- run$enqueue(duration_contact(MCMC_parameters, "student", "LIC/LMIC", TRUE, student_data))
# uni_duration_LIC_employment <- run$enqueue(duration_contact(MCMC_parameters, "employment", "LIC/LMIC", TRUE, employment_data))
# uni_duration_UMIC_age <- run$enqueue(duration_contact(MCMC_parameters, "age3cat", "UMIC", TRUE, data))
# uni_duration_UMIC_gender <- run$enqueue(duration_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
# uni_duration_UMIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "UMIC", FALSE, data))
# mv_physical_HIC_method <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "method"), "HIC", FALSE, data))
# mv_physical_HIC_student <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "HIC", TRUE, student_data))
# mv_physical_UMIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, c("age3cat", "gender", "hh_size"), "UMIC", TRUE, data))
# mv_physical_UMIC_student <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "student"), "UMIC", TRUE, student_data))
# mv_physical_UMIC_employment <- run$enqueue(physical_contact(MCMC_parameters, c("part_age", "gender", "employment"), "UMIC", TRUE, employment_data))
# 
# 
# uni_total_UMIC_gender$status()
# uni_total_UMIC_student$status()
# uni_total_noadd_LIC_age$status()
# uni_total_noadd_UMIC_hhsize$status()
# uni_physical_UMIC_gender$status()
# uni_physical_UMIC_method$status()
# uni_physical_UMIC_weekday$status()
# uni_duration_LIC_student$status()
# uni_duration_LIC_employment$status()
# uni_duration_UMIC_age$status()
# uni_duration_UMIC_gender$status()
# uni_duration_UMIC_method$status()
# mv_physical_HIC_method$status()
# mv_physical_HIC_student$status()
# mv_physical_UMIC_hhsize$status()
# mv_physical_UMIC_student$status()
# mv_physical_UMIC_employment$status()

# total_income_strata <- data %>%
#   group_by(income) %>%
#   summarise(total_income_participants = n())
# study_size <- data %>%
#   group_by(income, study) %>%
#   summarise(total_study_participants = n())
# number_studies <- data %>%
#   group_by(income) %>%
#   summarise(unique_studies = length(unique(study)))
# 
# data <- data %>%
#   left_join(total_income_strata, by = "income") %>%
#   left_join(study_size, by = c("income", "study")) %>%
#   left_join(number_studies, by = "income") %>%
#   mutate(average_study_size = round(total_income_participants/unique_studies, 0),
#          weight = average_study_size * (1/total_study_participants))
# sum(!is.na(data$gender) & !is.na(data$age3cat))
# covariates <- c("age3cat", "gender")
# filtered <- data 
# for (i in 1:length(covariates)) {
#   group_var <- rlang::sym(covariates[i])
#   temp <- filtered %>%
#     filter(!is.na(!!group_var))
#   filtered <- temp
# }
# sum(is.na(data$gender) & is.na(data$age3cat))
# sum(is.na(filtered$gender) & is.na(filtered$age3cat))

# # Univariate Analyses (Not Adjusting for Age or Gender)
# uni_total_LIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "age3cat", "LIC/LMIC", TRUE, data))
# uni_total_LIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "gender", "LIC/LMIC", TRUE, data))
# uni_total_LIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "method", "LIC/LMIC", FALSE, data))
# uni_total_LIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "weekday", "LIC/LMIC", TRUE, data))
# uni_total_LIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "hh_size", "LIC/LMIC", TRUE, data))
# uni_total_LIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "student", "LIC/LMIC", TRUE, student_data))
# uni_total_LIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "employment", "LIC/LMIC", TRUE, employment_data))
# 
# uni_total_UMIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "age3cat", "UMIC", TRUE, data))
# uni_total_UMIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "gender", "UMIC", TRUE, data))
# uni_total_UMIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "method", "UMIC", FALSE, data))
# uni_total_UMIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "weekday", "UMIC", TRUE, data))
# uni_total_UMIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "hh_size", "UMIC", TRUE, data))
# uni_total_UMIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "student", "UMIC", TRUE, student_data))
# uni_total_UMIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "employment", "UMIC", TRUE, employment_data))
# 
# uni_total_HIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "age3cat", "HIC", TRUE, data))
# uni_total_HIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "gender", "HIC", TRUE, data))
# uni_total_HIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "method", "HIC", FALSE, data))
# uni_total_HIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "weekday", "HIC", TRUE, data))
# uni_total_HIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "hh_size", "HIC", TRUE, data))
# uni_total_HIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "student", "HIC", TRUE, student_data))
# uni_total_HIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "employment", "HIC", TRUE, employment_data))
# 

# Univariate Analyses (Not Adjusting for Age or Gender)
# uni_total_LIC_age_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "age3cat", "LIC/LMIC", TRUE, data, TRUE))
# uni_total_LIC_gender_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "gender", "LIC/LMIC", TRUE, data, TRUE))
# uni_total_LIC_method_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "method", "LIC/LMIC", FALSE, data, TRUE))
# uni_total_LIC_weekday_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "weekday", "LIC/LMIC", TRUE, data, TRUE))
# uni_total_LIC_hhsize_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "hh_size", "LIC/LMIC", TRUE, data, TRUE))
# uni_total_LIC_student_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "student", "LIC/LMIC", TRUE, student_data, TRUE))
# uni_total_LIC_employment_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "employment", "LIC/LMIC", TRUE, employment_data, TRUE))
# 
# uni_total_UMIC_age_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "age3cat", "UMIC", TRUE, data, TRUE))
# uni_total_UMIC_gender_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "gender", "UMIC", TRUE, data, TRUE))
# uni_total_UMIC_method_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "method", "UMIC", FALSE, data, TRUE))
# uni_total_UMIC_weekday_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "weekday", "UMIC", TRUE, data, TRUE))
# uni_total_UMIC_hhsize_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "hh_size", "UMIC", TRUE, data, TRUE))
# uni_total_UMIC_student_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "student", "UMIC", TRUE, student_data, TRUE))
# uni_total_UMIC_employment_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "employment", "UMIC", TRUE, employment_data, TRUE))
# 
# uni_total_HIC_age_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "age3cat", "HIC", TRUE, data, TRUE))
# uni_total_HIC_gender_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "gender", "HIC", TRUE, data, TRUE))
# uni_total_HIC_method_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "method", "HIC", FALSE, data, TRUE))
# uni_total_HIC_weekday_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "weekday", "HIC", TRUE, data, TRUE))
# uni_total_HIC_hhsize_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "hh_size", "HIC", TRUE, data, TRUE))
# uni_total_HIC_student_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "student", "HIC", TRUE, student_data, TRUE))
# uni_total_HIC_employment_weighted <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts", "employment", "HIC", TRUE, employment_data, TRUE))

# Univariate Analyses (Not Adjusting for Age or Gender)
# uni_total_noadd_LIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "age3cat", "LIC/LMIC", TRUE, data))
# uni_total_noadd_LIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "gender", "LIC/LMIC", TRUE, data))
# uni_total_noadd_LIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "method", "LIC/LMIC", FALSE, data))
# uni_total_noadd_LIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "weekday", "LIC/LMIC", TRUE, data))
# uni_total_noadd_LIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "hh_size", "LIC/LMIC", TRUE, data))
# uni_total_noadd_LIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "student", "LIC/LMIC", TRUE, student_data))
# uni_total_noadd_LIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "employment", "LIC/LMIC", TRUE, employment_data))
# 
# uni_total_noadd_UMIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "age3cat", "UMIC", TRUE, data))
# uni_total_noadd_UMIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "gender", "UMIC", TRUE, data))
# uni_total_noadd_UMIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "method", "UMIC", FALSE, data))
# uni_total_noadd_UMIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "weekday", "UMIC", TRUE, data))
# uni_total_noadd_UMIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "hh_size", "UMIC", TRUE, data))
# uni_total_noadd_UMIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "student", "UMIC", TRUE, student_data))
# uni_total_noadd_UMIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "employment", "UMIC", TRUE, employment_data))
# 
# uni_total_noadd_HIC_age <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "age3cat", "HIC", TRUE, data))
# uni_total_noadd_HIC_gender <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "gender", "HIC", TRUE, data))
# uni_total_noadd_HIC_method <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "method", "HIC", FALSE, data))
# uni_total_noadd_HIC_weekday <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "weekday", "HIC", TRUE, data))
# uni_total_noadd_HIC_hhsize <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "hh_size", "HIC", TRUE, data))
# uni_total_noadd_HIC_student <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "student", "HIC", TRUE, student_data))
# uni_total_noadd_HIC_employment <- run$enqueue(total_contacts(MCMC_parameters, "tot_contacts_no_add", "employment", "HIC", TRUE, employment_data))


# Univariate Analyses (Not Adjusting for Age or Gender)
# uni_physical_LIC_age <- run$enqueue(physical_contact(MCMC_parameters, "age3cat", "LIC/LMIC", TRUE, data))
# uni_physical_LIC_gender <- run$enqueue(physical_contact(MCMC_parameters, "gender", "LIC/LMIC", TRUE, data))
# uni_physical_LIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "LIC/LMIC", FALSE, data))
# uni_physical_LIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, "weekday", "LIC/LMIC", TRUE, data))
# uni_physical_LIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, "hh_size", "LIC/LMIC", TRUE, data))
# uni_physical_LIC_student <- run$enqueue(physical_contact(MCMC_parameters, "student", "LIC/LMIC", TRUE, student_data))
# uni_physical_LIC_employment <- run$enqueue(physical_contact(MCMC_parameters, "employment", "LIC/LMIC", TRUE, employment_data))
# 
# uni_physical_UMIC_age <- run$enqueue(physical_contact(MCMC_parameters, "age3cat", "UMIC", TRUE, data))
# uni_physical_UMIC_gender <- run$enqueue(physical_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
# uni_physical_UMIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "UMIC", FALSE, data))
# uni_physical_UMIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, "weekday", "UMIC", TRUE, data))
# uni_physical_UMIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, "hh_size", "UMIC", TRUE, data))
# uni_physical_UMIC_student <- run$enqueue(physical_contact(MCMC_parameters, "student", "UMIC", TRUE, student_data))
# uni_physical_UMIC_employment <- run$enqueue(physical_contact(MCMC_parameters, "employment", "UMIC", TRUE, employment_data))
# 
# uni_physical_HIC_age <- run$enqueue(physical_contact(MCMC_parameters, "age3cat", "HIC", TRUE, data))
# uni_physical_HIC_gender <- run$enqueue(physical_contact(MCMC_parameters, "gender", "HIC", TRUE, data))
# uni_physical_HIC_method <- run$enqueue(physical_contact(MCMC_parameters, "method", "HIC", FALSE, data))
# uni_physical_HIC_weekday <- run$enqueue(physical_contact(MCMC_parameters, "weekday", "HIC", TRUE, data))
# uni_physical_HIC_hhsize <- run$enqueue(physical_contact(MCMC_parameters, "hh_size", "HIC", TRUE, data))
# uni_physical_HIC_student <- run$enqueue(physical_contact(MCMC_parameters, "student", "HIC", TRUE, student_data))
# uni_physical_HIC_employment <- run$enqueue(physical_contact(MCMC_parameters, "employment", "HIC", TRUE, employment_data))


# Univariate Analyses (Not Adjusting for Age or Gender)
# uni_duration_LIC_age <- run$enqueue(duration_contact(MCMC_parameters, "age3cat", "LIC/LMIC", TRUE, data))
# uni_duration_LIC_gender <- run$enqueue(duration_contact(MCMC_parameters, "gender", "LIC/LMIC", TRUE, data))
# uni_duration_LIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "LIC/LMIC", FALSE, data))
# uni_duration_LIC_weekday <- run$enqueue(duration_contact(MCMC_parameters, "weekday", "LIC/LMIC", TRUE, data))
# uni_duration_LIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, "hh_size", "LIC/LMIC", TRUE, data))
# uni_duration_LIC_student <- run$enqueue(duration_contact(MCMC_parameters, "student", "LIC/LMIC", TRUE, student_data))
# uni_duration_LIC_employment <- run$enqueue(duration_contact(MCMC_parameters, "employment", "LIC/LMIC", TRUE, employment_data))
# 
# uni_duration_UMIC_age <- run$enqueue(duration_contact(MCMC_parameters, "age3cat", "UMIC", TRUE, data))
# uni_duration_UMIC_gender <- run$enqueue(duration_contact(MCMC_parameters, "gender", "UMIC", TRUE, data))
# uni_duration_UMIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "UMIC", FALSE, data))
# uni_duration_UMIC_weekday <- run$enqueue(duration_contact(MCMC_parameters, "weekday", "UMIC", TRUE, data))
# uni_duration_UMIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, "hh_size", "UMIC", TRUE, data))
# uni_duration_UMIC_student <- run$enqueue(duration_contact(MCMC_parameters, "student", "UMIC", TRUE, student_data))
# uni_duration_UMIC_employment <- run$enqueue(duration_contact(MCMC_parameters, "employment", "UMIC", TRUE, employment_data))
# 
# uni_duration_HIC_age <- run$enqueue(duration_contact(MCMC_parameters, "age3cat", "HIC", TRUE, data))
# uni_duration_HIC_gender <- run$enqueue(duration_contact(MCMC_parameters, "gender", "HIC", TRUE, data))
# uni_duration_HIC_method <- run$enqueue(duration_contact(MCMC_parameters, "method", "HIC", FALSE, data))
# uni_duration_HIC_weekday <- run$enqueue(duration_contact(MCMC_parameters, "weekday", "HIC", TRUE, data))
# uni_duration_HIC_hhsize <- run$enqueue(duration_contact(MCMC_parameters, "hh_size", "HIC", TRUE, data))
# uni_duration_HIC_student <- run$enqueue(duration_contact(MCMC_parameters, "student", "HIC", TRUE, student_data))
# uni_duration_HIC_employment <- run$enqueue(duration_contact(MCMC_parameters, "employment", "HIC", TRUE, employment_data))
