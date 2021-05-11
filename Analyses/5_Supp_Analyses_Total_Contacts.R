# Loading required libraries
library(tidyverse)

# Sourcing required functions
source("Functions/brms_output_summary_functions.R")

# Load in model results
files <- list.files(path = paste0("Outputs/Multivariate/"))

# Total contacts
tot_age_gend_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_LIC_LMIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
tot_hh_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_LIC_LMIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
tot_meth_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_LIC_LMIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
tot_wk_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_LIC_LMIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
tot_stu_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_LIC_LMIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
tot_emp_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_LIC_LMIC_part_age_gender_employment_10000iter_2chains_2021-04-30.rds")))[4, , drop = FALSE]
tot_LIC <- data.frame(rbind(tot_age_gend_LIC, tot_hh_LIC, tot_meth_LIC, tot_wk_LIC, tot_stu_LIC,tot_emp_LIC))
tot_LIC$income <- "LIC"
tot_LIC$type <- "all_contacts"

tot_age_gend_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_UMIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
tot_hh_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_UMIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
tot_meth_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_UMIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
tot_wk_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_UMIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
tot_stu_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_UMIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
tot_emp_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_UMIC_part_age_gender_employment_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
tot_UMIC <- data.frame(rbind(tot_age_gend_UMIC, tot_hh_UMIC, tot_meth_UMIC, tot_wk_UMIC, tot_stu_UMIC, tot_emp_UMIC))
tot_UMIC$income <- "UMIC"
tot_UMIC$type <- "all_contacts"

tot_age_gend_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_HIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
tot_hh_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_HIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
tot_meth_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_HIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
tot_wk_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_HIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
tot_stu_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_HIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
tot_emp_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/total_HIC_part_age_gender_employment_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
tot_HIC <- data.frame(rbind(tot_age_gend_HIC, tot_hh_HIC, tot_meth_HIC, tot_wk_HIC, tot_stu_HIC, tot_emp_HIC))
tot_HIC$income <- "HIC"
tot_HIC$type <- "all_contacts"

tot_overall <- rbind(tot_LIC, tot_UMIC, tot_HIC)
tot_overall$variable <- rownames(tot_overall)
tot_overall <- tot_overall %>%
  pivot_longer(cols = c("X5.", "mean", "X95."))
tot_overall$income <- factor(tot_overall$income, levels = c("LIC", "UMIC", "HIC"))

# Total contacts without additional contacts
totnoadd_age_gend_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_LIC_LMIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
totnoadd_hh_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_LIC_LMIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
totnoadd_meth_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_LIC_LMIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
totnoadd_wk_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_LIC_LMIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
totnoadd_stu_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_LIC_LMIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
totnoadd_emp_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_LIC_LMIC_part_age_gender_employment_10000iter_2chains_2021-04-30.rds")))[4, , drop = FALSE]
totnoadd_LIC <- data.frame(rbind(totnoadd_age_gend_LIC, totnoadd_hh_LIC, totnoadd_meth_LIC, totnoadd_wk_LIC, totnoadd_stu_LIC, totnoadd_emp_LIC))
totnoadd_LIC$income <- "LIC"
totnoadd_LIC$type <- "not_all_contacts"

totnoadd_age_gend_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_UMIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
totnoadd_hh_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_UMIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
totnoadd_meth_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_UMIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
totnoadd_wk_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_UMIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
totnoadd_stu_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_UMIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
totnoadd_emp_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_UMIC_part_age_gender_employment_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
totnoadd_UMIC <- data.frame(rbind(totnoadd_age_gend_UMIC, totnoadd_hh_UMIC, totnoadd_meth_UMIC, totnoadd_wk_UMIC, totnoadd_stu_UMIC, totnoadd_emp_UMIC))
totnoadd_UMIC$income <- "UMIC"
totnoadd_UMIC$type <- "not_all_contacts"

totnoadd_age_gend_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_HIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
totnoadd_hh_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_HIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
totnoadd_meth_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_HIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
totnoadd_wk_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_HIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
totnoadd_stu_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_HIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
totnoadd_emp_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariate/totalnoadd_HIC_part_age_gender_employment_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
totnoadd_HIC <- data.frame(rbind(totnoadd_age_gend_HIC, totnoadd_hh_HIC, totnoadd_meth_HIC, totnoadd_wk_HIC, totnoadd_stu_HIC, totnoadd_emp_HIC))
totnoadd_HIC$income <- "HIC"
totnoadd_HIC$type <- "not_all_contacts"

totnoadd_overall <- rbind(totnoadd_LIC, totnoadd_UMIC, totnoadd_HIC)
totnoadd_overall$variable <- rownames(totnoadd_overall)
totnoadd_overall <- totnoadd_overall %>%
  pivot_longer(cols = c("X5.", "mean", "X95."))
totnoadd_overall$income <- factor(totnoadd_overall$income, levels = c("LIC", "UMIC", "HIC"))

overall <- rbind(tot_overall, totnoadd_overall) %>%
  pivot_wider(names_from = c(type, name),
              values_from = value)

ggplot(overall) +
  geom_point(aes(x = all_contacts_mean, y = not_all_contacts_mean)) +
  geom_errorbar(aes(x = all_contacts_mean, y = not_all_contacts_mean, 
                    ymin = `not_all_contacts_X5.`, ymax = `not_all_contacts_X95.`)) +
  geom_errorbarh(aes(x = all_contacts_mean, y = not_all_contacts_mean, 
                    xmin = `all_contacts_X5.`, xmax = `all_contacts_X95.`)) +
  facet_wrap(. ~ income, scales = "free")

x <- overall %>%
  select(income, all_contacts_mean, not_all_contacts_mean) %>%
  group_by(income) %>%
  summarise(correlation = cor(all_contacts_mean, not_all_contacts_mean))
