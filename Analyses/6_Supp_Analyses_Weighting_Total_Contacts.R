# Loading required libraries
library(tidyverse)

# Sourcing required functions
source("Functions/brms_output_summary_functions.R")

# Load in model results
files <- list.files(path = paste0("Outputs/Multivariable/"))

# Total contacts
unw_tot_age_gend_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_LIC_LMIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
unw_tot_hh_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_LIC_LMIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
unw_tot_meth_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_LIC_LMIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
unw_tot_wk_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_LIC_LMIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
unw_tot_stu_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_LIC_LMIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
unw_tot_emp_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_LIC_LMIC_part_age_gender_employment_10000iter_2chains_2021-04-30.rds")))[4, , drop = FALSE]
unw_tot_LIC <- data.frame(rbind(unw_tot_age_gend_LIC, unw_tot_hh_LIC, unw_tot_meth_LIC, unw_tot_wk_LIC, unw_tot_stu_LIC,unw_tot_emp_LIC))
unw_tot_LIC$income <- "LIC"
unw_tot_LIC$type <- "unweighted"

unw_tot_age_gend_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_UMIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
unw_tot_hh_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_UMIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
unw_tot_meth_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_UMIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
unw_tot_wk_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_UMIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
unw_tot_stu_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_UMIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
unw_tot_emp_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_UMIC_part_age_gender_employment_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
totunw__UMIC <- data.frame(rbind(unw_tot_age_gend_UMIC, unw_tot_hh_UMIC, unw_tot_meth_UMIC, unw_tot_wk_UMIC, unw_tot_stu_UMIC, unw_tot_emp_UMIC))
unw_tot_UMIC$income <- "UMIC"
unw_tot_UMIC$type <- "unweighted"

unw_tot_age_gend_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_HIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
unw_tot_hh_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_HIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
unw_tot_meth_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_HIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
unw_tot_wk_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_HIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
unw_tot_stu_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_HIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
unw_tot_emp_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/total_HIC_part_age_gender_employment_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
unw_tot_HIC <- data.frame(rbind(unw_tot_age_gend_HIC, unw_tot_hh_HIC, unw_tot_meth_HIC, unw_tot_wk_HIC, unw_tot_stu_HIC, unw_tot_emp_HIC))
unw_tot_HIC$income <- "HIC"
unw_tot_HIC$type <- "unweighted"

unw_tot_overall <- rbind(unw_tot_LIC, unw_tot_UMIC, unw_tot_HIC)
unw_tot_overall$variable <- rownames(unw_tot_overall)
unw_tot_overall <- unw_tot_overall %>%
  pivot_longer(cols = c("X5.", "mean", "X95."))
unw_tot_overall$income <- factor(unw_tot_overall$income, levels = c("LIC", "UMIC", "HIC"))

# Total contacts without additional contacts
wei_tot_age_gend_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_LIC_LMIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
wei_tot_hh_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_LIC_LMIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
wei_tot_meth_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_LIC_LMIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
wei_tot_wk_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_LIC_LMIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
wei_tot_stu_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_LIC_LMIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
wei_tot_emp_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_LIC_LMIC_part_age_gender_employment_10000iter_2chains_2021-04-30.rds")))[4, , drop = FALSE]
wei_tot_LIC <- data.frame(rbind(wei_tot_age_gend_LIC, wei_tot_hh_LIC, wei_tot_meth_LIC, wei_tot_wk_LIC, wei_tot_stu_LIC, wei_tot_emp_LIC))
wei_tot_LIC$income <- "LIC"
wei_tot_LIC$type <- "weighted"

wei_tot_age_gend_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_UMIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
wei_tot_hh_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_UMIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
wei_tot_meth_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_UMIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
wei_tot_wk_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_UMIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
wei_tot_stu_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_UMIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
wei_tot_emp_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_UMIC_part_age_gender_employment_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
wei_tot_UMIC <- data.frame(rbind(wei_tot_age_gend_UMIC, wei_tot_hh_UMIC, wei_tot_meth_UMIC, wei_tot_wk_UMIC, wei_tot_stu_UMIC, wei_tot_emp_UMIC))
wei_tot_UMIC$income <- "UMIC"
wei_tot_UMIC$type <- "weighted"

wei_tot_age_gend_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_HIC_age3cat_gender_10000iter_2chains_2021-04-29.rds")))[2:4, ]
wei_tot_hh_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_HIC_age3cat_gender_hh_size_10000iter_2chains_2021-04-29.rds")))[5:9, ]
wei_tot_meth_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_HIC_age3cat_gender_method_10000iter_2chains_2021-04-28.rds")))[5, , drop = FALSE]
wei_tot_wk_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_HIC_age3cat_gender_weekday_10000iter_2chains_2021-04-29.rds")))[5, , drop = FALSE]
wei_tot_stu_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_HIC_part_age_gender_student_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
wei_tot_emp_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_total_HIC_part_age_gender_employment_10000iter_2chains_2021-04-29.rds")))[4, , drop = FALSE]
wei_tot_HIC <- data.frame(rbind(wei_tot_age_gend_HIC, wei_tot_hh_HIC, wei_tot_meth_HIC, wei_tot_wk_HIC, wei_tot_stu_HIC, wei_tot_emp_HIC))
wei_tot_HIC$income <- "HIC"
wei_tot_HIC$type <- "weighted"

wi_tot_overall <- rbind(wei_tot_LIC, wei_tot_UMIC, wei_tot_HIC)
wi_tot_overall$variable <- rownames(wi_tot_overall)
wi_tot_overall <- wi_tot_overall %>%
  pivot_longer(cols = c("X5.", "mean", "X95."))
wi_tot_overall$income <- factor(wi_tot_overall$income, levels = c("LIC", "UMIC", "HIC"))

overall <- rbind(tot_overall, wi_tot_overall) %>%
  pivot_wider(names_from = c(type, name), values_from = value)

ggplot(overall) +
  geom_point(aes(x = unweighted, y = weighted)) +
  geom_errorbar(aes(x = unweighted, y = weighted, 
                    ymin = `weighted_X5.`, ymax = `weighted_X95.`)) +
  geom_errorbarh(aes(x = unweighted, y = weighted, 
                    xmin = `unweighted_X5.`, xmax = `unweighted_X95.`)) +
  facet_wrap(. ~ income, scales = "free")

x <- overall %>%
  select(income, unweighted, weighted) %>%
  group_by(income) %>%
  summarise(correlation = cor(unweighted, weighted))
