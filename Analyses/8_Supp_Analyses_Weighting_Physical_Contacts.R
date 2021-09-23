# Loading required libraries
library(tidyverse); library(rstati)

# Sourcing required functions
source("Functions/brms_output_summary_functions.R")

# physical contacts
unw_phys_age_gend_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_LIC_LMIC_age3cat_gender_10000iter_2chains_2021-05-04.rds")))[2:4, ]
unw_phys_hh_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_LIC_LMIC_age3cat_gender_hh_size_10000iter_2chains_2021-05-04.rds")))[5:9, ]
unw_phys_meth_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_LIC_LMIC_age3cat_gender_method_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_phys_wk_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_LIC_LMIC_age3cat_gender_weekday_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_phys_stu_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_LIC_LMIC_part_age_gender_student_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_phys_emp_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_LIC_LMIC_part_age_gender_employment_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_phys_LIC <- data.frame(rbind(unw_phys_age_gend_LIC, unw_phys_hh_LIC, unw_phys_meth_LIC, unw_phys_wk_LIC, unw_phys_stu_LIC,unw_phys_emp_LIC))
unw_phys_LIC$income <- "LIC"
unw_phys_LIC$type <- "unweighted"

unw_phys_age_gend_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_UMIC_age3cat_gender_10000iter_2chains_2021-05-04.rds")))[2:4, ]
unw_phys_hh_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_UMIC_age3cat_gender_hh_size_10000iter_2chains_2021-05-04.rds")))[5:9, ]
unw_phys_meth_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_UMIC_age3cat_gender_method_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_phys_wk_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_UMIC_age3cat_gender_weekday_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_phys_stu_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_UMIC_part_age_gender_student_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_phys_emp_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_UMIC_part_age_gender_employment_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_phys_UMIC <- data.frame(rbind(unw_phys_age_gend_UMIC, unw_phys_hh_UMIC, unw_phys_meth_UMIC, unw_phys_wk_UMIC, unw_phys_stu_UMIC, unw_phys_emp_UMIC))
unw_phys_UMIC$income <- "UMIC"
unw_phys_UMIC$type <- "unweighted"

unw_phys_age_gend_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_HIC_age3cat_gender_10000iter_2chains_2021-05-04.rds")))[2:4, ]
unw_phys_hh_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_HIC_age3cat_gender_hh_size_10000iter_2chains_2021-05-04.rds")))[5:9, ]
unw_phys_meth_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_HIC_age3cat_gender_method_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_phys_wk_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_HIC_age3cat_gender_weekday_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_phys_stu_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_HIC_part_age_gender_student_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_phys_emp_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/physical_HIC_part_age_gender_employment_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_phys_HIC <- data.frame(rbind(unw_phys_age_gend_HIC, unw_phys_hh_HIC, unw_phys_meth_HIC, unw_phys_wk_HIC, unw_phys_stu_HIC, unw_phys_emp_HIC))
unw_phys_HIC$income <- "HIC"
unw_phys_HIC$type <- "unweighted"

unw_phys_overall <- rbind(unw_phys_LIC, unw_phys_UMIC, unw_phys_HIC)
unw_phys_overall$variable <- rownames(unw_phys_overall)
unw_phys_overall <- unw_phys_overall %>%
  pivot_longer(cols = c("X5.", "mean", "X95."))
unw_phys_overall$income <- factor(unw_phys_overall$income, levels = c("LIC", "UMIC", "HIC"))

# physical contacts without additional contacts
wei_phys_age_gend_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_LIC_LMIC_age3cat_gender_10000iter_2chains_2021-09-22.rds")))[2:4, ]
wei_phys_hh_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_LIC_LMIC_age3cat_gender_hh_size_10000iter_2chains_2021-09-22.rds")))[5:9, ]
wei_phys_meth_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_LIC_LMIC_age3cat_gender_method_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_phys_wk_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_LIC_LMIC_age3cat_gender_weekday_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_phys_stu_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_LIC_LMIC_part_age_gender_student_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_phys_emp_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_LIC_LMIC_part_age_gender_employment_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_phys_LIC <- data.frame(rbind(wei_phys_age_gend_LIC, wei_phys_hh_LIC, wei_phys_meth_LIC, wei_phys_wk_LIC, wei_phys_stu_LIC, wei_phys_emp_LIC))
wei_phys_LIC$income <- "LIC"
wei_phys_LIC$type <- "weighted"

wei_phys_age_gend_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_UMIC_age3cat_gender_10000iter_2chains_2021-09-22.rds")))[2:4, ]
wei_phys_hh_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_UMIC_age3cat_gender_hh_size_10000iter_2chains_2021-09-22.rds")))[5:9, ]
wei_phys_meth_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_UMIC_age3cat_gender_method_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_phys_wk_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_UMIC_age3cat_gender_weekday_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_phys_stu_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_UMIC_part_age_gender_student_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_phys_emp_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_UMIC_part_age_gender_employment_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_phys_UMIC <- data.frame(rbind(wei_phys_age_gend_UMIC, wei_phys_hh_UMIC, wei_phys_meth_UMIC, wei_phys_wk_UMIC, wei_phys_stu_UMIC, wei_phys_emp_UMIC))
wei_phys_UMIC$income <- "UMIC"
wei_phys_UMIC$type <- "weighted"

wei_phys_age_gend_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_HIC_age3cat_gender_10000iter_2chains_2021-09-22.rds")))[2:4, ]
wei_phys_hh_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_HIC_age3cat_gender_hh_size_10000iter_2chains_2021-09-22.rds")))[5:9, ]
wei_phys_meth_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_HIC_age3cat_gender_method_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_phys_wk_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_HIC_age3cat_gender_weekday_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_phys_stu_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_HIC_part_age_gender_student_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_phys_emp_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_physical_HIC_part_age_gender_employment_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_phys_HIC <- data.frame(rbind(wei_phys_age_gend_HIC, wei_phys_hh_HIC, wei_phys_meth_HIC, wei_phys_wk_HIC, wei_phys_stu_HIC, wei_phys_emp_HIC))
wei_phys_HIC$income <- "HIC"
wei_phys_HIC$type <- "weighted"

wi_phys_overall <- rbind(wei_phys_LIC, wei_phys_UMIC, wei_phys_HIC)
wi_phys_overall$variable <- rownames(wi_phys_overall)
wi_phys_overall <- wi_phys_overall %>%
  pivot_longer(cols = c("X5.", "mean", "X95."))
wi_phys_overall$income <- factor(wi_phys_overall$income, levels = c("LIC", "UMIC", "HIC"))

overall <- rbind(wi_phys_overall, unw_phys_overall) %>%
  pivot_wider(names_from = c(type, name), values_from = value)

ggplot(overall) +
  geom_point(aes(x = unweighted_mean, y = weighted_mean)) +
  geom_errorbar(aes(x = unweighted_mean, y = weighted_mean, 
                    ymin = `weighted_X5.`, ymax = `weighted_X95.`)) +
  geom_errorbarh(aes(x = unweighted_mean, y = weighted_mean, 
                     xmin = `unweighted_X5.`, xmax = `unweighted_X95.`)) +
  geom_abline(intercept = 0) +
  facet_wrap(. ~ income, scales = "free")

x <- overall %>%
  select(income, unweighted_mean, weighted_mean) %>%
  group_by(income) %>%
  summarise(correlation = cor(unweighted_mean, weighted_mean))

res.fried <- rbind(wi_phys_overall, unw_phys_overall) %>% 
  filter(name == "mean") %>%
  friedman_test(value ~ type |variable)
res.fried

res.fried_income <- rbind(wi_phys_overall, unw_phys_overall) %>% 
  filter(name == "mean") %>%
  group_by(income) %>%
  friedman_test(value ~ type |variable)
res.fried_income
