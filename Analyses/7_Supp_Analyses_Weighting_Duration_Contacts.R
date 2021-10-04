# Loading required libraries
library(tidyverse); library(rstatix)

# Sourcing required functions
source("Functions/brms_output_summary_functions.R")

# duration contacts
unw_dur_age_gend_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_LIC_LMIC_age3cat_gender_10000iter_2chains_2021-05-04.rds")))[2:4, ]
unw_dur_hh_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_LIC_LMIC_age3cat_gender_hh_size_10000iter_2chains_2021-05-04.rds")))[5:9, ]
unw_dur_meth_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_LIC_LMIC_age3cat_gender_method_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_dur_wk_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_LIC_LMIC_age3cat_gender_weekday_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_dur_stu_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_LIC_LMIC_part_age_gender_student_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_dur_emp_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_LIC_LMIC_part_age_gender_employment_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_dur_LIC <- data.frame(rbind(unw_dur_age_gend_LIC, unw_dur_hh_LIC, unw_dur_meth_LIC, unw_dur_wk_LIC, unw_dur_stu_LIC,unw_dur_emp_LIC))
unw_dur_LIC$income <- "LIC/LMIC"
unw_dur_LIC$type <- "unweighted"

unw_dur_age_gend_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_UMIC_age3cat_gender_10000iter_2chains_2021-05-04.rds")))[2:4, ]
unw_dur_hh_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_UMIC_age3cat_gender_hh_size_10000iter_2chains_2021-05-04.rds")))[5:9, ]
unw_dur_meth_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_UMIC_age3cat_gender_method_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_dur_wk_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_UMIC_age3cat_gender_weekday_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_dur_stu_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_UMIC_part_age_gender_student_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_dur_emp_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_UMIC_part_age_gender_employment_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_dur_UMIC <- data.frame(rbind(unw_dur_age_gend_UMIC, unw_dur_hh_UMIC, unw_dur_meth_UMIC, unw_dur_wk_UMIC, unw_dur_stu_UMIC, unw_dur_emp_UMIC))
unw_dur_UMIC$income <- "UMIC"
unw_dur_UMIC$type <- "unweighted"

unw_dur_age_gend_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_HIC_age3cat_gender_10000iter_2chains_2021-05-04.rds")))[2:4, ]
unw_dur_hh_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_HIC_age3cat_gender_hh_size_10000iter_2chains_2021-05-04.rds")))[5:9, ]
unw_dur_meth_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_HIC_age3cat_gender_method_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_dur_wk_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_HIC_age3cat_gender_weekday_10000iter_2chains_2021-05-04.rds")))[5, , drop = FALSE]
unw_dur_stu_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_HIC_part_age_gender_student_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_dur_emp_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Unweighted/duration_HIC_part_age_gender_employment_10000iter_2chains_2021-05-04.rds")))[4, , drop = FALSE]
unw_dur_HIC <- data.frame(rbind(unw_dur_age_gend_HIC, unw_dur_hh_HIC, unw_dur_meth_HIC, unw_dur_wk_HIC, unw_dur_stu_HIC, unw_dur_emp_HIC))
unw_dur_HIC$income <- "HIC"
unw_dur_HIC$type <- "unweighted"

unw_dur_overall <- rbind(unw_dur_LIC, unw_dur_UMIC, unw_dur_HIC)
unw_dur_overall$variable <- rownames(unw_dur_overall)
unw_dur_overall <- unw_dur_overall %>%
  pivot_longer(cols = c("X5.", "mean", "X95."))
unw_dur_overall$income <- factor(unw_dur_overall$income, levels = c("LIC/LMIC", "UMIC", "HIC"))

# duration contacts without additional contacts
wei_dur_age_gend_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_LIC_LMIC_age3cat_gender_10000iter_2chains_2021-09-22.rds")))[2:4, ]
wei_dur_hh_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_LIC_LMIC_age3cat_gender_hh_size_10000iter_2chains_2021-09-22.rds")))[5:9, ]
wei_dur_meth_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_LIC_LMIC_age3cat_gender_method_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_dur_wk_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_LIC_LMIC_age3cat_gender_weekday_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_dur_stu_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_LIC_LMIC_part_age_gender_student_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_dur_emp_LIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_LIC_LMIC_part_age_gender_employment_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_dur_LIC <- data.frame(rbind(wei_dur_age_gend_LIC, wei_dur_hh_LIC, wei_dur_meth_LIC, wei_dur_wk_LIC, wei_dur_stu_LIC, wei_dur_emp_LIC))
wei_dur_LIC$income <- "LIC/LMIC"
wei_dur_LIC$type <- "weighted"

wei_dur_age_gend_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_UMIC_age3cat_gender_10000iter_2chains_2021-09-22.rds")))[2:4, ]
wei_dur_hh_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_UMIC_age3cat_gender_hh_size_10000iter_2chains_2021-09-22.rds")))[5:9, ]
wei_dur_meth_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_UMIC_age3cat_gender_method_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_dur_wk_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_UMIC_age3cat_gender_weekday_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_dur_stu_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_UMIC_part_age_gender_student_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_dur_emp_UMIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_UMIC_part_age_gender_employment_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_dur_UMIC <- data.frame(rbind(wei_dur_age_gend_UMIC, wei_dur_hh_UMIC, wei_dur_meth_UMIC, wei_dur_wk_UMIC, wei_dur_stu_UMIC, wei_dur_emp_UMIC))
wei_dur_UMIC$income <- "UMIC"
wei_dur_UMIC$type <- "weighted"

wei_dur_age_gend_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_HIC_age3cat_gender_10000iter_2chains_2021-09-22.rds")))[2:4, ]
wei_dur_hh_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_HIC_age3cat_gender_hh_size_10000iter_2chains_2021-09-22.rds")))[5:9, ]
wei_dur_meth_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_HIC_age3cat_gender_method_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_dur_wk_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_HIC_age3cat_gender_weekday_10000iter_2chains_2021-09-22.rds")))[5, , drop = FALSE]
wei_dur_stu_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_HIC_part_age_gender_student_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_dur_emp_HIC <- t(summarise_coefs(readRDS("Outputs/Multivariable/Weighted/weighted_duration_HIC_part_age_gender_employment_10000iter_2chains_2021-09-22.rds")))[4, , drop = FALSE]
wei_dur_HIC <- data.frame(rbind(wei_dur_age_gend_HIC, wei_dur_hh_HIC, wei_dur_meth_HIC, wei_dur_wk_HIC, wei_dur_stu_HIC, wei_dur_emp_HIC))
wei_dur_HIC$income <- "HIC"
wei_dur_HIC$type <- "weighted"

wi_dur_overall <- rbind(wei_dur_LIC, wei_dur_UMIC, wei_dur_HIC)
wi_dur_overall$variable <- rownames(wi_dur_overall)
wi_dur_overall <- wi_dur_overall %>%
  pivot_longer(cols = c("X5.", "mean", "X95."))
wi_dur_overall$income <- factor(wi_dur_overall$income, levels = c("LIC/LMIC", "UMIC", "HIC"))

overall <- rbind(wi_dur_overall, unw_dur_overall) %>%
  pivot_wider(names_from = c(type, name), values_from = value)

ggplot(overall) +
  geom_point(aes(x = unweighted_mean, y = weighted_mean)) +
  geom_errorbar(aes(x = unweighted_mean, y = weighted_mean, 
                    ymin = `weighted_X5.`, ymax = `weighted_X95.`)) +
  geom_errorbarh(aes(x = unweighted_mean, y = weighted_mean, 
                     xmin = `unweighted_X5.`, xmax = `unweighted_X95.`)) +
  geom_abline(intercept = 0) +
  facet_wrap(. ~ income, scales = "free") +
  geom_abline(slope=1) +
  labs( x="Odds Ratio (main analysis)", y="Odds Ratio (sensitivity analysis)", title="(C) Duration of contact") +
  theme( axis.text = element_text( size = 12 ),    ##y axis label size
         axis.text.x = element_text( size = 12 ),   ##x axis label size
         axis.title = element_text( size = 14), # Axis titles size
         strip.text = element_text(size = 15), ##facet title size
         plot.title = element_text(size = 18)) 

x <- overall %>%
  select(income, unweighted_mean, weighted_mean) %>%
  group_by(income) %>%
  summarise(correlation = cor(unweighted_mean, weighted_mean), 
            ranked_correlation = cor(unweighted_mean, weighted_mean, method = "spearman"))


res.fried <- rbind(wi_dur_overall, unw_dur_overall) %>% 
  filter(name == "mean") %>%
  friedman_test(value ~ type |variable)
res.fried

res.fried_income <- rbind(wi_dur_overall, unw_dur_overall) %>% 
  filter(name == "mean") %>%
  group_by(income) %>%
  friedman_test(value ~ type |variable)
res.fried_income
