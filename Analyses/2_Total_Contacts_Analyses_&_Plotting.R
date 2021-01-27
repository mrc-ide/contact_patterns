# Loading Required Libraries
library(forestplot); library(tidyverse); library(friendlyeval)

# Sourcing Required Function
source("Functions/brms_output_summary_functions.R")

# Loading In Data
data <- read.csv("Data/combined_participant_lvl_all.csv") %>%
  mutate(hh_size = case_when(hh_size == 1 ~ "1", hh_size == 2 ~ "2",
                             hh_size == 3 ~ "3", hh_size == 4 ~ "4", 
                             hh_size == 5 ~ "5", hh_size >= 6 ~ "6+", TRUE ~ NA_character_),
         hh_size = factor(hh_size)) 

# Loading In Relevant Model Outputs
metric <- "total" # out of "total", "location", "physical" and "duration"
income_strata <- "HIC" # out of "LIC_LMIC", "UMIC" and "HIC"
files <- list.files(path = "Outputs/")

hh <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_hh_size"), files)]))
age <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_age3cat"), files)]))
gender <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_gender"), files)]))
student <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_student"), files)]))
method <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_method"), files)]))
weekday <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_weekday"), files)]))
employment <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_employment"), files)]))

# Extract Model Coefficients
age_coef <- total_summarise_coefs(age)
gender_coef <- total_summarise_coefs(gender)
weekday_coef <- total_summarise_coefs(weekday)
hh_coef <- total_summarise_coefs(hh)
method_coef <- total_summarise_coefs(method)
student_coef <- total_summarise_coefs(student)
employment_coef <- total_summarise_coefs(employment)

# Generating Empirical Summaries
age_emp <- total_empirical_summaries(data, "age3cat", income_strata)
gender_emp <- total_empirical_summaries(data, "part_gender", income_strata)
weekday_emp <- total_empirical_summaries(data, "weekday", income_strata)
hh_emp <- total_empirical_summaries(data, "hh_size", income_strata)
method_emp <- total_empirical_summaries(data, "method", income_strata)
student_emp <- total_empirical_summaries(data, "student", income_strata)
employment_emp <- total_empirical_summaries(data, "employment", income_strata)

# Forest Plotting
age_text <- c("", "Study", "Age Group", "0-15yrs", "15-49yrs", "49+yrs")
gender_text <- c(NA, "Gender", "Female", "Male")
weekday_text <- c(NA, "Weekday", "No", "Yes")
hh_text <- c(NA, "Household Size", "1", "2", "3", "4", "5", "6+")
method_text <- c(NA, "Method", "Diary", "Interview")
student_text <- c(NA, "Student?", "No", "Yes")
employment_text <- c(NA, "Employment?", "No", "Yes", NA)
full_text <- c(age_text, gender_text, weekday_text, hh_text, method_text, student_text, employment_text)

age_avg <- gen_avgs(age_emp)
gender_avg <- gen_avgs(gender_emp)
weekday_avg <- gen_avgs(weekday_emp)
hh_size_avg <- gen_avgs(hh_emp)
method_avg <- gen_avgs(method_emp)
student_avg <- gen_avgs(student_emp)
employment_avg <- gen_avgs(employment_emp)

age_ORs <- gen_RRs(age_coef)
gender_ORs <- gen_RRs(gender_coef)
weekday_ORs <- gen_RRs(weekday_coef)
hh_size_ORs <- gen_RRs(hh_coef)
method_ORs <- gen_RRs(method_coef)
student_ORs <- gen_RRs(student_coef)
employment_ORs <-gen_RRs(employment_coef)

tabletext <- cbind(
  unname(full_text),
  c(NA, "N", NA, age_emp$n, NA, NA, gender_emp$n, NA, NA, weekday_emp$n, NA, NA, hh_emp$n, 
    NA, NA, method_emp$n, NA, NA, student_emp$n, NA, NA, employment_emp$n, NA),
  c("Average", "Contacts (95% CI)", NA, age_avg, NA, NA, gender_avg, NA, NA, weekday_avg, NA, NA, hh_size_avg, 
    NA, NA, method_avg, NA, NA, student_avg, NA, NA, employment_avg, NA),
  c("Rate Ratio", "(95% CI)", NA, age_ORs, NA, NA, gender_ORs, NA, NA, weekday_ORs, NA, NA, hh_size_ORs, 
    NA, NA, method_ORs, NA, NA, student_ORs, NA, NA, employment_ORs, NA))

forest_data_to_plot <- 
  structure(list(
    mean  = c(NA, NA, NA, unname(c(1, unlist(age_coef[2, ]))), 
              NA, NA, unname(c(1, unlist(gender_coef[2, ]))), 
              NA, NA, unname(c(1, unlist(weekday_coef[2, ]))), 
              NA, NA, unname(c(1, unlist(hh_coef[2, ]))),
              NA, NA, unname(c(1, unlist(method_coef[2,  ]))), 
              NA, NA, unname(c(1, unlist(student_coef[2, ]))), 
              NA, NA, unname(c(1, unlist(employment_coef[2, ]))), NA),
    lower = c(NA, NA, NA, unname(c(1, unlist(age_coef[1, ]))), 
              NA, NA, unname(c(1, unlist(gender_coef[1, ]))),
              NA, NA, unname(c(1, unlist(weekday_coef[1, ]))), 
              NA, NA, unname(c(1, unlist(hh_coef[1, ]))),
              NA, NA, unname(c(1, unlist(method_coef[1,  ]))), 
              NA, NA, unname(c(1, unlist(student_coef[1, ]))), 
              NA, NA, unname(c(1, unlist(employment_coef[1, ]))), NA),
    upper = c(NA, NA, NA, unname(c(1, unlist(age_coef[3, ]))), 
              NA, NA, unname(c(1, unlist(gender_coef[3, ]))), 
              NA, NA, unname(c(1, unlist(weekday_coef[3, ]))), 
              NA, NA, unname(c(1, unlist(hh_coef[3, ]))),
              NA, NA, unname(c(1, unlist(method_coef[3,  ]))), 
              NA, NA, unname(c(1, unlist(student_coef[3, ]))), 
              NA, NA, unname(c(1, unlist(employment_coef[3, ]))), NA)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -35L), # note 12 has to match up to length
    class = "data.frame")

forestplot(tabletext, 
           graph.pos = 4,
           forest_data_to_plot,
           hrzl_lines = list("3" = gpar(lwd=1)),
           txt_gp = fpTxtGp(label = gpar(cex=0.8),
                            ticks = gpar(cex=0.7),
                            xlab  = gpar(cex=0.8)),
           boxsize = 0.4,
           new_page = TRUE,
           align = c("l", "c", "c", "c", "c", "c","r"),
           is.summary = c(TRUE, TRUE, TRUE, rep(FALSE, 4), TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 7), TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 4)),
           col = fpColors(box = "#003f5c", line = "#003f5c", summary = "#003f5c", hrz_lines = "#444444"), 
           colgap = unit(0, "mm"), 
           cex = 0.4,
           lineheight = unit(0.5, "cm"),
           xlab = c("Contact Rate Ratio"),
           graphwidth = unit(100, "mm"),
           xticks = c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2),
           ci.vertices = TRUE,
           xlog = FALSE, 
           zero = 0.5,
           clip = c(0.5, 2))
