#######################################################
# Functions for Total Contacts
#######################################################
total_summarise_coefs <- function(model_output, CI_choice =  c(0.05, 0.95)) {
  mean <- apply(exp(brms::fixef(model_output$fitting_output, summary = FALSE)), 2, mean)
  CIs <- apply(exp(brms::fixef(model_output$fitting_output, summary = FALSE)), 2, quantile, CI_choice)
  overall <- rbind(CIs[1, ], mean, CIs[2, ])
  row.names(overall) <- c(paste0(CI_choice[1] * 100, "%"), "mean", paste0(CI_choice[2] * 100, "%"))
  overall <- overall[, -1]
  return(as.data.frame(overall))
}
total_empirical_summaries <- function(data, variable, income_strata) {
  temp <- data %>%
    mutate(income = ifelse(income == "LIC/LMIC", "LIC_LMIC", income)) %>%
    filter(income == income_strata)
  summary <- temp %>%
    filter(!is.na(!!treat_input_as_col(variable)) & !!treat_input_as_col(variable) != "") %>%
    group_by(!!treat_input_as_col(variable)) %>%
    summarise(mean = mean(tot_all, na.rm = TRUE),
              lower = mean(tot_all, na.rm = TRUE) - sd(tot_all, na.rm = TRUE)/n(),
              upper = mean(tot_all, na.rm = TRUE) + sd(tot_all, na.rm = TRUE)/n(),
              n = n())
  return(summary)
}

total_generate_forestplot_data <- function(data, income_strata) {
  
  # Filtering Data to Remove Not Included During Model Fitting
  data <- data %>%
    filter(!is.na(tot_all))
  
  # Loading Data 
  files <- list.files(path = "Outputs/")
  hh <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0("total", "_", income_strata, "_hh_size"), files)]))
  age <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0("total", "_", income_strata, "_age3cat"), files)]))
  gender <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0("total", "_", income_strata, "_gender"), files)]))
  student <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0("total", "_", income_strata, "_student"), files)]))
  method <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0("total", "_", income_strata, "_method"), files)]))
  weekday <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0("total", "_", income_strata, "_weekday"), files)]))
  employment <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0("total", "_", income_strata, "_employment"), files)]))
  
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
      row.names = c(NA, -length(full_text)), # note 12 has to match up to length
      class = "data.frame")
  
  return(list(tabletext = tabletext,
              forest_data_to_plot = forest_data_to_plot))
}

#######################################################
# Functions for Physical Contact Proportion
#######################################################
physical_summarise_coefs <- function(model_output, CI_choice =  c(0.05, 0.95)) {
  mean <- apply(exp(brms::fixef(model_output$fitting_output, summary = FALSE)), 2, mean)
  CIs <- apply(exp(brms::fixef(model_output$fitting_output, summary = FALSE)), 2, quantile, CI_choice)
  overall <- rbind(CIs[1, ], mean, CIs[2, ])
  row.names(overall) <- c(paste0(CI_choice[1] * 100, "%"), "mean", paste0(CI_choice[2] * 100, "%"))
  overall <- overall[, -1]
  return(as.data.frame(overall))
}

physical_empirical_summaries <- function(data, variable, income_strata) {
  temp <- data %>%
    mutate(income = ifelse(income == "LIC/LMIC", "LIC_LMIC", income)) %>%
    filter(income == income_strata)
  summary <- temp %>%
    filter(!is.na(!!treat_input_as_col(variable)) & !!treat_input_as_col(variable) != "") %>%
    group_by(!!treat_input_as_col(variable)) %>%
    summarise(mean = mean(tot_phys/tot_phys_recorded, na.rm = TRUE),
              lower = mean(tot_phys/tot_phys_recorded, na.rm = TRUE) - sd(tot_phys/tot_phys_recorded, na.rm = TRUE)/n(),
              upper = mean(tot_phys/tot_phys_recorded, na.rm = TRUE) + sd(tot_phys/tot_phys_recorded, na.rm = TRUE)/n(),
              n = n())
  return(summary)
}

physical_generate_forestplot_data <- function(data, metric, income_strata) {
  
  # Filtering Data to Remove Not Included During Model Fitting
  data <- data %>%
    filter(!is.na(tot_phys) & !is.na(tot_phys_recorded))
  
  # Loading Data 
  files <- list.files(path = "Outputs/")
  hh <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_hh_size"), files)]))
  age <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_age3cat"), files)]))
  gender <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_gender"), files)]))
  student <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_student"), files)]))
  method <- if (income_strata == "HIC") NA else readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_method"), files)]))
  weekday <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_weekday"), files)]))
  employment <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_employment"), files)]))
  
  # Extract Model Coefficients
  age_coef <- physical_summarise_coefs(age)
  gender_coef <- physical_summarise_coefs(gender) 
  weekday_coef <- physical_summarise_coefs(weekday)
  hh_coef <- physical_summarise_coefs(hh)
  method_coef <- if (income_strata == "HIC") data.frame(rep(NA, 3)) else physical_summarise_coefs(method)
  student_coef <- physical_summarise_coefs(student)
  employment_coef <- physical_summarise_coefs(employment)
  
  # Generating Empirical Summaries
  age_emp <- physical_empirical_summaries(data, "age3cat", income_strata)
  gender_emp <- physical_empirical_summaries(data, "part_gender", income_strata)
  weekday_emp <- physical_empirical_summaries(data, "weekday", income_strata)
  hh_emp <- physical_empirical_summaries(data, "hh_size", income_strata)
  method_emp <- physical_empirical_summaries(data, "method", income_strata)
  if (income_strata == "HIC") {
    method_emp[2, ] <- NA
    method_emp[2, 1] <- "Interview"
  }                    
  student_emp <- physical_empirical_summaries(data, "student", income_strata)
  employment_emp <- physical_empirical_summaries(data, "employment", income_strata)
  
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
  method_avg <- if (income_strata == "HIC") c(gen_avgs(method_emp[1, ]), NA) else gen_avgs(method_emp)
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
      row.names = c(NA, -length(full_text)), # note 12 has to match up to length
      class = "data.frame")
  
  return(list(tabletext = tabletext,
              forest_data_to_plot = forest_data_to_plot))
}

#######################################################
# Functions for Duration Contact Proportion
#######################################################

duration_summarise_coefs <- function(model_output, CI_choice =  c(0.05, 0.95)) {
  mean <- apply(exp(brms::fixef(model_output$fitting_output, summary = FALSE)), 2, mean)
  CIs <- apply(exp(brms::fixef(model_output$fitting_output, summary = FALSE)), 2, quantile, CI_choice)
  overall <- rbind(CIs[1, ], mean, CIs[2, ])
  row.names(overall) <- c(paste0(CI_choice[1] * 100, "%"), "mean", paste0(CI_choice[2] * 100, "%"))
  overall <- overall[, -1]
  return(as.data.frame(overall))
}

duration_empirical_summaries <- function(data, variable, income_strata) {
  temp <- data %>%
    mutate(income = ifelse(income == "LIC/LMIC", "LIC_LMIC", income)) %>%
    filter(income == income_strata)
  summary <- temp %>%
    filter(!is.na(!!treat_input_as_col(variable)) & !!treat_input_as_col(variable) != "") %>%
    group_by(!!treat_input_as_col(variable)) %>%
    summarise(mean = mean(tot_dur_under_1hr/tot_dur_recorded, na.rm = TRUE),
              lower = mean(tot_dur_under_1hr/tot_dur_recorded, na.rm = TRUE) - sd(tot_dur_under_1hr/tot_dur_recorded, na.rm = TRUE)/n(),
              upper = mean(tot_dur_under_1hr/tot_dur_recorded, na.rm = TRUE) + sd(tot_dur_under_1hr/tot_dur_recorded, na.rm = TRUE)/n(),
              n = n())
  return(summary)
}

duration_generate_forestplot_data <- function(data, metric, income_strata) {
  
  # Filtering Data to Remove Not Included During Model Fitting
  data <- data %>%
    filter(!is.na(tot_dur_under_1hr) & !is.na(tot_dur_recorded))
  
  # Loading Data 
  files <- list.files(path = "Outputs/")
  hh <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_hh_size"), files)]))
  age <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_age3cat"), files)]))
  gender <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_gender"), files)]))
  student <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_student"), files)]))
  method <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_method"), files)]))
  weekday <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_weekday"), files)]))
  employment <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_employment"), files)]))
  
  # Extract Model Coefficients
  age_coef <- duration_summarise_coefs(age)
  gender_coef <- duration_summarise_coefs(gender) 
  weekday_coef <- duration_summarise_coefs(weekday)
  hh_coef <- duration_summarise_coefs(hh)
  method_coef <- duration_summarise_coefs(method)
  student_coef <- duration_summarise_coefs(student)
  employment_coef <- duration_summarise_coefs(employment)
  
  # Generating Empirical Summaries
  age_emp <- duration_empirical_summaries(data, "age3cat", income_strata)
  gender_emp <- duration_empirical_summaries(data, "part_gender", income_strata)
  weekday_emp <- duration_empirical_summaries(data, "weekday", income_strata)
  hh_emp <- duration_empirical_summaries(data, "hh_size", income_strata)
  method_emp <- duration_empirical_summaries(data, "method", income_strata)
  student_emp <- duration_empirical_summaries(data, "student", income_strata)
  employment_emp <- duration_empirical_summaries(data, "employment", income_strata)
  
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
      row.names = c(NA, -length(full_text)), # note 12 has to match up to length
      class = "data.frame")
  
  return(list(tabletext = tabletext,
              forest_data_to_plot = forest_data_to_plot))
}

#######################################################
# Functions for Location Contact Proportion
#######################################################

location_summarise_coefs <- function(model_output, CI_choice =  c(0.05, 0.95)) {
  mean <- apply(exp(brms::fixef(model_output$fitting_output, summary = FALSE)), 2, mean)
  CIs <- apply(exp(brms::fixef(model_output$fitting_output, summary = FALSE)), 2, quantile, CI_choice)
  overall <- rbind(CIs[1, ], mean, CIs[2, ])
  row.names(overall) <- c(paste0(CI_choice[1] * 100, "%"), "mean", paste0(CI_choice[2] * 100, "%"))
  overall <- overall[, -c(1:3)]
  return(as.data.frame(overall))
}

location_empirical_summaries <- function(data, variable, income_strata) {
  temp <- data %>%
    mutate(income = ifelse(income == "LIC/LMIC", "LIC_LMIC", income)) %>%
    filter(income == income_strata)
  summary <- temp %>%
    filter(!is.na(!!treat_input_as_col(variable)) & !!treat_input_as_col(variable) != "") %>%
    group_by(!!treat_input_as_col(variable)) %>%
    summarise(mean_home = mean(tot_home/tot_all, na.rm = TRUE),
              lower_home = mean(tot_home/tot_all, na.rm = TRUE) - sd(tot_home/tot_all, na.rm = TRUE)/n(),
              upper_home = mean(tot_home/tot_all, na.rm = TRUE) + sd(tot_home/tot_all, na.rm = TRUE)/n(),
              mean_work = mean(tot_work/tot_all, na.rm = TRUE),
              lower_work = mean(tot_work/tot_all, na.rm = TRUE) - sd(tot_work/tot_all, na.rm = TRUE)/n(),
              upper_work = mean(tot_work/tot_all, na.rm = TRUE) + sd(tot_work/tot_all, na.rm = TRUE)/n(),
              mean_school = mean(tot_school/tot_all, na.rm = TRUE),
              lower_school = mean(tot_school/tot_all, na.rm = TRUE) - sd(tot_school/tot_all, na.rm = TRUE)/n(),
              upper_school = mean(tot_school/tot_all, na.rm = TRUE) + sd(tot_school/tot_all, na.rm = TRUE)/n(),
              mean_other = mean(tot_other/tot_all, na.rm = TRUE),
              lower_other = mean(tot_other/tot_all, na.rm = TRUE) - sd(tot_other/tot_all, na.rm = TRUE)/n(),
              upper_other = mean(tot_other/tot_all, na.rm = TRUE) + sd(tot_other/tot_all, na.rm = TRUE)/n(),
              n = n())
  return(summary)
}

location_generate_forestplot_data <- function(data, metric, income_strata) {
  
  # Filtering Data to Remove Not Included During Model Fitting
  data <- data %>%
    filter(!is.na(tot_all) & !is.na(tot_other) & !is.na(tot_school) & !is.na(tot_work) & !is.na(tot_home))
  
  # Loading Data 
  files <- list.files(path = "Outputs/")
  hh <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_hh_size"), files)]))
  age <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_age3cat"), files)]))
  gender <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_gender"), files)]))
  student <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_student"), files)]))
  method <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_method"), files)]))
  weekday <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_weekday"), files)]))
  employment <- readRDS(file = paste0("Outputs/" , files[pmatch(paste0(metric, "_", income_strata, "_employment"), files)]))
  
  # Extract Model Coefficients
  age_coef <- location_summarise_coefs(age)
  gender_coef <- location_summarise_coefs(gender) 
  weekday_coef <- location_summarise_coefs(weekday)
  hh_coef <- location_summarise_coefs(hh)
  method_coef <- location_summarise_coefs(method)
  student_coef <- location_summarise_coefs(student)
  employment_coef <- location_summarise_coefs(employment)
  
  # Generating Empirical Summaries
  age_emp <- location_empirical_summaries(data, "age3cat", income_strata)
  gender_emp <- location_empirical_summaries(data, "part_gender", income_strata)
  weekday_emp <- location_empirical_summaries(data, "weekday", income_strata)
  hh_emp <- location_empirical_summaries(data, "hh_size", income_strata)
  method_emp <- location_empirical_summaries(data, "method", income_strata)
  student_emp <- location_empirical_summaries(data, "student", income_strata)
  employment_emp <- location_empirical_summaries(data, "employment", income_strata)
  
  # Forest Plotting
  age_text <- c("", "Study", "Age Group", "0-15yrs", "15-49yrs", "49+yrs")
  gender_text <- c(NA, "Gender", "Female", "Male")
  weekday_text <- c(NA, "Weekday", "No", "Yes")
  hh_text <- c(NA, "Household Size", "1", "2", "3", "4", "5", "6+")
  method_text <- c(NA, "Method", "Diary", "Interview")
  student_text <- c(NA, "Student?", "No", "Yes")
  employment_text <- c(NA, "Employment?", "No", "Yes", NA)
  full_text <- c(age_text, gender_text, weekday_text, hh_text, method_text, student_text, employment_text)
  
  # School Forest Plot Data
  mean_school <- c(NA, NA, NA, unname(c(1, unlist(age_coef[2, grep("school", colnames(age_coef))]))), 
                   NA, NA, unname(c(1, unlist(gender_coef[2, grep("school", colnames(gender_coef))]))), 
                   NA, NA, unname(c(1, unlist(weekday_coef[2, grep("school", colnames(weekday_coef))]))), 
                   NA, NA, unname(c(1, unlist(hh_coef[2, grep("school", colnames(hh_coef))]))), 
                   NA, NA, unname(c(1, unlist(method_coef[2, grep("school", colnames(method_coef))]))), 
                   NA, NA, unname(c(1, unlist(student_coef[2, grep("school", colnames(student_coef))]))), 
                   NA, NA, unname(c(1, unlist(employment_coef[2, grep("school", colnames(employment_coef))]))), NA)
  lower_school <- c(NA, NA, NA, unname(c(1, unlist(age_coef[1, grep("school", colnames(age_coef))]))), 
                    NA, NA, unname(c(1, unlist(gender_coef[1, grep("school", colnames(gender_coef))]))), 
                    NA, NA, unname(c(1, unlist(weekday_coef[1, grep("school", colnames(weekday_coef))]))), 
                    NA, NA, unname(c(1, unlist(hh_coef[1, grep("school", colnames(hh_coef))]))), 
                    NA, NA, unname(c(1, unlist(method_coef[1, grep("school", colnames(method_coef))]))), 
                    NA, NA, unname(c(1, unlist(student_coef[1, grep("school", colnames(student_coef))]))), 
                    NA, NA, unname(c(1, unlist(employment_coef[1, grep("school", colnames(employment_coef))]))), NA)
  upper_school <- c(NA, NA, NA, unname(c(1, unlist(age_coef[3, grep("school", colnames(age_coef))]))), 
                    NA, NA, unname(c(1, unlist(gender_coef[3, grep("school", colnames(gender_coef))]))), 
                    NA, NA, unname(c(1, unlist(weekday_coef[3, grep("school", colnames(weekday_coef))]))), 
                    NA, NA, unname(c(1, unlist(hh_coef[3, grep("school", colnames(hh_coef))]))), 
                    NA, NA, unname(c(1, unlist(method_coef[3, grep("school", colnames(method_coef))]))), 
                    NA, NA, unname(c(1, unlist(student_coef[3, grep("school", colnames(student_coef))]))), 
                    NA, NA, unname(c(1, unlist(employment_coef[3, grep("school", colnames(employment_coef))]))), NA)
  
  # Home Forest Plot Data - not relevant as it's our reference class
  # mean_home <- c(NA, NA, NA, unname(c(1, unlist(age_coef[2, grep("home", colnames(age_coef))]))), 
  #                  NA, NA, unname(c(1, unlist(gender_coef[2, grep("home", colnames(gender_coef))]))), 
  #                  NA, NA, unname(c(1, unlist(weekday_coef[2, grep("home", colnames(weekday_coef))]))), 
  #                  NA, NA, unname(c(1, unlist(hh_coef[2, grep("home", colnames(hh_coef))]))), 
  #                  NA, NA, unname(c(1, unlist(method_coef[2, grep("home", colnames(method_coef))]))), 
  #                  NA, NA, unname(c(1, unlist(student_coef[2, grep("home", colnames(student_coef))]))), 
  #                  NA, NA, unname(c(1, unlist(employment_coef[2, grep("home", colnames(employment_coef))]))), NA)
  # lower_home <- c(NA, NA, NA, unname(c(1, unlist(age_coef[1, grep("home", colnames(age_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(gender_coef[1, grep("home", colnames(gender_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(weekday_coef[1, grep("home", colnames(weekday_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(hh_coef[1, grep("home", colnames(hh_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(method_coef[1, grep("home", colnames(method_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(student_coef[1, grep("home", colnames(student_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(employment_coef[1, grep("home", colnames(employment_coef))]))), NA)
  # upper_home <- c(NA, NA, NA, unname(c(1, unlist(age_coef[3, grep("home", colnames(age_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(gender_coef[3, grep("home", colnames(gender_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(weekday_coef[3, grep("home", colnames(weekday_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(hh_coef[3, grep("home", colnames(hh_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(method_coef[3, grep("home", colnames(method_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(student_coef[3, grep("home", colnames(student_coef))]))), 
  #                   NA, NA, unname(c(1, unlist(employment_coef[3, grep("home", colnames(employment_coef))]))), NA)
  
  # Work Forest Plot Data
  mean_work <- c(NA, NA, NA, unname(c(1, unlist(age_coef[2, grep("work", colnames(age_coef))]))), 
                 NA, NA, unname(c(1, unlist(gender_coef[2, grep("work", colnames(gender_coef))]))), 
                 NA, NA, unname(c(1, unlist(weekday_coef[2, grep("work", colnames(weekday_coef))]))), 
                 NA, NA, unname(c(1, unlist(hh_coef[2, grep("work", colnames(hh_coef))]))), 
                 NA, NA, unname(c(1, unlist(method_coef[2, grep("work", colnames(method_coef))]))), 
                 NA, NA, unname(c(1, unlist(student_coef[2, grep("work", colnames(student_coef))]))), 
                 NA, NA, unname(c(1, unlist(employment_coef[2, grep("work", colnames(employment_coef))]))), NA)
  lower_work <- c(NA, NA, NA, unname(c(1, unlist(age_coef[1, grep("work", colnames(age_coef))]))), 
                  NA, NA, unname(c(1, unlist(gender_coef[1, grep("work", colnames(gender_coef))]))), 
                  NA, NA, unname(c(1, unlist(weekday_coef[1, grep("work", colnames(weekday_coef))]))), 
                  NA, NA, unname(c(1, unlist(hh_coef[1, grep("work", colnames(hh_coef))]))), 
                  NA, NA, unname(c(1, unlist(method_coef[1, grep("work", colnames(method_coef))]))), 
                  NA, NA, unname(c(1, unlist(student_coef[1, grep("work", colnames(student_coef))]))), 
                  NA, NA, unname(c(1, unlist(employment_coef[1, grep("work", colnames(employment_coef))]))), NA)
  upper_work <- c(NA, NA, NA, unname(c(1, unlist(age_coef[3, grep("work", colnames(age_coef))]))), 
                  NA, NA, unname(c(1, unlist(gender_coef[3, grep("work", colnames(gender_coef))]))), 
                  NA, NA, unname(c(1, unlist(weekday_coef[3, grep("work", colnames(weekday_coef))]))), 
                  NA, NA, unname(c(1, unlist(hh_coef[3, grep("work", colnames(hh_coef))]))), 
                  NA, NA, unname(c(1, unlist(method_coef[3, grep("work", colnames(method_coef))]))), 
                  NA, NA, unname(c(1, unlist(student_coef[3, grep("work", colnames(student_coef))]))), 
                  NA, NA, unname(c(1, unlist(employment_coef[3, grep("work", colnames(employment_coef))]))), NA)
  
  # Other Forest Plot Data
  mean_other <- c(NA, NA, NA, unname(c(1, unlist(age_coef[2, grep("other", colnames(age_coef))]))), 
                 NA, NA, unname(c(1, unlist(gender_coef[2, grep("other", colnames(gender_coef))]))), 
                 NA, NA, unname(c(1, unlist(weekday_coef[2, grep("other", colnames(weekday_coef))]))), 
                 NA, NA, unname(c(1, unlist(hh_coef[2, grep("other", colnames(hh_coef))]))), 
                 NA, NA, unname(c(1, unlist(method_coef[2, grep("other", colnames(method_coef))]))), 
                 NA, NA, unname(c(1, unlist(student_coef[2, grep("other", colnames(student_coef))]))), 
                 NA, NA, unname(c(1, unlist(employment_coef[2, grep("other", colnames(employment_coef))]))), NA)
  lower_other <- c(NA, NA, NA, unname(c(1, unlist(age_coef[1, grep("other", colnames(age_coef))]))), 
                  NA, NA, unname(c(1, unlist(gender_coef[1, grep("other", colnames(gender_coef))]))), 
                  NA, NA, unname(c(1, unlist(weekday_coef[1, grep("other", colnames(weekday_coef))]))), 
                  NA, NA, unname(c(1, unlist(hh_coef[1, grep("other", colnames(hh_coef))]))), 
                  NA, NA, unname(c(1, unlist(method_coef[1, grep("other", colnames(method_coef))]))), 
                  NA, NA, unname(c(1, unlist(student_coef[1, grep("other", colnames(student_coef))]))), 
                  NA, NA, unname(c(1, unlist(employment_coef[1, grep("other", colnames(employment_coef))]))), NA)
  upper_other <- c(NA, NA, NA, unname(c(1, unlist(age_coef[3, grep("other", colnames(age_coef))]))), 
                  NA, NA, unname(c(1, unlist(gender_coef[3, grep("other", colnames(gender_coef))]))), 
                  NA, NA, unname(c(1, unlist(weekday_coef[3, grep("other", colnames(weekday_coef))]))), 
                  NA, NA, unname(c(1, unlist(hh_coef[3, grep("other", colnames(hh_coef))]))), 
                  NA, NA, unname(c(1, unlist(method_coef[3, grep("other", colnames(method_coef))]))), 
                  NA, NA, unname(c(1, unlist(student_coef[3, grep("other", colnames(student_coef))]))), 
                  NA, NA, unname(c(1, unlist(employment_coef[3, grep("other", colnames(employment_coef))]))), NA)
  
  mean <- cbind(mean_school, mean_work, mean_other)
  lower <- cbind(lower_school, lower_work, lower_other)
  upper <- cbind(upper_school, upper_work, upper_other)
  
  tabletext <- cbind(
    unname(full_text),
    c(NA, "N", NA, age_emp$n, NA, NA, gender_emp$n, NA, NA, weekday_emp$n, NA, NA, hh_emp$n, 
      NA, NA, method_emp$n, NA, NA, student_emp$n, NA, NA, employment_emp$n, NA))
  
  return(list(tabletext = tabletext,
              mean = mean,
              lower = lower,
              upper = upper))
}





# Generic Functions 
gen_RRs <- function(summary) {
  ref <- paste0("Ref RR = 1")
  storage <- c()
  for (i in 1:dim(summary)[2]) {
    temp <- paste0(round(summary[2, i], 2), " (", round(summary[1, i], 2), " - ", round(summary[3, i], 2), ")")
    storage <- c(storage, temp)
  }
  return(c(ref, storage))
}

gen_avgs <- function(summary) {
  storage <- c()
  for (i in 1:dim(summary)[1]) {
    temp <- paste0(round(summary[i, 2], 2), " (", round(summary[i, 3], 2), " - ", round(summary[i, 4], 2), ")")
    storage <- c(storage, temp)
  }
  return(storage)
}

# empirical_summaries <- function(data, variable, outcome, income_strata) {
#   temp <- data %>%
#     mutate(income = ifelse(income == "LIC/LMIC", "LIC_LMIC", income)) %>%
#     filter(income == income_strata)
#   summary <- temp %>%
#     filter(!is.na(!!treat_input_as_col(variable)) & !!treat_input_as_col(variable) != "") %>%
#     group_by(!!treat_input_as_col(variable)) %>%
#     summarise(mean = mean(!!treat_input_as_col(outcome), na.rm = TRUE),
#               lower = mean(!!treat_input_as_col(outcome), na.rm = TRUE) - sd(!!treat_input_as_col(outcome), na.rm = TRUE)/n(),
#               upper = mean(!!treat_input_as_col(outcome), na.rm = TRUE) + sd(!!treat_input_as_col(outcome), na.rm = TRUE)/n(),
#               n = n())
#   return(summary)
# }