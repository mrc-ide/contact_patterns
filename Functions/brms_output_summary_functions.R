# Functions for Total Contacts
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

# Functions for Physical Contact Proportion
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
  gender_coef <- physical_summarise_coefs(gender) # check this
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