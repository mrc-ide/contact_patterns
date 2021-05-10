#######################################################
# Generic Functions for All Outcomes
#######################################################
gen_RRs <- function(summary, round = 2) {
  summary <- as.matrix(summary)
  ref <- paste0("Ref RR = 1")
  storage <- c()
  for (i in 1:dim(summary)[2]) {
    temp <- paste0(round(summary["mean", i], round), " (", round(summary["5%", i], round), " - ", round(summary["95%", i], round), ")")
    storage <- c(storage, temp)
  }
  return(c(ref, storage))
}

gen_avgs <- function(summary, round = 1) {
  storage <- c()
  for (i in 1:dim(summary)[1]) {
    temp <- summary[i, ]
    temp2 <- paste0(round(temp$mean, round), " (", round(temp$lower, round), " - ", round(temp$upper, round), ")")
    storage <- c(storage, temp2)
  }
  return(storage)
}

summarise_coefs <- function(model_output, CI_choice =  c(0.05, 0.95)) {
  mean <- apply(exp(brms::fixef(model_output$fitting_output, summary = FALSE)), 2, median)
  CIs <- apply(exp(brms::fixef(model_output$fitting_output, summary = FALSE)), 2, quantile, CI_choice)
  overall <- rbind(CIs[1, ], mean, CIs[2, ])
  row.names(overall) <- c(paste0(CI_choice[1] * 100, "%"), "mean", paste0(CI_choice[2] * 100, "%"))
  return(as.data.frame(overall))
}

empirical_summaries <- function(data, response_variable, grouping_variable, income_strata) {
  temp <- data %>%
    mutate(income = as.character(income)) %>%
    mutate(income = ifelse(income == "LIC/LMIC", "LIC_LMIC", income)) %>%
    filter(income == income_strata)
  if (response_variable == "total") {
    summary <- temp %>%
      filter(!is.na(!!treat_input_as_col(grouping_variable)) & !!treat_input_as_col(grouping_variable) != "") %>%
      group_by(!!treat_input_as_col(grouping_variable)) %>%
      summarise(mean = mean(tot_contacts, na.rm = TRUE),
                lower = quantile(tot_contacts, probs = 0.25, na.rm = TRUE),
                upper = quantile(tot_contacts, probs = 0.75, na.rm = TRUE),
                n = n())
  } else if (response_variable == "duration") {
    summary <- temp %>%
      filter(!is.na(!!treat_input_as_col(grouping_variable)) & !!treat_input_as_col(grouping_variable) != "") %>%
      group_by(!!treat_input_as_col(grouping_variable)) %>%
      summarise(mean = mean(tot_dur_under_1hr/tot_dur_recorded, na.rm = TRUE),
                lower = quantile(tot_dur_under_1hr/tot_dur_recorded, probs = 0.25, na.rm = TRUE),
                upper = quantile(tot_dur_under_1hr/tot_dur_recorded, probs = 0.75, na.rm = TRUE),
                n = n())
  } else if (response_variable == "physical") {
    summary <- temp %>%
      filter(!is.na(!!treat_input_as_col(grouping_variable)) & !!treat_input_as_col(grouping_variable) != "") %>%
      group_by(!!treat_input_as_col(grouping_variable)) %>%
      summarise(mean = mean(tot_phys/tot_phys_recorded, na.rm = TRUE),
                lower = quantile(tot_phys/tot_phys_recorded, probs = 0.25, na.rm = TRUE),
                upper = quantile(tot_phys/tot_phys_recorded, probs = 0.75, na.rm = TRUE),
                n = n())
  }
  return(summary)
}

#######################################################
# Functions for Total Contacts
#######################################################
total_generate_forestplot_data <- function(data, model, income_strata) {
  
  # Filtering Data to Remove Not Included During Model Fitting
  data <- data %>%
    dplyr::filter(!is.na(tot_contacts))
  
  # Loading Data 
  if (model == "Univariate") {
    files <- list.files(path = paste0("Outputs/Univariate/"))
    age <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("total", "_", income_strata, "_age3cat"), files)]))
    gender <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("total", "_", income_strata, "_gender"), files)]))
    hh <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("total", "_", income_strata, "_hh_size"), files)]))
    method <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("total", "_", income_strata, "_method"), files)]))
    weekday <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("total", "_", income_strata, "_weekday"), files)]))
    student <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("total", "_", income_strata, "_student"), files)]))
    employment <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("total", "_", income_strata, "_employment"), files)]))
  } else if (model == "Multivariate") {
    files <- list.files(path = paste0("Outputs/Multivariate/"))
    age <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("total", "_", income_strata, "_age3cat_gender_1"), files)]))
    gender <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("total", "_", income_strata, "_age3cat_gender_1"), files)]))
    hh <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("total", "_", income_strata, "_age3cat_gender_hh_size"), files)]))
    method <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("total", "_", income_strata, "_age3cat_gender_method"), files)]))
    weekday <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("total", "_", income_strata, "_age3cat_gender_weekday"), files)]))
    student <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("total", "_", income_strata, "_part_age_gender_student"), files)]))
    employment <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("total", "_", income_strata, "_part_age_gender_employment"), files)]))
  } else {
    return("Stop - either Univariate or Multivariate")
  }

  # Extract Model Coefficients
  age_coef <- summarise_coefs(age)[, c("age3cat2", "age3cat3")]
  gender_coef <- summarise_coefs(gender)[, "gender", drop = FALSE]
  weekday_coef <- summarise_coefs(weekday)[, "weekday1", drop = FALSE]
  hh_coef <- summarise_coefs(hh)[, c("hh_size2", "hh_size3", "hh_size4", "hh_size5", "hh_size6P")]
  method_coef <- summarise_coefs(method)[, "methodInterview", drop = FALSE]
  student_coef <- summarise_coefs(student)[, "student1", drop = FALSE]
  employment_coef <- summarise_coefs(employment)[, "employment1", drop = FALSE]

  # Generating Empirical Summaries
  age_emp <- empirical_summaries(data, "total", "age3cat", income_strata)
  gender_emp <- empirical_summaries(data, "total", "part_gender", income_strata)
  weekday_emp <- empirical_summaries(data, "total", "weekday", income_strata)
  hh_emp <- empirical_summaries(data, "total", "hh_size", income_strata)
  method_emp <- empirical_summaries(data, "total", "method", income_strata)
  student_emp <- empirical_summaries(data, "total", "student", income_strata)
  employment_emp <- empirical_summaries(data, "total", "employment", income_strata)
  
  # Forest Plotting
  age_text <- c("", "Study", "Age Group","0 to <15yrs", "15 to <65yrs", "65+yrs")
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
    c("Average", "Contacts (IQR)", NA, age_avg, NA, NA, gender_avg, NA, NA, weekday_avg, NA, NA, hh_size_avg, 
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
physical_generate_forestplot_data <- function(data, model, income_strata) {
  
  # Filtering Data to Remove Not Included During Model Fitting
  data <- data %>%
    filter(!is.na(tot_phys) & !is.na(tot_phys_recorded))
  
  # Loading Data 
  if (model == "Univariate") {
    files <- list.files(path = paste0("Outputs/Univariate/"))
    age <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("physical", "_", income_strata, "_age3cat"), files)]))
    gender <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("physical", "_", income_strata, "_gender"), files)]))
    hh <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("physical", "_", income_strata, "_hh_size"), files)]))
    method <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("physical", "_", income_strata, "_method"), files)]))
    weekday <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("physical", "_", income_strata, "_weekday"), files)]))
    student <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("physical", "_", income_strata, "_student"), files)]))
    employment <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("physical", "_", income_strata, "_employment"), files)]))
  } else if (model == "Multivariate") {
    files <- list.files(path = paste0("Outputs/Multivariate/"))
    age <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("physical", "_", income_strata, "_age3cat_gender_1"), files)]))
    gender <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("physical", "_", income_strata, "_age3cat_gender_1"), files)]))
    hh <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("physical", "_", income_strata, "_age3cat_gender_hh_size"), files)]))
    method <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("physical", "_", income_strata, "_age3cat_gender_method"), files)]))
    weekday <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("physical", "_", income_strata, "_age3cat_gender_weekday"), files)]))
    student <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("physical", "_", income_strata, "_part_age_gender_student"), files)]))
    employment <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("physical", "_", income_strata, "_part_age_gender_employment"), files)]))
  } else {
    return("Stop - either Univariate or Multivariate")
  }
  
  # Extract Model Coefficients
  age_coef <- summarise_coefs(age)[, c("age3cat2", "age3cat3")]
  gender_coef <- summarise_coefs(gender)[, "gender", drop = FALSE] 
  weekday_coef <- summarise_coefs(weekday)[, "weekday1", drop = FALSE]
  hh_coef <- summarise_coefs(hh)[, c("hh_size2", "hh_size3", "hh_size4", "hh_size5", "hh_size6P")]
  method_coef <- summarise_coefs(method)[, "methodInterview", drop = FALSE]
  student_coef <- summarise_coefs(student)[, "student1", drop = FALSE]
  employment_coef <- summarise_coefs(employment)[, "employment1", drop = FALSE]
  
  # Generating Empirical Summaries
  age_emp <- empirical_summaries(data, "physical", "age3cat", income_strata)
  gender_emp <- empirical_summaries(data, "physical", "part_gender", income_strata)
  weekday_emp <- empirical_summaries(data, "physical", "weekday", income_strata)
  hh_emp <- empirical_summaries(data, "physical", "hh_size", income_strata)
  method_emp <- empirical_summaries(data, "physical", "method", income_strata)
  student_emp <- empirical_summaries(data, "physical", "student", income_strata)
  employment_emp <- empirical_summaries(data, "physical", "employment", income_strata)
  
  # Forest Plotting
  age_text <- c("", "Study", "Age Group", "0 to <15yrs", "15 to <65yrs", "65+yrs")
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
    c("Mean", "Proportion (IQR)", NA, age_avg, NA, NA, gender_avg, NA, NA, weekday_avg, NA, NA, hh_size_avg, 
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
duration_generate_forestplot_data <- function(data, model, income_strata) {
  
  # Filtering Data to Remove Not Included During Model Fitting
  data <- data %>%
    filter(!is.na(tot_dur_under_1hr) & !is.na(tot_dur_recorded))
  
  # Loading Data 
  if (model == "Univariate") {
    files <- list.files(path = paste0("Outputs/Univariate/"))
    age <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("duration", "_", income_strata, "_age3cat"), files)]))
    gender <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("duration", "_", income_strata, "_gender"), files)]))
    hh <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("duration", "_", income_strata, "_hh_size"), files)]))
    method <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("duration", "_", income_strata, "_method"), files)]))
    weekday <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("duration", "_", income_strata, "_weekday"), files)]))
    student <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("duration", "_", income_strata, "_student"), files)]))
    employment <- readRDS(file = paste0("Outputs/Univariate/", files[pmatch(paste0("duration", "_", income_strata, "_employment"), files)]))
  } else if (model == "Multivariate") {
    files <- list.files(path = paste0("Outputs/Multivariate/"))
    age <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("duration", "_", income_strata, "_age3cat_gender_1"), files)]))
    gender <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("duration", "_", income_strata, "_age3cat_gender_1"), files)]))
    hh <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("duration", "_", income_strata, "_age3cat_gender_hh_size"), files)]))
    method <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("duration", "_", income_strata, "_age3cat_gender_method"), files)]))
    weekday <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("duration", "_", income_strata, "_age3cat_gender_weekday"), files)]))
    student <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("duration", "_", income_strata, "_part_age_gender_student"), files)]))
    employment <- readRDS(file = paste0("Outputs/Multivariate/", files[pmatch(paste0("duration", "_", income_strata, "_part_age_gender_employment"), files)]))
  } else {
    return("Stop - either Univariate or Multivariate")
  }
  
  # Extract Model Coefficients
  age_coef <- summarise_coefs(age)[, c("age3cat2", "age3cat3")]
  gender_coef <- summarise_coefs(gender)[, "gender", drop = FALSE] 
  weekday_coef <- summarise_coefs(weekday)[, "weekday1", drop = FALSE]
  hh_coef <- summarise_coefs(hh)[, c("hh_size2", "hh_size3", "hh_size4", "hh_size5", "hh_size6P")]
  method_coef <- summarise_coefs(method)[, "methodInterview", drop = FALSE]
  student_coef <- summarise_coefs(student)[, "student1", drop = FALSE]
  employment_coef <- summarise_coefs(employment)[, "employment1", drop = FALSE]

  # Generating Empirical Summaries
  age_emp <- empirical_summaries(data, "duration", "age3cat", income_strata)
  gender_emp <- empirical_summaries(data, "duration", "part_gender", income_strata)
  weekday_emp <- empirical_summaries(data, "duration", "weekday", income_strata)
  hh_emp <- empirical_summaries(data, "duration", "hh_size", income_strata)
  method_emp <- empirical_summaries(data, "duration", "method", income_strata)
  student_emp <- empirical_summaries(data, "duration", "student", income_strata)
  employment_emp <- empirical_summaries(data, "duration", "employment", income_strata)
  
  # Forest Plotting
  age_text <- c("", "Study", "Age Group", "0 to <15yrs", "15 to <65yrs", "65+yrs")
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
    c("Mean", "Proportion (95% CI)", NA, age_avg, NA, NA, gender_avg, NA, NA, weekday_avg, NA, NA, hh_size_avg, 
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
