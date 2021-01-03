# Loading Required Libraries
library(forestplot)

#-----------------PLOTS TO COMPARE AND A SUMMARY PLOT OF THE TWO MEAN VALUES AND SAR-----------------------
age_cats <- 3
gender_cats <- 2
weekday_cats <- 2
hh_cats <- 6
method_cats <- 3
student_cats <- 2
employ_cats <- 2

age_text <- c("", "Study", "Age Group", "0-15yrs", "15-49yrs", "49+yrs")
gender_text <- c(NA, "Gender", "Male", "Female")
weekday_text <- c(NA, "Weekday", "No", "Yes")
hh_text <- c(NA, "Household Size", "1", "2", "3", "4", "5", "6+")
method_text <- c(NA, "Method", "Diary", "Interview", "Online")
student_text <- c(NA, "Student?", "No", "Yes")
employment_text <- c(NA, "Employment?", "No", "Yes", NA)

full_text <- c(age_text, gender_text, weekday_text, hh_text, method_text, student_text, employment_text)

mean_RRs <- c(0.5, 0.5)
lower_RRs <- c(0.25, 0.25)
upper_RRs <- c(0.75, 0.75)


gen_RRs <- function(mean_RRs, lower_RRs, upper_RRs) {
  ref <- paste0("Ref RR = 1")
  RRs <- paste0(round(mean_RRs, digits = 2), " (", round(lower_RRs, digits = 2),
                " - ", round(upper_RRs, digits = 2), ")")
  return(c(ref, RRs))
}

gen_avgs <- function(mean, lower, upper) {
  avg <- paste0(round(mean), " (", round(lower), " - ", round(upper), ")")
  return(avg)
}

mean_RR <- 0.5
lower_RR <- 0.25
upper_RR <- 0.75
age_ORs <- gen_RRs(rep(mean_RR, 2), rep(lower_RR, 2), rep(upper_RR, 2))
gender_ORs <- gen_RRs(rep(mean_RR, 1), rep(lower_RR, 1), rep(upper_RR, 1)) 
weekday_ORs <- gen_RRs(rep(mean_RR, 1), rep(lower_RR, 1), rep(upper_RR, 1)) 
hh_size_ORs <- gen_RRs(rep(mean_RR, 5), rep(lower_RR, 5), rep(upper_RR, 5))
method_ORs <- gen_RRs(rep(mean_RR, 2), rep(lower_RR, 2), rep(upper_RR, 2))
student_ORs <- gen_RRs(rep(mean_RR, 1), rep(lower_RR, 1), rep(upper_RR, 1)) 
employment_ORs <- gen_RRs(rep(mean_RR, 1), rep(lower_RR, 1), rep(upper_RR, 1)) 

mean <- 2
lower <- 1
upper <- 4
age_avg <- gen_avgs(rep(mean, 3), rep(lower, 3), rep(upper, 3))
gender_avg <- gen_avgs(rep(mean, 2), rep(lower, 2), rep(upper, 2)) 
weekday_avg <- gen_avgs(rep(mean, 2), rep(lower, 2), rep(upper, 2)) 
hh_size_avg <- gen_avgs(rep(mean, 6), rep(lower, 6), rep(upper, 6))
method_avg <- gen_avgs(rep(mean, 3), rep(lower, 3), rep(upper, 3))
student_avg <- gen_avgs(rep(mean, 2), rep(lower, 2), rep(upper, 2)) 
employment_avg <- gen_avgs(rep(mean, 2), rep(lower, 2), rep(upper, 2)) 


tabletext <- list(
  full_text,
  c("Average Contact", "Number (95% CI)", NA, age_avg, NA, NA, gender_avg, NA, NA, weekday_avg, NA, NA, hh_size_avg, 
    NA, NA, method_avg, NA, NA, student_avg, NA, NA, employment_avg, NA),
  c("Rate Ratio", "(95% CI)", NA, age_ORs, NA, NA, gender_ORs, NA, NA, weekday_ORs, NA, NA, hh_size_ORs, 
    NA, NA, method_ORs, NA, NA, student_ORs, NA, NA, employment_ORs, NA))

forest_data_to_plot <- 
  structure(list(
    mean  = c(NA, NA, NA, c(0.5, 0.5, 0.5), NA, NA, c(0.5, 0.5), NA, NA, c(0.5, 0.5), NA, NA, c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
              NA, NA, c(0.5, 0.5, 0.5), NA, NA, c(0.5, 0.5), NA, NA, c(0.5, 0.5), NA),
    lower = c(NA, NA, NA, c(0.25, 0.25, 0.25), NA, NA, c(0.25, 0.25), NA, NA, c(0.25, 0.25), NA, NA, c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25),
              NA, NA, c(0.25, 0.25, 0.25), NA, NA, c(0.25, 0.25), NA, NA, c(0.25, 0.25), NA),
    upper = c(NA, NA, NA, c(0.75, 0.75, 0.75), NA, NA, c(0.75, 0.75), NA, NA, c(0.75, 0.75), NA, NA, c(0.75, 0.75, 0.75, 0.75, 0.75, 0.75),
              NA, NA, c(0.75, 0.75, 0.75), NA, NA, c(0.75, 0.75), NA, NA, c(0.75, 0.75), NA)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -36), # note 12 has to match up to length
    class = "data.frame")

forestplot(tabletext, 
           graph.pos = 3,
           forest_data_to_plot,
           hrzl_lines = list("3" = gpar(lwd=1)),
           txt_gp = fpTxtGp(label = gpar(cex=0.8),
                            ticks = gpar(cex=0.7),
                            xlab  = gpar(cex=0.8)),
           boxsize = 0.4,
           new_page = TRUE,
           clip = c(0, 1), 
           align = c("l", "c", "c", "c", "c", "c","r"),
           is.summary = c(TRUE, TRUE, TRUE, rep(FALSE, 4), TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 7), TRUE, rep(FALSE, 4), TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 4)),
           col = fpColors(box = "#003f5c", line = "#003f5c", summary = "#003f5c", hrz_lines = "#444444"), 
           colgap = unit(0, "mm"), 
           cex = 0.4,
           lineheight = unit(0.5, "cm"),
           xlab = c("Contact Rate Ratio"),
           graphwidth = unit(100, "mm"),
           xticks = c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0),
           ci.vertices = TRUE)
