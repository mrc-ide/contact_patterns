# Loading Required Libraries
library(forestplot)

#-----------------PLOTS TO COMPARE AND A SUMMARY PLOT OF THE TWO MEAN VALUES AND SAR-----------------------
tabletext <- cbind(
  c("", "Variable", "Age Group", "0-15yrs", "15-49yrs", "49+yrs", NA, "Gender", "Male", "Female", NA,
    "Weekday", "No", "Yes", NA, "Household Size", "1", "2", "3", "4", "5", "6+", NA,
    "Method", "Diary", "Interview", "Online", NA, "Student?", "No", "Yes", NA,
    "Employment?", "No", "Yes", NA),
  c("Average Contact", "Number (95% CI)", NA, rep(0.5, 3), NA, NA, rep(0.5, 2), NA, NA,
    rep(0.5, 2), NA, NA, rep(0.5, 6), NA, NA, rep(0.5, 3), NA, NA, rep(0.5, 2), NA, NA,
    rep(0.5, 2), NA),
  c("Rate Ratio", "(95% CI)", NA, rep(0.5, 3), NA, NA, rep(0.5, 2), NA, NA,
    rep(0.5, 2), NA, NA, rep(0.5, 6), NA, NA, rep(0.5, 3), NA, NA, rep(0.5, 2), NA, NA,
    rep(0.5, 2), NA))

mean_plot <- c(NA, NA, NA, rep(0.5, 3), NA, NA, rep(0.5, 2), NA, NA,
               rep(0.5, 2), NA, NA, rep(0.5, 6), NA, NA, rep(0.5, 3), NA, NA, rep(0.5, 2), NA, NA,
               rep(0.5, 2), NA)
lower_plot <- c(NA, NA, NA, rep(0.25, 3), NA, NA, rep(0.25, 2), NA, NA,
                rep(0.25, 2), NA, NA, rep(0.25, 6), NA, NA, rep(0.25, 3), NA, NA, rep(0.25, 2), NA, NA,
                rep(0.25, 2), NA)
upper_plot <- c(NA, NA, NA,rep(0.75, 3), NA, NA, rep(0.75, 2), NA, NA,
                rep(0.75, 2), NA, NA, rep(0.75, 6), NA, NA, rep(0.75, 3), NA, NA, rep(0.75, 2), NA, NA,
                rep(0.75, 2), NA)

forestplot(tabletext,
           graph.pos = 3,
           mean  = cbind(mean_plot, mean_plot, mean_plot),
           lower = cbind(lower_plot, lower_plot, lower_plot),
           upper = cbind(upper_plot, upper_plot, upper_plot),
           hrzl_lines = list("3" = gpar(lwd=1, lineend="butt", col="black")),
           txt_gp = fpTxtGp(label = gpar(cex=1),
                            ticks = gpar(cex=0.7),
                            xlab  = gpar(cex=1)),
           is.summary = c(TRUE, TRUE, TRUE, rep(FALSE, 4), TRUE, rep(FALSE, 3),
                          TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 7), TRUE, rep(FALSE, 4),
                          TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 2)),
           boxsize = 0.15,
           new_page = TRUE,
           clip = c(0, 1),
           align = c("l", "c", "c", "c", "c", "c","r"),
           col = fpColors(box = c("#D64933", "#088C83", "#BB9F06"),
                          line = c("#D64933", "#088C83", "#BB9F06"),
                          summary = "#003f5c"),
           colgap = unit(0, "mm"),
           lineheight = unit(0.70, "cm"),
           xlab = c("Contact Rate Ratio"),
           xticks = c(0, 0.5, 1.0, 1.5),
           ci.vertices = TRUE)
