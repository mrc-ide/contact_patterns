# Loading Required Libraries
library(forestplot); library(tidyverse); library(friendlyeval)

# Sourcing Required Function
source("Functions/brms_output_summary_functions.R")

# Loading In Data
data <- read.csv("Data/combined_participant_lvl_all.csv") %>%
  mutate(hh_size = case_when(hh_size == 1 ~ "1", hh_size == 2 ~ "2",
                             hh_size == 3 ~ "3", hh_size == 4 ~ "4", 
                             hh_size == 5 ~ "5", hh_size >= 6 ~ "6+", TRUE ~ NA_character_),
         hh_size = factor(hh_size),
         method = ifelse(method == "diary", "Diary", method)) 

# Loading In Relevant Model Outputs
total_LIC_LMIC <- total_generate_forestplot_data("total", "LIC_LMIC")
total_UMIC <- total_generate_forestplot_data("total", "UMIC")
total_HIC <- total_generate_forestplot_data("total", "HIC")

# Individual Forest Plots for Each
forestplot(labeltext = total_LIC_LMIC$tabletext, graph.pos = 4,
           total_LIC_LMIC$forest_data_to_plot,
           hrzl_lines = list("3" = gpar(lwd=1)),
           txt_gp = fpTxtGp(label = gpar(cex=0.8), ticks = gpar(cex=0.7), xlab  = gpar(cex=0.8)),
           boxsize = 0.4, new_page = TRUE,
           align = c("l", "c", "c", "c", "c", "c","r"),
           is.summary = c(TRUE, TRUE, TRUE, rep(FALSE, 4), TRUE, 
                          rep(FALSE, 3), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 7), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 4)),
           col = fpColors(box = "#003f5c", line = "#003f5c", summary = "#003f5c", hrz_lines = "#444444"), 
           colgap = unit(0, "mm"),  cex = 0.4, lineheight = unit(0.5, "cm"),
           xlab = c("Contact Rate Ratio"), graphwidth = unit(100, "mm"),
           xticks = c(0.75, 1, 1.25, 1.5, 1.75, 2), clip = c(0.75, 2), 
           ci.vertices = TRUE, xlog = FALSE)

forestplot(labeltext = total_HIC$tabletext, graph.pos = 4,
           total_HIC$forest_data_to_plot,
           hrzl_lines = list("3" = gpar(lwd=1)),
           txt_gp = fpTxtGp(label = gpar(cex=0.8), ticks = gpar(cex=0.7), xlab  = gpar(cex=0.8)),
           boxsize = 0.4, new_page = TRUE,
           align = c("l", "c", "c", "c", "c", "c","r"),
           is.summary = c(TRUE, TRUE, TRUE, rep(FALSE, 4), TRUE, 
                          rep(FALSE, 3), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 7), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 4)),
           col = fpColors(box = "#003f5c", line = "#003f5c", summary = "#003f5c", hrz_lines = "#444444"), 
           colgap = unit(0, "mm"),  cex = 0.4, lineheight = unit(0.5, "cm"),
           xlab = c("Contact Rate Ratio"), graphwidth = unit(100, "mm"),
           xticks = c(0.75, 1, 1.25, 1.5, 1.75, 2), clip = c(0.75, 2), 
           ci.vertices = TRUE, xlog = FALSE)

combined <- total_LIC_LMIC$tabletext[, 1]
forestplot(combined, graph.pos = 2,
           mean  = cbind(total_LIC_LMIC$forest_data_to_plot[, 1], total_HIC$forest_data_to_plot[, 1]),
           lower = cbind(total_LIC_LMIC$forest_data_to_plot[, 2], total_HIC$forest_data_to_plot[, 2]),
           upper = cbind(total_LIC_LMIC$forest_data_to_plot[, 3], total_HIC$forest_data_to_plot[, 3]), 
           hrzl_lines = list("3" = gpar(lwd=1, lineend="butt", col="black")),
           txt_gp = fpTxtGp(label = gpar(cex=0.8), ticks = gpar(cex=0.7), xlab  = gpar(cex=0.8)),
           new_page = TRUE,
           align = c("l", "c", "c", "c", "c", "c","r"),
           is.summary = c(TRUE, TRUE, TRUE, rep(FALSE, 4), TRUE, 
                          rep(FALSE, 3), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 7), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 4)),
           col = fpColors(box = c("#D64933", "#088C83", "#BB9F06"), 
                          line = c("#D64933", "#088C83", "#BB9F06"),
                          summary = "#003f5c"), 
           colgap = unit(0, "mm"),  cex = 0.4, 
           boxsize = 0.2, lineheight = unit(0.5, "cm"),
           xlab = c("Contact Rate Ratio"), graphwidth = unit(100, "mm"),
           xticks = c(0.75, 1, 1.25, 1.5, 1.75, 2), clip = c(0.75, 2), 
           ci.vertices = FALSE, xlog = FALSE)





