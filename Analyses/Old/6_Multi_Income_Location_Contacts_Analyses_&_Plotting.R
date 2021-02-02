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
         method = ifelse(method == "diary", "Diary", method),
         tot_phys_recorded = tot_phys + tot_nonphys,
         tot_dur_recorded = tot_dur_under_1hr + tot_dur_1hr_plus)

# Loading In Relevant Model Outputs
location_LIC_LMIC <- location_generate_forestplot_data(data, "location", "LIC_LMIC")
location_UMIC <- location_generate_forestplot_data(data, "location", "UMIC")
location_HIC <- location_generate_forestplot_data(data, "location", "HIC")

# Individual Forest Plots for Each - order is school, work, other
forestplot(labeltext = location_LIC_LMIC$tabletext, graph.pos = 2,
           mean = location_LIC_LMIC$mean,
           lower = location_LIC_LMIC$lower,
           upper = location_LIC_LMIC$upper,
           hrzl_lines = list("3" = gpar(lwd=1)), new_page = TRUE,
           txt_gp = fpTxtGp(label = gpar(cex=0.8), ticks = gpar(cex=0.7), xlab  = gpar(cex=0.8)),
           align = c("l", "c", "c", "c", "c", "c","r"),
           is.summary = c(TRUE, TRUE, TRUE, rep(FALSE, 4), TRUE, 
                          rep(FALSE, 3), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 7), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 4)),
           col = fpColors(box = c("#D64933", "#088C83", "#BB9F06"), 
                          line = c("#D64933", "#088C83", "#BB9F06"),
                          summary = "#003f5c"),
           boxsize = 0.2, lineheight = unit(0.5, "cm"),
           colgap = unit(0, "mm"),  cex = 0.4, 
           xticks = c(0, 1, 2, 3, 4), clip = c(0.01, 3), 
           ci.vertices = TRUE, xlog = FALSE)

forestplot(labeltext = location_UMIC$tabletext, graph.pos = 2,
           mean = location_UMIC$mean,
           lower = location_UMIC$lower,
           upper = location_UMIC$upper,
           hrzl_lines = list("3" = gpar(lwd=1)), new_page = TRUE,
           txt_gp = fpTxtGp(label = gpar(cex=0.8), ticks = gpar(cex=0.7), xlab  = gpar(cex=0.8)),
           align = c("l", "c", "c", "c", "c", "c","r"),
           is.summary = c(TRUE, TRUE, TRUE, rep(FALSE, 4), TRUE, 
                          rep(FALSE, 3), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 7), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 4)),
           col = fpColors(box = c("#D64933", "#088C83", "#BB9F06"), 
                          line = c("#D64933", "#088C83", "#BB9F06"),
                          summary = "#003f5c"),
           boxsize = 0.2, lineheight = unit(0.5, "cm"),
           colgap = unit(0, "mm"),  cex = 0.4, 
           xticks = c(0, 1, 2, 3, 4), clip = c(0.01, 3), 
           ci.vertices = TRUE, xlog = FALSE)

forestplot(labeltext = location_HIC$tabletext, graph.pos = 2,
           mean = location_HIC$mean,
           lower = location_HIC$lower,
           upper = location_HIC$upper,
           hrzl_lines = list("3" = gpar(lwd=1)), new_page = TRUE,
           txt_gp = fpTxtGp(label = gpar(cex=0.8), ticks = gpar(cex=0.7), xlab  = gpar(cex=0.8)),
           align = c("l", "c", "c", "c", "c", "c","r"),
           is.summary = c(TRUE, TRUE, TRUE, rep(FALSE, 4), TRUE, 
                          rep(FALSE, 3), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 7), TRUE, rep(FALSE, 3), 
                          TRUE, rep(FALSE, 3), TRUE, rep(FALSE, 4)),
           col = fpColors(box = c("#D64933", "#088C83", "#BB9F06"), 
                          line = c("#D64933", "#088C83", "#BB9F06"),
                          summary = "#003f5c"),
           boxsize = 0.2, lineheight = unit(0.5, "cm"),
           colgap = unit(0, "mm"),  cex = 0.4, 
           xticks = c(0, 1, 2, 3, 4), clip = c(0.01, 3), 
           ci.vertices = TRUE, xlog = FALSE)

