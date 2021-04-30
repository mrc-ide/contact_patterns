# Loading Required Libraries
library(forestplot); library(tidyverse); library(friendlyeval)

# Sourcing Required Function
source("Functions/brms_output_summary_functions.R")

# Loading In Data
data <- read.csv("Data/combined_participant_lvl_final.csv") %>%
  mutate(age3cat = as.factor(age3cat),
         hh_size = ifelse(hh_size == "6+", 6, hh_size),
         hh_size = as.numeric(hh_size),
         hh_size = case_when(hh_size == 1 ~ "1", hh_size == 2 ~ "2",
                             hh_size == 3 ~ "3", hh_size == 4 ~ "4", 
                             hh_size == 5 ~ "5", hh_size >= 6 ~ "6+", TRUE ~ NA_character_),
         hh_size = as.factor(hh_size),
         tot_contacts = as.numeric(tot_contacts),
         tot_contacts_no_add = as.numeric(tot_contacts_no_add),
         gender = case_when(part_gender == "Female" ~ 0, part_gender == "Male" ~ 1, TRUE ~ NA_real_),
         employment = as.factor(employment),
         student = as.factor(student),
         study = as.factor(study),
         income = factor(income, levels = c("LIC/LMIC", "UMIC", "HIC")),
         weekday = as.factor(weekday),
         method = ifelse(method == "diary", "Diary", method),
         method = ifelse(method == "Online", "Interview", method)) %>%
  mutate(tot_dur_recorded = tot_dur_under_1hr + tot_dur_1hr_plus) %>%
  mutate(tot_dur_recorded = ifelse(tot_dur_recorded == 0, NA, tot_dur_recorded))

# Loading In Relevant Model Outputs
model <- "Multivariate"
duration_LIC_LMIC <- duration_generate_forestplot_data(data = data, model = model, income_strata = "LIC_LMIC")
duration_UMIC <- duration_generate_forestplot_data(data, model, "UMIC")
duration_HIC <- duration_generate_forestplot_data(data, model, "HIC")

# Individual Forest Plots for Each
pdf(file = paste0("Figures/duration_", model, ".pdf"), width = 9, height = 7.25, useDingbats = FALSE)
forestplot(labeltext = duration_LIC_LMIC$tabletext, 
           graph.pos = 4,
           duration_LIC_LMIC$forest_data_to_plot,
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
           xticks = c(0, 0.5, 1, 1.5, 2), 
           clip = c(0, 2), 
           zero = 0, 
           ci.vertices = TRUE, xlog = FALSE)

forestplot(labeltext = duration_UMIC$tabletext, graph.pos = 4,
           duration_UMIC$forest_data_to_plot,
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
           xticks = c(0, 0.5, 1, 1.5, 2), 
           clip = c(0, 2), 
           zero = 0, 
           ci.vertices = TRUE, xlog = FALSE)

forestplot(labeltext = duration_HIC$tabletext, graph.pos = 4,
           duration_HIC$forest_data_to_plot,
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
           xticks = c(0, 0.5, 1, 1.5, 2), 
           clip = c(0, 2), 
           zero = 0, 
           ci.vertices = TRUE, xlog = FALSE)

combined <- duration_LIC_LMIC$tabletext[, 1]
forestplot(combined, graph.pos = 2,
           mean  = cbind(duration_LIC_LMIC$forest_data_to_plot[, 1], duration_UMIC$forest_data_to_plot[, 1], duration_HIC$forest_data_to_plot[, 1]),
           lower = cbind(duration_LIC_LMIC$forest_data_to_plot[, 2], duration_UMIC$forest_data_to_plot[, 2], duration_HIC$forest_data_to_plot[, 2]),
           upper = cbind(duration_LIC_LMIC$forest_data_to_plot[, 3], duration_UMIC$forest_data_to_plot[, 3], duration_HIC$forest_data_to_plot[, 3]), 
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
           xticks = c(0, 0.5, 1, 1.5, 2), 
           clip = c(0, 2), 
           zero = 0, 
           ci.vertices = FALSE, xlog = FALSE)
dev.off()




