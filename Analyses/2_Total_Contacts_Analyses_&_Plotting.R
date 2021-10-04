# Loading Required Libraries
# Note friendlyeval requires devtools::install_github("milesmcbain/friendlyeval")
library(forestplot); library(tidyverse); library(cowplot); library(friendlyeval)

# Sourcing Required Function
source("Functions/brms_output_summary_functions.R")

# Loading In Data
new_data <- read.csv("Data/combined_participant_lvl_final.csv") %>%
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
         method = ifelse(method == "Online", "Interview", method))

# Loading In Relevant Model Outputs
model <- "Multivariable"
total_LIC_LMIC <- total_generate_forestplot_data(data = new_data, model = model, income_strata = "LIC_LMIC")
total_UMIC <- total_generate_forestplot_data(new_data, model = model, "UMIC")
total_HIC <- total_generate_forestplot_data(new_data, model = model, "HIC")

# Individual Forest Plots for Each
pdf(file = paste0("Figures/total_", model, ".pdf"), width = 7.7, height = 7.25, useDingbats = FALSE)
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
           xticks = c(0.5, 1, 1.5, 2), 
           clip = c(0.5, 2), 
           zero = 0.5,
           ci.vertices = TRUE, xlog = FALSE,
           grid = structure(c(1), 
           gp = gpar(lwd=2,lty = 2, col = "#CCCCFF")))
#dev.off()

#pdf(file = paste0("Figures/total_UMIC_", model, ".pdf"), width = 7.7, height = 7.25, useDingbats = FALSE)
forestplot(labeltext = total_UMIC$tabletext, graph.pos = 4,
           total_UMIC$forest_data_to_plot,
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
           xticks = c(0.5, 1, 1.5, 2,2.5,3), 
           clip = c(0.5, 3), 
           zero = 0.5,
           ci.vertices = TRUE, xlog = FALSE,
           grid = structure(c(1), 
           gp = gpar(lwd=2,lty = 2, col = "#CCCCFF")))
#dev.off()

#pdf(file = paste0("Figures/total_HIC_", model, ".pdf"), width = 7.7, height = 7.25, useDingbats = FALSE)
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
           xticks = c(0.5, 1, 1.5, 2), 
           clip = c(0.5, 2), 
           zero = 0.5,
           ci.vertices = TRUE, xlog = FALSE,
           grid = structure(c(1), 
           gp = gpar(lwd=2,lty = 2, col = "#CCCCFF")))
#dev.off()

#pdf(file = paste0("Figures/total_All_", model, ".pdf"), width = 7.7, height = 7.25, useDingbats = FALSE)
combined <- total_LIC_LMIC$tabletext[, 1]
forestplot(combined, graph.pos = 2,
           mean  = cbind(total_LIC_LMIC$forest_data_to_plot[, 1], total_UMIC$forest_data_to_plot[, 1], total_HIC$forest_data_to_plot[, 1]),
           lower = cbind(total_LIC_LMIC$forest_data_to_plot[, 2], total_UMIC$forest_data_to_plot[, 2], total_HIC$forest_data_to_plot[, 2]),
           upper = cbind(total_LIC_LMIC$forest_data_to_plot[, 3], total_UMIC$forest_data_to_plot[, 3], total_HIC$forest_data_to_plot[, 3]), 
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
           xticks = c(0.5, 1, 1.5, 2,2.5,3), 
           clip = c(0.5, 3), 
           zero = 0.5,
           ci.vertices = FALSE, xlog = FALSE,
           grid = structure(c(1), 
          gp = gpar(lwd=2,lty = 2, col = "#CCCCFF")))
dev.off()





