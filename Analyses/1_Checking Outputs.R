files <- list.files(path = "Outputs/Univariate")
for (i in 1:length(files)) {
  x <- readRDS(paste0("Outputs/Univariate/", files[i]))
  print(c(files[i], x$diagnostics$divergent))
}
files <- list.files(path = "Outputs/Multivariate")
for (i in 1:length(files)) {
  x <- readRDS(paste0("Outputs/Multivariate/", files[i]))
  print(c(files[i], x$diagnostics$divergent))
}

### Total Contacts ###
LIC_total_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("total_LIC_LMIC_hh_size", files)]))
UMIC_total_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("total_UMIC_hh_size", files)]))
HIC_total_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("total_HIC_hh_size", files)]))

LIC_total_age <- readRDS(file = paste0("Outputs/" , files[pmatch("total_LIC_LMIC_age3cat", files)]))
UMIC_total_age <- readRDS(file = paste0("Outputs/" , files[pmatch("total_UMIC_age3cat", files)]))
HIC_total_age <- readRDS(file = paste0("Outputs/" , files[pmatch("total_HIC_age3cat", files)]))

LIC_total_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("total_LIC_LMIC_gender", files)]))
UMIC_total_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("total_UMIC_gender", files)]))
HIC_total_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("total_HIC_gender", files)]))

LIC_total_student <- readRDS(file = paste0("Outputs/" , files[pmatch("total_LIC_LMIC_student", files)]))
UMIC_total_student <- readRDS(file = paste0("Outputs/" , files[pmatch("total_UMIC_student", files)]))
HIC_total_student <- readRDS(file = paste0("Outputs/" , files[pmatch("total_HIC_student", files)]))

LIC_total_method <- readRDS(file = paste0("Outputs/" , files[pmatch("total_LIC_LMIC_method", files)]))
UMIC_total_method <- readRDS(file = paste0("Outputs/" , files[pmatch("total_UMIC_method", files)]))
HIC_total_method <- readRDS(file = paste0("Outputs/" , files[pmatch("total_HIC_method", files)]))

LIC_total_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("total_LIC_LMIC_weekday", files)]))
UMIC_total_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("total_UMIC_weekday", files)]))
HIC_total_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("total_HIC_weekday", files)]))

LIC_total_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("total_LIC_LMIC_employment", files)]))
UMIC_total_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("total_UMIC_employment", files)]))
HIC_total_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("total_HIC_employment", files)]))

### Duration ###
LIC_duration_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_LIC_LMIC_hh_size", files)]))
UMIC_duration_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_UMIC_hh_size", files)]))
HIC_duration_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_HIC_hh_size", files)]))

LIC_duration_age <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_LIC_LMIC_age3cat", files)]))
UMIC_duration_age <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_UMIC_age3cat", files)]))
HIC_duration_age <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_HIC_age3cat", files)]))

LIC_duration_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_LIC_LMIC_gender", files)]))
UMIC_duration_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_UMIC_gender", files)]))
HIC_duration_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_HIC_gender", files)]))

LIC_duration_student <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_LIC_LMIC_student", files)]))
UMIC_duration_student <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_UMIC_student", files)]))
HIC_duration_student <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_HIC_student", files)]))

LIC_duration_method <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_LIC_LMIC_method", files)]))
UMIC_duration_method <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_UMIC_method", files)]))
HIC_duration_method <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_HIC_method", files)]))

LIC_duration_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_LIC_LMIC_weekday", files)]))
UMIC_duration_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_UMIC_weekday", files)]))
HIC_duration_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_HIC_weekday", files)]))

LIC_duration_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_LIC_LMIC_employment", files)]))
UMIC_duration_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_UMIC_employment", files)]))
HIC_duration_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("duration_HIC_employment", files)]))

# Physical
LIC_physical_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_LIC_LMIC_hh_size", files)]))
UMIC_physical_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_UMIC_hh_size", files)]))
HIC_physical_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_HIC_hh_size", files)]))

LIC_physical_age <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_LIC_LMIC_age3cat", files)]))
UMIC_physical_age <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_UMIC_age3cat", files)]))
HIC_physical_age <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_HIC_age3cat", files)]))

LIC_physical_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_LIC_LMIC_gender", files)]))
UMIC_physical_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_UMIC_gender", files)]))
HIC_physical_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_HIC_gender", files)]))

LIC_physical_student <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_LIC_LMIC_student", files)]))
UMIC_physical_student <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_UMIC_student", files)]))
HIC_physical_student <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_HIC_student", files)]))

LIC_physical_method <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_LIC_LMIC_method", files)]))
UMIC_physical_method <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_UMIC_method", files)]))
HIC_physical_method <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_HIC_method", files)])) # doesn't exist. Are all 1 method so no variation.

LIC_physical_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_LIC_LMIC_weekday", files)]))
UMIC_physical_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_UMIC_weekday", files)]))
HIC_physical_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_HIC_weekday", files)]))

LIC_physical_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_LIC_LMIC_employment", files)]))
UMIC_physical_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_UMIC_employment", files)]))
HIC_physical_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("physical_HIC_employment", files)]))

# Location
LIC_location_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("location_LIC_LMIC_hh_size", files)]))
UMIC_location_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("location_UMIC_hh_size", files)]))
HIC_location_hh <- readRDS(file = paste0("Outputs/" , files[pmatch("location_HIC_hh_size", files)]))

LIC_location_age <- readRDS(file = paste0("Outputs/" , files[pmatch("location_LIC_LMIC_age3cat", files)]))
UMIC_location_age <- readRDS(file = paste0("Outputs/" , files[pmatch("location_UMIC_age3cat", files)]))
HIC_location_age <- readRDS(file = paste0("Outputs/" , files[pmatch("location_HIC_age3cat", files)]))

LIC_location_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("location_LIC_LMIC_gender", files)]))
UMIC_location_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("location_UMIC_gender", files)]))
HIC_location_gender <- readRDS(file = paste0("Outputs/" , files[pmatch("location_HIC_gender", files)]))

LIC_location_student <- readRDS(file = paste0("Outputs/" , files[pmatch("location_LIC_LMIC_student", files)]))
UMIC_location_student <- readRDS(file = paste0("Outputs/" , files[pmatch("location_UMIC_student", files)]))
HIC_location_student <- readRDS(file = paste0("Outputs/" , files[pmatch("location_HIC_student", files)]))

LIC_location_method <- readRDS(file = paste0("Outputs/" , files[pmatch("location_LIC_LMIC_method", files)]))
UMIC_location_method <- readRDS(file = paste0("Outputs/" , files[pmatch("location_UMIC_method", files)]))
HIC_location_method <- readRDS(file = paste0("Outputs/" , files[pmatch("location_HIC_method", files)])) # doesn't exist. Are all 1 method so no variation.

LIC_location_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("location_LIC_LMIC_weekday", files)]))
UMIC_location_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("location_UMIC_weekday", files)]))
HIC_location_weekday <- readRDS(file = paste0("Outputs/" , files[pmatch("location_HIC_weekday", files)]))

LIC_location_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("location_LIC_LMIC_employment", files)]))
UMIC_location_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("location_UMIC_employment", files)]))
HIC_location_employment <- readRDS(file = paste0("Outputs/" , files[pmatch("location_HIC_employment", files)]))
