## --------------------------------------------------------- ##
# Zhong Grasshopper - Process Data
## --------------------------------------------------------- ##

# Purpose
## Generate desired summary metrics (and other pre-stats wrangling)

## --------------------------------- ##
# Housekeeping ----
## --------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Clear environment
rm(list = ls()); gc()

## --------------------------------- ##
# Arthropod Field Survey ----
## --------------------------------- ##

# Read in file
field_arthro <- read.csv(file.path("data", "zhong_2019_field-surveys_arthropods.csv"))

# Check structure
dplyr::glimpse(field_arthro)

# Do needed summarization
field_arthro_v2 <- field_arthro %>%
  # Average grasshopper / mantis abundance across sampling dates
  dplyr::group_by(plot) %>%
  dplyr::summarize(ct = dplyr::n(),
                   mean_grasshopper_abun_m2 = mean((grasshopper_count * 10), na.rm = T),
                   sd_grasshopper_abun_m2 = sd((grasshopper_count * 10), na.rm = T),
                   se_grasshopper_abun_m2 = sd_grasshopper_abun_m2 / sqrt(ct),
                   mean_mantis_abun_m2 = mean((mantis_count * 10), na.rm = T),
                   sd_mantis_abun_m2 = sd((mantis_count * 10), na.rm = T),
                   se_mantis_abun_m2 = sd_mantis_abun_m2 / sqrt(ct),
                   .groups = "keep") %>%
  dplyr::ungroup() %>%
  # Ditch sample size column
  dplyr::select(-ct)

# Check structure
dplyr::glimpse(field_arthro_v2)

# Export locally
write.csv(x = field_arthro_v2, row.names = F, na = '',
          file = file.path("data", "zhong_2019_field-surveys_mean-arthropods.csv"))

## --------------------------------- ##
# Treatment Effects ----
## --------------------------------- ##

# Read in data
trt_effects <- read.csv(file.path("data", "zhong_2020-2022_treatment-effects_other.csv"))

# Check structure
dplyr::glimpse(trt_effects)

# Do needed summarization
trt_effects_v2 <- trt_effects %>%
  # Pivot to long format (makes summarization cleaner)
  tidyr::pivot_longer(cols = -year:-treatment_predator) %>%
  # Summarize across years
  dplyr::group_by(block, treatment_forb, treatment_predator, name) %>%
  dplyr::summarize(ct = dplyr::n(),
                   mean = mean(value, na.rm = T),
                   sd = sd(value, na.rm = T),
                   se = sd / sqrt(ct),
                   .groups = "keep") %>%
  dplyr::ungroup() %>%
  # Ditch count column
  dplyr::select(-ct) %>%
  # Pivot even further long format
  tidyr::pivot_longer(cols = mean:se, names_to = "metric") %>%
  # Combine metric with original column name
  dplyr::mutate(name_actual = paste0(metric, "_", name)) %>%
  # Ditch superseded columns
  dplyr::select(-name, -metric) %>%
  # Flip back to wide format
  tidyr::pivot_wider(names_from = name_actual, values_from = value) %>%
  # Reorder columns slightly
  dplyr::relocate(dplyr::contains("leaf_damage"), .after = treatment_predator)

# Check structure
dplyr::glimpse(trt_effects_v2)

# Export locally
write.csv(x = trt_effects_v2, row.names = F, na = '',
          file = file.path("data", "zhong_2020-2022_treatment-effects_mean-other.csv"))

# End ----
