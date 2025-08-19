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
# Field Survey ----
## --------------------------------- ##

# Read in file
field_arthro <- read.csv(file.path("data", "tidy", "zhong_2019_field-surveys_arthropods.csv"))

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

# Read in plant data from field survey
field_plant <- read.csv(file.path("data", "tidy", "zhong_2019_field-surveys_plants.csv"))

# Check structure
dplyr::glimpse(field_plant)

# Process that (slightly)
field_plant_v2 <- field_plant %>%
  # Ditch sample timing info
  dplyr::select(-year, -month)

# Check structure
dplyr::glimpse(field_plant_v2)

# Combine plant/arthropod info
field_actual <- field_plant_v2 %>%
  dplyr::full_join(x = ., y = field_arthro_v2,
                   by = "plot")

# Check structure
dplyr::glimpse(field_actual)

# Export locally
write.csv(x = field_actual, row.names = F, na = '',
          file = file.path("data", "zhong_2019_field-surveys.csv"))

## --------------------------------- ##
# Treatment Effects ----
## --------------------------------- ##

# Read in data
trt_arthro <- read.csv(file.path("data", "tidy", "zhong_2020-2022_treatment-effects_other.csv"))

# Check structure
dplyr::glimpse(trt_arthro)

# Do needed summarization
trt_arthro_v2 <- trt_arthro %>%
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
dplyr::glimpse(trt_arthro_v2)

# Read in the treatment effects on L. chinensis ('leychin') biomass too
trt_biomass <- read.csv(file.path("data", "tidy", "zhong_2020-2022_treatment-effects_leymus-chinesis-biomass.csv"))

# Check structure
dplyr::glimpse(trt_biomass)

# Ditch sample timing information
trt_biomass_v2 <- trt_biomass %>%
  dplyr::select(-year, -month)

# Check structure
dplyr::glimpse(trt_biomass_v2)

# Attach these together
trt_actual <- trt_biomass_v2 %>%
  dplyr::full_join(x = ., y = trt_arthro_v2,
                   by = c("block", "treatment_forb", "treatment_predator"))

# Check structure
dplyr::glimpse(trt_actual)

# Export locally
write.csv(x = trt_actual, row.names = F, na = '',
          file = file.path("data", "zhong_2020-2022_treatment-effects.csv"))

# End ----
