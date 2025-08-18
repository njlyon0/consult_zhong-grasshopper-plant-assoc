## --------------------------------------------------------- ##
# Zhong Grasshopper - Process Data
## --------------------------------------------------------- ##

# Purpose
## Get the data into a format that is ready for analysis/visualization

## --------------------------------- ##
# Housekeeping ----
## --------------------------------- ##

# Load libraries
librarian::shelf(tidyverse)

# Clear environment
rm(list = ls()); gc()

## --------------------------------- ##
# Load Data ----
## --------------------------------- ##

# Identify file path to Excel file
raw_path <- file.path("data", "raw", "all data_forb and mantis.xlsx")

# Load various sheets from that file
## Data are not in 'tidy format' so need to do some careful wrangling
df19_v0 <- readxl::read_excel(path = raw_path, sheet = "2019 field surveys")
df20_v0 <- readxl::read_excel(path = raw_path, sheet = "2020 original conditions")
df22_v0 <- readxl::read_excel(path = raw_path, sheet = "2022 treatment effects")

## --------------------------------- ##
# Wrangle 2019 Sheet (Plants) ----
## --------------------------------- ##

# Isolate first table of info
df19_plants <- df19_v0 %>%
  # Grab just needed columns
  dplyr::select(`Plant and arthropod variables in the 30 1 ×1mpermanent sampling plotsinfield surveys of 2019`:...3) %>%
  # Rename columns with actual column name row
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = as.character(.[2, ])) %>%
  # Ditch remaining bad header row
  dplyr::filter(!plot %in% c("mid-August, 2019", "plot")) %>%
  # Make column names clearer
  dplyr::rename(forb_cover_perc = `Forb cover (%)`,
                leychin_cover_perc = `Leymus chinesis cover (%)`) %>%
  # Add some useful other information that was in the original headers
  dplyr::mutate(year = 2019,
                month = 8,
                .before = plot) %>%
  # Get columns into right class
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("cover_perc"), .fns = as.numeric)) %>%
  dplyr::mutate(plot = paste0("plot ", plot))

# Check structure of result
dplyr::glimpse(df19_plants)

# Export locally
write.csv(x = df19_plants, row.names = F, na = '',
          file = file.path("data", "zhong_2019_plants.csv"))

## --------------------------------- ##
# Wrangle 2019 Sheet (Arthropods) ----
## --------------------------------- ##

# Isolate grasshopper info
df19_ghop <- df19_v0 %>%
  # Grab just needed columns
  dplyr::select(...6:...14) %>%
  # Rename columns with actual column name row
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = as.character(.[2, ])) %>%
  # Ditch remaining bad header row
  dplyr::filter(!plot %in% c("Grasshopper", "plot")) %>%
  # Flip to long format
  tidyr::pivot_longer(cols = dplyr::contains("date"),
                      names_to = "dates", values_to = "grasshopper_count")

# Check structure
dplyr::glimpse(df19_ghop)

# Isolate mantis info
df19_mant <- df19_v0 %>%
  # Grab just needed columns
  dplyr::select(...18:...26) %>%
  # Rename columns with actual column name row
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = as.character(.[2, ])) %>%
  # Ditch remaining bad header row
  dplyr::filter(!plot %in% c("Mantis", "plot")) %>%
  # Flip to long format
  tidyr::pivot_longer(cols = dplyr::contains("date"),
                      names_to = "dates", values_to = "mantis_count")

# Check structure
dplyr::glimpse(df19_mant)

# Combine the two arthropod dataframes and do more wrangling
df19_arthro <- df19_ghop %>%
  # Join by plot and date
  dplyr::full_join(x = ., y = df19_mant, by = c("plot", "dates")) %>%
  # Add some useful other information that was in the original headers
  dplyr::mutate(year = 2019,
                month = ifelse(dates %in% paste0("date_", 5:8),
                               yes = 8, no = 7),
                .before = plot) %>%
  # Streamline 'dates' column
  dplyr::mutate(date = gsub("sampling ", "", x = dates), .after = month) %>%
  dplyr::select(-dates) %>%
  # Get columns into right class
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("count"), .fns = as.numeric)) %>%
  dplyr::mutate(plot = paste0("plot ", plot))

# Check structure again
dplyr::glimpse(df19_arthro)

# Export locally
write.csv(x = df19_arthro, row.names = F, na = '',
          file = file.path("data", "zhong_2019_arthropods.csv"))


## --------------------------------- ##
# ----
## --------------------------------- ##




# End ----
