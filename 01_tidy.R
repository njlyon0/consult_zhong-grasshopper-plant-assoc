## --------------------------------------------------------- ##
# Zhong Grasshopper - Tidy Data
## --------------------------------------------------------- ##

# Purpose
## Get the data into a format that is ready for analysis/visualization

## --------------------------------- ##
# Housekeeping ----
## --------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR)

# Clear environment
rm(list = ls()); gc()

## --------------------------------- ##
# Wrangle 2019 Sheet ----
## --------------------------------- ##

# Load the relevant sheet from that file
df19_v1 <- readxl::read_excel(path = file.path("data", "raw", "all data_forb and mantis.xlsx"),
                              sheet = "2019 field surveys")

# Isolate first table of info
df19_plants <- df19_v1 %>%
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
          file = file.path("data", "zhong_2019_field-surveys_plants.csv"))

# Isolate grasshopper info
df19_ghop <- df19_v1 %>%
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
df19_mant <- df19_v1 %>%
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
          file = file.path("data", "zhong_2019_field-surveys_arthropods.csv"))

# Clear environment
rm(list = ls()); gc()

## --------------------------------- ##
# Wrangle 2020 Sheet ----
## --------------------------------- ##

# Load the relevant sheet from that file
df20_v1 <- readxl::read_excel(path = file.path("data", "raw", "all data_forb and mantis.xlsx"),
                              sheet = "2020 original conditions")

# Tidy the single table in the 2020 sheet
df20_forbs <- df20_v1 %>%
  # Drop empty columns
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.)))) %>%
  # Rename columns with actual column name row
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = as.character(.[1, ])) %>%
  # Rename prettier / more machine-readable
  dplyr::rename(block = Block,
                treatment_forb = `Forb treatment`,
                treatment_predator = `Predator treatment`,
                forb_cover_perc = `forb cover (%)`,
                leychin_cover_perc = `L. chinensis cover (%)`,
                other_grass_cover_perc = `Other grasses cover (%)`,
                spp_richness = `Species richness`) %>%
  # Ditch remaining bad header row
  dplyr::filter(!block %in% c("Block")) %>%
  # Add some useful other information that was in the original headers
  dplyr::mutate(year = 2020,
                month = 6,
                .before = block) %>%
  # Get columns into right class
  dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("cover_perc"), .fns = as.numeric)) %>%
  dplyr::mutate(block = paste0("block ", block))

# Check structure
dplyr::glimpse(df20_forbs)

# Export locally
write.csv(x = df20_forbs, row.names = F, na = '',
          file = file.path("data", "zhong_2020_original-conditions.csv"))

# Clear environment
rm(list = ls()); gc()

## --------------------------------- ##
# Wrangle 2022 Sheet ----
## --------------------------------- ##

# Load the relevant sheet from that file
df22_v1 <- readxl::read_excel(path = file.path("data", "raw", "all data_forb and mantis.xlsx"),
                              sheet = "2022 treatment effects")

# Do needed wrangling for plant data
df22_leaf <- df22_v1 %>%
  # Grab desired columns
  dplyr::select(`August, 2022, L. chinensis biomass, in the 2 m2 mesocosms`,
                ...3:...6,
                `August, 2020...8`, `2021...9`, `2022...10`) %>%
  # Drop any completely empty columns
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.)))) %>%
  # Rename columns based on 'real' column name row
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = c("block", "treatment_forb", "treatment_predator",
                                       "leychin_biomass_g_m2", "leaf.dmg_2020",
                                       "leaf.dmg_2021", "leaf.dmg_2022")) %>%
  # Drop unwanted row(s)
  dplyr::filter(block != "Block") %>%
  # Rotate longer
  tidyr::pivot_longer(cols = dplyr::contains("leaf.dmg"),
                      values_to = "leaf_damage_perc") %>%
  # Break apart 'name' column
  tidyr::separate_wider_delim(cols = name, names = c("junk", "year"), delim = "_") %>%
  # Ditch leftover junk
  dplyr::select(-junk) %>%
  # Relocate date information columns
  dplyr::relocate(year, .before = block) %>%
  dplyr::mutate(month = 8, .after = year) %>%
  # Get things in correct classes
  dplyr::mutate(dplyr::across(.cols = dplyr::contains(c("year", "biomass", "damage")),
                              .fns = as.numeric)) %>%
  dplyr::mutate(block = paste0("block ", block))

# Check structure
dplyr::glimpse(df22_leaf)

# Do needed wrangling for grasshopper data
df22_ghop <- df22_v1 %>%
  # Grab desired columns
  dplyr::select(`August, 2022, L. chinensis biomass, in the 2 m2 mesocosms`,
                ...3:...5,
                `August, 2020...14`, `2021...15`, `2022...16`,
                `August, 2020...19`, `2021...20`, `2022...21`,
                `August, 2020...24`, `2021...25`, `2022...26`,
                `August, 2020...29`, `2021...30`, `2022...31`) %>%
  # Drop any completely empty columns
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.)))) %>%
  # Rename columns based on 'real' column name row
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = c("block", "treatment_forb", "treatment_predator",
                                       "ghop.feed_2020", "ghop.feed_2021", "ghop.feed_2022",
                                       "ghop.walk_2020", "ghop.walk_2021", "ghop.walk_2022",
                                       "ghop.jump_2020", "ghop.jump_2021", "ghop.jump_2022",
                                       "ghop.miss_2020", "ghop.miss_2021", "ghop.miss_2022")) %>%
  # Drop unwanted row(s)
  dplyr::filter(block != "Block") %>%
  # Rotate longer
  tidyr::pivot_longer(cols = dplyr::contains("ghop"),
                      values_to = "count") %>%
  # Break apart 'name' column
  tidyr::separate_wider_delim(cols = name, names = c("behavior", "year"), delim = "_") %>%
  # Tidy up the 'behavior' entries for use as columns
  dplyr::mutate(behavior = dplyr::case_when(
    behavior == "ghop.feed" ~ "grasshopper_feed_times_4h",
    behavior == "ghop.walk" ~ "grasshopper_walk_times_4h",
    behavior == "ghop.jump" ~ "grasshopper_jump_times_4h",
    behavior == "ghop.miss" ~ "grasshopper_missing_count")) %>%
  # Pivot back to (slightly) wide format
  tidyr::pivot_wider(names_from = behavior, values_from = count) %>%
  # Relocate date information columns
  dplyr::relocate(year, .before = block) %>%
  dplyr::mutate(month = 8, .after = year) %>%
  # Get things in correct classes
  dplyr::mutate(dplyr::across(.cols = dplyr::contains(c("year", "grasshopper")),
                              .fns = as.numeric)) %>%
  dplyr::mutate(block = paste0("block ", block)) %>%
  # Compute mortality
  dplyr::mutate(grasshopper_mortality_perc = (grasshopper_missing_count / 30) * 100)

# Check structure
dplyr::glimpse(df22_ghop)

# Do needed wrangling for mantis data
df22_mantis <- df22_v1 %>%
  # Grab desired columns
  dplyr::select(...39:...41,
                `August, 2020...43`, `2021...44`, `2022...45`,
                `August, 2020...48`, `2021...49`, `2022...50`,
                `August, 2020...53`, `2021...54`, `2022...55`) %>%
  # Drop any completely empty columns
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.)))) %>%
  # Rename columns based on 'real' column name row
  supportR::safe_rename(data = ., bad_names = names(.),
                        good_names = c("block", "treatment_forb", "treatment_predator",
                                       "mant.walk_2020", "mant.walk_2021", "mant.walk_2022",
                                       "mant.jump_2020", "mant.jump_2021", "mant.jump_2022",
                                       "mant.atk_2020", "mant.atk_2021", "mant.atk_2022")) %>%
  # Drop unwanted row(s)
  dplyr::filter(block != "Block") %>%
  # Rotate longer
  tidyr::pivot_longer(cols = dplyr::contains("mant"),
                      values_to = "count") %>%
  # Break apart 'name' column
  tidyr::separate_wider_delim(cols = name, names = c("behavior", "year"), delim = "_") %>%
  # Tidy up the 'behavior' entries for use as columns
  dplyr::mutate(behavior = dplyr::case_when(
    behavior == "mant.walk" ~ "mantis_walk_times_4h",
    behavior == "mant.jump" ~ "mantis_jump_times_4h",
    behavior == "mant.atk" ~ "mantis_attack_times_4h")) %>%
  # Pivot back to (slightly) wide format
  tidyr::pivot_wider(names_from = behavior, values_from = count) %>%
  # Relocate date information columns
  dplyr::relocate(year, .before = block) %>%
  dplyr::mutate(month = 8, .after = year) %>%
  # Get things in correct classes
  dplyr::mutate(dplyr::across(.cols = dplyr::contains(c("year", "mantis")),
                              .fns = as.numeric)) %>%
  dplyr::mutate(block = paste0("block ", block))

# Check structure
dplyr::glimpse(df22_mantis)

# Need to join these together into one data object
df22_v2 <- df22_leaf %>%
  # Attach grasshopper data
  dplyr::full_join(x = ., y = df22_ghop,
                   by = c("year", "month", "block",
                          "treatment_forb", "treatment_predator")) %>%
  # Then attach mantis data
  dplyr::full_join(x = ., y = df22_mantis,
                   by = c("year", "month", "block",
                          "treatment_forb", "treatment_predator"))

# Check structure
dplyr::glimpse(df22_v2)

# Export locally
write.csv(x = df22_v2, row.names = F, na = '',
          file = file.path("data", "zhong_2020-2022_treatment-effects.csv"))

# Clear environment
rm(list = ls()); gc()

# End ----
