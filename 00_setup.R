## --------------------------------------------------------- ##
# Zhong Grasshopper - Setup
## --------------------------------------------------------- ##

# Purpose
## Create needed local folders
## Download 'raw' data from Google Drive

## --------------------------------- ##
# Housekeeping -----
## --------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Clear environment
rm(list = ls()); gc()

## --------------------------------- ##
# Make Folder(s) ----
## --------------------------------- ##

# Create desired folder(s)
dir.create(path = file.path("data", "raw"), showWarnings = F, recursive = T)
dir.create(path = file.path("graphs"), showWarnings = F)

## --------------------------------- ##
# Download Input(s) ----
## --------------------------------- ##

# Identify Drive folder
zhong_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1tlyKbUULHHEB5FzwmxqWziavfpwfU7tJ")

# List files in that folder
(zhong_files <- googledrive::drive_ls(path = zhong_drive))

# Download 'em
purrr::walk2(.x = zhong_files$id, .y = zhong_files$name,
            .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                               path = file.path("data", "raw", .y)))

# End ----
