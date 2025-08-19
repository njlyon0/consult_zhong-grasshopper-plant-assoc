## --------------------------------------------------------- ##
# Zhong Grasshopper - Visualize Data
## --------------------------------------------------------- ##

# Purpose
## Generate the desired publication-quality figures

## --------------------------------- ##
# Housekeeping ----
## --------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, supportR, cowplot)

# Clear environment
rm(list = ls()); gc()

# Read in the various needed datasets
field_df <- read.csv(file.path("data", "zhong_2019_field-surveys.csv"))
og_df <- read.csv(file.path("data", "zhong_2020_original-conditions.csv"))
trt_df <- read.csv(file.path("data", "zhong_2020-2022_treatment-effects.csv"))

## --------------------------------- ##
# Fig. 1 (Interaction Diagram) ----
## --------------------------------- ##

# No code needed to produce this figure

## --------------------------------- ##
# Fig. 2 (Field Survey)
## --------------------------------- ##

# 2x2 grid of effect of forb cover and mantis abundance on grasshopper density and leaf damage respectively

# Check data structure
dplyr::glimpse(field_df)

# Graph L. chinensis against forb cover
fig2a <- ggplot(field_df, aes(x = forb_cover_perc, y = leychin_leaf_damage_perc)) +
  geom_point(size = 4, pch = 21, fill = "#99582a") +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "#000") +
  labs(x = "Forb Cover (%)", y = "Leaf Damage (%)") +
  supportR::theme_lyon(); fig2a

# Graph L. chinensis against mantis abundance
fig2b <- ggplot(field_df, aes(x = mean_mantis_abun_m2, y = leychin_leaf_damage_perc)) +
  geom_point(size = 4, pch = 21, fill = "#99582a") +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "#000") +
  labs(x = expression(paste("Mantis Density (no./", m^2, ")")),
       y = "Leaf Damage (%)") +
  supportR::theme_lyon(); fig2b

# Graph grasshopper abundance against forb cover
fig2c <- ggplot(field_df, aes(x = forb_cover_perc, y = mean_grasshopper_abun_m2)) +
  geom_point(size = 4, pch = 23, fill = "#a3b18a") +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "#000") +
  labs(x = "Forb Cover (%)",
       y = expression(paste("Grasshopper Density (no./", m^2, ")"))) +
  supportR::theme_lyon(); fig2c

# Graph grasshopper abundance against mantis abundance
fig2d <- ggplot(field_df, aes(x = mean_mantis_abun_m2, y = mean_grasshopper_abun_m2)) +
  geom_point(size = 4, pch = 23, fill = "#a3b18a") +
  # geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "#000") +
  labs(x = expression(paste("Mantis Density (no./", m^2, ")")),
       y = expression(paste("Grasshopper Density (no./", m^2, ")"))) +
  supportR::theme_lyon(); fig2d

# Get all graphs together
cowplot::plot_grid(fig2a, fig2b, fig2c, fig2d, nrow = 2, labels = "AUTO")

# Export locally
ggsave(filename = file.path("graphs", "fig-2_field-survey.png"),
       height = 8, width = 8, units = "in")

# Partially clear environment
rm(list = setdiff(ls(), c("field_df", "og_df", "trt_df"))); gc()

# End ----

