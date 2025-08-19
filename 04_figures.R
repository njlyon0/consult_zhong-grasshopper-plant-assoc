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

# Identify some key aesthetics used across figures
forb_cols <- c("Forb" = "#f4a261", "No forb" = "#2a9d8f")

## --------------------------------- ##
# Fig. 1 (Interaction Diagram) ----
## --------------------------------- ##

# No code needed to produce this figure

## --------------------------------- ##
# Fig. 2 (Field Survey) ----
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
cowplot::plot_grid(fig2a, fig2b, fig2c, fig2d, nrow = 2, labels = "AUTO", align = "v")

# Export locally
ggsave(filename = file.path("graphs", "fig-2_field-survey.png"),
       height = 8, width = 8, units = "in")

# Partially clear environment
rm(list = setdiff(ls(), c("field_df", "og_df", "trt_df"))); gc()

## --------------------------------- ##
# Fig. 3 (Treatment Effect - L. chinensis) ----
## --------------------------------- ##

# Stacked plots for L. chinensis leaf damage / biomass response to factorial treatments

# Check structure of relevant data
dplyr::glimpse(trt_df)

# Summarize the relevant bit of the table
fig3a_df <- supportR::summary_table(data = trt_df, response = "mean_leaf_damage_perc",
                                    groups = c("treatment_forb", "treatment_predator"))

# Graph leaf damage against treatment
fig3a <- ggplot(fig3a_df, aes(x = treatment_predator, y = mean,
                              fill = treatment_forb)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymax = mean + std_error,
                    ymin = mean - std_error),
                position = position_dodge(width = 1),
                stat = "identity", width = 0.2) +
  labs(y = "Leaf Damage (%)") +
  scale_fill_manual(values = forb_cols) +
  geom_text(label = "b", x = 0.75, y = 28, size = 6) +
  geom_text(label = "a", x = 1.25, y = 37, size = 6) +
  geom_text(label = "c", x = 1.75, y = 20, size = 6) +
  geom_text(label = "c", x = 2.25, y = 22, size = 6) +
  ylim(0, 40) +
  supportR::theme_lyon() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.9),
        axis.title.x = element_blank()); fig3a

# Graph L. chinensis biomass against treatment
fig3b_df <- supportR::summary_table(data = trt_df, response = "leychin_biomass_g_m2",
                                    groups = c("treatment_forb", "treatment_predator"))

# Make the graph
fig3b <- ggplot(fig3b_df, aes(x = treatment_predator, y = mean,
                              fill = treatment_forb)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymax = mean + std_error,
                    ymin = mean - std_error),
                position = position_dodge(width = 1),
                stat = "identity", width = 0.2) +
  labs(y = expression(paste(italic("L. chinensis"), " Biomass (g/", m^2, ")"))) +
  scale_fill_manual(values = forb_cols) +
  geom_text(label = "b", x = 0.75, y = 115, size = 6) +
  geom_text(label = "a", x = 1.25, y = 95, size = 6) +
  geom_text(label = "b", x = 1.75, y = 125, size = 6) +
  geom_text(label = "b", x = 2.25, y = 127, size = 6) +
  ylim(0, 130) +
  supportR::theme_lyon() +
  theme(legend.position = "none",
        axis.title.x = element_blank()); fig3b

# Assemble figure
cowplot::plot_grid(fig3a, fig3b, nrow = 2, labels = "AUTO", align = "v")

# Export locally
ggsave(filename = file.path("graphs", "fig-3_l-chinensis-response.png"),
       height = 7, width = 4, units = "in")

# Partially clear environment
rm(list = setdiff(ls(), c("field_df", "og_df", "trt_df"))); gc()

# End ----

