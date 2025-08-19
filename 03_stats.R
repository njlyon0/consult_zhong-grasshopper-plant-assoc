## --------------------------------------------------------- ##
# Zhong Grasshopper - Analyze Data
## --------------------------------------------------------- ##

# Purpose
## Test core hypotheses of the project

## --------------------------------- ##
# Housekeeping ----
## --------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, lme4)

# Clear environment
rm(list = ls()); gc()

# Load custom function(s)
purrr::walk(.x = dir(path = file.path("tools")),
            .f = ~ source(file.path("tools", .x)))

## --------------------------------- ##
# Field Surveys ----
## --------------------------------- ##

# Read in data
field_df <- read.csv(file.path("data", "zhong_2019_field-surveys.csv"))

# Check structure
dplyr::glimpse(field_df)

# Test relationship between grasshopper abundance (per m2) and forb cover
aov_ghop.forb <- aov(mean_grasshopper_abun_m2 ~ forb_cover_perc, data = field_df)

# Tidy the output
(smry_ghop.forb <- parse_aov(mod = aov_ghop.forb))

# Export locally
write.csv(x = smry_ghop.forb, row.names = F, na = '',
          file = file.path("results", "field-survey_grasshoppers-v-forbs.csv"))

# Do the same for the relationship between leaf damage and forb cover
aov_dmg.forb <- aov(leychin_leaf_damage_perc ~ forb_cover_perc, data = field_df)
smry_dmg.forb <- parse_aov(mod = aov_dmg.forb)
write.csv(x = smry_dmg.forb, row.names = F, na = '',
          file = file.path("results", "field-survey_grass-damage-v-forbs.csv"))

# And for both of the previous response variable against mantis abundance
## Grasshoppers ~ Mantids
aov_ghop.mant <- aov(mean_grasshopper_abun_m2 ~ mean_mantis_abun_m2, data = field_df)
smry_ghop.mant <- parse_aov(mod = aov_ghop.mant)
write.csv(x = smry_ghop.mant, row.names = F, na = '',
          file = file.path("results", "field-survey_grasshoppers-v-mantids.csv"))

## Leaf damage ~ Mantids
aov_dmg.mant <- aov(leychin_leaf_damage_perc ~ mean_mantis_abun_m2, data = field_df)
smry_dmg.mant <- parse_aov(mod = aov_dmg.mant)
write.csv(x = smry_dmg.mant, row.names = F, na = '',
          file = file.path("results", "field-survey_grass-damage-v-mantids.csv"))

# Partially clear environment
rm(list = setdiff(x = ls(), y = c("parse_aov"))); gc()

## --------------------------------- ##
# Original Conditions ----
## --------------------------------- ##

# Read in data
og_df <- read.csv(file.path("data", "zhong_2020_original-conditions.csv"))

# Check structure
dplyr::glimpse(og_df)

# Check whether forb differs among plots before the experiment was applied
lm_forb <- lme4::lmer(forb_cover_perc ~ treatment_forb * treatment_predator +
                        (1 | block), data = og_df)
anova(lm_forb)

# Do the same for L. chinensis ('leychin') cover
lm_lchin <- lme4::lmer(leychin_cover_perc ~ treatment_forb * treatment_predator +
                         (1 | block), data = og_df)
anova(lm_lchin)

# Do the same for other grass cover
lm_grass <- lme4::lmer(other_grass_cover_perc ~ treatment_forb * treatment_predator +
                         (1 | block), data = og_df)
anova(lm_grass)

# Do the same for species richness
lm_rich <- lme4::lmer(spp_richness ~ treatment_forb * treatment_predator +
                         (1 | block), data = og_df)
anova(lm_rich)

# Partially clear environment
rm(list = setdiff(x = ls(), y = c("parse_aov"))); gc()





# End ----

