# How to calculate biomass using tree-level data                            R Class | 16th November 2023 | Dheeraj
# An introduction into using R to collect, filter and calculate
# biomass using data from the REMOTE network


# 0 - Set-up --------------------------------------------------------------

# Set the working directory [if you desire to load/connect to a folder that is in a different location to the project]
setwd("D:/Documents/Carpathian")

# Loading in the libraries
library(pool); library(tidyverse); library(zoo); library(TapeR)

# Connect to the database
source("remote_db.r") # ensure the file is saved witihn the working directory, if not, specify it's exact location

# 1 - Data collection -----------------------------------------------------

# 1.1 Allometric equations ------------------------------------------------

# Here, we're acquiring the data from the dataset "biomass_eq" which contains the all
# important allometric equations for calculation LIVE tree biomass

## Allometric equations
eq <- tbl(KELuser, "biomass_eq") %>%
  collect() %>%
  mutate(branches_mass_f = gsub("ba_live_60", "ba_live_100", branches_mass_f),
         foliage_mass_f = gsub("ba_live_60", "ba_live_100", foliage_mass_f),
         root_mass_f = gsub("ba_live_60", "ba_live_100", root_mass_f),
         stem_mass_f = gsub("ba_live_60", "ba_live_100", stem_mass_f))


# You'll notice that two forms of columns are being pulled for each equation
# This is because there are two versions of each equation to fit both dbh
# thresholds - i.e. 6 cm and 10 cm.

## Wood density ~ n = 300
wood_density <- tbl(KELuser, "wood_density") %>% collect()

# Wood density values are crucial for calculation LIVE & DEAD biomass
# since it provide species/genus-specific values of wood density derived
# from the global wood density database - Harmon et al. (2008)
# These values are given in (g cm^3) as live wood density mass


# 1.2 - Plot-selection ----------------------------------------------------

# This is a useful feature for filtering your dataset to fit your desired
# plots for your study. So when you acquire data from the database, it will
# provide all the information pertaining to your plots you wish to investigate

plot.id <- tbl(KELuser, "plot") %>%
  filter(foresttype %in% c("beech", "spruce"),
         country %in% "Romania",
         census %in% 1, # for multiple censuses, used c(1:3) for up to 3 censuses, e.c.t.
         plottype %in% c(2,3,4,8)) %>% # selecting circular plots
  pull(id)


# 2 - Tree-level data -----------------------------------------------------

# 2.1 - Live [standing] trees ---------------------------------------------

tree.live <- tbl(KELuser, "tree") %>%
  filter(plot_id %in% plot.id,
         status %in% c(1:4), # specifying live trees
         !onplot %in% c(0, 99),
         dbh_mm >= 100, # setting the dbh threshold to 10 cm
         !species %in% "99") %>%
  inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
  select(plot_id, plotsize, treeid, species, dbh_mm) %>%
  collect() %>% ungroup()

# 2. 1. Basal area (living) ---------------------------------------------------------

# Some species/genus require the use of basal area as a secondary coefficient for
# calculating biomass, which is why we calculate it here

ba.live <- tree.live %>%
  mutate(dbh_cm = dbh_mm * 0.1,
         ba_100 = pi * dbh_mm ^2 / 4 / 1000000) %>%  # 1000000 mm per square metres
  group_by(plot_id, plotsize) %>%
  summarise(ba = round(sum(ba_100) * 10000 / first(plotsize), 4)) %>%
  ungroup()

# 2. 2. DBH (living) ----------------------------------------------------------------

# Optional step, if you wish to have mean plot-level dbh in a separate dataframe
dbh.live <- tree.live %>%
  group_by(plot_id) %>%
  summarise(dbh_live_100 = round(mean(dbh_mm), 0)) %>%
  ungroup()

# 2. 3. Biomass (living) ------------------------------------------------------------

## Calculate aboveground biomass on tree scale
biomass.tree <- tree.live %>%
  left_join(., eq, by = "species") %>%        # Specifying the allometric equations
  left_join(., ba.live, by = "plot_id") %>%   # Basal area data
  rowwise() %>%
  mutate(plotsize = plotsize.x,
         BM = eval(parse(text = branches_mass_f)), # Branch mass
         FM = eval(parse(text = foliage_mass_f)),  # Foliage mass
         SM = eval(parse(text = stem_mass_f)),     # Stem mass
         RM = eval(parse(text = root_mass_f)),     # Root mass, if necessary to calculate
         biomass_tree_total = BM + FM + SM + RM,
         biomass_tree_above = BM + FM + SM,
         biomass_tree_below = RM,
         biomass_tree_stem = SM,
         biomass_tree_foliage = BM + FM) %>%
  select(-plotsize.x, -plotsize.y) %>% # Caveat to removed duplicated columns. I'm sure there's a better way
  ungroup()

## Upscale from tree to plot scale [t/ha]
## Using the tree-level estimate, here we upscale biomass to one hectare for each plot
## represented in Mg ha^1 or t ha^1

biomass.plot <- biomass.tree %>%
  group_by(plot_id) %>%
  summarise(plotsize = first(plotsize), # since plot size varies, it's important to use the first value for each row [i.e. plot]
            ntrees_live = n(),
            biomass_plot_total = sum(biomass_tree_total),
            biomass_plot_total.sd = sd(biomass_tree_total), # total contains root mass
            biomass_plot_above = sum(biomass_tree_above),
            biomass_plot_above.sd = sd(biomass_tree_above),
            biomass_plot_below = sum(biomass_tree_below),
            biomass_plot_stem = sum(biomass_tree_stem),
            biomass_plot_foliage = sum(biomass_tree_foliage)) %>%
  mutate_at(vars(-plot_id, -plotsize, -ntrees_live), funs(. * 10 / plotsize)) %>%
  mutate_at(vars(-plot_id, -plotsize, -ntrees_live), funs(round(., 2))) %>%
  select(-plotsize) %>%
  ungroup()

# Ignore the warning

# 3. Tree level - dead [snags] ------------------------------------------------------

tree.dead <- tbl(KELuser, "tree") %>%
  filter(plot_id %in% plot.id,
         !onplot %in% c(0, 99),
         status %in% c(11:13,15,17:23), # Selecting dead trees only
         dbh_mm >= 100,
         !decayht %in% 99,
         !species %in% "99",
         decay %in% c(1:5) | decay_wood %in% c(1:5)) %>%
  inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
  select(plot_id, plotsize, treeid, species, dbh_mm, decay, decay_wood, decayht) %>%
  collect()

# 3. 1. Basal area (dead) -----------------------------------------------------------

ba.dead <- tree.dead %>%
  mutate(ba = pi * dbh_mm ^ 2 / 4 / 1000000) %>%
  group_by(plot_id) %>%
  summarise(ba_dead_100 = round(sum(ba) * 10000 / first(plotsize), 4)) %>%
  ungroup()

# 3. 2. DBH (dead) ------------------------------------------------------------------

dbh.dead <- tree.dead %>%
  group_by(plot_id) %>%
  summarise(dbh_dead_100 = round(mean(dbh_mm), 0)) %>%
  ungroup()


dbh <- dbh.live %>% left_join(., dbh.dead, by = "plot_id") # if necessary

# 3. 3. Biomass (dead) --------------------------------------------------------------

## Calculate aboveground biomass on tree scale
## Dead standing and downed biomass do not use the allometric equations for calculating biomass.
## Instead, we use the 'TapeR' package to help predict the tree height of all individualss using
## 'decayht' which is identified during forest inventories.

## Taper model for calculation of the tree volume
data(SK.par.lme)

necromass.tree <- tree.dead %>%
  mutate(decayht = case_when(
    decayht == 0 ~ 5,
    decayht == 1 ~ 15,
    decayht == 2 ~ 25,
    decayht == 3 ~ 35,
    decayht == 4 ~ 45,
    decayht == 5 ~ 55),
    decay_class = ifelse(decay_wood %in% 99, round(0.2924953 + 0.7131269 * decay, 0), decay_wood)) %>%
  left_join(., wood_density, by = c("species", "decay_class")) %>%
  rowwise() %>%
  mutate(volume_snag = E_VOL_AB_HmDm_HT.f(Hm = 1.3, Dm = (dbh_mm * 0.1),
                                          mHt = (log(dbh_mm * 0.1)-1.08261)^2/0.275541, sHt = 0, par.lme = SK.par.lme,
                                          A = 0, B = decayht, iDH = "H")$E_VOL,
         dead_tree_biomass = volume_snag * (density_gCm3 * relative_density * 1000)) %>%
  ungroup()

# In this step, we first predicted the height of each dead standing tree, calculating its volume then biomass


# Once predicted tree height is calculated, the latter part of the script uses this value to first
# calculate dead standing tree volume, which is converted into biomass

## Upscale from tree to plot scale [t/ha]
necromass.plot <- necromass.tree %>%
  group_by(plot_id) %>%
  summarise(plotsize = first(plotsize),
            ntrees_dead = n(),
            dead_standing = round(sum(dead_tree_biomass) * 10 / plotsize, 2),
            dead_standing.sd = round(sd(dead_tree_biomass) * 10 / plotsize, 2)) %>%
  select(-plotsize) %>%
  ungroup()



# 3.4 Coarse woody debris -------------------------------------------------

# 4. 1. Deadwood - transect ---------------------------------------------------------

deadwood.transect <- tbl(KELuser, "deadwood")  %>%
  filter(plot_id %in% plot.id,
         !species %in% "99",
         !decay %in% 99) %>%
  inner_join(., tbl(KELuser, "plot"), by = c("plot_id" = "id")) %>%
  filter(date < 2014 & dbh_mm >= 100 | date >= 2014 & dbh_mm >= 60) %>% # adjust, accordingly
  select(plot_id, species, decay, dbh_mm) %>%
  collect()

## Transect deadwood: Upscale to plot level [Mg ha]
biomass.lying.transect <- deadwood.transect %>%
  filter(dbh_mm >= 100) %>%
  left_join(., wood_density, by = c("species", "decay" = "decay_class")) %>%
  group_by(plot_id, species, decay) %>%
  summarise(volume = ((pi ^ 2 * sum((dbh_mm * 0.001) ^ 2)) / 800) * 10000,
            biomass = volume * (first(density_gCm3) * first(relative_density) * 1000),
  ) %>%
  group_by(plot_id) %>%
  summarise(biomass_lying_transect = round(sum(biomass) / 1000, 2),
            biomass_lying_transect.sd = round(sd(biomass) / 1000, 2)) %>%
  ungroup()

# 4 - Plot-level biomass --------------------------------------------------

# Now that we have biomass summed at the plot-level for live and dead standing trees,
# we are now ready to merge the datasets today, to obtain a total biomass value for each plot

{plot_biomass <- biomass.plot %>%
  full_join(., necromass.plot, by = "plot_id") %>% # Dead standing biomass
  full_join(., biomass.lying.transect, by = "plot_id") %>% # Dead downed / CWD
  replace(., is.na(.), 0) %>%
  summarise(n_total = ntrees_live + ntrees_dead,
            n_live = ntrees_live,
            n_dead = ntrees_dead,
            total_standing = biomass_plot_above + dead_standing + biomass_lying_transect,
            total_standing.sd = biomass_plot_above.sd + dead_standing.sd + biomass_lying_transect.sd,
            live = biomass_plot_above,
            live.sd = biomass_plot_above.sd,
            dead_standing = dead_standing,
            dead_standing.sd = dead_standing.sd,
            dead_cwd = biomass_lying_transect,
            dead_cwd.sd = biomass_lying_transect.sd)

remove(ba.dead, ba.live, biomass.plot, biomass.tree, dbh, dbh.dead, dbh.live, eq,
       deadwood.transect, biomass.lying.transect, SK.par.lme,
       necromass.tree, necromass.plot, tree.dead, tree.live, wood_density)
}

# 5 - Disconnect ----------------------------------------------------------
poolClose(KELuser); remove(KELuser)
#closeAllConnections(); rm(list=ls())