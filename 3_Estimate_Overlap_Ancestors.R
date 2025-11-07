#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Living Ancestors - Estimate Overlap Ancestors
# U:/SOCSIM/SOCSIM_Living_Ancestors/2_Estimate_Ancestors.R

## Estimate overlap with living ancestors from a SOCSIM microsimulation for Sweden (1751-2100)

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 04-11-2025
# Last modified on 07-11-2025
#----------------------------------------------------------------------------------------------------
## General settings and functions -----
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=999999)

## Load packages 
library(tidyverse)
library(viridis)
library(cowplot) # For separate legends of facetted plots
library(ggh4x)  # To facet scales-
#library(patchwork) # To combine ggplots
#library(svglite) # To save svg files

## Load function to retrieve direct ancestors
source("Functions/Functions_Ancestors.R")

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

#------------------------------------------------------------------------------------------------------
## Estimate overlap with ancestors of cohorts born over 1750-2000 ----

## Load opop generated in 0_Set_Up_Simulation.R 
load("opop.RData")

# Load pids of ancestors of individuals born 1750-2000
load("ancestors.RData")

# Define some constants 
last_month <- opop %>% pull(dod) %>% max()
final_sim_year <- 2100

# Calculate overlap with all ancestors per ego, considering dod==0 as censored at last_month
overlap_all_ego <- ancestors %>%
  # Add dates of birth and death of egos and their ancestors
  left_join(select(opop, c(pid, dob, dod)), 
            by = join_by("pid_ego" == "pid")) %>% 
  rename(dob_ego = dob, dod_ego = dod) %>% 
  left_join(select(opop, c(pid, dob, dod)), 
            by = join_by("pid_kin" == "pid")) %>% 
  rename(dob_kin = dob, dod_kin = dod) %>% 
  filter(kin_type != "ego") %>%
  # Set last_month as dod for ego and kin alive at the end of the simulation
  mutate(dod_ego2 = if_else(dod_ego == 0, last_month, dod_ego),
         dod_kin2 = if_else(dod_kin == 0, last_month, dod_kin)) %>% 
  select(pid_ego, dob_ego, kin_type, dod_ego2, dod_kin2) %>% 
  # Compute overlap in months: 0 when dob_ego = dod_kin 
  mutate(overlap_end = pmin(dod_ego2, dod_kin2),
         overlap_months_ego = pmax(0L, overlap_end - dob_ego),
         dob_ego_year = asYr(dob_ego, last_month, final_sim_year))
  # Sum overlap months per ego and kin_type (grouping ancestors of same kin_type2)
  # group_by(pid_ego, dob_ego, kin_type) %>%
  # summarise(overlap_months_ego = sum(overlap_months_ego, na.rm = TRUE),
  #           .groups = "drop")

# Calculate overlap with all ancestors  
overlap_all <- overlap_all_ego %>% 
  # Aggregate by cohort and kin_type
  group_by(dob_ego_year, kin_type) %>%
  summarise(overlap_months = sum(overlap_months_ego, na.rm = TRUE),
            .groups = "drop") %>%
  # Add count of distinct egos per birth year
  left_join(overlap_all_ego %>%
              distinct(dob_ego_year, pid_ego) %>%
              count(dob_ego_year, name = "n_ego"), 
            by = "dob_ego_year") %>%
  # Calculate mean overlap in months and years
  mutate(overlap_months_mean = overlap_months / n_ego,
         overlap_years_mean = overlap_months_mean / 12)

# Calculate maximum breaks for each generation
generation_max <- overlap_all %>% 
  recode_ancestors_sex2() %>% 
  group_by(kin_type2, dob_ego_year) %>%
  summarise(total = sum(overlap_years_mean, na.rm = TRUE), .groups = "drop") %>% 
  mutate(generation = case_when(kin_type2 %in% c("Mother","Father" ) ~ 4,
                                kin_type2 %in% c("Grandmothers","Grandfathers" ) ~ 3,
                                kin_type2 %in% c("Greatgrandmothers","Greatgrandfathers" ) ~ 2,
                                kin_type2 %in% c("Greatgreatgrandmothers","Greatgreatgrandfathers") ~ 1,
                                TRUE ~ NA)) %>% 
  group_by(generation) %>%
  summarise(max_total = max(total, na.rm = TRUE), .groups = "drop")

# Define maximum y breaks for each generation
y_scales_list <- setNames(lapply(generation_max$max_total, function(x) {
  brks <- pretty(c(0, x), n = 4)    
  ymax <- max(brks)
  scale_y_continuous(expand = c(0, 0), limits = c(0, ymax + 0.1 * ymax), breaks = brks)}), 
  generation_max$generation)
  
# Plot overlap with all living ancestors
overlap_all %>% 
  recode_ancestors_sex2() %>% 
  mutate(Sex = ifelse(kin_type2 %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female Ancestors", "Male Ancestors"),
         # Order ancestors to get maternal in lighter colours than paternal
         kin_type = factor(kin_type, levels = c("mom", 
                                               "mm", "mmm", "mfm", 
                                               "mmmm", "mmfm", "mfmm", "mffm",  
                                               "mf", "mmf", "mff",
                                               "mmmf", "mmff", "mfmf", "mfff",
                                               "pop",
                                               "fm", "fmm", "ffm", 
                                               "fmmm", "fmfm", "ffmm", "fffm", 
                                               "ff", "fmf", "fff",
                                               "fmmf", "fmff", "ffmf", "ffff" ))) %>%
  split(.$Sex) %>%
  map(~ ggplot(., aes(x= dob_ego_year, overlap_years_mean, fill = kin_type)) +
        geom_col(position = "stack", width = 1) +
        facet_grid(kin_type2 ~ Sex, scales = "free_y")+
        scale_fill_viridis_d(option = "F", direction =-1)+
        scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2050, 2100))+
        facetted_pos_scales(y = y_scales_list) +
        labs(x = "Birth Cohort", y = "Mean person-years of overlap with all ancestors", colour = "Ancestor") +
        theme_graphs() +
        theme(legend.title=element_blank(),
              strip.text = element_text(size = 22))) %>% 
  cowplot::plot_grid(plotlist = .)
ggsave(file="Graphs/5_Overlap_All_Ancestors.jpeg", width=20, height=18, dpi=300)
ggsave(file="Graphs/5_Overlap_All_Ancestors_Birth.pdf", width=20, height=18, dpi=300, device = "pdf")