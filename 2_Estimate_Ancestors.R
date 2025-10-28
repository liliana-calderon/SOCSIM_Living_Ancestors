#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Living Ancestors - Estimate Living Ancestors
# U:/SOCSIM/SOCSIM_Living_Ancestors/2_Estimate_Ancestors.R

## Estimate living ancestors from a SOCSIM microsimulation for Sweden (1751-2100)

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 27-10-2025
# Last modified on 28-10-2025
#----------------------------------------------------------------------------------------------------
## General settings and functions -----

rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=999999)

## Load packages 
library(tidyverse)
library(viridis)
library(cowplot) # For separate legends of facetted plots
#library(ggh4x)  # To facet scales-
#library(patchwork) # To combine ggplots
#library(svglite) # To save svg files

## Load function to retrieve direct ancestors
source("Functions/Functions_Ancestors.R")

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

#------------------------------------------------------------------------------------------------------
## Trace direct ancestors of cohorts born over 1750-2000 ----

## Load opop generated in 0_Set_Up_Simulation.R 
load("opop.RData")

# Define some constants 
last_month <- opop %>% pull(dod) %>% max()
final_sim_year <- 2100

# Get pids of individuals born between 1750 and 2000
egos <- opop %>% 
  mutate(birth_year = asYr(dob, last_month, final_sim_year)) %>% 
  filter(birth_year >= 1750) %>% ## Check if we add the filter of birth_year =< 2000
  pull(pid)

# Check length of number of egos selected
length(egos) # 3465578

# Retrieve pids of ancestors of selected individuals
ancestors <- retrieve_ancestors(opop = opop, egos = egos) 

# Add sex and dates of birth and death of egos and their ancestors
ancestors2 <- ancestors %>%
  left_join(select(opop, c(pid, dob, dod, fem)), 
            by = join_by("pid_ego" == "pid")) %>% 
  rename(dob_ego = dob, dod_ego = dod, fem_ego = fem) %>% 
  left_join(select(opop, c(pid, dob, dod, fem)), 
            by = join_by("pid_kin" == "pid")) %>% 
  rename(dob_kin = dob, dod_kin = dod, fem_kin = fem)

# Calculate mean number of ancestors alive at birth 
mean_ancestors_birth <- ancestors2 %>%
  recode_ancestors_sex() %>%
  # Keep only ancestors who died in/after egos birth month or are still alive
  filter(dob_ego <= dod_kin | dod_kin == 0) %>%
  mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year)) %>%
  # Count number of kin types by year
  count(dob_ego_year, kin_type, name = "n_kin") %>%
  group_by(dob_ego_year) %>%
  # Add number of egos as denominator for all kin_types
  mutate(n_ego = n_kin[kin_type == "Ego"], 
         mean_ancestors = n_kin / n_ego) %>%
  ungroup()

  
# Plot mean number of ancestors alive at birth

mean_ancestors_birth %>% 
  filter(kin_type != "Ego") %>% 
  # filter(dob_ego_year <= 2000) %>% 
  mutate(Sex = ifelse(kin_type %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female", "Male")) %>% 
  ggplot(aes(x= dob_ego_year, mean_ancestors, group = kin_type, colour = kin_type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~Sex)+
  theme_graphs() +
  scale_color_viridis_d(option = "G")+
  labs(x = "Birth Cohort", y = "Mean Number Alive at Birth", , colour = "Ancestor")


mean_ancestors_birth %>% 
  filter(kin_type != "Ego") %>% 
  # filter(dob_ego_year <= 2000) %>% 
  mutate(Sex = ifelse(kin_type %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female", "Male")) %>%
  split(.$Sex) %>%
  map(~ ggplot(., aes(x= dob_ego_year, mean_ancestors, group = kin_type, colour = kin_type)) +
        geom_line(linewidth = 1) +
        facet_wrap(~Sex)+
        theme_graphs() +
        scale_color_viridis_d(option = "G", direction = -1)+
        scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2050, 2100))+
        labs(x = "Birth Cohort", y = "Mean Number Alive at Birth", colour = "Ancestor")) %>% 
  cowplot::plot_grid(plotlist = .)
ggsave(file="Graphs/1_Mean_Number_Ancestors_Birth.jpeg", width=22, height=10, dpi=300)