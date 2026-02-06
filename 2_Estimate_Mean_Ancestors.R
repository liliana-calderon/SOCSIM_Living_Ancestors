#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Living Ancestors - Estimate Mean Number of Living Ancestors
# U:/SOCSIM/SOCSIM_Living_Ancestors/2_Estimate_Mean_Ancestors.R

## Estimate living ancestors from a SOCSIM microsimulation for Sweden (1751-2100)

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 27-10-2025
# Last modified on 05-02-2026
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

# Get pids of individuals born between 1751 and 2000
egos <- opop %>% 
  mutate(birth_year = asYr(dob, last_month, final_sim_year)) %>% 
  filter(birth_year >=1751 & birth_year <= 2000) %>% 
  pull(pid)

# Check length of number of egos selected
length(egos) # 2845816

# Retrieve pids of ancestors of individuals born 1751-2000
ancestors <- retrieve_ancestors(opop = opop, egos = egos) 
save(ancestors, file = "ancestors.RData")


# Load data of ancestors
load("ancestors.RData")

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
  # Keep only ancestors who died in/after egos' birth month or are still alive
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
  mutate(Sex = ifelse(kin_type %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female Ancestors", "Male Ancestors")) %>%
  split(.$Sex) %>%
  map(~ ggplot(., aes(x= dob_ego_year, mean_ancestors, group = kin_type, colour = kin_type)) +
        geom_line(linewidth = 1) +
        facet_wrap(~Sex)+
        theme_graphs() +
        scale_color_viridis_d(option = "D", direction =-1)+
        scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2050, 2100))+
        scale_y_continuous(breaks = c(0, 0.5, 1, 1.5, 2), limits = c(0, 2))+
        labs(x = "Birth Cohort", y = "Mean Number Alive at Birth", colour = "Ancestor")+
        theme(legend.title=element_blank())) %>% 
  cowplot::plot_grid(plotlist = .)
ggsave(file="Graphs/1_Mean_Number_Ancestors_Birth.jpeg", width=20, height=9, dpi=300)
ggsave(file="Graphs/1_Mean_Number_Ancestors_Birth.pdf", width=20, height=9, dpi=300, device = "pdf")


# Plot the sex gap in the mean number of ancestors alive at birth
mean_ancestors_birth %>% 
  select(-c(n_kin, n_ego)) %>% 
  filter(kin_type != "Ego") %>% 
  mutate(Sex = ifelse(kin_type %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female", "Male"),
         Generation = case_when(kin_type %in% c("Mother","Father" ) ~ "Parents",
                                kin_type %in% c("Grandmothers","Grandfathers" ) ~ "Grandparents",
                                kin_type %in% c("Greatgrandmothers","Greatgrandfathers" ) ~ "Greatgrandparents",
                                kin_type %in% c("Greatgreatgrandmothers","Greatgreatgrandfathers") ~ "Greatgreatgrandparents",
                                TRUE ~ NA), 
         Generation = factor(Generation, levels = c("Parents", "Grandparents", 
                                                    "Greatgrandparents", "Greatgreatgrandparents"))) %>% 
  select(-kin_type) %>% 
  pivot_wider(names_from = Sex, values_from = mean_ancestors) %>% 
  mutate(Sex_gap = Female - Male) %>% 
  ggplot(aes(x= dob_ego_year, Sex_gap, group = Generation, colour = Generation)) +
  geom_line(linewidth = 1) +
  theme_graphs() +
  scale_color_viridis_d(option = "D", direction =-1)+
  labs(x = "Birth Cohort", y = "Sex Gap in the Mean Number Alive at Birth", colour = "Ancestor") +
  theme(legend.title=element_blank())
ggsave(file="Graphs/App_Mean_Number_Ancestors_Birth_Sex_Gap.jpeg", width=20, height=9, dpi=300)
ggsave(file="Graphs/App_Mean_Number_Ancestors_Birth_Sex_Gap.pdf", width=20, height=9, dpi=300, device = "pdf")


mean_ancestors_birth %>% 
  select(-c(n_kin, n_ego)) %>% 
  filter(kin_type != "Ego") %>% 
  mutate(Sex = ifelse(kin_type %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female", "Male"),
         Generation = case_when(kin_type %in% c("Mother","Father" ) ~ "Parents",
                                kin_type %in% c("Grandmothers","Grandfathers" ) ~ "Grandparents",
                                kin_type %in% c("Greatgrandmothers","Greatgrandfathers" ) ~ "Greatgrandparents",
                                kin_type %in% c("Greatgreatgrandmothers","Greatgreatgrandfathers") ~ "Greatgreatgrandparents",
                                TRUE ~ NA), 
         Generation = factor(Generation, levels = c("Parents", "Grandparents", 
                                                    "Greatgrandparents", "Greatgreatgrandparents"))) %>% 
  select(-kin_type) %>% 
  pivot_wider(names_from = Sex, values_from = mean_ancestors) %>% 
  mutate(Sex_gap = Female - Male) %>% 
  filter(dob_ego_year <1850) %>% 
  select(-c(Female, Male)) %>% 
  group_by(Generation) %>% 
  summarise(Sex_gap_mean = mean(Sex_gap, na.rm = T)) %>% 
  ungroup()