#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Living Ancestors - Estimate distribution of the number of generations alive 
# U:/SOCSIM/SOCSIM_Living_Ancestors/3_Estimate_Distribution_Generations.R

# Estimate the distribution of the number of generations (ego and ancestors) alive at birth and at age 20 
# of the Swedish synthetic cohorts born 1751--2000 from a SOCSIM microsimulation for Sweden (1751-2100)

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 04-03-2026
# Last modified on 06-03-2026
#----------------------------------------------------------------------------------------------------
## General settings and functions -----
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=999999)

## Load packages 
library(tidyverse)
library(viridis)
library(cowplot) # For separate legends of faceted plots
library(ggh4x)  # To facet scales-
#library(patchwork) # To combine ggplots
#library(svglite) # To save svg files

## Load function to retrieve direct ancestors
source("Functions/Functions_Ancestors.R")

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

#------------------------------------------------------------------------------------------------------
## Load and wrangle ancestors data ----

# Load data of ancestors
load("ancestors.RData")

## Load opop generated in 0_Set_Up_Simulation.R 
load("opop.RData")

# Define some constants 
last_month <- opop %>% pull(dod) %>% max()
final_sim_year <- 2100

# Add sex and dates of birth and death of egos and their ancestors
ancestors2 <- ancestors %>%
  left_join(select(opop, c(pid, dob, dod, fem)), 
            by = join_by("pid_ego" == "pid")) %>% 
  rename(dob_ego = dob, dod_ego = dod, fem_ego = fem) %>% 
  left_join(select(opop, c(pid, dob, dod, fem)), 
            by = join_by("pid_kin" == "pid")) %>% 
  rename(dob_kin = dob, dod_kin = dod, fem_kin = fem)

#------------------------------------------------------------------------------------------------------
## Calculate cumulative distribution of the number of generations (ego and ancestors) alive at birth and at age 20 ----
#  An individual is counted as living in a 3-generation family if they have at least one parent AND at least one grandparent alive 
# (i.e., all intermediate generations must be present).

# Calculate (cumulative) distribution of the number of generations (ego and ancestors) alive at birth
multigenerations_birth <- ancestors2 %>%
  recode_ancestors_sex() %>%
  # Keep only ancestors who died in/after egos' birth month or are still alive
  filter(dob_ego <= dod_kin | dod_kin == 0) %>%
  # Estimate birth year of ego and assign generation of living kin
  mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year), 
         generation = case_when(kin_type == "Ego" ~ "Ego",
                                kin_type %in% c("Mother","Father" ) ~ "Parents",
                                kin_type %in% c("Grandmothers","Grandfathers" ) ~ "Grandparents",
                                kin_type %in% c("Greatgrandmothers","Greatgrandfathers" ) ~ "Greatgrandparents",
                                kin_type %in% c("Greatgreatgrandmothers","Greatgreatgrandfathers") ~ "Greatgreatgrandparents",
                                TRUE ~ NA_character_)) %>% 
  filter(!is.na(generation)) %>%
  # Check if each ego has any ancestor of each generation
  group_by(pid_ego, dob_ego_year, generation) %>%
  summarise(any_ancestor = TRUE, .groups = "drop") %>%
  # Pivot to wide format to have one column per generation
  pivot_wider(names_from = generation, 
              values_from = any_ancestor, 
              values_fill = FALSE) %>%
  # Estimate cumulative number of generations alive (Must have all previous generations alive)
  mutate(any_parent = Parents,
         any_grandparent = Parents & Grandparents,
         any_greatgrandparent = Parents & Grandparents & Greatgrandparents,
         any_greatgreatgrandparent = Parents & Grandparents & Greatgrandparents & Greatgreatgrandparents,
         # Count generations
         n_generations = case_when(any_greatgreatgrandparent ~ 5,
                                   any_greatgrandparent ~ 4,
                                   any_grandparent ~ 3,
                                   any_parent ~ 2,
                                   TRUE ~ 1)) %>%
  # Calculate proportion of individuals with each number of generations alive by cohort
  group_by(dob_ego_year, n_generations) %>%
  summarise(n_egos_size = n(), .groups = "drop_last") %>%
  mutate(n_egos = sum(n_egos_size),
         proportion = n_egos_size / n_egos * 100) %>%
  ungroup()

# Plot (cumulative) distribution of the number of generations (ego and ancestors) alive at birth
plot_multigenerations_birth <- multigenerations_birth %>% 
  mutate(age_label = "Age 0") %>% 
  ggplot(aes(x = dob_ego_year, y = proportion, fill = factor(n_generations))) +
  geom_col(position = position_stack(reverse = TRUE), width = 1) +  
  facet_wrap(~ age_label) + 
  scale_fill_viridis_d(option = "D", direction = -1) +
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000)) +
  labs(x = "Birth Cohort", 
       y = "Proportion (%)", 
       fill = "Number of generations") +
  theme_graphs()
#ggsave(file="Graphs/2_Distribution_Generations_Alive_Birth.jpeg", width=20, height=9, dpi=300)
#ggsave(file="Graphs/2_Distribution_Generations_Alive_Birth.pdf", width=20, height=9, dpi=300, device = "pdf")


# Calculate (cumulative) distribution of the number of generations (ego and ancestors) alive at age 20
multigenerations_20 <- ancestors2 %>%
  recode_ancestors_sex() %>%
  # Calculate month of ego's age 20
  mutate(ego_age_20 = dob_ego + 20 * 12) %>%
  # Filter ego who died after age 20 
  filter(dod_ego > ego_age_20) %>% 
  # Keep ancestors who died after egos' age 20 or are still alive
  filter(ego_age_20 < dod_kin | dod_kin == 0) %>%
  mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year), 
         generation = case_when(kin_type == "Ego" ~ "Ego",
                                kin_type %in% c("Mother","Father") ~ "Parents",
                                kin_type %in% c("Grandmothers","Grandfathers") ~ "Grandparents",
                                kin_type %in% c("Greatgrandmothers","Greatgrandfathers") ~ "Greatgrandparents",
                                kin_type %in% c("Greatgreatgrandmothers","Greatgreatgrandfathers") ~ "Greatgreatgrandparents",
                                TRUE ~ NA_character_)) %>% 
  filter(!is.na(generation)) %>%
  # Check if each ego has any ancestor of each generation
  group_by(pid_ego, dob_ego_year, generation) %>%
  summarise(any_ancestor = TRUE, .groups = "drop") %>%
  # Pivot to wide format to have one column per generation
  pivot_wider(names_from = generation, 
              values_from = any_ancestor, 
              values_fill = FALSE) %>%
  # Estimate cumulative number of generations alive (Must have all previous generations alive)
  mutate(any_parent = Parents,
         any_grandparent = Parents & Grandparents,
         any_greatgrandparent = Parents & Grandparents & Greatgrandparents,
         any_greatgreatgrandparent = Parents & Grandparents & Greatgrandparents & Greatgreatgrandparents,
         # Count generations
         n_generations = case_when(any_greatgreatgrandparent ~ 5,
                                   any_greatgrandparent ~ 4,
                                   any_grandparent ~ 3,
                                   any_parent ~ 2,
                                   TRUE ~ 1)) %>%
  # Calculate proportion of individuals with each number of generations alive by cohort
  group_by(dob_ego_year, n_generations) %>%
  summarise(n_egos_size = n(), .groups = "drop_last") %>%
  mutate(n_egos = sum(n_egos_size),
         proportion = n_egos_size / n_egos * 100) %>%
  ungroup()

# Plot (cumulative) distribution of the number of generations (ego and ancestors) alive at age 20
plot_multigenerations_20 <- 
  multigenerations_20 %>% 
  mutate(age_label = "Age 20") %>% 
  ggplot(aes(x = dob_ego_year, y = proportion, fill = factor(n_generations))) +
  geom_col(position = position_stack(reverse = TRUE), width = 1) +  
  facet_wrap(~ age_label) + 
  scale_fill_manual(values = c("#F4A460", "#FDE725FF", "#35B779FF", "#31688EFF", "#440154FF"))+ # Based on viridis, option D -1
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000)) +
  labs(x = "Birth Cohort", 
       y = "Proportion (%)", 
       fill = "Number of generations") +
  theme_graphs()
#ggsave(file="Graphs/2_Distribution_Generations_Alive_Age_20.jpeg", width=20, height=9, dpi=300)
#ggsave(file="Graphs/2_Distribution_Generations_Alive_Age_20.pdf", width=20, height=9, dpi=300, device = "pdf")


#------------------------------------------------------------------------------------------------------
## Calculate non-cumulative distribution of the number of generations (ego and ancestors) alive at birth and age 20 ----

# Check alternative estimation that allow for 'skipped' generations (e.g., grandparent alive without living parent) 

# Calculate (non-cumulative) distribution of the number of generations (ego and ancestors) alive at birth
# Younger generation does not have to be alive to count Older generation
multigenerations_birth_non_cum <- ancestors2 %>%
  recode_ancestors_sex() %>%
  # Keep only ancestors who died in/after egos' birth month or are still alive
  filter(dob_ego <= dod_kin | dod_kin == 0) %>%
  # Estimate birth year of ego and assign generation of living kin
  mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year), 
         generation = case_when(kin_type == "Ego" ~ "Ego",
                                kin_type %in% c("Mother","Father" ) ~ "Parents",
                                kin_type %in% c("Grandmothers","Grandfathers" ) ~ "Grandparents",
                                kin_type %in% c("Greatgrandmothers","Greatgrandfathers" ) ~ "Greatgrandparents",
                                kin_type %in% c("Greatgreatgrandmothers","Greatgreatgrandfathers") ~ "Greatgreatgrandparents",
                                TRUE ~ NA_character_)) %>% 
  filter(!is.na(generation)) %>%
  # Check if each ego has any ancestor of each generation
  group_by(pid_ego, dob_ego_year, generation) %>%
  summarise(any_ancestor = TRUE, .groups = "drop") %>%
  # Pivot to wide format to have one column per generation
  pivot_wider(names_from = generation, 
              values_from = any_ancestor, 
              values_fill = FALSE) %>%
  # Sum number of generations alive (i.e., TRUE)
  mutate(n_generations = 1 +  # Count ego's generation
           as.integer(Parents) + 
           as.integer(Grandparents) + 
           as.integer(Greatgrandparents) + 
           as.integer(Greatgreatgrandparents)) %>%
  # Calculate proportion of individuals with each number of generations alive by cohort
  group_by(dob_ego_year, n_generations) %>%
  summarise(n_egos_size = n(), .groups = "drop_last") %>%
  mutate(n_egos = sum(n_egos_size),
         proportion = n_egos_size / n_egos * 100) %>%
  ungroup()

# Plot (non-cumulative) Proportion of individuals with multiple generations of living ancestors at birth
plot_multigenerations_birth_non_cum <- multigenerations_birth_non_cum %>% 
  mutate(age_label = "Age 0") %>% 
  ggplot(aes(x = dob_ego_year, y = proportion, fill = factor(n_generations))) +
  geom_col(position = position_stack(reverse = TRUE), width = 1) +  
  facet_wrap(~ age_label) + 
  scale_fill_viridis_d(option = "D", direction = -1) +
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000)) +
  labs(x = "Birth Cohort", 
       y = "Proportion (%)", 
       fill = "Number of generations") +
  theme_graphs()
#ggsave(file="Graphs/2_Distribution_Generations_Alive_Birth_non_cum.jpeg", width=20, height=9, dpi=300)
#ggsave(file="Graphs/2_Distribution_Generations_Alive_Birth_non_cum.pdf", width=20, height=9, dpi=300, device = "pdf")
# Compared to the cumulative plot, 2 generations are slightly lower and 3 generations slightly higher

# Calculate (non-cumulative) distribution of the number of generations (ego and ancestors) alive at age 20
# Generation -1 does not have to be alive to count generation -2

multigenerations_20_non_cum <- ancestors2 %>%
  recode_ancestors_sex() %>%
  # Calculate month of ego's age 20
  mutate(ego_age_20 = dob_ego + 20 * 12) %>%
  # Filter ego who died after age 20 
  filter(dod_ego > ego_age_20) %>% 
  # Keep ancestors who died after egos' age 20 or are still alive
  filter(ego_age_20 < dod_kin | dod_kin == 0) %>%
  mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year), 
         generation = case_when(kin_type == "Ego" ~ "Ego",
                                kin_type %in% c("Mother","Father") ~ "Parents",
                                kin_type %in% c("Grandmothers","Grandfathers") ~ "Grandparents",
                                kin_type %in% c("Greatgrandmothers","Greatgrandfathers") ~ "Greatgrandparents",
                                kin_type %in% c("Greatgreatgrandmothers","Greatgreatgrandfathers") ~ "Greatgreatgrandparents",
                                TRUE ~ NA_character_)) %>% 
  filter(!is.na(generation)) %>%
  # Check if each ego has any ancestor of each generation
  group_by(pid_ego, dob_ego_year, generation) %>%
  summarise(any_ancestor = TRUE, .groups = "drop") %>%
  # Pivot to wide format to have one column per generation
  pivot_wider(names_from = generation, 
              values_from = any_ancestor, 
              values_fill = FALSE) %>%
  # Sum total generations alive (i.e., TRUE)
  mutate(n_generations = 1 +  # Count ego's generation
           as.integer(Parents) + 
           as.integer(Grandparents) + 
           as.integer(Greatgrandparents) + 
           as.integer(Greatgreatgrandparents)) %>%
  # Calculate proportion of individuals with each number of generations alive by cohort
  group_by(dob_ego_year, n_generations) %>%
  summarise(n_egos_size = n(), .groups = "drop_last") %>%
  mutate(n_egos = sum(n_egos_size),
         proportion = n_egos_size / n_egos * 100) %>%
  ungroup()


# Plot (non-cumulative) distribution of the number of generations (ego and ancestors) alive at birth
plot_multigenerations_20_non_cum <- 
  multigenerations_20_non_cum %>% 
  mutate(age_label = "Age 20") %>% 
  ggplot(aes(x = dob_ego_year, y = proportion, fill = factor(n_generations))) +
  geom_col(position = position_stack(reverse = TRUE), width = 1) +  
  facet_wrap(~ age_label) + 
  scale_fill_manual(values = c("#F4A460", "#FDE725FF", "#35B779FF", "#31688EFF", "#440154FF"))+ # Based on viridis, option D -1
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000)) +
  labs(x = "Birth Cohort", 
       y = "Proportion (%)", 
       fill = "Number of generations") +
  theme_graphs()
#ggsave(file="Graphs/2_Distribution_Generations_Alive_Age_20_non_cum.jpeg", width=20, height=9, dpi=300)
#ggsave(file="Graphs/2_Distribution_Generations_Alive_Age_20_non_cum.pdf", width=20, height=9, dpi=300, device = "pdf")

#------------------------------------------------------------------------------------------------------
## Combine plots of distributions of the number of generations alive at birth and at age 20 ----

# Combine plots of cumulative distributions of the number of generations alive
cowplot::plot_grid(plot_multigenerations_birth + theme(legend.position = "none"), 
                   plot_multigenerations_20, nrow = 2)
ggsave(file="Graphs/2_Distribution_Generations_Alive_Birth_20.jpeg", width=20, height=18, dpi=300)
ggsave(file="Graphs/2_Distribution_Generations_Alive_Birth_20.pdf", width=20, height=18, dpi=300, device = "pdf")


# Combine plots of non-cumulative distributions of the number of generations alive
cowplot::plot_grid(plot_multigenerations_birth_non_cum + theme(legend.position = "none"), 
                   plot_multigenerations_20_non_cum, nrow = 2)
ggsave(file="Graphs/2_Distribution_Generations_Alive_Birth_20_non_cum.jpeg", width=20, height=18, dpi=300)
ggsave(file="Graphs/2_Distribution_Generations_Alive_Birth_20_non_cum.pdf", width=20, height=18, dpi=300, device = "pdf")