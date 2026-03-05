#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Living Ancestors - Estimate Overlap Ancestors
# U:/SOCSIM/SOCSIM_Living_Ancestors/2_Estimate_Ancestors.R

## Estimate overlap with living ancestors from a SOCSIM microsimulation for Sweden (1751-2100)

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 04-11-2025
# Last modified on 04-03-2026
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
library(metR) # To label the contours
#library(patchwork) # To combine ggplots
#library(svglite) # To save svg files

## Load function to retrieve direct ancestors
source("Functions/Functions_Ancestors.R")

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

#------------------------------------------------------------------------------------------------------
## Estimate overlap with ancestors of cohorts born over 1750-2000 ----

# The calculation of overlap is strict. 
# Then if dob_ego and dod_kin are the same-month, overlap is 0. 
# For an inclusive version of overlapping months, 
# replace the overlap calculation with overlap_end - dob_ego + 1

## Load opop generated in 0_Set_Up_Simulation.R 
load("opop.RData")

# Load pids of ancestors of individuals born 1750-2000
load("ancestors.RData")

# Define some constants 
last_month <- opop %>% pull(dod) %>% max()
final_sim_year <- 2100


#### Calculate overlap with all ancestors per ego ----
# considering dod == 0 as censored at last_month
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
  select(pid_ego, dob_ego, dob_kin, kin_type, dod_ego2, dod_kin2) %>% 
  # Compute overlap in months with all ancestors: 0 when dob_ego = dod_kin 
  mutate(overlap_end = pmin(dod_ego2, dod_kin2),
         overlap_months_ego = pmax(0L, overlap_end - dob_ego),
         dob_ego_year = asYr(dob_ego, last_month, final_sim_year))
save(overlap_all_ego, file = "Measures/overlap_all_ego.RData")



## Calculate overlap with any ancestors by cohort and ancestors' generation ----
overlap_any <- overlap_all_ego %>%
  recode_ancestors_sex() %>%   
  # Identify maximum month of overlap end, by ego and kin_type
  group_by(pid_ego, dob_ego, dob_ego_year, kin_type) %>%
  summarise(max_overlap_end = max(overlap_end), 
            .groups = "drop") %>%
  # Compute overlap in months with any ancestors
  mutate(overlap_months_ego_any = pmax(0L, max_overlap_end - dob_ego)) %>% 
  # Aggregate by cohort and kin_type
  group_by(dob_ego_year, kin_type) %>%
  summarise(sum_overlap_months_any = sum(overlap_months_ego_any, na.rm = TRUE),
            .groups = "drop") %>%
  # Add count of distinct egos per birth year
  left_join(overlap_all_ego %>%
              distinct(dob_ego_year, pid_ego) %>%
              count(dob_ego_year, name = "n_egos"), 
            by = "dob_ego_year") %>%
  # Calculate mean overlap with any ancestor in months and years
  mutate(mean_overlap_months_any = sum_overlap_months_any / n_egos,
         mean_overlap_years_any  = mean_overlap_months_any / 12)

overlap_any %>% 
  group_by(kin_type) %>% 
  reframe(min = min(mean_overlap_years_any), 
          max = max(mean_overlap_years_any), 
          range = max - min)
  
# Plot overlap with any living ancestors, facetted by sex
overlap_any %>% 
  mutate(Sex = ifelse(kin_type %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female Ancestors", "Male Ancestors")) %>%
  split(.$Sex) %>%
  map(~ ggplot(., aes(x= dob_ego_year, mean_overlap_years_any, group = kin_type, colour = kin_type)) +
        geom_line(linewidth = 1) +
        facet_wrap(~ Sex)+
        scale_colour_viridis_d(option = "D", direction =-1)+
        scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2050, 2100))+
        scale_y_continuous(breaks = c(0, 20, 40, 60), limits = c(0, 60))+
        labs(x = "Birth Cohort", y = "Mean years of overlap with any ancestors generation", colour = "Ancestor") +
        theme_graphs() +
        theme(legend.title=element_blank(),
              strip.text = element_text(size = 22))) %>% 
  cowplot::plot_grid(plotlist = .)


# Plot overlap with any living ancestors
overlap_any %>% 
  mutate(Sex = ifelse(kin_type %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female", "Male"),
         Generation = case_when(kin_type %in% c("Mother","Father" ) ~ "Parents",
                                kin_type %in% c("Grandmothers","Grandfathers" ) ~ "Grandparents",
                                kin_type %in% c("Greatgrandmothers","Greatgrandfathers" ) ~ "Greatgrandparents",
                                kin_type %in% c("Greatgreatgrandmothers","Greatgreatgrandfathers") ~ "Greatgreatgrandparents",
                                TRUE ~ NA), 
         Generation = factor(Generation, levels = c("Parents", "Grandparents", 
                                                    "Greatgrandparents", "Greatgreatgrandparents"))) %>% 
  ggplot(aes(x= dob_ego_year, mean_overlap_years_any, group = kin_type, colour = Generation, linetype = Sex)) +
        geom_line(linewidth = 1) +
        scale_colour_viridis_d(option = "D", direction =-1)+
        scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2050, 2100))+
        scale_y_continuous(breaks = c(0, 20, 40, 60), limits = c(0, 60))+
        labs(x = "Birth Cohort", y = "Mean years of overlap with any ancestors generation", 
             colour = "Ancestors' Generation", linetype = "Ancestors' Sex") +
  guides(colour = guide_legend(order = 1),
         linetype = guide_legend(order = 2))+
        theme_graphs() +
        theme(strip.text = element_text(size = 22))
ggsave(file="Graphs/2_Overlap_Any_Ancestors.jpeg", width=20, height=9, dpi=300)
ggsave(file="Graphs/2_Overlap_Any_Ancestors.pdf", width=20, height=9, dpi=300, device = "pdf")


## Calculate age-specific estimates of overlap with any/all ancestors by cohort ----

# Create df with one row per reached age (in years) for each ego
ego_ages <- opop %>%
  rename(dob_ego = dob, dod_ego = dod) %>% 
  mutate(dob_ego_year = asYr(dob_ego, last_month, final_sim_year)) %>%
  filter(dob_ego_year >= 1751 & dob_ego_year <= 2000) %>%
  mutate(dod_ego2 = if_else(dod_ego == 0, last_month, dod_ego),
         max_age = floor((dod_ego2 - dob_ego) / 12)) %>%  
  select(pid_ego = pid, dob_ego, dod_ego2, dob_ego_year, max_age) %>%
  # Expand to one row per integer age
  mutate(age = map(max_age, ~0:.x)) %>%
  unnest(age) %>%
  # Calculate the month when ego reaches each year of age
  mutate(age_month = dob_ego + age * 12)


# Create a function to estimate the cohort x age measures by cohort groups. 
# This allow to handle the large number of rows 
# resulting from a data frame with several ego-age rows for many ancestors

estimates_cohort_age <- function(cohort_start, cohort_end) {
  
  cat("Processing cohorts:", cohort_start, "-", cohort_end, "\n")
  
  # Filter ego_ages for the selected cohorts
  ego_ages_cohort <- ego_ages %>%
    filter(dob_ego_year >= cohort_start & dob_ego_year <= cohort_end)
  
  # Get egos belonging to the selected cohorts
  cohort_pids <- unique(ego_ages_cohort$pid_ego)
  
  # Filter ancestors for these egos only
  ancestors_cohort <- ancestors %>%
    filter(pid_ego %in% cohort_pids)
  
  #  Join ancestors and check if they are alive at each age
  ego_age_ancestors <- ego_ages_cohort %>%
    left_join(ancestors_cohort, by = "pid_ego", 
              relationship = "many-to-many") %>% 
    filter(kin_type != "ego") %>%
    left_join(opop %>% select(pid_kin = pid, dob_kin = dob, dod_kin = dod),
              by = "pid_kin") %>%
    mutate(dod_kin2 = if_else(dod_kin == 0, last_month, dod_kin),
           ancestor_alive = if_else(age_month >= dob_kin & age_month < dod_kin2, 1L, 0L)) %>%
    recode_ancestors_sex()
  
  # Calculate proportion with at least one ancestor by age
  prop_any_age <- ego_age_ancestors %>%
    group_by(pid_ego, dob_ego_year, age, kin_type) %>%
    summarise(has_any = any(ancestor_alive == 1L), .groups = "drop") %>%
    # Aggregate by cohort-age-kin type
    group_by(dob_ego_year, age, kin_type) %>%
    summarise(n_with_any = sum(has_any, na.rm = TRUE), .groups = "drop") %>%
    # Complete with all possible combinations of cohort-age-kin type
    complete(dob_ego_year = cohort_start:cohort_end,
             age = 0:100,
             kin_type = unique(ego_age_ancestors$kin_type),
             fill = list(n_with_any = 0)) %>%
    filter(kin_type != "Ego") %>% 
    # Add number of egos by cohort and age
    left_join(ego_ages_cohort %>%
                distinct(dob_ego_year, age, pid_ego) %>%
                count(dob_ego_year, age, name = "n_egos"), 
              by = c("dob_ego_year", "age")) %>%
    mutate(prop_any = if_else(is.na(n_egos), NA_real_, n_with_any / n_egos))
  
  # Calculate mean number of ancestors alive at each age
  mean_number_age <- ego_age_ancestors %>%
    group_by(pid_ego, dob_ego_year, age, kin_type) %>%
    summarise(n_alive = sum(ancestor_alive, na.rm = TRUE), .groups = "drop") %>%
    # Aggregate by cohort-age-kin type
    group_by(dob_ego_year, age, kin_type) %>%
    summarise(sum_alive = sum(n_alive, na.rm = TRUE), .groups = "drop") %>%
    # Complete with all possible combinations of cohort-age-kin type
    complete(dob_ego_year = cohort_start:cohort_end,
             age = 0:100,
             kin_type = unique(ego_age_ancestors$kin_type),
             fill = list(sum_alive = 0)) %>%
    filter(kin_type != "Ego") %>% 
    # Add number of egos by cohort and age
    left_join(ego_ages_cohort %>%
                distinct(dob_ego_year, age, pid_ego) %>%
                count(dob_ego_year, age, name = "n_egos"), 
              by = c("dob_ego_year", "age")) %>% 
    mutate(mean_number = if_else(is.na(n_egos), NA_real_, sum_alive / n_egos))
  
  # Return both results
  return(list(prop_any_age = prop_any_age, 
              mean_number_age = mean_number_age))
}

# Map the function to all birth cohorts
results <- map(1751:2000, ~ estimates_cohort_age(.x, .x), 
               .progress = TRUE)


# Combine results for the proportion of individuals with any ancestor alive by age
prop_any_age <- results %>%
  map("prop_any_age") %>%
  bind_rows()
save(prop_any_age, file = "prop_any_age.RData")

load("prop_any_age.RData")

# Check number of NAs, minimum and maximum
prop_any_age %>% 
  summarise(n_missing_prop = sum(is.na(prop_any)),
            min_prop = min(prop_any, na.rm = TRUE),
            max_prop = max(prop_any, na.rm = TRUE))

# Check the maximum age until which a low proportion of individuals overlap with ancestors by cohorts
prop_any_age %>% 
  # filter(dob_ego_year < 1800) %>% 
  # filter(dob_ego_year >= 1800 & dob_ego_year < 1900) %>% 
  #filter(dob_ego_year < 1850) %>% 
  filter(dob_ego_year >= 1850 & dob_ego_year < 1900) %>% 
  # filter(dob_ego_year > 1900) %>% 
  # filter(dob_ego_year > 1950) %>% 
  # filter(dob_ego_year == 2000) %>% 
 # filter(kin_type %in% c("Mother", "Father")) %>% 
  filter(kin_type %in% c("Grandmothers", "Grandfathers")) %>% 
  filter(prop_any > 0.05) %>% 
  group_by(kin_type, dob_ego_year) %>% 
  summarise(threshold = max(age),
            .groups = "drop") %>% 
  group_by(kin_type) %>% 
  summarise(threshold = mean(threshold))


# Plot proportion of individuals with at least one living ancestor by age
prop_any_age %>% 
  filter(dob_ego_year > 1750) %>% 
  filter(kin_type != "Greatgreatgrandmothers" & kin_type != "Greatgreatgrandfathers") %>% 
  mutate(prop_any = prop_any*100, 
         kin_type = factor(kin_type, 
                           levels = c("Greatgrandmothers", "Greatgrandfathers",
                                      "Grandmothers", "Grandfathers",
                                      "Mother", "Father"))) %>% 
  ggplot(aes(x = dob_ego_year, y = age)) +
  geom_tile(aes(fill = prop_any), linewidth = 0) + 
  geom_contour(aes(z = prop_any), 
               breaks = c(5, 50, 95), 
               color = "#FFFFFF", 
               linewidth = 0.6) +
  metR::geom_text_contour(aes(z = prop_any, label = after_stat(paste0(level, "%"))), 
                          breaks = c(5, 50, 95),
                          color = "black",
                          size = 4,
                          stroke = 0.2,
                          skip = 0, 
                          na.rm = TRUE) +
  facet_wrap(~ kin_type, ncol =2, axes = "all") +
  scale_fill_viridis_c(option = "G", limits = c(0, 100), direction = -1, 
                       na.value = "grey90", labels = scales::label_number(suffix = "%")) +
  scale_x_continuous(breaks = seq(1750, 2000, 50)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  labs(x = "Birth Cohort", y = "Age", fill = "Proportion") +
  theme_graphs() +
  theme(legend.text = element_text(size = 14),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(2, "cm"))
ggsave("Graphs/3_Proportion_Any_Ancestors_Age.jpg",  width = 20, height = 16, dpi = 300)
ggsave("Graphs/3_Proportion_Any_Ancestors_Age.pdf",  width = 20, height = 16, dpi = 300, device = "pdf")


# Combine results for the mean number of co-existing ancestors alive by age
mean_number_age <- results %>%
  map("mean_number_age") %>%
  bind_rows()
save(mean_number_age, file = "mean_number_age.RData")

# Load mean number of ancestors by age 
load("mean_number_age.RData")

# Check number of NAs, minimum and maximum
mean_number_age %>%
  summarise(n_missing_prop = sum(is.na(mean_number)),
            min_number = min(mean_number, na.rm = TRUE),
            max_number = max(mean_number, na.rm = TRUE))

# Plot the mean number of co-existing ancestors by age
mean_number_age %>%
  filter(kin_type != "Greatgreatgrandmothers" & kin_type != "Greatgreatgrandfathers") %>% 
  mutate(kin_type = factor(kin_type, 
                           levels = c("Greatgreatgrandmothers", "Greatgreatgrandfathers", 
                                      "Greatgrandmothers", "Greatgrandfathers",
                                      "Grandmothers", "Grandfathers",
                                      "Mother", "Father"))) %>% 
  ggplot(aes(x = dob_ego_year, y = age)) +
  geom_tile(aes(fill = mean_number), linewidth = 0) + 
  geom_contour(aes(z = mean_number), 
               breaks = c(0.01, 0.5, 0.99, 1.5), 
               color = "#FFFFFF", 
               linewidth = 0.6) +
  metR::geom_text_contour(aes(z = mean_number),
                          breaks = c(0.01, 0.5, 0.99, 1.5),
                          color = "black",
                          size = 4,
                          skip = 0,
                          stroke = 0.2,
                          na.rm = TRUE) +
  facet_wrap(~ kin_type, ncol =2, axes = "all") +
  scale_fill_viridis_c(option = "F", na.value = "grey90", direction = -1, limits = c(0, 2)) +
  scale_x_continuous(breaks = seq(1750, 2000, 50)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  labs(x = "Birth Cohort", y = "Age", fill = "Co-existing Ancestors") +
  theme_graphs() +
  theme(legend.text = element_text(size = 14),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(2, "cm"))
ggsave("Graphs/5_Number_Coexisting_Ancestors_Age.jpg", width = 20, height = 16, dpi = 300)
ggsave("Graphs/5_Number_Coexisting_Ancestors_Age.pdf", width = 20, height = 16, dpi = 300, device = "pdf")


## Calculate overlap with all ancestors by cohort and ancestors' generation ----
overlap_all <- overlap_all_ego %>% 
  # Aggregate by cohort and kin_type (sum across ancestors)
  group_by(dob_ego_year, kin_type) %>%
  summarise(sum_overlap_months_all = sum(overlap_months_ego, na.rm = TRUE),
            .groups = "drop") %>%
  # Add count of distinct egos per birth year
  left_join(overlap_all_ego %>%
              distinct(dob_ego_year, pid_ego) %>%
              count(dob_ego_year, name = "n_egos"), 
            by = "dob_ego_year") %>%
  # Calculate mean overlap in months and years
  mutate(mean_overlap_months_all = sum_overlap_months_all / n_egos,
         mean_overlap_years_all = mean_overlap_months_all / 12) %>% 
  recode_ancestors_sex2()

# Calculate maximum for each generation
generation_max <- overlap_all %>% 
  group_by(kin_type2, dob_ego_year) %>%
  summarise(sum_overlap_years_gen = sum(mean_overlap_years_all, na.rm = TRUE), .groups = "drop") %>% 
  mutate(generation = case_when(kin_type2 %in% c("Mother","Father" ) ~ 4,
                                kin_type2 %in% c("Grandmothers","Grandfathers" ) ~ 3,
                                kin_type2 %in% c("Greatgrandmothers","Greatgrandfathers" ) ~ 2,
                                kin_type2 %in% c("Greatgreatgrandmothers","Greatgreatgrandfathers") ~ 1,
                                TRUE ~ NA)) %>% 
  group_by(generation) %>%
  summarise(max_overlap_gen = max(sum_overlap_years_gen, na.rm = TRUE), .groups = "drop")

# Define maximum y breaks for each generation
y_scales_list <- setNames(lapply(generation_max$max_overlap_gen, function(x) {
  brks <- pretty(c(0, x), n = 4)    
  ymax <- max(brks)
  scale_y_continuous(expand = c(0, 0), limits = c(0, ymax + 0.1 * ymax), breaks = brks)}), 
  generation_max$generation)

# Order kin types to get maternal side in lighter colours than paternal side
overlap_all <- overlap_all %>% 
  mutate(Sex = ifelse(kin_type2 %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female Ancestors", "Male Ancestors"),
         kin_type = factor(kin_type, levels = c("mom", 
                                                "mm", "mmm", "mfm", 
                                                "mmmm", "mmfm", "mfmm", "mffm",  
                                                "mf", "mmf", "mff",
                                                "mmmf", "mmff", "mfmf", "mfff",
                                                "pop",
                                                "fm", "fmm", "ffm", 
                                                "fmmm", "fmfm", "ffmm", "fffm", 
                                                "ff", "fmf", "fff",
                                                "fmmf", "fmff", "ffmf", "ffff" )))

# Calculate label positions for plots
label_positions <- overlap_all %>%
  filter(dob_ego_year == 1985) %>%  
  group_by(Sex, kin_type2) %>%
  arrange(desc(kin_type), .by_group = TRUE) %>%  # REVERSE for stacking calculation
  mutate(cumulative = cumsum(mean_overlap_years_all),
         y_end = cumulative,
         y_start = lag(cumulative, default = 0),
         lineage = ifelse(str_sub(kin_type, -1) == "m", "Maternal", "Paternal")) %>%
  group_by(Sex, kin_type2, lineage) %>%
  summarise(y_pos = (min(y_start) + max(y_end)) / 2,
            .groups = "drop") %>%
  filter(kin_type2 != "Mother" & kin_type2 != "Father")

# Plot Overlap with all Female ancestors
overlap_all_f <- overlap_all %>%
  filter(Sex == "Female Ancestors") %>%
  ggplot(aes(x = dob_ego_year, y = mean_overlap_years_all, fill = kin_type)) +
  geom_col(position = "stack", width = 1) + 
  geom_text(data = label_positions %>% filter(Sex == "Female Ancestors"),
            aes(x = 1985, y = y_pos, label = lineage),
            inherit.aes = FALSE,
            size = 4, family = "serif", fontface = "bold", color = "white") +
  facet_grid(kin_type2 ~ Sex, scales = "free_y") +
  scale_fill_viridis_d(option = "F", direction = -1, guide = "none") +
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000)) +
  facetted_pos_scales(y = y_scales_list) +
  labs(x = "Birth Cohort", y = "Mean person-years of overlap with all ancestors") +
  theme_graphs() +
  theme(strip.text = element_text(size = 22))

# Plot Overlap with all Male ancestors
overlap_all_m <- overlap_all %>%
  filter(Sex == "Male Ancestors") %>%
  ggplot(aes(x = dob_ego_year, y = mean_overlap_years_all, fill = kin_type)) +
  geom_col(position = "stack", width = 1) +  
  geom_text(data = label_positions %>% filter(Sex == "Male Ancestors"),
            aes(x = 1985, y = y_pos, label = lineage),
            inherit.aes = FALSE,
            size = 4, family = "serif", fontface = "bold", color = "white") +
  facet_grid(kin_type2 ~ Sex, scales = "free_y") +
  scale_fill_viridis_d(option = "F", direction = -1, guide = "none") +
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000)) +
  facetted_pos_scales(y = y_scales_list) +
  labs(x = "Birth Cohort", y = "Mean person-years of overlap with all ancestors") +
  theme_graphs() +
  theme(strip.text = element_text(size = 22))

# Combine plots 
cowplot::plot_grid(overlap_all_f, overlap_all_m, ncol = 2)
ggsave(file="Graphs/4_Overlap_All_Ancestors.jpeg", width=20, height=18, dpi=300)
ggsave(file="Graphs/4_Overlap_All_Ancestors.pdf", width=20, height=18, dpi=300, device = "pdf")


#### Calculate overlap per ancestors generation by cohort ----

overlap_per_gen <- overlap_all %>% 
  # Sum mean overlap years per cohort and kin_type (grouping ancestors of same kin_type2)
  group_by(dob_ego_year, kin_type2) %>%
  summarise(sum_overlap_years_gen = sum(mean_overlap_years_all, na.rm = TRUE),
            .groups = "drop") %>% 
  # Add maximum number of kin per generation
  left_join(overlap_all %>% 
              distinct(kin_type, kin_type2) %>%
              count(kin_type2, name = "max_gen"), 
            by = "kin_type2") %>%
  # Calculate mean overlapping years with each generation
  mutate(overlap_years_gen = sum_overlap_years_gen / max_gen)

# Plot overlap per living ancestors, facetted by sex 
overlap_per_gen %>% 
  mutate(Sex = ifelse(kin_type2 %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female Ancestors", "Male Ancestors"), 
         kin_type2 = factor(kin_type2, 
                            levels = c("Ego", "Mother", "Father", 
                                       "Grandmothers", "Grandfathers",
                                       "Greatgrandmothers", "Greatgrandfathers",
                                       "Greatgreatgrandmothers", "Greatgreatgrandfathers"))) %>%
  split(.$Sex) %>%
  map(~ ggplot(., aes(x= dob_ego_year, overlap_years_gen, group = kin_type2, colour = kin_type2)) +
        geom_line(linewidth = 1) +
        facet_wrap(~ Sex)+
        scale_colour_viridis_d(option = "D", direction =-1)+
        scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2050, 2100))+
        scale_y_continuous(breaks = c(0, 20, 40, 60), limits = c(0, 60))+
        labs(x = "Birth Cohort", y = "Mean years of overlap per ancestors generation", colour = "Ancestor") +
        theme_graphs() +
        theme(legend.title=element_blank(),
              strip.text = element_text(size = 22))) %>% 
  cowplot::plot_grid(plotlist = .)


# Plot overlap per living ancestors
overlap_per_gen %>% 
  mutate(Sex = ifelse(kin_type2 %in% c("Mother", "Grandmothers", "Greatgrandmothers", "Greatgreatgrandmothers"), 
                      "Female", "Male"), 
         kin_type2 = factor(kin_type2, 
                            levels = c("Ego", "Mother", "Father", 
                                       "Grandmothers", "Grandfathers",
                                       "Greatgrandmothers", "Greatgrandfathers",
                                       "Greatgreatgrandmothers", "Greatgreatgrandfathers")),
         Generation = case_when(kin_type2 %in% c("Mother","Father" ) ~ "Parents",
                                kin_type2 %in% c("Grandmothers","Grandfathers" ) ~ "Grandparents",
                                kin_type2 %in% c("Greatgrandmothers","Greatgrandfathers" ) ~ "Greatgrandparents",
                                kin_type2 %in% c("Greatgreatgrandmothers","Greatgreatgrandfathers") ~ "Greatgreatgrandparents",
                                TRUE ~ NA), 
         Generation = factor(Generation, levels = c("Parents", "Grandparents", 
                                                    "Greatgrandparents", "Greatgreatgrandparents"))) %>% 
  ggplot(aes(x= dob_ego_year, overlap_years_gen, group = kin_type2, colour = Generation, linetype = Sex)) +
  geom_line(linewidth = 1) +
  scale_colour_viridis_d(option = "D", direction =-1)+
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2050, 2100))+
  scale_y_continuous(breaks = c(0, 20, 40, 60), limits = c(0, 60))+
  labs(x = "Birth Cohort", y = "Mean years of overlap per ancestors generation", 
       colour = "Ancestors' Generation", linetype = "Ancestors' Sex") +
  guides(colour = guide_legend(order = 1),
         linetype = guide_legend(order = 2))+
  theme_graphs() +
  theme(strip.text = element_text(size = 22))
ggsave(file="Graphs/App_Overlap_Per_Ancestors.jpeg", width=20, height=9, dpi=300)
ggsave(file="Graphs/App_Overlap_Per_Ancestors.pdf", width=20, height=9, dpi=300, device = "pdf")
