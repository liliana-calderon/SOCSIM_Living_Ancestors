#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Living Ancestors - Compare Input and Output Rates
# U:/SOCSIM/SOCSIM_Living_Ancestors/1_Compare_Input_Output.R

## Compare input and output age-specific rates from a SOCSIM microsimulation for Sweden (1751-2100)
# as well as summary measures such as TFR, e0

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 14-10-2025
# Last modified on 30-10-2025

## Based on code prepared for the microsimulation vs registers paper
# U:/SOCSIM/SOCSIM_Registers/1_Compare_Input_Output.R
#----------------------------------------------------------------------------------------------------
## General settings and functions -----
# Prevent scientific notation (useful for the rate calculation)
options(scipen=999999)

## Load packages 
library(tidyverse)
library(ggh4x)  # To facet scales-
library(HMDHFDplus)
library(patchwork) # To combine ggplots
library(readxl)
library(rsocsim) # Functions to estimate rates
library(svglite) # To save svg files
library(viridis)

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

# Load function to calculate life table from asmr 1x1
# Currently, it only works with asmr calculated with rsocsim::estimate_mortality_rates()
source("Functions/Functions_Life_Table.R")

## Type inside the quotes HFD and HMD credentials (username and password) for the new website
# This information is necessary to run the comparisons below

# HFD credentials
HFD_username <- "Type_here_HFD_username"
HFD_password <- "Type_here_HFD_password"

# HMD credentials
HMD_username <- "Type_here_HMD_username"
HMD_password <- "Type_here_HMD_password"


## Load opop and omar generated in 0_Set_Up_Simulation.R 
# from simulation with no heterogeneous fertility
load("opop.RData")
load("omar.RData")

#------------------------------------------------------------------------------------------------------
# Age-Specific Fertility and Mortality rates, 5x5 ----

# Create a sub-folder called "Measures" to save the output measures if it does not exist.
ifelse(!dir.exists("Measures"), dir.create("Measures"), FALSE)

# Retrieve output age-specific fertility rates
asfr <- estimate_fertility_rates(opop = opop,
                                 final_sim_year = 2100, #[Jan-Dec]
                                 year_min = 1750, # Closed [
                                 year_max = 2100, # Open )
                                 year_group = 5, 
                                 age_min_fert = 10, # Closed [
                                 age_max_fert = 55, # Open ) 
                                 age_group = 5) # [,)
save(asfr, file = "Measures/asfr.RData")

# Retrieve output age-specific mortality rates
asmr <- estimate_mortality_rates(opop = opop,
                                 final_sim_year = 2100, #[Jan-Dec]
                                 year_min = 1750, # Closed
                                 year_max = 2100, # Open )
                                 year_group = 5,
                                 age_max_mort = 100, # Open )
                                 age_group = 5) # [,) 
save(asmr, file = "Measures/asmr.RData")


## Plot Age-specific fertility and mortality rates

# Load ASFR and ASMR from the simulations 
load("Measures/asfr.RData")
load("Measures/asmr.RData")

# Create a sub-folder called "Graphs" to save the plots if it does not exist.
ifelse(!dir.exists("Graphs"), dir.create("Graphs"), FALSE)

## ASFR and ASMR from SOCSIM for women
# Age groups for fertility and mortality must be the same to plot the figure below

# Choose years to plot (in intervals).
yrs_plot <- c("[1795,1800)", "[1895,1900)", "[1995,2000)", "[2095,2100)") 

# Get the age levels to define them before plotting and avoid wrong order
age_levels <- levels(asmr$age)

bind_rows(asfr %>%
            mutate(rate = "ASFR",                   
                   sex = "female"),
          asmr %>% 
            mutate(rate = "ASMR") %>% 
            filter(sex == "female")) %>% 
  mutate(age = factor(as.character(age), levels = age_levels)) %>% 
  # Some ages can have rates of 0, infinite (N_Deaths/0_Pop) and NaN (0_Deaths/0_Pop) values
  filter(socsim !=0 & !is.infinite(socsim) & !is.nan(socsim)) %>% 
  filter(year %in% yrs_plot) %>% 
  ggplot(aes(x = age, y = socsim, group = year, colour = year)) +
  geom_line(linewidth = 1) +
  facet_wrap(. ~ rate, scales = "free") + 
  facetted_pos_scales(y = list(ASFR = scale_y_continuous(),
                               ASMR =  scale_y_continuous(trans = "log10"))) +
  theme_graphs() +
  scale_color_manual(values = c("#B76527", "#79B727", "#2779B7", "#6527B7"))+ 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Age", y = "Estimate")
ggsave(file="Graphs/ASFR_ASMR_SOCSIM.jpeg", width=17, height=9, dpi=300)

#----------------------------------------------------------------------------------------------------
## Comparison with HFC, HFD, HMD and WPP data used as input ----

# ASFR ----

# Get HFD ASFR for Sweden (by single year of age and 5 calendar year), 1751-1890
HFC <- read_csv(paste0("https://www.fertilitydata.org/File/GetFile/Country/SWE/SWE_ASFRstand_TOT.txt"),
                col_names = T, show_col_types = F) %>% 
  filter(RefCode %in% "SWE_02" & Year2 <= 1890)

# Repeat rates of year groups for each calendar year
HFC <- HFC  %>% 
  select(Year1, Age, ASFR) %>% 
  mutate(ASFR = if_else(ASFR == ".", "0", ASFR), 
         ASFR = as.double(ASFR), 
         Year2 = Year1 + 1, 
         Year3 = Year1 + 2, 
         Year4 = Year1 + 3,
         Year5 = Year1 + 4) %>% 
  select(Year1, Year2, Year3, Year4, Year5, Age, ASFR) %>%
  pivot_longer(cols = c(Year1:Year5), names_to = "Delete", values_to = "Year") %>% 
  select(Year, Age, ASFR) %>% 
  arrange(Year, Age)


# Get HFD ASFR for Sweden (by single year of age and calendar year), 1891-2023
HFD <- readHFDweb(CNTRY = "SWE",
                  item = "asfrRR",
                  username = HFD_username,
                  password = HFD_password) %>% 
  filter(Year <= 2023) %>% 
  select(-OpenInterval)


# Load the WPP fertility data
WPP_fert <- read_csv("Data/WPP2024_Fertility_by_Age1.csv", show_col_types = FALSE)

# Filter WPP fertility data for Sweden and medium variant for the projected period
WPP_fert <- WPP_fert %>% 
  filter(ISO3_code == "SWE" & Variant == "Medium" & Time >= 2024 & Time <= 2100) %>% 
  select(Year = Time, Age = AgeGrpStart, ASFR)  %>% 
  mutate(ASFR = ASFR/1000)


# Extract year and age breaks used in estimate_fertility_rates() to apply the same values to HFC, HFD and WPP data

# Year breaks. Extract all the unique numbers from the intervals. 
year_breaks_fert <- unique(as.numeric(str_extract_all(asfr$year, "\\d+", simplify = T)))

# Year range to filter HFD data
year_range_fert <- min(year_breaks_fert):max(year_breaks_fert-1)

# Age breaks of fertility rates. Extract all the unique numbers from the intervals 
age_breaks_fert <- unique(as.numeric(str_extract_all(asfr$age, "\\d+", simplify = T)))


# Wrangle HFC, HFD and WPP data
Input_fert <- bind_rows(HFC, HFD, WPP_fert) %>%
  filter(Year %in% year_range_fert) %>% 
  mutate(year = cut(Year, breaks = year_breaks_fert, 
                    include.lowest = F, right = F, ordered_results = T),
         age = cut(Age, breaks = age_breaks_fert, 
                   include.lowest = F, right = F, ordered_results = T)) %>% 
  filter(!is.na(age)) %>% 
  group_by(year, age) %>%
  summarise(ASFR = mean(ASFR, na.rm = T)) %>%
  ungroup() %>%
  mutate(Source = "HFC/HFD/WPP", 
         Rate = "ASFR")

# Wrangle SOCSIM data
SocsimF0 <- asfr %>% 
  rename(ASFR = socsim) %>% 
  mutate(Source = "SOCSIM",
         Rate = "ASFR")

## Plot ASFR from HFD vs SOCSIM   

# Same years to plot than above (in intervals). 
yrs_plot <- c("[1795,1800)", "[1895,1900)", "[1995,2000)", "[2095,2100)") 

bind_rows(Input_fert, SocsimF0) %>%
  filter(year %in% yrs_plot) %>% 
  ggplot(aes(x = age, y = ASFR, group = interaction(year, Source)))+
  geom_line(aes(colour = year, linetype = Source), linewidth = 1.2)+
  scale_color_manual(values = c("#B76527", "#79B727", "#2779B7", "#6527B7"))+ 
  scale_linetype_manual(values = c("HFC/HFD/WPP" = "dotted", "SOCSIM" = "solid")) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_graphs()
ggsave(file="Graphs/ASFR_Input_Output.jpeg", width=17, height=9, dpi=300)


# ASMR ----
## We compare the mid-year ASMR with central death rates from HMD life tables 1x1
# Rates will be aggregated as defined in the estimate_mortality_rates()

# Get female life tables from HMD, 1x1
ltf <- readHMDweb(CNTRY = "SWE",
                  item = "fltper_1x1",
                  username = HMD_username,
                  password = HMD_password)

# Get male life tables from HMD, 1x1
ltm <- readHMDweb(CNTRY = "SWE",
                  item = "mltper_1x1",
                  username = HMD_username,
                  password = HMD_password)

# Load the WPP 2024 projected life tables in csv format
WPP_ltf <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv", show_col_types = FALSE)
WPP_ltm <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv", show_col_types = FALSE)

# Combine WPP male and female life tables and filter for Sweden
WPP_mort <- bind_rows(WPP_ltf, WPP_ltm) %>% 
  filter(ISO3_code == "SWE") %>%
  select(Year = Time, Age = AgeGrpStart, mx, Sex)


# Extract year and age breaks used in the estimate_mortality_rates() to apply the same values to HMD data

# Year breaks. Extract all the unique numbers from the intervals 
year_breaks_mort <- unique(as.numeric(str_extract_all(asmr$year, "\\d+", simplify = T)))

# Year range to filter HMD data
year_range_mort <- min(year_breaks_mort):max(year_breaks_mort-1)

# Age breaks of mortality rates. Extract all the unique numbers from the intervals 
age_breaks_mort <- unique(as.numeric(str_extract_all(asmr$age, "\\d+", simplify = T)))

# Wrangle HMD-WPP life tables
Input_mort <- ltf %>%
  select(Year, Age, mx) %>% 
  mutate(Sex = "Female") %>% 
  bind_rows(ltm %>% 
              select(Year, Age, mx) %>%  
              mutate(Sex = "Male")) %>% 
  bind_rows(WPP_mort %>% select(Year, Age, mx, Sex)) %>% 
  filter(Year %in% year_range_mort) %>% 
  mutate(year = cut(Year, breaks = year_breaks_mort, 
                    include.lowest = F, right = F, ordered_results = T),
         age = cut(Age, breaks = age_breaks_mort, 
                   include.lowest = F, right = F, ordered_results = T)) %>% 
  filter(!is.na(age)) %>% 
  group_by(year, Sex, age) %>% 
  summarise(mx = mean(mx, na.rm = T)) %>%
  ungroup() %>%
  mutate(Source = "HMD/WPP",
         Rate = "ASMR")

# Wrangle SOCSIM data
SocsimM <- asmr %>% 
  rename(mx = socsim) %>% 
  mutate(Sex = ifelse(sex == "male", "Male", "Female"),
         Source = "SOCSIM",
         Rate = "ASMR") %>% 
  select(year, Sex, age,  mx, Source, Rate)


## Plot ASMR from HMD vs SOCSIM   

yrs_plot <- c("[1795,1800)", "[1895,1900)", "[1995,2000)", "[2095,2100)") 

bind_rows(Input_mort, SocsimM) %>% 
  filter(year %in% yrs_plot) %>% 
  # Some ages can have rates of 0, infinite (N_Deaths/0_Pop) and NaN (0_Deaths/0_Pop) values
  filter(mx != 0 & !is.infinite(mx) & !is.nan(mx)) %>% 
  ggplot(aes(x = age, y = mx, group = interaction(year, Source))) +
  facet_wrap(~Sex) +
  geom_line(aes(colour = year, linetype = Source), linewidth = 1.3)+
  scale_y_log10() +
  scale_color_manual(values = c("#B76527", "#79B727", "#2779B7", "#6527B7"))+ 
  scale_linetype_manual(values = c("HMD/WPP" = "dotted", "SOCSIM" = "solid")) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_graphs()
ggsave(file="Graphs/ASMR_Input_Output.jpeg", width=17, height=9, dpi=300)

#---------------------------------------------------------------------------------------------------
## Final plot combining ASFR and ASMR ----

# Years to plot
yrs_plot <- c("[1795,1800)", "[1895,1900)", "[1995,2000)", "[2095,2100)") 

# Get the age levels to define them before plotting and avoid wrong order
age_levels <- levels(SocsimM$age)

# Define the same x and y breaks for all plots
y_breaks_asfr <- c(0.0, 0.05, 0.1, 0.15, 0.2)
y_breaks_asmr <- c(0.0, 0.0001, 0.001, 0.01, 0.1, 0.3)

bind_rows(Input_fert %>% rename(Estimate = ASFR), 
            SocsimF0 %>% rename(Estimate = ASFR)) %>% 
  mutate(Sex = "Female") %>%   
  bind_rows(Input_mort %>% rename(Estimate = mx),
            SocsimM %>% rename(Estimate = mx)) %>% 
  # There can be rates of 0, infinite (N_Deaths/0_Pop) and NaN (0_Deaths/0_Pop) values
  filter(Estimate != 0 & !is.infinite(Estimate) & !is.nan(Estimate)) %>% 
  filter(Sex == "Female") %>% 
  mutate(Year = year,
         age = factor(as.character(age), levels = age_levels), 
         Source = ifelse(Source == "SOCSIM", "SOCSIM", "HFC/HFD/HMD/WPP"), 
         Rate = ifelse(Rate == "ASFR", "Age-Specific Fertility Rates", "Age-Specific Mortality Rates")) %>%
  filter(Year %in% yrs_plot) %>% 
  ggplot(aes(x = age, y = Estimate, group = interaction(Year, Source)))+
  facet_wrap(. ~ Rate, scales = "free") + 
  geom_line(aes(colour = Year, linetype = Source), linewidth = 1.5)+ 
  scale_color_manual(values = c("#B76527", "#79B727", "#2779B7", "#6527B7"))+ 
  scale_linetype_manual(values = c("dotted", "solid"))+
  facetted_pos_scales(y = list("Age-Specific Fertility Rates" = NULL,
                               "Age-Specific Mortality Rates" =  scale_y_continuous(trans = "log10")))+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Age")+
  theme_graphs()
ggsave(file="Graphs/ASFR_ASMR_Input_Output.jpeg", width=17, height=9, dpi=300)
ggsave(file="Graphs/ASFR_ASMR_Input_Output.pdf", width=17, height=9, dpi=300, device = "pdf")
#----------------------------------------------------------------------------------------------------
## Summary measures: TFR and e0 ----
# Here, we use the socsim rates by 1 year age group and 1 calendar year
# Total Fertility Rate ----

## Retrieve age-specific fertility rates, by 1 year age group and 1 calendar year
asfr_1 <- estimate_fertility_rates(opop = opop,
                                   final_sim_year = 2100 , #[Jan-Dec]
                                   year_min = 1751, # Closed [
                                   year_max = 2101, # Open )
                                   year_group = 1, 
                                   age_min_fert = 10, # Closed [
                                   age_max_fert = 55, # Open )
                                   age_group = 1) # [,)
save(asfr_1, file = "Measures/asfr_1.RData")

# Load ASFR 1x1 and calculate TFR for plotting 
load("Measures/asfr_1.RData")

# Year breaks. Extract all the unique numbers from the intervals. 
year_breaks_fert_1 <- unique(as.numeric(str_extract_all(asfr_1$year, "\\d+", simplify = T)))

# Year range to filter the data to the simulated period
year_range_fert_1 <- min(year_breaks_fert_1):max(year_breaks_fert_1-1)

# Calculate TFR from HFC, HFD and WPP
TFR_Input <- bind_rows(HFC, HFD, WPP_fert) %>%
  group_by(Year) %>% 
  summarise(TFR = sum(ASFR, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Source = "HFC/HFD/WPP")

# Calculate TFR from SOCSIM
TFR <-  asfr_1 %>% 
  mutate(Year = as.numeric(str_extract(year, "\\d+"))) %>% 
  group_by(Year) %>% 
  summarise(TFR = sum(socsim, na.rm = T)) %>%
  ungroup() %>% 
  mutate(Source = "SOCSIM") 

## Plot TFR from HFC, HFD, WPP vs SOCSIM 
bind_rows(TFR_Input, TFR) %>%
  filter(Year %in% year_range_fert_1) %>% 
  ggplot(aes(x = Year, y = TFR, group = Source)) +
  geom_line(aes(colour = Source), linewidth = 1.3)+
  scale_color_manual(values = c("#CA650D", "#007A75"))+
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2050, 2100))+
  theme_graphs()
ggsave(file="Graphs/TFR_Input_Output.jpeg", width=17, height=9, dpi=300)


# Life Expectancy at birth ----
# Calculate life expectancy at birth 1x1 for the SOCSIM simulation

# Retrieve age-specific mortality rates, by 1 year age group and 1 calendar year
asmr_1 <- estimate_mortality_rates(opop = opop,
                                   final_sim_year = 2100, #[Jan-Dec]
                                   year_min = 1751, # Closed
                                   year_max = 2101, # Open )
                                   year_group = 1,
                                   age_max_mort = 100, # Open )
                                   age_group = 1) # [,)
save(asmr_1, file = "Measures/asmr_1.RData")

# Compute life tables from asmr 1x1 from the SOCSIM simulation
lt <- lt_socsim(asmr_socsim = asmr_1)
save(lt, file = "Measures/lt.RData")


# Load and wrangle life tables for plotting

# Load life tables from asmr 1x1 from the SOCSIM simulation
load("Measures/lt.RData")

# Load asmr 1x1 for the simulations
load("Measures/asmr_1.RData")

# Year breaks. Extract all the unique numbers from the intervals 
year_breaks_mort_1 <- unique(as.numeric(str_extract_all(asmr_1$year, "\\d+", simplify = T)))

# Year range to filter the data to the simulated period
year_range_mort_1 <- min(year_breaks_mort_1):max(year_breaks_mort_1-1)

## Compare with ex at age 0 for Sweden in HMD and WPP

# Combine WPP male and female life tables and filter for Sweden
WPP_ex <- bind_rows(WPP_ltf, WPP_ltm) %>% 
  filter(ISO3_code == "SWE") %>%
  select(Year = Time, Age = AgeGrpStart, ex, Sex)

# Wrangle HMD-WPP life tables
lt_Input <- ltf %>%
  select(Year, Age, ex) %>% 
  mutate(Sex = "Female") %>% 
  bind_rows(ltm %>% 
              select(Year, Age, ex) %>%  
              mutate(Sex = "Male")) %>% 
  bind_rows(WPP_ex %>% select(Year, Age, ex, Sex)) %>% 
  mutate(Source = "HMD/WPP")

# Wrangle SOCSIM life tables
lt_Output <- lt %>%
  mutate(Year = as.numeric(str_extract(year, "\\d+")),
         Sex = ifelse(sex == "female", "Female", "Male"),
         Source = "SOCSIM") %>% 
  select(Year, Age, ex, Sex, Source)

# Plot life expectancy at birth, Input vs Output
bind_rows(lt_Input, lt_Output) %>% 
  filter(Age == 0 & Year %in% year_range_mort_1) %>%
  ggplot(aes(x = Year, y = ex, group = Source))+
  geom_line(aes(colour = Source), linewidth = 1.3)+
  scale_color_manual(values = c("#0C0B7F", "#007A75"))+
  facet_wrap(~Sex) +
  theme_graphs()+
  labs(y = "e0") 
ggsave(file="Graphs/e0_Input_Output.jpeg", width=17, height=9, dpi=300)


#----------------------------------------------------------------------------------------------------
## Final plot combining TFR and e0 ----

## Plotting TFR and e0 (for females) from HFD/HMD vs SOCSIM 
bind_rows(TFR_Input %>% rename(Estimate = TFR) %>%  mutate(Rate = "TFR"),
          TFR %>% rename(Estimate = TFR) %>% mutate(Rate = "TFR")) %>% 
  mutate(Sex = "Female") %>%   
  bind_rows(lt_Input %>% filter(Age == 0) %>% rename(Estimate = ex) %>% mutate(Rate = "e0"),
            lt_Output %>% filter(Age == 0) %>% rename(Estimate = ex) %>% mutate(Rate = "e0")) %>% 
  filter(Sex == "Female") %>%
  mutate(Rate = ifelse(Rate == "TFR", "Total Fertility Rate", "Life Expectancy at Birth"), 
         Rate = factor(Rate, levels = c("Total Fertility Rate", "Life Expectancy at Birth"))) %>% 
  ggplot(aes(x = Year, y = Estimate, group = Source))+
  facet_wrap(. ~ Rate, scales = "free") + 
  geom_line(aes(colour = Source), linewidth = 1.2) +
  scale_color_manual(values = c("#CA650D", "#0C0B7F", "#007A75")) +
  facetted_pos_scales(y = list("Total Fertility Rate" = scale_y_continuous(limits = c(0, NA)),
                               "Life Expectancy at Birth" =  scale_y_continuous()))+
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2050, 2100))+
  theme_graphs()
ggsave(file="Graphs/TFR_e0_Input_Output.jpeg", width=17, height=9, dpi=300)
ggsave(file="Graphs/TFR_e0_Input_Output.pdf", width=17, height=9, dpi=300, device = "pdf")