#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Living Ancestors - Compare sources HFD, HMD, WPP
# U:/SOCSIM/SOCSIM_Living_Ancestors/0_Compare_HFD_HMD_WPP.R

# Compare age-specific fertility rates from the Human Fertility Database and the World Population Prospects 2024 
# and age-specific probabilities of death (qx) from the Human Mortality Database and the World Population Prospects 2024
# for the overlapping period 1950-2023 

# Created on 15-10-2025
# Last modified on 23-10-2025
#----------------------------------------------------------------------------------------------------
## Global Settings ----

# Clear work space
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=999999)

# Load necessary packages
library(tidyverse)
#install.packages("HMDHFDplus")
library(HMDHFDplus)
library(viridis)

# Load function to download WPP2024 and save it as .csv
source("Functions/Functions_Download_WPP2024.R")

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

#----------------------------------------------------------------------------------------------------
# Obtain the data from the different sources

# Get ASFR from HFD for Sweden (by single year of age and calendar year), 1891-2022
HFD <- readHFDweb(CNTRY = "SWE",
                  item = "asfrRR",
                  username = HFD_username,
                  password = HFD_password)

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

# Download and save WPP 2024 data
download_WPP2024()

# Load the WPP data in csv format
fertility <- read_csv("Data/WPP2024_Fertility_by_Age1.csv", show_col_types = FALSE)
lifetable_female_hist <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv", show_col_types = FALSE)
lifetable_male_hist <- read_csv("Data/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv", show_col_types = FALSE)

# Filter WPP fertility data for Sweden for the overlapping period (1950-2023)
WPP_fert <- fertility %>% 
  filter(ISO3_code == "SWE" & Time >= 1950 & Time <= 2023 & Variant == "Medium") %>% 
  select(Year = Time, Age = AgeGrpStart, ASFR)  %>% 
  mutate(ASFR = ASFR/1000)

# Combine WPP male and female life tables and filter for Sweden
WPP_mort <- bind_rows(lifetable_female_hist, lifetable_male_hist) %>% 
  filter(ISO3_code == "SWE") %>%
  select(Year = Time, Sex, Age = AgeGrpStart, mx, qx, ex)

#----------------------------------------------------------------------------------------------------
## Comparison of HFD/HMD data with WPP for overlapping years (1950-2023) ----

# Create a sub-folder called "Graphs" to save the plots if it does not exist.
ifelse(!dir.exists("Graphs"), dir.create("Graphs"), FALSE) 

## Age-specific fertility rates ----

# Extract years in WPP data to filter HFD data
year_range_fert <- WPP_fert %>% pull(Year) %>% unique()
# Wrangle HFC and HFD data
HFD <- HFD %>% 
  filter(Year %in% year_range_fert) %>% 
  select(-OpenInterval) %>% 
  mutate(Source = "HFD")

# Wrangle SOCSIM data
WPP_fert <- WPP_fert %>% 
  mutate(Source = "WPP")

## Plot ASFR from HFD vs WPP   

# Choose years to plot
yrs_plot <- c(1950, 1975, 2000, 2023) 

bind_rows(HFD, WPP_fert) %>%
  filter(Year %in% yrs_plot) %>% 
  mutate(Year = factor(Year)) %>%   
  ggplot(aes(x = Age, y = ASFR, group = interaction(Year, Source)))+
  geom_line(aes(colour = Year, linetype = Source), linewidth = 1.2)+
  scale_color_viridis_d(option = "D") +  
  scale_x_continuous() +
  theme_graphs()
ggsave(file="Graphs/ASFR_HFD_WPP.jpeg", width=17, height=9, dpi=300)


## Age-specific mortality rates ----

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

# Extract years in WPP data to filter HFD data
year_range_mort <- WPP_mort %>% pull(Year) %>% unique()

# Wrangle HMD life tables
HMD_qx <- ltf %>%
  select(Year, Age, qx) %>% 
  mutate(Sex = "Female") %>% 
  bind_rows(ltm %>% 
              select(Year, Age, qx) %>%  
              mutate(Sex = "Male")) %>% 
  filter(Year %in% year_range_mort) %>% 
  mutate(Source = "HMD")

# Wrangle WPP life tables
WPP_qx <- WPP_mort %>% 
  select(Year, Sex, Age, qx) %>% 
  mutate(Source = "WPP")
  
## Plot ASMR from HMD vs WPP   

# Choose years to plot
yrs_plot <- c(1950, 1975, 2000, 2023) 

bind_rows(HMD_qx, WPP_qx) %>%
  filter(Year %in% yrs_plot) %>% 
  filter(qx != 0) %>% 
  mutate(Year = factor(Year)) %>%   
  ggplot(aes(x = Age, y = qx, group = interaction(Year, Source)))+
  geom_line(aes(colour = Year, linetype = Source), linewidth = 1.3)+
  facet_wrap(~ Sex) +
  scale_color_viridis_d(option = "D") +  
  scale_x_continuous(breaks = seq(0,100, by = 20), guide = guide_axis(angle = 90)) +
  scale_y_log10() +
  theme_graphs()
ggsave(file="Graphs/qx_HFD_WPP.jpeg", width=17, height=9, dpi=300)

#----------------------------------------------------------------------------------------------------
## Summary measures: TFR and e0 ----

# Total Fertility Rate ----

# Calculate TFR from HFD
HFD_TFR <- HFD %>% 
  group_by(Year) %>% 
  summarise(TFR = sum(ASFR, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Source = "HFD")

# Calculate TFR from WPP
WPP_TFR <- WPP_fert %>% 
  group_by(Year) %>% 
  summarise(TFR = sum(ASFR, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Source = "WPP")

## Plot TFR from HFD vs WPP 
bind_rows(HFD_TFR, WPP_TFR) %>%
  ggplot(aes(x = Year, y = TFR, group = Source)) +
  geom_line(aes(colour = Source), linewidth = 1.3)+
  scale_color_manual(values = c("#CA650D", "#5B92E5"))+
  scale_x_continuous(guide = guide_axis(angle = 90)) +
  theme_graphs()
ggsave(file="Graphs/TFR_HFD_WPP.jpeg", width=17, height=9, dpi=300)


# Life Expectancy at birth ----

# Wrangle HMD life tables
HMD_ex <- ltf %>%
  select(Year, Age, ex) %>% 
  mutate(Sex = "Female") %>% 
  bind_rows(ltm %>% 
              select(Year, Age, ex) %>%  
              mutate(Sex = "Male")) %>% 
  filter(Year %in% year_range_mort) %>% 
  mutate(Source = "HMD")

# Wrangle WPP life tables
WPP_ex <- WPP_mort %>% 
  select(Year, Age, ex, Sex) %>% 
  mutate(Source = "WPP") 


## Plot ex from HMD vs WPP
bind_rows(HMD_ex, WPP_ex) %>% 
  filter(Age == 0 & Year %in% year_range_mort) %>%
  ggplot(aes(x = Year, y = ex, group = Source))+
  geom_line(aes(colour = Source), linewidth = 1.3)+
  scale_color_manual(values = c("#0C0B7F", "#5B92E5"))+
  facet_wrap(~Sex) +
  theme_graphs()+
  labs(y = "Life expectancy at birth") 
ggsave(file="Graphs/e0_HMD_WPP.jpeg", width=17, height=9, dpi=300)