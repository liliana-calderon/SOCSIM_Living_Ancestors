#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Living Ancestors - Write rates, create initial population and run simulation
# U:/SOCSIM/SOCSIM_Living_Ancestors/0_Set_Up_Simulation.R

# This code has three main steps: write rates, create the initial populations and run the simulation

## 1. Write the input fertility and mortality rates for a SOCSIM micro-simulation for Sweden (1751-2100)
# Age-specific fertility rates are retrieved from the Human Fertility Collection (HFC) for 1751-1890, 
# the Human Fertility Database for 1891-2023 and the World Population Prospects 2024 Revision for 2024-2100. 
# Age-specific probabilities of death (qx) are retrieved from the Human Mortality Database for 1751-2023
# and the World Population Prospects 2024 Revision for 2024-2100. 

## 2. Create an initial population file and empty marriage file for the simulations

## 3. Run a SOCSIM demographic microsimulation using the 'rsocsim' package and data for Sweden (1751-2100)
## and read the output into R. 

# Created on 14-10-2025
# Last modified on 21-07-2026
#------------------------------------------------------------------------------------------------------
# SOCSIM simulation: input rates and assumptions ----

# The SOCSIM simulation is implemented using the 'rsocsim' package,
# Available at https://github.com/MPIDR/rsocsim 

# It uses as input historical (1751-2023) and projected (2024-2100) data for Sweden
# Age-specific fertility rates come from the Human Fertility Collection (HFC) for 1751-1890,
# the Human Fertility Database (HFD) for 1891-2022,
# and the World Population Prospects 2024 Revision for 2024-2100. 
# Age-specific probabilities of death come from the Human Mortality Database for 1751-2023
# and the World Population Prospects 2024 Revision for 2024-2100. 

# To run the simulation, original HFC, HFD, HMD and WPP rates 
# are converted to monthly rates/probabilities and SOCSIM format

# Female fertility rates are identical for all marital status, but are specified for single and married women
# Other marital status (divorced, widowed, cohabiting) follow the SOCSIM rate default rules. 
# Age-specific female fertility rates are for all birth orders combined over the whole period

# Mortality rates correspond to probabilities of death (qx) of HMD and WPP period life tables. 
# They are identical for all marital status of each sex, and are only specified for single women and men. 
# Other marital status will follow the rate default rules. 
# To ensure compatibility between HMD and WPP life tables, we limit the age range of the HMD life tables to [0-100]

# The first segment of the simulation runs for 100 years to produce a stable age structure, 
# based on 1751-HFC and 1751-HMD age-specific rates

#------------------------------------------------------------------------------------------------------
# Rate files format (Cf. Socsim oversimplified, p. 26):

# A rate block is a complete set of age speciﬁc rates governing a demographic event 
# for people of a particular sex, group and marital status. 
# In the rate files, the order matters and is always: 
# Event (birth, death, marriage), group (1, ..), sex (F or M), and
# marital status (married, single, divorced, widowed)
# For birth rates, this can be followed by number indicating parity.
# Each subsequent line contains a one month rate or probability
# and the age interval over which it holds (years and months of the upper age bound)
# The interval includes upper age bound in previous line and ends just before upper age bound in current line.
# The first two numbers are years and months of the upper age bound, which are added together.
# e.g. upper age bound of "0 1" refers to the first month of life, thus, [0,1)

#----------------------------------------------------------------------------------------------------
## Global Settings ----

# Clear work space
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=9999)

# Load necessary packages
library(tidyverse)
library(readr)
# install.packages("HMDHFDplus")
library(HMDHFDplus)
library(MortalityLaws)
#install.packages("rsocsim")
library(rsocsim)

# Load functions to write SOCSIM rate files from HFC, HFD, HMD and WPP
source("Functions/Functions_Input_Rates.R")
#----------------------------------------------------------------------------------------------------
## 1.1. Write historical fertility rate files for SOCSIM using data from Human Fertility Collection ----

# For the years before 1890 that are covered by HMD but not by HFD, 
# We use data from the Human Fertility Collection (RefCode = "SWE_02")
# Vital registers in Sweden for this period are available from Statistics Sweden (1969). 
# Historisk statistik för Sverige. Del 1. Bevolkning 1720-1967. Örebro : Statistiska centralbyrån. 

# These data have Age Definition = ACY, Age in Completed Years and age range [15-49] 
# Age Interval is 1 for most ages [15-49], but for Age=14 (AgeInt=-99) and Age=50 (AgeInt=99)
# We keep the whole age range [15-49], and limit the open age intervals to one year, i.e. [14-15) and [50-51)
# The ages below and above [12-14) and [51-56) (available in HFD but not in HFC) are set to 0 
# The rates are provided by 5 calendar years and hence considered to be constant over each sub-period
# Additionally, SOCSIM requires a final age category with an upper bound
# corresponding to the maximum length of life. Here, we fixed it to [56-110]

# To convert the annual rates to monthly rates
# we assume that the fertility rates are constant over the year and divide them by 12 (months)
# According to SOCSIM oversimplified, these are rates rather than probabilities. 
# So multiplying a rate by the number of months in the age category gives
# the expected number of births that a woman who lives through the age category will experience.

# The rates are identical for married, single, divorced and widowed females. 
# We only specify the rates for single and married, 
# as other marital status will follow the rate default rules. 


# To write the input fertility rate files from HFC, please run
write_socsim_rates_HFC(Country = "SWE",
                       RefCode = "SWE_02", # For Vital statistics 1751 - 1965
                       Year_Max = 1890) # Only before 1891 (start of HFD)

#----------------------------------------------------------------------------------------------------
## 1.2 Write historical fertility rate files for SOCSIM using data from HFD ----

## NOTES:
# For 1891-2023, we use data from HFD and keep the whole age range [12-55], 
# but limit the open-ended age intervals 12- and 55+ to one year, i.e. [12-13) and [55-56)
# Additionally, SOCSIM requires a final age category with an upper bound
# corresponding to the maximum length of life. Here, we fixed it to [56-110]

# To write the input fertility rate files from HFD, 
# please type inside the quotes the name of the country as used in HFD) 
# and your HFD credentials (username and password) for the new website
# If needed, check the countries' names and availability with getHFDcountries()

write_socsim_rates_HFD(Country = "SWE",
                       Year_Max = 2023,
                       HFD_username = "Type_here_HFD_username",
                       HFD_password = "Type_here_HFD_password")

#----------------------------------------------------------------------------------------------------
## 1.3 Write projected fertility rate files for SOCSIM using data from WPP ----

## NOTES:
# We keep the whole range of the fertility rates by single age of the mother provided by the WPP 2024 [15-49]. 
# Younger [10-14] and older [50-54] age groups are only provided in the estimates by 5-year age groups. 
# Hence, the ages below and above [12-14) and [51-56) (available in HFD) are set to 0 
# Additionally, SOCSIM requires a final age category with an upper bound corresponding to the maximum length of life. 
# Here, we fixed it to [56-110]

# To write the projected input fertility rates from WPP 2024, obtained with "download_WPP2024()", 
# type the ISO3_code of the country as listed in the WPP 2024. 

write_socsim_rates_WPP_fertility(Country = "SWE", 
                                 Year_Min = 2024,
                                 Year_Max = 2100) 
#----------------------------------------------------------------------------------------------------
## 1.4 Write historical mortality rate files for SOCSIM using data from HMD ----

## NOTES:

# As input mortality data, SOCSIM requires probabilities of death (qx) from period life tables. 
# For 1751-2023, we use the qx from HMD period life tables, which are smoothed for ages 80 and above 
# (See HMD explanatory notes on old-age mortality https://www.mortality.org/Data/ExplanatoryNotes).

# We keep the entire age range of the HMD lifetables to [0-110+]
# and limit the final age interval 110+ to one year, i.e. [110-111]

## To convert the annual probabilities to monthly probabilities,  
# we assume that the probability of dying is constant over the year
# and use the formula 1-(1-nqx)^(1/n) proposed by Kenneth Watcher (2014, p. 53). 
# For the final age interval monthly probabilities are equal to 1

# The probabilities are identical for all marital status 
# (married, single, divorced and widowed) of each sex
# We only specify the rates by sex for single as other marital status will follow the rate default rules. 

# To write the input mortality rate files from HMD, 
# please type inside the quotes the name of the country (as used in HMD) 
# and your HMD credentials (username and password) for the new website
# If needed, check the countries' names and availability with getHMDcountries()

write_socsim_rates_HMD(Country = "SWE", 
                       Year_Max = 2023,
                       HMD_username = "Type_here_HMD_username",
                       HMD_password = "Type_here_HMD_password")

#----------------------------------------------------------------------------------------------------
## 1.5 Write projected mortality rate files for SOCSIM using data from WPP ----

# To ensure compatibility with the HMD data, we use the age range from HMD [0-110]
# For ages 0-99, we use the probabilities of dying from the WPP single age life tables
# Ages 100-110 are extrapolated from WPP age pattern 80-99 using a Kannisto old-age model
# The final age interval is limited to one year [110-111]

# To convert the annual WPP probabilities to monthly 'SOCSIM' probabilities, 
# we assume that the probability of dying is constant over the year 
# and use the formula $1-(1-_nq_x)^{1/n}$ proposed by Kenneth Watcher. 
# For the final age interval 110, monthly probabilities are equal to 1/12.

# The probabilities of death are identical for all marital status of each sex, 
# and are only specified for single women and single men in the rate files. 
# Other marital status will follow SOCSIM's rate default rules. 

# The WPP2024 methods used to estimate mortality and life tables vary across countries 
# depending on the type and quality of available empirical data. 
# For further details, see the Methodology of the United Nations population estimates and projections

# The function below filters the WPP 2024 mortality data for the selected country and period
# extend the mortality rates for 100-110 based on qx80-99, using the Kannisto model
# and convert to SOCSIM format following the procedure explained above. 
# To run it, type the ISO_Code3 of the country as listed in the WPP 2024, and the minimum and maximum years

write_socsim_rates_WPP_mortality(Country = "SWE", 
                                 Year_Min = 2024,
                                 Year_Max = 2100)
#------------------------------------------------------------------------------------------------------
## 2. Create initial .opop and empty omar file for the simulation ----

# Set size of initial population
size_opop <-  50000

# Create data.frame with 14 columns and nrows = size_opop
presim.opop <- setNames(data.frame(matrix(data = 0, ncol = 14, nrow = size_opop)), 
                        c("pid","fem","group","nev","dob","mom","pop",
                          "nesibm","nesibp","lborn","marid","mstat","dod","fmult"))

# Add pid 1:sizeopop
presim.opop$pid <- 1:size_opop

# Add sex randomly
presim.opop$fem <- sample(0:1, nrow(presim.opop), replace = T)

# Add group 1 for all individuals
presim.opop$group <- 1

# Add random dates of birth (max age around 50)
presim.opop$dob <- sample(600:1200, nrow(presim.opop), replace = T)

## Check distribution of sex 
table(presim.opop$fem)

# Write initial population for pre-simulation (without fertility multiplier)
write.table(presim.opop, "presim.opop", row.names = F, col.names = F)


## Create an empty data frame for presim.omar
presim.omar <- data.frame()

# Write empty omar for pre-simulation
write.table(presim.omar, "presim.omar", row.names = F, col.names = F)

#------------------------------------------------------------------------------------------------------
## Run SOCSIM simulations with 'rsocsim' package ----

# rsocsim can be now easily installed from CRAN:
# install.packages("rsocsim")

# To get the development version, from binary package or source
# follow the instructions on https://mpidr.github.io/rsocsim/

# Load rsocsim package
library("rsocsim")

# Specify the working directory, where the supfile and ratefiles are.
# If the R session is running through the project file (SWE_Living_Ancestors.Rproj), the following command can be used.
folder <- getwd()
# Otherwise, the working directory must be specified after "folder <- "

# Name of the supervisory file stored in the above folder:
# Sup file for rates retrieved from HFC/HFD and HMD (1751-2023) and WPP (2024-2100)
# with marry after childbirth and hetfert 0
supfile <- "Sweden_long.sup" 

# Random number generator seed:
seed <- as.character(sample(1:99999, 1, replace = F))
# Random seed used for this simulation seed <- "79589"

# Save the seed number to use them later to read the data 
save(seed, file = "seed.Rda")
load("seed.Rda")

## Run the simulation for the random seed. 
start <- Sys.time()

### Run a single SOCSIM-simulation with a given folder and supervisory file
rsocsim::socsim(folder, supfile, seed, process_method = "future")
end <- Sys.time()

print(end-start)
# Time difference of 1.802847 hours for  1 simulation, with initial opop of 50000 and hetfert 0
#----------------------------------------------------------------------------------------------------
## Read the output .opop and .omar files ----

# Load seeds numbers and convert them into numeric
load("seed.Rda")
seed <- as.numeric(seed)

# Read opop
opop <- rsocsim::read_opop(folder = getwd(),
                           supfile = "Sweden_long.sup",
                           seed = seed,
                           suffix = "",
                           fn = NULL)
# Save opop to use later
save(opop, file = "opop.RData")

# Read omar
omar <- rsocsim::read_omar(folder = getwd(),
                           supfile = "Sweden_long.sup",
                           seed = seed,
                           suffix = "",
                           fn = NULL)
# Save omar to use later
save(omar, file = "omar.RData")