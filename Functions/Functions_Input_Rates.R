#----------------------------------------------------------------------------------------------------
# Functions to Write SOCSIM fertility and mortality input rate files using 
# HFD and HMD data retrieved using the HMDHFDplus package  
# the Human Fertility Collection for the period not covered in HFD
# and the World Population Prospects 2024 Revision

# Initial version created on 08-06-2022
# Last modified on 21-07-2026

#----------------------------------------------------------------------------------------------------
#### Create subfolders ----

# Create a sub-folder called "rates" to save the rate files if it does not exist.
ifelse(!dir.exists("rates"), dir.create("rates"), FALSE)
# If the sub-folder name changes, 
# it must be also changed in the functions when opening the output file connection

#----------------------------------------------------------------------------------------------------
#### Write SOCSIM input fertility rates from HFC for early period not covered by HFD ----

write_socsim_rates_HFC <- function(Country, RefCode, Year_Max) {
  
  ## Download ASFR from HFC
  HFC <- read_csv(paste0("https://www.fertilitydata.org/File/GetFile/Country/",Country,"/",Country,"_ASFRstand_TOT.txt"),
                  col_names = T, show_col_types = F)
  
  # Wrangling HFC data
  HFC <- HFC %>%
    filter(RefCode %in% "SWE_02" & Year2 <= Year_Max) %>%
    select(Year1, Age, ASFR) %>% # Select useful columns
    mutate(ASFR = ifelse(ASFR == '.', "0", ASFR),
           ASFR = as.numeric(ASFR),
           Age_up = Age + 1, # SOCSIM uses the upper age bound
           Month = 0,
           ASFR_mo = ASFR/12) %>%
    select(-ASFR)
  
  # Tibble with expected ages
  Tibble_ages <- tibble(Age = c(0, 12, 13, 14, seq(15,55), 56))
  
  # Add rows with rates = 0 for ages [0-14] and [51-111] to keep the same format than HFD
  ASFR <- HFC %>%
    complete(Year1, Age = Tibble_ages$Age) %>%
    # Fill blanks and add Age_up and Month consistently
    mutate(ASFR_mo = coalesce(ASFR_mo, 0),
           Age_up  = case_when(Age == 0 ~ 12L,
                               Age == 56 ~ 111L,
                               TRUE ~ Age + 1L),
           Month   = 0) %>%
    arrange(Year1, Age_up) %>%
    # Add years that hold for each set of rates
    mutate(Year2 = Year1 + 1,
           Year3 = Year1 + 2,
           Year4 = Year1 + 3,
           Year5 = Year1 + 4) %>%
    select(Year1, Year2, Year3, Year4, Year5, Age_up, Month, ASFR_mo) %>%
    # Repeat rates for year groups for each calendar year
    pivot_longer(cols = c(Year1:Year5), names_to = "Delete", values_to = "Year") %>%
    select(Year, Age_up, Month, ASFR_mo) %>%
    arrange(Year, Age_up)
  
  # Extract the years available in HFC
  years <- ASFR %>% pull(Year) %>% unique()
  
  # Row numbers corresponding to sequence of years of age in ASFR
  rows_ageF <- ASFR %>% pull(Age_up) %>% unique() %>% seq_along()
  
  ## Write the fertility rate files for each year
  
  for(Year in years) {
    
    # Find the index of each year of the iteration
    n <- which(Year == years)
    n_row <- (n-1)*46 + rows_ageF
    
    # Open an output file connection
    outfilename <- file(paste0("rates/",Country,"fert",Year), "w")
    # without ".txt" specification as the original files had no format.
    
    # Include country and year of the corresponding rates
    cat(c("** Period (Monthly) Age-Specific Fertility Rates for", Country, "in", Year, "\n"),
        file = outfilename)
    cat(c("* Retrieved from the Human Fertility Collection, www.fertilitydata.org", "\n"),
        file = outfilename)
    cat(c("* Max Planck Institute for Demographic Research (Germany) and", "\n"),
        file = outfilename)
    cat(c("* Vienna Institute of Demography (Austria)", "\n"),
        file = outfilename)
    cat(c("* Data downloaded on ", format(Sys.time(), format= "%d %b %Y %X %Z"), "\n"),
        file = outfilename)
    cat(c("** NB: The original HFC annual rates have been converted into monthly rates", "\n"),
        file = outfilename)
    cat(c("** The open age intervals (14- and 49+) are limited to one year [14-15) and [49-50). Ages below and above are set to 0", "\n"),
        file = outfilename)
    cat("\n", file = outfilename)
    
    # Print birth rates (single females)
    cat("birth", "1", "F", "single", "0", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASFR)[i,-1], "\n"), file = outfilename) }
    cat("\n", file = outfilename)
    
    # Print birth rates (married females)
    cat("birth", "1", "F", "married", "0", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASFR)[i,-1],"\n"), file = outfilename) }
    
    close(outfilename)
    
  }
  
}

#----------------------------------------------------------------------------------------------------
#### Write SOCSIM input fertility rates from HFD using the HMDHFDplus package ----

write_socsim_rates_HFD <- function(Country, Year_Max, HFD_username, HFD_password) {
  
  # Get ASFR from HFD
  asfr <-
    readHFDweb(CNTRY = Country,
               item = "asfrRR",
               username = HFD_username,
               password = HFD_password)
  
  # Wrangle data and compute monthly fertility rates
  ASFR <-
    asfr %>%
    filter(Year <= Year_Max) %>%
    select(-"OpenInterval") %>%
    mutate(Age_up = Age + 1, # SOCSIM uses the upper age bound
           Month = 0,
           ASFR_mo = ASFR/12) %>%
    select(-ASFR)
  
  # Tibble with expected ages
  Tibble_ages <- tibble(Age = c(0, 12, 13, 14, seq(15,55), 56))
  
  # Add rows with rates = 0 for ages 0-12 and 56-110
  ASFR <-
    ASFR %>%
    complete(Year, Age = Tibble_ages$Age) %>%
    mutate(ASFR_mo = coalesce(ASFR_mo, 0),
           Age_up  = case_when(Age == 0  ~ 12L,
                               Age == 56 ~ 111L,
                               TRUE ~ Age + 1L),
           Month = 0) %>%
    arrange(Year, Age_up) %>%
    select(Year, Age_up, Month, ASFR_mo)
  
  # Extract the years available in HFD
  years <- ASFR %>% pull(Year) %>% unique()
  
  # Row numbers corresponding to sequence of years of age in ASFR
  rows_ageF <- ASFR %>% pull(Age_up) %>% unique() %>% seq_along()
  
  
  ## Write the fertility rate files for each year
  
  for(Year in years) {
    
    # Find the index of each year of the iteration
    n <- which(Year == years)
    n_row <- (n-1)*46 + rows_ageF
    
    # Open an output file connection
    outfilename <- file(paste0("rates/",Country,"fert",Year), "w")
    # without ".txt" specification as the original files had no format.
    
    # Include country and year of the corresponding rates
    cat(c("** Period (Monthly) Age-Specific Fertility Rates for", Country, "in", Year, "\n"),
        file = outfilename)
    cat(c("* Retrieved from the Human Fertility Database, www.humanfertility.org", "\n"),
        file = outfilename)
    cat(c("* Max Planck Institute for Demographic Research (Germany) and", "\n"),
        file = outfilename)
    cat(c("* Vienna Institute of Demography (Austria)", "\n"),
        file = outfilename)
    cat(c("* Data downloaded on ", format(Sys.time(), format= "%d %b %Y %X %Z"),
          "using the HMDHFDplus R-Package", "\n"),
        file = outfilename)
    cat(c("** NB: The original HFD annual rates have been converted into monthly rates", "\n"),
        file = outfilename)
    cat(c("** The open age intervals (12- and 55+) are limited to one year [12-13) and [55-56)", "\n"),
        file = outfilename)
    cat("\n", file = outfilename)
    
    # Print birth rates (single females)
    cat("birth", "1", "F", "single", "0", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASFR)[i,-1], "\n"), file = outfilename) }
    cat("\n", file = outfilename)
    
    # Print birth rates (married females)
    cat("birth", "1", "F", "married", "0", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASFR)[i,-1],"\n"), file = outfilename) }
    
    close(outfilename)
    
  }
  
}


#----------------------------------------------------------------------------------------------------
#### Write SOCSIM input fertility rates from WPP 2024 data ----

write_socsim_rates_WPP_fertility <- function(Country, Year_Min, Year_Max) {
  
  # Load the WPP data in csv format
  fertility <- read_csv("Data/WPP2024_Fertility_by_Age1.csv", show_col_types = FALSE)
  
  # Wrangle data and compute monthly fertility rates
  ASFR <- fertility %>%
    filter(ISO3_code == Country & Time >= Year_Min & Time <= Year_Max & Variant == "Medium") %>%
    select(Year = Time, Age = AgeGrpStart, ASFR)  %>%
    mutate(ASFR = ASFR/1000,
           Age_up = Age + 1, # SOCSIM uses the upper Age bound
           Month = 0,
           ASFR_mo = ASFR/12) %>%
    select(-ASFR)
  
  # Tibble with expected ages
  Tibble_ages <- tibble(Age = c(0, 12, 13, 14, seq(15,55), 56))
  
  # Add rows with rates = 0 for ages [0-14] and [51-111]
  ASFR <- ASFR %>%
    # create every Year × Age combination
    complete(Year, Age = Tibble_ages$Age) %>%
    # Fill blanks and compute Age_up and Month consistently
    mutate(ASFR_mo = coalesce(ASFR_mo, 0),
           Age_up  = case_when(Age == 0 ~ 12L,
                               Age == 56 ~ 111L,
                               TRUE ~ Age + 1L),
           Month   = 0) %>%
    arrange(Year, Age_up) %>%
    select(Year, Age_up, Month, ASFR_mo)
  
  # Extract the years in the dataset
  years <- ASFR %>% pull(Year) %>% unique()
  
  # Row numbers corresponding to sequence of years of age in ASFR
  rows_ageF <- ASFR %>% pull(Age_up) %>% unique() %>% seq_along()
  
  ## Write the fertility rate files for each year
  
  for(Year in years) {
    
    # Find the index of each year of the iteration
    n <- which(Year == years)
    n_row <- (n-1)*46 + rows_ageF
    
    # Open an output file connection
    outfilename <- file(paste0("rates/",Country,"fert",Year), "w")
    # without ".txt" specification as the original files had no format.
    
    # Include Country and year of the corresponding rates
    cat(c("** Period (Monthly) Age-Specific Fertility Rates for", Country, "in", Year, "\n"),
        file = outfilename)
    cat(c("* Retrieved from the World Population Prospects 2024", "\n"),
        file = outfilename)
    cat(c("* https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Fertility_by_Age1.csv.gz", "\n"),
        file = outfilename)
    cat(c("* United Nations. Population Division. Department of Economic and Social Affairs", "\n"),
        file = outfilename)
    cat(c("* Data downloaded on ", format(Sys.time(), format= "%d %b %Y %X %Z"), "\n"),
        file = outfilename)
    cat(c("** NB: The original WPP annual rates have been converted into monthly rates", "\n"),
        file = outfilename)
    cat(c("** The open age intervals (14- and 49+) are limited to one year [14-15) and [49-50). Ages below and above are set to 0", "\n"),
        file = outfilename)
    cat("\n", file = outfilename)
    
    # Print birth rates (single females)
    cat("birth", "1", "F", "single", "0", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASFR)[i,-1], "\n"), file = outfilename) }
    cat("\n", file = outfilename)
    
    # Print birth rates (married females)
    cat("birth", "1", "F", "married", "0", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASFR)[i,-1],"\n"), file = outfilename) }
    
    close(outfilename)
    
  }
  
}


#----------------------------------------------------------------------------------------------------
#### Write SOCSIM input mortality rates from HMD using the HMDHFDplus package ----

write_socsim_rates_HMD <- function(Country, Year_Max, HMD_username, HMD_password) {
  
  # Get female life tables from HMD
  ltf <-
    readHMDweb(CNTRY = Country,
               item = "fltper_1x1",
               username = HMD_username,
               password = HMD_password)
  
  # Get male life tables from HMD
  ltm <-
    readHMDweb(CNTRY = Country,
               item = "mltper_1x1",
               username = HMD_username,
               password = HMD_password)
  
  # Wrangle data and compute monthly mortality probabilities
  ASMP <-
    ltf %>%
    select(Year, Age, qx) %>%
    left_join(ltm %>% select(Year, Age, qx),
              by = c("Year","Age"), suffix = c("_F","_M")) %>%
    filter(Year <= Year_Max) %>%
    mutate(qx_Fmo = ifelse(Age == 110, 1/12, 1-(1-qx_F)^(1/12)), 
           qx_Mmo = ifelse(Age == 110, 1/12, 1-(1-qx_M)^(1/12)),
           Age_up = Age + 1, # SOCSIM uses the upper age bound
           Month = 0) %>%
    select(c(Year, Age_up, Month, qx_Fmo, qx_Mmo))
  
  # Extract the years available in HMD
  years <- ASMP %>% pull(Year) %>% unique()
  
  # Row numbers corresponding to sequence of years of age in ASMP
  rows_ageM <- ASMP %>% pull(Age_up) %>% unique() %>% seq_along()
  
  
  ## Write the mortality rate files for each year
  
  for(Year in years) {
    
    # Find the index of each year of the iteration
    n <- which(Year == years)
    n_row <- (n-1)*111 + rows_ageM
    
    # Open an output file connection
    outfilename <- file(paste0("rates/",Country,"mort",Year), "w")
    # without ".txt" specification as the original files had no format.
    
    # Include country and year of the corresponding probabilities
    cat(c("** Period (Monthly) Age-Specific Probabilities of Death for", Country, "in", Year, "\n"),
        file = outfilename)
    cat(c("* Retrieved from the Human Mortality Database (Life tables), www.mortality.org.", "\n"),
        file = outfilename)
    cat(c("* Max Planck Institute for Demographic Research (Germany),", "\n"),
        file = outfilename)
    cat(c("* University of California, Berkeley (USA),", "\n"),
        file = outfilename)
    cat(c("* and French Institute for Demographic Studies (France)", "\n"),
        file = outfilename)
    cat(c("* Data downloaded on ", format(Sys.time(), format= "%d %b %Y %X %Z"),
          "using the HMDHFDplus R-Package", "\n"),
        file = outfilename)
    cat(c("** NB: The original HMD annual probabilities have been converted into monthly probabilities", "\n"),
        file = outfilename)
    cat(c("** The open age interval (110+) is limited to one year [110-111)", "\n"),
        file = outfilename)
    cat("\n", file = outfilename)
    
    # Print mortality probabilities (single females)
    cat("death", "1", "F", "single", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASMP)[i,-c(1,5)], "\n"), file = outfilename) }
    cat("\n", file = outfilename)
    
    # Print mortality probabilities (single males)
    cat("death", "1", "M", "single", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASMP)[i,-c(1,4)], "\n"), file = outfilename) }
    
    close(outfilename)
    
  }
}
#----------------------------------------------------------------------------------------------------
#### Write SOCSIM input mortality rates from WPP 2024 data ----

write_socsim_rates_WPP_mortality <- function(Country, Year_Min, Year_Max) {
  
  # Load the WPP data in csv format
  lifetable_female <- read_csv(paste0("Data/WPP2024_Life_Table_Complete_Medium_Female_",Year_Min,"-",Year_Max,".csv"), 
                               show_col_types = FALSE)
  lifetable_male <- read_csv(paste0("Data/WPP2024_Life_Table_Complete_Medium_Male_",Year_Min,"-",Year_Max,".csv"), 
                             show_col_types = FALSE)
  
  
  # Combine the female and male life tables and filter
  wpp_raw <- bind_rows(lifetable_female, lifetable_male) %>%
    filter(ISO3_code == Country) %>%
    select(Year = Time, Sex, Age = AgeGrpStart, mx, qx)
  
  # Create a function to extend the mortality rates using the Kannisto model
  extend_with_kannisto <- function(lt) {
    
    observed <- lt %>%
      arrange(Age) %>%
      distinct(Age, .keep_all = TRUE) %>%
      filter(Age <= 99)
    
    # Fit on 80-99
    fit_data <- observed %>%
      filter(Age >= 80, Age <= 99) %>%
      filter(!is.na(mx) & mx > 0)
    
    fit <- MortalityLaws::MortalityLaw(x = fit_data$Age, 
                                       mx = fit_data$mx, 
                                       law = "kannisto")
    
    # Extrapolate old-age mortality rates for ages 100-110
    pred_ages <- 100:110
    pred_mx <- MortalityLaws::predict(fit, x = pred_ages)
    
    extra <- tibble(Age = pred_ages, 
                    mx = pred_mx, 
                    qx = 1 - exp(-pred_mx))
    
    bind_rows(observed %>% 
                select(Year, Sex, Age, mx, qx),
              extra %>% 
                mutate(Year = observed$Year[1],
                       Sex  = observed$Sex[1]) %>% 
                select(Year, Sex, Age, mx, qx)) %>%
      arrange(Age) }
  
  wpp_ext <- wpp_raw %>%
    group_by(Year, Sex) %>%
    group_split() %>%
    purrr::map_dfr(extend_with_kannisto) %>%
    ungroup()
  
  # Complete age grid 0:110
  full_grid <- expand_grid(Year = seq(Year_Min, Year_Max),
                           Sex  = c("Female", "Male"),
                           Age  = 0:110)
  
  ASMP <- full_grid %>%
    left_join(wpp_ext, by = c("Year", "Sex", "Age")) %>%
    group_by(Year, Sex) %>%
    arrange(Age, .by_group = TRUE) %>%
    tidyr::fill(qx, .direction = "down") %>%
    ungroup() %>%
    mutate(qx_mo = if_else(Age == 110, 1/12, 1 - (1 - qx)^(1/12)), 
           Age_up = Age + 1L, # SOCSIM uses the upper age bound
           Month  = 0L) %>%
    select(Year, Age_up, Month, Sex, qx_mo) %>%
    pivot_wider(names_from = Sex, values_from = qx_mo)
  
  # Extract the years available in WPP
  years <- ASMP %>% pull(Year) %>% unique()
  
  # Row numbers corresponding to sequence of years of age in ASMP
  rows_ageM <- ASMP %>% pull(Age_up) %>% unique() %>% seq_along()
  
  ## Write the mortality rate files for each year
  
  for(Year in years) {
    
    # Find the index of each Year of the iteration
    n <- which(Year == years)
    n_row <- (n-1)*111 + rows_ageM
    
    # Open an output file connection
    outfilename <- file(paste0("rates/",Country,"mort",Year), "w")
    # without ".txt" specification as the original files had no format.
    
    # Include Country and Year of the corresponding probabilities
    cat(c("** Period (Monthly) Age-Specific Probabilities of Death for", Country, "in", Year, "\n"),
        file = outfilename)
    cat(c("* Retrieved from the World Population Prospects 2024. Single age life tables up to age 100 Medium variant", "\n"),
        file = outfilename)
    cat(c("* https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv.gz", "and", "\n"),
        file = outfilename)
    cat(c("* https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv.gz", "and", "\n"),
        file = outfilename)
    cat(c("* United Nations. Population Division. Department of Economic and Social Affairs", "\n"),
        file = outfilename)
    cat(c("* Data downloaded on ", format(Sys.time(), format= "%d %b %Y %X %Z"), "\n"),
        file = outfilename)
    cat(c("** NB: The original WPP annual probabilities have been converted into monthly probabilities", "\n"),
        file = outfilename)
    cat(c("* Ages 100-110 are extrapolated from WPP age pattern using a Kannisto-type old-age model.", "\n"),
        file = outfilename)
    cat(c("** The final age interval is limited to one year [110-111)", "\n"),
        file = outfilename)
    cat("\n", file = outfilename)
    
    # Print mortality probabilities (single females)
    cat("death", "1", "F", "single", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASMP)[i,-c(1,5)], "\n"), file = outfilename) }
    cat("\n", file = outfilename)
    
    # Print mortality probabilities (single males)
    cat("death", "1", "M", "single", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASMP)[i,-c(1,4)], "\n"), file = outfilename) }
    
    close(outfilename)
    
  }
}