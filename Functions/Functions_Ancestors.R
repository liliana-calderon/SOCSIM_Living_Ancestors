#----------------------------------------------------------------------------------------------------
# Function to retrieve pids of direct ancestors up to the 4th generation of a given ego(s)
# from SOCSIM microsimulation outputs

# Created on 27-10-2025
# Last modified on 28-10-2025

# This is a simplified version of the function written to retrieve 10 generations of ancestors 
# "U:/SOCSIM/SOCSIM_Genealogies/Functions/Function_Ancestors.R" 
#----------------------------------------------------------------------------------------------------

retrieve_ancestors <- function(opop, egos) {
  
  opop2 <- opop %>% 
    select(pid, mom, pop) %>% 
    # Change 0s (nothing) to NA for the pids of the initial population
    mutate(mom = ifelse(mom == 0, NA, mom), 
           pop = ifelse(pop == 0, NA, pop))
  
  ### Grandmothers
  opop2$mm <- opop2$mom[match(opop2$mom, opop2$pid)]
  opop2$mf <- opop2$mom[match(opop2$pop, opop2$pid)]
  ### Grandfathers
  opop2$fm <- opop2$pop[match(opop2$mom, opop2$pid)]  
  opop2$ff <- opop2$pop[match(opop2$pop, opop2$pid)]

  
  ### Great-grandmothers
  opop2$mmm <- opop2$mm[match(opop2$mom, opop2$pid)]
  opop2$mfm <- opop2$mf[match(opop2$mom, opop2$pid)]
  opop2$mmf <- opop2$mm[match(opop2$pop, opop2$pid)]
  opop2$mff <- opop2$mf[match(opop2$pop, opop2$pid)]
  ### Great-grandfathers
  opop2$fmm <- opop2$fm[match(opop2$mom, opop2$pid)]
  opop2$ffm <- opop2$ff[match(opop2$mom, opop2$pid)] 
  opop2$fmf <- opop2$fm[match(opop2$pop, opop2$pid)]
  opop2$fff <- opop2$ff[match(opop2$pop, opop2$pid)]
  
  
  ### Great-great-grandmothers
  opop2$mmmm <- opop2$mmm[match(opop2$mom, opop2$pid)]
  opop2$mmfm <- opop2$mmf[match(opop2$mom, opop2$pid)]
  opop2$mfmm <- opop2$mfm[match(opop2$mom, opop2$pid)]
  opop2$mffm <- opop2$mff[match(opop2$mom, opop2$pid)]
  opop2$mmmf <- opop2$mmm[match(opop2$pop, opop2$pid)]
  opop2$mmff <- opop2$mmf[match(opop2$pop, opop2$pid)]
  opop2$mfmf <- opop2$mfm[match(opop2$pop, opop2$pid)]
  opop2$mfff <- opop2$mff[match(opop2$pop, opop2$pid)]
  ### Great-great-grandfathers
  opop2$fmmm <- opop2$fmm[match(opop2$mom, opop2$pid)]
  opop2$fmfm <- opop2$fmf[match(opop2$mom, opop2$pid)]
  opop2$ffmm <- opop2$ffm[match(opop2$mom, opop2$pid)]
  opop2$fffm <- opop2$fff[match(opop2$mom, opop2$pid)]
  opop2$fmmf <- opop2$fmm[match(opop2$pop, opop2$pid)]
  opop2$fmff <- opop2$fmf[match(opop2$pop, opop2$pid)]
  opop2$ffmf <- opop2$ffm[match(opop2$pop, opop2$pid)]
  opop2$ffff <- opop2$fff[match(opop2$pop, opop2$pid)]
  
  egos <- egos
  
  opop2 <- opop2 %>% 
    filter(pid %in% egos) %>% 
    rename(ego = pid) %>% 
    mutate(pid_ego = ego) %>% 
    pivot_longer(-pid_ego, names_to = "kin_type", values_to = "pid_kin") %>% 
    filter(!is.na(pid_kin))
  
  return(opop2)
}


## Re-code the kin_type for the ancestors

recode_ancestors <- function(opop_ancestors) {
    opop_ancestors <- opop_ancestors %>% 
    mutate(kin_type = case_when(kin_type %in% c("ego") ~ "Ego", 
                                kin_type %in% c("mom", "pop") ~ "Parents", 
                                kin_type %in% c("mm", "mf", "fm","ff") ~ "Grandparents", 
                                kin_type %in% c("mmm", "mfm", "mmf", "mff", 
                                                "fmm", "ffm", "fmf", "fff") ~ "Greatgrandparents", 
                                kin_type %in% c("mmmm", "mmfm", "mfmm", "mffm", "mmmf", "mmff", "mfmf", "mfff",
                                                "fmmm", "fmfm", "ffmm", "fffm", "fmmf", "fmff", "ffmf", "ffff") ~ "Greatgreatgrandparents", 
                                TRUE ~ NA_character_))
    return(opop_ancestors)
}

recode_ancestors_sex <- function(opop_ancestors) {
  opop_ancestors <- opop_ancestors %>%
    mutate(kin_type = case_when(kin_type %in% c("ego") ~ "Ego", 
                                kin_type %in% c("mom") ~ "Mother", 
                                kin_type %in% c("pop") ~ "Father", 
                                kin_type %in% c("mm", "mf") ~ "Grandmothers", 
                                kin_type %in% c("fm","ff") ~ "Grandfathers", 
                                kin_type %in% c("mmm", "mfm", "mmf", "mff") ~ "Greatgrandmothers", 
                                kin_type %in% c("fmm", "ffm", "fmf", "fff") ~ "Greatgrandfathers", 
                                kin_type %in% c("mmmm", "mmfm", "mfmm", "mffm", "mmmf", "mmff", "mfmf", "mfff") ~ "Greatgreatgrandmothers", 
                                kin_type %in% c("fmmm", "fmfm", "ffmm", "fffm", "fmmf", "fmff", "ffmf", "ffff") ~ "Greatgreatgrandfathers", 
                                TRUE ~ NA_character_),
           kin_type= factor(kin_type, 
                            levels = c("Ego", 
                                       "Mother", "Father", 
                                       "Grandmothers", "Grandfathers", 
                                       "Greatgrandmothers", "Greatgrandfathers", 
                                       "Greatgreatgrandmothers", "Greatgreatgrandfathers")))
  return(opop_ancestors)
}

recode_ancestors_sex2 <- function(opop_ancestors) {
  opop_ancestors <- opop_ancestors %>%
    mutate(kin_type2 = case_when(kin_type %in% c("ego") ~ "Ego", 
                                kin_type %in% c("mom") ~ "Mother", 
                                kin_type %in% c("pop") ~ "Father", 
                                kin_type %in% c("mm", "mf") ~ "Grandmothers", 
                                kin_type %in% c("fm","ff") ~ "Grandfathers", 
                                kin_type %in% c("mmm", "mfm", "mmf", "mff") ~ "Greatgrandmothers", 
                                kin_type %in% c("fmm", "ffm", "fmf", "fff") ~ "Greatgrandfathers", 
                                kin_type %in% c("mmmm", "mmfm", "mfmm", "mffm", "mmmf", "mmff", "mfmf", "mfff") ~ "Greatgreatgrandmothers", 
                                kin_type %in% c("fmmm", "fmfm", "ffmm", "fffm", "fmmf", "fmff", "ffmf", "ffff") ~ "Greatgreatgrandfathers", 
                                TRUE ~ NA_character_),
           kin_type2 = factor(kin_type2, 
                              levels = c("Greatgreatgrandmothers", "Greatgreatgrandfathers", 
                                         "Greatgrandmothers", "Greatgrandfathers",
                                         "Grandmothers", "Grandfathers",
                                         "Mother", "Father", 
                                         "Ego")))
  return(opop_ancestors)
}