# This script contains functions for PDD diagnosis algorithm according to Level I from
# Dubois et al.'s (2007, https://doi.org/10.1002/mds.21844) Table 2
#

# DIAGNOSE A SINGLE PATIENT ----
diagnose_patient <- function(pat, att = "sevens", exec = "fluency") {
  
  # extract results for Criterion 5 (impaired cognition)
  cognition <- with(
    
    pat, c(
      
      # a) Attention
      attention = case_when(
        att == "sevens" ~ ifelse(mmse_7 < 4, T, F),
        att == "months" ~ NA # not used
      ),
      
      # b) Executive Function
      executive = case_when(
        exec == "fluency" ~ ifelse(vf_s < 10, T, F),
        exec == "clocks" ~ ifelse(clox_num + clox_hands < 2, T, F)
      ),
      
      # c) Visuo-Constructive Ability
      construction = ifelse(mmse_pent == 0, T, F),
      
      # d) Memory Impairment
      memory = ifelse(mmse_3words < 3, T, F)
      
    )
  )
  
  # explicitly list criteria #3 (MMSE < 26) and #5 (impaied cognition)
  crit3 <- ifelse(pat$mmse < 26, T, F)
  crit5 <- ifelse(sum(cognition) > 1, T, F)
  
  # give a preliminary pdd diagnosis based on criterion #3 (MMSE < 26)
  # and criterion #5 (Impaired cognition) from Dubois et al.'s (2007) Table 2
  pdd <- ifelse(crit3 & crit5, 1, 0)
  
  # return a vector with all criteria and final verdict
  return( data.frame(
    
    id = pat$id,
    mmse = pat$mmse,
    preliminary_pdd = pdd,
    impaired_att = cognition["attention"],
    impaired_exec = cognition["executive"],
    impaired_cons = cognition["construction"],
    impaired_mem = cognition["memory"],
    crit1 = T, # 1. Parkinson’s disease
    crit2 = T, # 2. Parkinson’s disease developed before dementia
    crit3 = crit3, # 3. MMSE < 26
    crit4 = NA, # 4. Dementia has Impact on ADLs
    crit5 = crit5, # 5. Impaired cognition (For Yes, at least of 2 of 4 tests are abnormal)
    crit6 = NA, # 6. Absence of Major Depression 
    crit7 = NA, # 7. Absence of Major Depression 
    crit8 = NA # 8. Absence of other abnormalities that obscure diagnosis
  ) )
  
}


# RUN PDD ALGORITHM FOR EACH PATIENT ----
preliminary_pdd <- function(data) lapply(
  
  1:nrow(data),
  function(i)
    diagnose_patient( data[i, ] )
  
) %>% reduce(full_join)
