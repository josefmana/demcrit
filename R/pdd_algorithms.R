# This script contains functions for PDD diagnosis algorithm according to Level I
# from Dubois et al.'s (2007, https://doi.org/10.1002/mds.21844) Table 2
#

# 
# DIAGNOSE PATIENTS ----
diagnose_pdd <- function(
    
  data, # data of all patients to be preliminary diagnosed
  source      = 'dubois', # Use Dubois et al. (2007) ('dubois') or Kulisevsky et al. (2024) ('kulisevsky')?
  dubois_att  = 'sevens', # Use MMSE's serial 7s ('sevens') or Months ('months') as a measure of Attention
  dubois_exec = 'fluency' # Use Phonemic verbal fluency ('fluency') or Clock drawing ('clock') as a measure of Executive Function
    
  
) {
  
  # ---- Dubois et al.'s (2007) PDD ----
  if (source == 'dubois') {
    
    data %>%
      
      mutate(
        
        # IMPAIRED COGNITION (CRITERION 5)
        #
        ## a) Attention
        impaired_att = case_when(
          dubois_att == 'sevens' ~ ifelse(mmse_7 < 4, T, F),
          dubois_att == 'months' ~ NA # not used
        ),
        #
        ## b) Executive Function
        impaired_exec = case_when(
          dubois_exec == 'fluency' ~ ifelse(vf_s < 10, T, F),
          dubois_exec == 'clocks'  ~ ifelse(clox_num + clox_hands < 2, T, F)
        ),
        #
        ## c) Visuo-Constructive Ability
        impaired_cons = ifelse(mmse_pent == 0, T, F),
        #
        ## d) Memory Impairment
        impaired_mem  = ifelse(mmse_3words < 3, T, F),
        
        # TABLE 2 CRITERIA
        crit1 = T, # 1. Parkinson’s disease
        crit2 = T, # 2. Parkinson’s disease developed before dementia
        crit3 = ifelse(mmse < 26, T, F), # 3. MMSE < 26
        crit4 = NA, # 4. Dementia has Impact on ADLs
        crit5 = ifelse( rowSums( across( starts_with('impaired_') ) ) > 1, T, F), # 5. Impaired cognition (For Yes, at least of 2 of 4 tests are abnormal)
        crit6 = NA, # 6. Absence of Major Depression 
        crit7 = NA, # 7. Absence of Major Depression 
        crit8 = NA, # 8. Absence of other abnormalities that obscure diagnosis
        
        # PELIMINARY PDD DIAGNOSIS BASED ON CRITERIA 3 & 5
        preliminary_pdd = factor(
          x = ifelse(crit3 & crit5, 1, 0),
          levels = 0:1,
          ordered = T
        )
        
      ) %>%
      
      # keep only variables of interest
      select( id, mmse, moca, preliminary_pdd, starts_with('impaired'), starts_with('crit') )
    
  }
  
}


# PREPARE SPECIFICATIONS OF CRITERIA UNDER INVESTIGATION ----
specify_criteria <- function() data.frame(
  
  nms = c('Dubois', 'Dubois'),
  tps = c('Type 1 (EF = fluency)', 'Type 2 (EF = clocks)'),
  src = c( rep('dubois',2) ),
  att = c( rep('sevens',2) ),
  exe = c('fluency', 'clocks')
  
)


# RUN DIAGNOSE PDD USING ALL CRITERIA ITERATIONS ----
iterate_pdd <- function(data, specs, format = 'long') {
  
  # prepare a long data file
  long_df <- with(
    
    specs, lapply(
      
      1:length(nms), # loop through rows of the specs tables
      function(i)
        
        diagnose_pdd(data, source = src[i], dubois_att = att[i], dubois_exec = exe[i]) %>% # run the diagnosis function
        mutate(crit = nms[i], type = tps[i], crit_type = paste(crit, type, sep = ', ') ) %>% # add index variables for criterion source
        select(id, crit, type, crit_type, preliminary_pdd) # keep only variables of interest
      
    ) %>% reduce(full_join) # pull all outcome possibilites to a single long-format file
  )
  
  # pivot it to a wide format
  wide_df <- long_df %>% select(-crit, -type) %>% pivot_wider(names_from = crit_type, values_from = preliminary_pdd)
  
  # return data set that was asked for
  if(format == 'long') return(long_df) else if(format == 'wide') return(wide_df)
  
}
