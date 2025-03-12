# This is a script containing functions for data import and wranggling before the main analysis.
#

#
# IMPORT ITEM-LEVEL DATA ----
import_outcome_data <- function(data, names, return = 'data') {
  
  # rename columns and mutate variables such that they are well aligned with REDCap data
  d0 <- data %>%
    
    slice(-192) %>% # NEED TO BE SOLVED!
    
    
    # rename variables:
    #   first (mostly) item-data-specific variables
    #   then item-data/metadata shared variables
    #   finally convert to lowercase
    rename('id' = 'IPN', 'born' = 'born_NA_RC', 'sex' = 'gender_NA_RC', 'hand' = 'hand_NA_RC') %>%
    rename('mmse' = 'MMSE_tot', 'faq' = 'FAQ_seb', 'bdi' = 'BDI.II', 'staix1' = 'STAI_1', 'staix2' = 'STAI_2') %>%
    `colnames<-`( tolower( colnames(.) ) ) %>%
    
    # re-code some
    mutate(
      sex = case_when( sex == 'F' ~ 'female', sex == 'M' ~ 'male' ),
      hand = case_when( hand == 'R' ~ 'right', hand == 'L' ~ 'left' ),
      across( c('firstname','surname'), ~ make_clean_names( .x, allow_dupes = T ) ),
      assdate = as.Date(assdate),
      mmse_7 = if_else(mmse_7 > 5, NA, mmse_7)
    ) %>%
    
    # add inclusion indicator column
    mutate(incl = 1, .after = id)
  
  # !duplicated cases, rows selected manually:
  # IPN138: keep the first assessment because it is the 'screening' in REDCap
  # IPN347: keep the first assessment because the second one was just one year later & the first one is REDCap's 'screening'
  # IPN661: keep the second assessment which was three years after the first one & is REDCap's 'screening'
  d0[with(d0, id == 'IPN138' & assdate == '2018-02-07'), 'incl'] <- 0
  d0[with(d0, id == 'IPN347' & assdate == '2021-01-25'), 'incl'] <- 0
  d0[with(d0, id == 'IPN661' & assdate == '2019-10-23'), 'incl'] <- 0
  
  # IPN225's name from item-data is consistent with IPN335 in REDCap (i.e., metadata)
  # date of birth is inconsistent with IPN225 though so re-coding
  d0[d0$id == 'IPN225', 'id'] <- 'IPN335'
  
  # names check
  nm <-
    
    names %>%
    filter(study_id %in% d0$id) %>% 
    column_to_rownames( 'study_id' ) %>%
    select(jmeno, prijmeni ) %>% # keep columns of interest only
    mutate( across( c('jmeno','prijmeni'), ~ make_clean_names(.x, allow_dupes = T) ) )
  
  # extract a table checking names consistency between item-data and REDCap data
  tnam <- sapply(
    
    rownames(nm),
    function(i) c(
      
      forname = nm[i,'jmeno'] == d0[with( d0, id == i & incl == 1), 'firstname'],
      surname = nm[i,'prijmeni'] == d0[with( d0, id == i & incl == 1 ), 'surname']
      
    )
  ) %>% t()
  
  # extract cases with name discrepancy between d0 & d1
  disc <- left_join(
    
    nm[ rownames( tnam[ ( !tnam[ ,1] | !tnam[ ,2] ), ] ) , ] %>% rownames_to_column('id'),
    d0[ d0$id %in% rownames( tnam[ ( !tnam[ ,1] | !tnam[ ,2] ), ] ), c('id','firstname','surname') ]
    
  ) %>% mutate(
    
    reason = if_else(id == 'IPN143', 'married', 'typo') # write down reasons, ought to benre-checked when data sets change
    
  )
  
  # return what was asked for
  if(return == 'data') return(d0) else if(return == 'names') return(disc)

}


# IMPORT META-DATA ----
import_metadata <- function(input_data, outcome_data, scoring) {
  
  d1 <- input_data %>%
    
    # keep only patients in the outcome data set (@ screening evaluation)
    filter( study_id %in% unique( outcome_data$id) & grepl( 'screening', redcap_event_name ) ) %>%
    
    # exclude post-DBS measurements to make further calculations easier
    select( -contains('dbs'), -contains('post') ) %>%
    
    # rename variables where needed
    rename(
      
      # demographics/helpers
      'id' = 'study_id',
      'event' = 'redcap_event_name',
      'pd_dur' = 'rok_vzniku_pn',
      'ledd' = 'levodopa_equivalent',
      
      # date variables
      'birth' = 'dob',
      'neuropsy_years' = 'datum_neuropsy_23afdc',
      
      # neuropsychology
      'drsii' = 'drsii_total',
      'moca' = 'moca_e04359',
      'nart' = 'nart_7fd846',
      'tol' = 'tol_anderson'
      
    ) %>%
    
    # rename using functions utilizing substitution(s) of characters
    rename_with( ~ sub( '_[^_]*$', '', . ) , contains('bdi') ) %>% # BDI items
    rename_with( ~ sub( 'madrs', 'madrs_', . ) , contains('madrs') ) %>% # MADRS items
    rename_with( ~ sub( 'q', '', . ) , contains('apatie') ) %>% # Apathy items
    rename_with( ~ gsub( '_', '', gsub( '_ldopatest', '', . ) ) , contains('mdsupdrs') ) # MDS-UPDRS III
  
  # extract FAQ item scores
  for( i in unlist( strsplit( with( scoring, item[scale=='faq'] ), ',' ) ) ) d1[ , paste0('faq_',i) ] <- case_when(
    
    d1[ , paste0('faq_uvod_',i) ] == 1 ~ d1[ , paste0('faq_vykon_',i) ], # the patient evaluated an activity directly
    d1[ , paste0('faq_uvod_',i) ] == 2 ~ d1[ , paste0('faq_nikdy_',i) ]  # the patient evaluated an activity indirectly
    
  )
  
  # reverse item scores where applicable
  with(
    
    scoring, for ( i in scale[complete.cases(rev)] ) for ( j in unlist( strsplit(rev[scale==i],',') ) ) {
  
      # reverse item scores by subtracting raw score from scale's (min + max)
      # double arrow to ensure the results will go beyond with()
      d1[ , paste0(i,'_',j) ] <<- ( max[scale==i] + min[scale==i] ) - d1[ , paste0(i,'_',j) ]
      
    }
  )
  
  # final touches
  d1 <- d1 %>%
    
    # drop unneeded columns
    select( -all_of( starts_with( paste0('faq_', c('fill','uvod','vykon','nikdy','score') ) ) ) ) %>%
    
    # compute sum scores
    mutate(
      
      # self-report and psychiatric questionnaires
      faq = rowSums( across( starts_with('faq') ) ),
      bdi = rowSums( across( starts_with('bdi') ) ),
      staix1 = rowSums( across( starts_with('staix1') ) ),
      staix2 = rowSums( across( starts_with('staix2') ) ),
      madrs = rowSums( across( starts_with('madrs') ) ),
      apathy = rowSums( across( starts_with('apatie') ) ),
      
      # motor symptoms (medication OFF vs ON)
      updrsiii_off = rowSums( across( all_of( paste0( 'mdsupdrs3', strsplit( scoring[ scoring$scale == 'updrs_iii', 'item' ], ',' )[[1]] ) ) ) ),
      updrsiii_on = rowSums( across( all_of( paste0( 'mdsupdrs3', strsplit( scoring[ scoring$scale == 'updrs_iii', 'item' ], ',' )[[1]],'on' ) ) ) ),
      
      # time variables
      # ! note that if doesn't work, you need to either update tidyverse (see session info below for version)
      # ! or load lubridate
      pd_dur = year( as.Date( neuropsy_years ) ) - pd_dur,
      `age_@lvl2` = time_length( difftime( as.Date(neuropsy_years), as.Date(birth) ), 'years' ), # age at Level II assessment
      
      # demographics and Parkinson's related variables
      sex = case_when( sex == 0 ~ 'female', sex == 1 ~ 'male' ),
      type_pd = case_when( type_pd == 1 ~ 'tremor-dominant', type_pd == 2 ~ 'akinetic-rigid' ),
      asym_park = case_when( asym_park == 1 ~ 'right', asym_park == 2 ~ 'left' ),
      
      # criteria specific variables
      faq_9 = faq_9,
      gds_15 = gds_15
      
    ) %>%
    
    # select variables of interest
    select(
      
      birth, # keep date of birth for calculation of age at level I assessment after merging with d0
      id, `age_@lvl2`, sex, # demographic variables
      type_pd, hy_stage, pd_dur, asym_park, ledd, # PD-specific variables
      
      # motor assessment
      updrsiii_off, updrsiii_on,
      
      # cognition
      drsii, mmse, moca, nart, # level-I neuropsychology
      
      # criteria sepcific variables
      faq_9, gds_15,
      
      # level-II neuropsychology
      lns, ds_b, corsi_b, tmt_a, pst_d, # attention & working memory
      tol, tmt_b, pst_w, pst_c, vf_skp, cf, # executive function
      sim, bnt_60, # language
      ravlt_irs, ravlt_b, ravlt_6, ravlt_30, ravlt_drec50, ravlt_drec15, bvmt_irs, bvmt_30, bvmt_drec, # memory
      jol, clox_i, # visuospatial function
      
      # psychomotor speed/hand-eye coordination
      gp_r, gp_l,
      
      # questionnaires
      bdi, staix1, staix2, # affect - neuropsychology
      madrs, apathy, # affect - neuropsychiatry
      faq, # IADL
      
      # neuropsychiatry
      contains('psychot'), illusion_pseudohaluc, hallucination, intrapsych_halluc, delusions, # psychotic symptoms
      anxi_dep, anxiety, gad, depression, organic_depression # affective symptoms
      
    ) %>%
    
    # return the data
    return()
  
}


# COMPATIBILITY CHECKS ----
compatibility_check <- function(input_data, meta_data) {
  
  # prepare a table comparing variables common across data sets
  tvar <- sapply(
    
    meta_data$id, # loop through ids of REDCap patients
    function(i) sapply(
      
      c('mmse','bdi','faq','sex'), # loop through common variables
      function(j)
        with( input_data, get(j)[id==i & incl==1] ) == with( meta_data, get(j)[id==i] )
      
    )
  ) %>% t()
  
  # extract patients with discrepancies
  tdisc <- lapply(
    
    set_names( colnames(tvar) ), # loop through the variables
    function(i) left_join(
      
      # join the data of patients with discrepancies
      input_data[ input_data$id %in% rownames( tvar[ !tvar[ ,i], ] ) & input_data$incl == 1, c('id',i) ],
      meta_data[ meta_data$id %in% rownames( tvar[ !tvar[ ,i], ] ), c('id',i) ],
      by = 'id', suffix = c('_inputdata','_metadata')
      
    )
  ) %>%
    
    # make a one nice file from it
    reduce( full_join, by = 'id' )
  
  # return discrepancies
  return(tdisc)
    
   
}


# MERGE OUTCOME- & META-DATA ----
merge_data <- function(outcome_data, meta_data) {
  
  # duplicated names
  dup <- c('sex','mmse','faq','bdi','staix1','staix2')
  
  # basic merge
  d0 <-
    outcome_data %>%
    left_join( meta_data, by = 'id' ) %>% # merge it
    mutate( `age_@lvl1` = time_length( difftime( assdate, as.Date(birth) ), 'years' ) ) %>%  # calculate age at level I
    filter(incl == 1) # keep included entries only
  
  # fill-in duplicated columns
  for (i in dup) {
    for (j in seq_len(nrow(d0))) {
      
      if (!is.na(d0[j, paste0(i,'.y')])) d0[j, i] <- d0[j ,paste0(i,'.y')] # preferring REDCap data
      else d0[j , i] <- d0[j ,paste0(i,'.x')] # if REDCap data are not present, use paper data
      
    }
    
    # drop the original duplicated columns
    d0[ ,paste0(i,'.x')] <- NULL
    d0[ ,paste0(i,'.y')] <- NULL
    
  }
  
  # return it
  return(d0)
  
}

