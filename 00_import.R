# This is a script used to prepare the data for the level-I PDD criteria study.

# list packages to be used
pkgs <- c("here","tidyverse")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}


# IN-HOUSE FUNCTIONS ---

# printing rounded numbers
rprint <- function( x , dec = 2 ) sprintf( paste0("%.",dec,"f"), round( x , dec ) )


# DATA READ ----

# data
d0 <- read.csv( here("_data","Level1critPDD.csv"), sep = ";" ) # the PDD criteria specific data
d1 <- read.csv( here("_data","ITEMPO-ManaExportNeuropsych_DATA_2024-01-17_1227.csv"), sep = "," ) # REDCap data

# helpers
nm <- read.csv( here("_data","ITEMPO_DATA_2024-01-17_1153.csv"), sep = "," ) # patient's identificators
sc <- read.csv( here("_data","test_scoring.csv"), sep = ";" ) # scoring for all tests included


# DATA PRE-PROCESSING ----

# ---- REDCap data ----

# prepare the RECAP data
d1 <-
  
  # keep only patients in the PDDcrit data set (@ screening evaluation)
  d1 %>%
  filter( study_id %in% unique( d0$IPN) & grepl( "screening", redcap_event_name ) ) %>%
  
  # select variables of interest
  select(
    
    study_id, redcap_event_name, dob, sex, # demographic variables
    type_pd, hy_stage, rok_vzniku_pn, asym_park, # PD-specific variables
    contains("ldopatest") & !contains("dbs"), # MDS-UPDRS III during Ldopatest & exclude post-DBS measurements
    
    # cognition
    drsii_total, moca_e04359, nart_7fd846, # level-I neuropsychology
    
    # level-II neuropsychology
    datum_neuropsy_23afdc,
    lns, ds_b, corsi_b, tmt_a, pst_d, # attention & working memory
    tol_anderson, tmt_b, pst_w, pst_c, vf_skp, cf, # executive function
    sim, bnt_60, # language
    ravlt_irs, ravlt_b, ravlt_6, ravlt_30, ravlt_drec50, ravlt_drec15, bvmt_irs, bvmt_30, bvmt_drec, # memory
    jol, clox_i, # visuospatial function
    
    # psychomotor speed/hand-eye coordination
    gp_r, gp_l,
    
    # questionnaires
    starts_with("bdi"), starts_with("staix"), # affect - neuropsychology
    starts_with("madrs"), starts_with("apatie"), # affect - neuropsychiatry
    starts_with("faq"), # IADL
    
    # neuropsychiatry
    contains("psychot"), illusion_pseudohaluc, hallucination, intrapsych_halluc, delusions, # psychotic symptoms
    anxi_dep, anxiety, gad, depression, organic_depression # affective symptoms
    
    
  ) %>%
  
  
  # rename variables where needed
  


# ---- PDDcrit specific data ----

# TO CHECK ALL DATA MAKE SENSE, EACH PATIENT INCLUDED EXACTLY ONCE, ALL DATA INCLUDED


# ---- names check ----

# TO CHECK THE NAMES IN d0 AND d1 MAP WELL


# ---- merging data sets ----

