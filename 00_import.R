# This is a script used to prepare the data for the level-I PDD criteria study.

# clear the environment
rm( list = ls() )
gc()

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
sc <- read.csv( here("helpers","test_scoring.csv"), sep = ";" ) # scoring for all tests included


# DATA PRE-PROCESSING ----


# ---- REDCap data ----

# prepare the RECAP data
d1 <-
  
  # keep only patients in the PDDcrit data set (@ screening evaluation)
  d1 %>%
  filter( study_id %in% unique( d0$IPN) & grepl( "screening", redcap_event_name ) ) %>%
  
  # exclude post-DBS measurements to make further calculations easier
  select( -contains("dbs"), -contains("post") ) %>%
  
  # rename variables where needed
  rename(
    
    # demographics/helpers
    "id" = "study_id",
    "event" = "redcap_event_name",
    "pd_dur" = "rok_vzniku_pn",
    "ledd" = "levodopa_equivalent",
    
    # date variables
    "birth" = "dob",
    "neuropsy_years" = "datum_neuropsy_23afdc",
    
    # neuropsychology
    "drsii" = "drsii_total",
    "moca" = "moca_e04359",
    "nart" = "nart_7fd846",
    "tol" = "tol_anderson"
    
  ) %>%
  
  # rename using function
  rename_with( ~ sub( "_[^_]*$", "", . ) , contains("bdi") ) %>% # BDI items
  rename_with( ~ sub( "madrs", "madrs_", . ) , contains("madrs") ) %>% # MADRS items
  rename_with( ~ sub( "q", "", . ) , contains("apatie") ) %>% # Apathy items
  rename_with( ~ gsub( "_", "", gsub( "_ldopatest", "", . ) ) , contains("mdsupdrs") ) # MDS-UPDRS III

# extract FAQ item scores
for( i in unlist( strsplit( with( sc, item[scale=="faq"] ), "," ) ) ) {
  
  d1[ , paste0("faq_",i) ] <-
    
    case_when(
      d1[ , paste0("faq_uvod_",i) ] == 1 ~ d1[ , paste0("faq_vykon_",i) ], # the patient evaluated an activity directly
      d1[ , paste0("faq_uvod_",i) ] == 2 ~ d1[ , paste0("faq_nikdy_",i) ]  # the patient evaluated an activity indirectly
    )

}

# reverse item scores where applicable
with(
  
  sc,
  for ( i in scale[complete.cases(rev)] ) for ( j in unlist( strsplit(rev[scale==i],",") ) ) {
    
    # reverse item scores by subtracting raw score from scale's (min + max)
    # double arrow to ensure the results will go beyond with()
    d1[ , paste0(i,"_",j) ] <<- ( max[scale==i] + min[scale==i] ) - d1[ , paste0(i,"_",j) ]

  }
)

# prepare sum scores
d1 <-
  
  d1 %>%
  
  # drop unneeded columns
  select( -all_of( starts_with( paste0("faq_", c("fill","uvod","vykon","nikdy","score") ) ) ) ) %>%
  
  # compute sum scores
  mutate(
    
    # self-report and psychiatric questionnaires
    faq = rowSums( across( starts_with("faq") ) ),
    bdi = rowSums( across( starts_with("bdi") ) ),
    staix1 = rowSums( across( starts_with("staix1") ) ),
    staix2 = rowSums( across( starts_with("staix2") ) ),
    madrs = rowSums( across( starts_with("madrs") ) ),
    apathy = rowSums( across( starts_with("apatie") ) ),
    
    # motor symptoms (medication OFF vs ON)
    updrsiii_off = rowSums( across( all_of( paste0( "mdsupdrs3", strsplit( sc[ sc$scale == "updrs_iii", "item" ], "," )[[1]] ) ) ) ),
    updrsiii_on = rowSums( across( all_of( paste0( "mdsupdrs3", strsplit( sc[ sc$scale == "updrs_iii", "item" ], "," )[[1]],"on" ) ) ) ),
    
    # time variables
    # ! note that if doesn't work, you need to either update tidyverse (see session info below for version)
    # ! or load lubridate
    pd_dur = year( as.Date( neuropsy_years ) ) - pd_dur,
    age_years = time_length( difftime( as.Date(neuropsy_years), as.Date(birth) ), "years" ),
    
    # demographics and Parkinson's related variables
    sex = case_when( sex == 0 ~ "female", sex == 1 ~ "male" ),
    type_pd = case_when( type_pd == 1 ~ "tremor-dominant", type_pd == 2 ~ "akinetic-rigid" ),
    asym_park = case_when( asym_park == 1 ~ "right", asym_park == 2 ~ "left" ),

  ) %>%
  
  # select variables of interest
  select(
    
    id, age_years, sex, # demographic variables
    type_pd, hy_stage, pd_dur, asym_park, ledd, # PD-specific variables
    
    # motor assessment
    updrsiii_off, updrsiii_on,
    
    # cognition
    drsii, moca, nart, # level-I neuropsychology
    
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
    contains("psychot"), illusion_pseudohaluc, hallucination, intrapsych_halluc, delusions, # psychotic symptoms
    anxi_dep, anxiety, gad, depression, organic_depression # affective symptoms
    
  )


# ---- PDDcrit specific data ----

# rename columns and mutate variables such that they are well aligned with REDCap data
d0 <-
  
  d0 %>%
  
  # renaming, first (mostly) d0-specific variables, then d0/d1 shared variables, finally converting to lowercase
  rename( "id" = "IPN", "born" = "born_NA_RC", "sex" = "gender_NA_RC", "hand" = "hand_NA_RC", "pd_dur" = "PD_years" ) %>%
  rename( "mmse" = "mmse_tot", "faq" = "FAQ_seb", "bdi" = "BDI.II", "staix1" = "STAI_1", "staix2" = "STAI_2") %>%
  `colnames<-`( tolower( colnames(.) ) ) %>%
  
  # re-code some
  mutate(
    sex = case_when( sex == "F" ~ "female", sex == "M" ~ "male" ),
    hand = case_when( hand == "R" ~ "right", hand == "L" ~ "left" ),
  )
  

# TO CHECK ALL DATA MAKE SENSE, EACH PATIENT INCLUDED EXACTLY ONCE, ALL DATA INCLUDED


# ---- names check ----

# TO CHECK THE NAMES IN d0 AND d1 MAP WELL


# ---- merging data sets ----

