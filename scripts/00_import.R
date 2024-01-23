# This is a script used to prepare the data for the level-I PDD criteria study.

# clear the environment
rm( list = ls() )
gc()

# list packages to be used
pkgs <- c("here","tidyverse","janitor","purrr")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare a data folder
if ( !dir.exists("_data") ) dir.create("_data")


# IN-HOUSE FUNCTIONS ---

# printing rounded numbers
rprint <- function( x , dec = 2 ) sprintf( paste0("%.",dec,"f"), round( x , dec ) )


# DATA READ ----

# data
d0 <- read.csv( here("_raw","PDD_cr1t2.0.csv"), sep = ";" ) # the PDD criteria specific data
d1 <- read.csv( here("_raw","ITEMPO-ManaExportNeuropsych_DATA_2024-01-23_1525.csv"), sep = "," ) # REDCap data

# helpers
nm <- read.csv( here("_raw","ITEMPO_DATA_2024-01-17_1153.csv"), sep = "," ) # patient's identificators
sc <- read.csv( here("helpers","test_scoring.csv"), sep = ";" ) # scoring for all tests included


# DATA PRE-PROCESSING ----


# ---- PDDcrit specific data ----

# rename columns and mutate variables such that they are well aligned with REDCap data
d0 <-
  
  d0 %>%
  
  # renaming, first (mostly) d0-specific variables, then d0/d1 shared variables, finally converting to lowercase
  rename( "id" = "IPN", "born" = "born_NA_RC", "sex" = "gender_NA_RC", "hand" = "hand_NA_RC" ) %>%
  rename( "mmse" = "MMSE_tot", "faq" = "FAQ_seb", "bdi" = "BDI.II", "staix1" = "STAI_1", "staix2" = "STAI_2") %>%
  `colnames<-`( tolower( colnames(.) ) ) %>%
  
  # re-code some
  mutate(
    sex = case_when( sex == "F" ~ "female", sex == "M" ~ "male" ),
    hand = case_when( hand == "R" ~ "right", hand == "L" ~ "left" ),
    across( c("firstname","surname"), ~ make_clean_names( .x, allow_dupes = T ) )
  ) %>%
  
  # add inclusion indicator column
  mutate( incl = 1, .after = id )

# !duplicated cases, rows selected manually:
# IPN138: keep the first assessment because it is the 'screening' in REDCap
# IPN347: keep the first assessment because the second one was just one year later & the first one is REDCap's "screening"
# IPN661: keep the second assessment which was three years after the first one & is REDCap's "screening"
d0[ with( d0, id == "IPN138" & assdate == "2018-02-07" ), "incl" ] <- 0
d0[ with( d0, id == "IPN347" & assdate == "2021-01-25" ), "incl" ] <- 0
d0[ with( d0, id == "IPN661" & assdate == "2019-10-23" ), "incl" ] <- 0

# names check
nm <-
  
  nm %>%
  filter( study_id %in% d0$id ) %>% 
  column_to_rownames( "study_id" ) %>%
  select( jmeno, prijmeni ) %>% # keep columns of interest only
  mutate( across( c("jmeno","prijmeni"), ~ make_clean_names( .x, allow_dupes = T ) ) )

# extract a table checking names consistency between d0 & d1
tnam <-
  
  sapply(
    
    rownames(nm),
    function(i)
      c( forname = nm[i,"jmeno"] == d0[ with( d0, id == i & incl == 1), "firstname"],
         surname = nm[i,"prijmeni"] == d0[ with( d0, id == i & incl == 1 ), "surname"]
      )
    
  ) %>% t()

# print cases with name discrepancy between d0 & d1
left_join(
  nm[ rownames( tnam[ ( !tnam[ ,1] | !tnam[ ,2] ), ] ) , ] %>% rownames_to_column("id"),
  d0[ d0$id %in% rownames( tnam[ ( !tnam[ ,1] | !tnam[ ,2] ), ] ), c("id","firstname","surname") ]
)

# IPNs 120,144,169,195,275,285,296,602,688, and 780 are typos only
# IPN143 changed surname after getting married but it is the same person

# IPN225's name from d0 is consistent with IPN335 in REDCap 
# date of birth is inconsistent with IPN225 though so re-coding
d0[ d0$id == "IPN225", "id" ] <- "IPN335"


# ---- REDCap data ----

# prepare the RECAP data
d1 <-
  
  # keep only patients in the PDDcrit data set (@ screening evaluation)
  d1 %>%
  filter( study_id %in% unique( d0$id) & grepl( "screening", redcap_event_name ) ) %>%
  
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
  
  # rename using functions utilizing substitution(s) of characters
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

# final touches
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
    
# NEXT STEP: COMPUTE AGE BASED ON d0 INSTEAD OF d1!
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
    drsii, mmse, moca, nart, # level-I neuropsychology
    
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


# ---- compatibility checks ----

# prepare a table comparing variables common across data sets
tvar <-
  
  sapply(
    
    d1$id, # loop through ids of REDCap patients
    function(i)
      
      # for each patient print T if the scores coincide, F if they do not
      sapply( c("mmse","bdi","faq","sex"),
              function(j)
                with( d0, get(j)[id==i & incl==1] ) == with( d1, get(j)[id==i] )
              )
    
  ) %>% t()

# extract patients with discrepancies
tdisc <-
  
  lapply(
    
    setNames( colnames(tvar), colnames(tvar) ), # loop through the variables
    function(i)
      
      # join the data of patients with discrepancies
      left_join(
        d0[ d0$id %in% rownames( tvar[ !tvar[ ,i], ] ) & d0$incl == 1, c("id",i) ],
        d1[ d1$id %in% rownames( tvar[ !tvar[ ,i], ] ), c("id",i) ],
        by = "id", suffix = c("_lvl1","_REDCap")
      )
    
  ) %>%
  
  # make a one nice file from it
  reduce( full_join, by = "id" )

# save the data with discrepancies
write.table( tdisc, here("_data","pdd_discepancies.csv"), sep = ",", row.names = F, quote = F, na = "" )


# ---- merging data sets ----


# SESSION INFO -----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = here("scripts","import_envir.txt") )
