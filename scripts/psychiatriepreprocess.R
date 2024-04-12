# This is a script used to prepare the data using psychiatric data.

# clear the environment
rm( list = ls() )
gc()

# list packages to be used
pkgs <- c("here","tidyverse","openxlsx")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}


# DATA READ ----

map <-
  read.xlsx( here("_raw","IPN_name_SUBJECT_key.xlsx"), 1 ) %>%
  filter( !is.na(SUBJECT) )

madrs <-
  read.xlsx(here("_raw","MADRS_apatie_SCL90_presurgery_key.xlsx"), "MADRS") %>%
  mutate( madrs = as.numeric(SouhrnnSkreMADRS) ) %>%
  mutate( id = sapply( 1:nrow(.), function(i) map[ map$SUBJECT == key[i],"IPN" ] ) )


apatie <- 
  read.xlsx(here("_raw","MADRS_apatie_SCL90_presurgery_key.xlsx"), "apatie") %>%
  mutate(
    apathy = as.numeric(SKRE),
    id = sapply( 1:nrow(.), function(i) map[map$SUBJECT == key[i], "IPN"] ),
    .after = key
  )

SCL90 <- 
  read.xlsx(here("_raw","MADRS_apatie_SCL90_presurgery_key.xlsx"), "SCL90") %>%
  mutate( across( all_of( starts_with("Dimenze") ), as.numeric ) ) %>%
  mutate ( id = sapply( 1:nrow(.), function(i) map [ map$SUBJECT == key[i], "IPN"]), .after = key )


# DATA PROCESSING ----

# Mapping months words to numbers
months <- data.frame(
  original = c("led","úno","bře","dub","kvě","čvn","čvc","srp","zář","říj","lis","pro"),
  better = sprintf( "%02s", 1:12)
)

# Rewrite dates to suitable format
for ( i in 1:nrow(madrs) ) for ( j in 1:nrow(months) ) madrs$MADRSDatumVyeten[i] <- sub( months$original[j], months$better[j], madrs$MADRSDatumVyeten[i] )
for ( i in 1:nrow(apatie) ) for ( j in 1:nrow(months) ) apatie$ApatieDatumVyplnn[i] <- sub( months$original[j], months$better[j], apatie$ApatieDatumVyplnn[i] )
for ( i in 1:nrow(SCL90) ) for ( j in 1:nrow(months) ) SCL90$SCL90DatumVyplnn[i] <- sub( months$original[j], months$better[j], SCL90$SCL90DatumVyplnn[i] )

# Creating long data sets
madrs <-
  madrs %>%
  select( id, MADRSDatumVyeten, madrs ) %>%
  rename( "date" = "MADRSDatumVyeten" ) %>%
  pivot_longer( madrs, names_to = "variable", values_to = "score" )

apatie <-
  apatie %>%
  select( id, ApatieDatumVyplnn, apathy ) %>%
  rename( "date" = "ApatieDatumVyplnn" ) %>%
  pivot_longer( apathy, names_to = "variable", values_to = "score" )

SCL90 <-
  SCL90 %>%
  select( id, SCL90DatumVyplnn, all_of( starts_with("Dimenze") ) ) %>%
  rename( "date" = "SCL90DatumVyplnn" ) %>%
  pivot_longer( all_of( starts_with("Dimenze")), names_to = "variable", values_to = "score" )
  
# Creating one long data set
d <- do.call( rbind.data.frame, list( apatie, madrs, SCL90 ) )

# export for manual corrections and data fill-in
write.table( d, here("_data","neuropsychiatry.csv"), sep = ",", row.names = F, quote = F )

