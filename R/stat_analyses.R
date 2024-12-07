# This script contains functions for statistical analyses of different PDD algorithms' outcomes.
#


# PRINT ROUNDED NUMBERS ----
rprint <- function(x, .dec = 2) sprintf( paste0("%.",.dec,"f"), round(x, .dec) )


# CALCULATE COHEN'S CAPPA FOR PAIRS OF CRITERIA ----
calculate_kappa <- function(df) {
  
  # extract all pairwise combinations
  combs <- combn(colnames(df)[-1], 2) # exclude the id column
  
  # compute Cohen's Kappa for each combination
  kappa_tab <- sapply(
    
    1:ncol(combs),
    function(j)
      
      cohen.kappa( x = cbind( df[ , combs[ , j] ] ) )[["confid"]] %>% # compute the kappa
      as.data.frame() %>% # formatting to make tidyverse work
      mutate( x = paste0( rprint(estimate,2),", [", rprint(lower,2),", ",rprint(upper,2),"]" ) ) %>% # print outcomes
      select(x) %>% # keep kappas only
      t() %>%
      cbind.data.frame(`criterion 1` = combs[1, 1], `criterion 2` = combs[2, 1], . ) # add criteria pair being kappaed (using data.frame to keep names)
      
  ) %>%
    
    t() %>% # swap rows and columns
    return()
  
}