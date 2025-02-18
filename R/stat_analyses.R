# This script contains functions for statistical analyses of different PDD algorithms' outcomes.
#


# PRINT ROUNDED NUMBERS ----
rprint <- function(x, .dec = 2) sprintf( paste0('%.',.dec,'f'), round(x, .dec) )


# CALCULATE COHEN'S KAPPA FOR PAIRS OF CRITERIA ----
calculate_kappa <- function(df) {
  
  # extract all pairwise combinations
  combs <- combn(colnames(df)[-1], 2) # exclude the id column
  
  # compute Cohen's Kappa for each combination
  kappa_tab <- sapply(
    
    1:ncol(combs),
    function(j)
      
      cohen.kappa( x = cbind( df[ , combs[ , j] ] ) )[['confid']] %>% # compute the kappa
      as.data.frame() %>% # formatting to make tidyverse work
      mutate( x = paste0( rprint(estimate,3),', [', rprint(lower,3),', ',rprint(upper,3),']' ) ) %>% # print outcomes
      select(x) %>% # keep kappas only
      t() %>%
      cbind.data.frame(`criterion 1` = combs[1, 1], `criterion 2` = combs[2, 1], . ) # add criteria pair being kappaed (using data.frame to keep names)
      
  ) %>%
    
    t() %>% # swap rows and columns
    as_tibble() %>%
    mutate_if(is.list, unlist)
  
}

# CALCULATE CONFUSION MATRIXES ----
calculate_confusion <- function(data, kappas) {
  
  # extract criteria names
  crits <- colnames(data)[-1]
  
  # extract Prediction/Reference pairs
  pairs <-
    crossing(Prediction = crits, Reference = crits) |>
    filter(Prediction != Reference) |>
    mutate(
      Kappa = unlist( # add Kappas
        sapply(
          seq_len(length(Prediction)),
          function(i)
            kappas[kappas[['criterion 1']] %in% c(Prediction[i], Reference[i]) & kappas[['criterion 2']] %in% c(Prediction[i], Reference[i]), 'unweighted kappa']
        ),
        use.names = F
      ) %>%
        sub(']', ')', . , fixed = T) %>%
        sub('[', '(', . , fixed = T) %>%
        sub(',', '' , . , fixed = T)
    )
  
  # calculate and return confusion matrixes
  conf_mat <- lapply(
    
    X = set_names(pairs$Prediction),
    FUN = function(x) lapply(
      
      X = set_names(pairs$Reference[pairs$Prediction == x]),
      FUN = function(y) {
        
        # get the row
        i <- which(pairs$Prediction == x, pairs$Reference == y)
        
        # calculate confusion matrix
        cm <- 
          data[ , c(x,y)] %>%
          mutate_all(~ 2-as.numeric(.x)) %>% # re-scoring such that prevalence is calculated for PDD == 1
          table() %>%
          confusionMatrix()
        
        # rename some
        names(cm$byClass)[3:4] <- c('PPV', 'NPV')
        
        # add accuracy details
        acc <- c(
          `Accuracy (95% CI)` = with(cm, paste0(rprint(overall['Accuracy'],3), ' (', rprint(overall['AccuracyLower'],3),', ',rprint(overall['AccuracyUpper'],3),')')),
          `Kappa (95% CI)` = pairs$Kappa[i]
        )
        
        # give the result
        return(list(cm = cm, acc = acc))
        
      }
    )
  )
  
  # add accuracy values to pairs as well
  pairs <-
    
    pairs %>%
    mutate(
      Accuracy = unlist(
        sapply(
          seq_len(nrow(.)),
          function(i)
            conf_mat[[pairs$Prediction[i]]][[pairs$Reference[i]]]$acc['Accuracy (95% CI)']
        ),
        use.names = F
      ),
      Prevalence = unlist(
        sapply(
          seq_len(nrow(.)),
          function(i)
            rprint(conf_mat[[pairs$Prediction[i]]][[pairs$Reference[i]]]$cm$byClass['Prevalence'], 3)
        ),
        use.names = F
      ),
      `Detection Prevalence` = unlist(
        sapply(
          seq_len(nrow(.)),
          function(i)
            rprint(conf_mat[[pairs$Prediction[i]]][[pairs$Reference[i]]]$cm$byClass['Detection Prevalence'], 3)
        ),
        use.names = F
      ),
      `Accuracy p value` = unlist(
        sapply(
          seq_len(nrow(.)),
          function(i)
            rprint(conf_mat[[pairs$Prediction[i]]][[pairs$Reference[i]]]$cm$overall['AccuracyPValue'], 3)
        ),
        use.names = F
      )
    )
  
  # return list of matrixes and summaries
  return( list(cm = conf_mat, sum = pairs) )

}
