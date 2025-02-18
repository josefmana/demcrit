# This script contains functions for visualisation the results.
#

#
# CREATE NEW FOLDER IF IT DOES NOT EXIST YET ----
new_folder <- function(name) if (!dir.exists(name)) dir.create(name)

#
# VISUALISE CONFUSION MATRIXES ----
plot_confusion <- function(mat) {
  
  # extract matrixes and summaries separately
  cm    <- mat$cm
  pairs <- mat$sum
 
  # prepare folder for figures
  new_folder('_figures')

  # prepare object where paths to figures will be saved
  paths <- rep(NA, nrow(pairs))

  # loop through matrixes and plot them
  for (i in seq_len(nrow(pairs))) {

    # get predictor/reference names
    x  <- pairs$Prediction[i]
    y  <- pairs$Reference[i]
    fn <- here('_figures', paste0('pred = ',x,' ref = ',y,'.jpg'))
    paths[i] <- fn

    # prepare plotting device
    jpeg(filename = fn, quality = 100)

    # plot it
    draw_confusion_matrix(mat = cm[[x]][[y]], nms = c(x, y))
    
    # save it
    dev.off()

  }
  
  # return paths for tracking
  return(paths)
  
}
