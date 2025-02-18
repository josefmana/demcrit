#' Makes a nice confusion matrix with all the relevant information
#'
#' Taken from https://stackoverflow.com/a/42940553
#' Takes in result from caret::confusionMatrix()
#'
draw_confusion_matrix <- function(mat, nms) {
  
  # split confusion matrix (cm) from accuracy measures (acc)
  cm  <- mat$cm
  acc <- mat$acc
  
  # prepare plot layout
  layout(matrix(c(1,1,2)))
  par(mar=c(1,0,3,0))
  plot(c(100, 345), c(300, 450), type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
  title('CONFUSION MATRIX', cex.main = 2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col = '#3F97D0')
  text(195, 435, 'PDD', cex = 1.5)
  rect(250, 430, 340, 370, col = '#F7AD50')
  text(295, 435, '¬PDD', cex = 1.5)
  text(125, 370, nms[1], cex = 1.66, srt = 90, font = 2)
  text(245, 450, nms[2], cex = 1.66, font = 2)
  rect(150, 305, 240, 365, col = '#F7AD50')
  rect(250, 305, 340, 365, col = '#3F97D0')
  text(140, 400, 'PDD', cex = 1.5, srt = 90)
  text(140, 335, '¬PDD', cex = 1.5, srt = 90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex = 2, font = 2, col='white')
  text(195, 335, res[2], cex = 2, font = 2, col='white')
  text(295, 400, res[3], cex = 2, font = 2, col='white')
  text(295, 335, res[4], cex = 2, font = 2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = 'n', xlab = '', ylab = '', main = 'DETAILS', xaxt = 'n', yaxt = 'n', cex.main = 1.8)
  text(10, 85, names(cm$byClass[1]), cex = 1.5, font = 2)
  text(10, 70, rprint(as.numeric(cm$byClass[1]), 3), cex = 1.5)
  text(30, 85, names(cm$byClass[2]), cex = 1.5, font = 2)
  text(30, 70, rprint(as.numeric(cm$byClass[2]), 3), cex = 1.5)
  text(50, 85, names(cm$byClass[3]), cex = 1.5, font = 2)
  text(50, 70, rprint(as.numeric(cm$byClass[3]), 3), cex = 1.5)
  text(70, 85, names(cm$byClass[4]), cex = 1.5, font = 2)
  text(70, 70, rprint(as.numeric(cm$byClass[4]), 3), cex = 1.5)
  text(90, 85, names(cm$byClass[8]), cex = 1.5, font = 2)
  text(90, 70, rprint(as.numeric(cm$byClass[8]), 3), cex = 1.5)
  
  # add in the accuracy information 
  text(30, 35, names(acc[1]), cex = 1.66, font = 2)
  text(30, 20, acc[1], cex = 1.66)
  text(70, 35, names(acc[2]), cex = 1.66, font = 2)
  text(70, 20, acc[2], cex = 1.66)
}
