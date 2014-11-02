###---------------------------------------------------------
###   HELPER FUNCTIONS
###   Author: Stephen O'Connell
###   Date: 05/03/2001
###---------------------------------------------------------

###-------------------------------------------------------------------------------------
###  Convert the data from Y Matrix
###-------------------------------------------------------------------------------------
createData <- function (Y) {
   data <- as.data.frame(Y)
  class <- c(rep("Normal",100), rep("Cyclic",100), rep("TrendUp",100), rep("TrendDown",100), rep("ShiftUp",100), rep("ShiftDown",100))
  data <-cbind(class, data)
  data$class <- as.factor(data$class)
  #str(data$class)
  #data[195:205,"class"]
  return(data)
}


###---------------------------------------------------------------
###  Confusion Matrix
###---------------------------------------------------------------
confusionM <- function(Yp, Y, no.print=FALSE) {
  
  ## ACCURACY
	x <- as.matrix(table(Y, Yp))
	s<-0
	for(i in 1:ncol(x)) (s <- s + x[i,i])
	A <-  s / length(Y)
	
	if (no.print == FALSE) {
		
		### COMPARE RESULTS
		cat("Predicted Values:\n")
		print(table(Yp) )            ###  PREDICTED VALUES
		cat("Y values:\n")
		print(table(Y))      ###  RIGHT VALUES - Ydata
		cat("Confusion Matrix:\n")
		print(table(Y, Yp))  ###  CONFUSION MATRIX
		
		cat("Accuracy = ", A)
	}
	
	return(A)
}

###---------------------------------------------------------------
###  Print Miss-Classified Observations
###---------------------------------------------------------------
printMissClassified <- function (p, c) {
  p <- as.character(p)
  c <- as.character(c)
  obs <- which(c != p)
  data.frame(Predicted=p[c != p],Actual=c[p != c],observation=obs)
}

