###---------------------------------------------------------
###   DEMO_2:  IMPROVE INITIAL SVM MODEL BY ADDING MORE DATA 
###   Author: Stephen O'Connell
###   Date: 05/03/2001
###---------------------------------------------------------


##-----------------------------------------------------------------------------------------
rm(list=ls())
require(e1071)
setwd("/Users/oconste/Documents/workspace/sao_projects/CMG_Presentation")
source("helperFunctions.R")

###--------------------------------  OUT OF THE BOX -----------------------------
##  ADD MORE DATA TO THE MODEL BUILD
Ynew <- dget("Y_7")
d <- createData(Ynew)
data <- d
Ynew <- dget("Y_5")
d <- createData(Ynew)
data <- rbind(data, d)
Ynew <- dget("Y_4")
d <- createData(Ynew)
data <- rbind(data, d)
Ynew <- dget("Y_3")
d <- createData(Ynew)
data <- rbind(data, d)
Ynew <- dget("Y_2")
d <- createData(Ynew)
data <- rbind(data, d)
Ynew <- dget("Y_1")
d <- createData(Ynew)
data <- rbind(data, d)

## SPLIT TO x and Y
x <- subset(data, select = -class)
y <- data$class

## BUILD MODEL
model <- svm(class ~ ., data = data)
summary(model)

## PREDICTIONS
pred <- predict(model, x)

# CHECK ACCURACY:
confusionM(pred, y)
printMissClassified(pred, y)

###--------------------------------- TUNING ----------------------------------------

#obj <- tune.svm(class~., data = data, gamma = 2^(-1:1), cost = 2^(2:4))


###--------------------------------  AFTER TUNING -----------------------------
## NEW MODEL WITH COST AND GAMMA
model <- svm(class ~ ., data = data, cost=2.25, gamma=.01)

## RE-DO THE PREDICTION
pred <- predict(model, x)

# CHECK ACCURACY
confusionM(pred, y)
printMissClassified(pred, y)


###--------------------------------  NEW DATA  -----------------------------
## READ IN DATA THAT MODEL HAS NOT SEEN
Ynew <- dget("Y_6")
data <- createData(Ynew)

## SPLIT TO X and Y
x <- subset(data, select = -class)
y <- data$class

## PREDICT CLASS USING PREVIOUSLY CREATED MODEL
pred <- predict(model, x)

# CHECK ACCURACY
confusionM(pred, y)
printMissClassified(pred, y)
