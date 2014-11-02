###---------------------------------------------------------
###   DEMO_1:  CREATE AN INITIAL SVM MODEL, TUNE, AND USE
###   Author: Stephen O'Connell
###   Date: 05/03/2001
###---------------------------------------------------------

##-----------------------------------------------------------------------------------------
rm(list=ls())
require(e1071)
setwd("/Users/oconste/Documents/workspace/sao_projects/CMG_Presentation")
source("helperFunctions.R")

###--------------------------------  OUT OF THE BOX -----------------------------
## GET DATA
Ynew <- dget("Y_7")
data <- createData(Ynew)

## SPLIT TO x and Y
x <- subset(data, select = -class)
y <- data$class

## BUILD MODEL
model <- svm(class ~ ., data = data, probability=TRUE)
summary(model)

## PREDICTIONS
pred <- predict(model, x, probability=TRUE)

# CHECK ACCURACY:
confusionM(pred, y)
printMissClassified(pred, y)

###--------------------------------- TUNING ----------------------------------------

#obj <- tune.svm(class~., data = data, gamma = 2^(-1:1), cost = 2^(2:4))

#system.time(obj2 <- tune.svm(class~., data = data, gamma = c(.1, .25, .3, .5), cost = c(1,2.5,4)))
#plot(obj)
#summary(obj)
#system.time(obj2 <- tune.svm(class~., data = data, gamma = c(.01, .05, .85), cost = c(1,1.5,2.25)))
#plot(obj)
#summary(obj2)
#dput(obj, "tune_svm.obj1.Rdput")
#dput(obj2, "tune_svm.obj2.Rdput")

###--------------------------------  AFTER TUNING -----------------------------
## NEW MODEL WITH COST AND GAMMA
model <- svm(class ~ ., data = data, cost=2.25, gamma=.01)
summary(model)

## RE-DO THE PREDICTION
pred <- predict(model, x)

# CHECK ACCURACY
confusionM(pred, y)

## DEBUGGING MISSCLASSIFIED
printMissClassified(pred, y)
plot(t(data[156,2:131]), type='l',main="Debug",ylab="%util", ylim=c(0,100))

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
