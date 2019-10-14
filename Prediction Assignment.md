---
title: "Practical Machine Learning"
author: "Courtney Rowlands"
date: "10/14/2019"
output: 
  html_document:
    keep_md: yes
pdf_document: default
---

<!-- rmarkdown v1 -->



# Practical Machine Learning - Peer Graded Assignment

### Summary
One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.

The goal of you project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.


```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```


 

```
## [1] 19622   160
```

```
## [1]  20 160
```

```
##  Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```

```r
inTrain <- createDataPartition(y=dataTraining$classe,p=0.6,list=FALSE)
myTrain <- dataTraining[inTrain, ]
myTest <- dataTraining[-inTrain, ]

dim(myTrain)
```

```
## [1] 11776   160
```

```r
## Remove variables that are most NAs.
myTrainClean <- myTrain
for (i in 1:length(myTrain)) {
  if (sum(is.na(myTrain[ , i])) / nrow(myTrain) >= .75) {
    for (j in 1:length(myTrainClean)) {
      if (length(grep(names(myTrain[i]), names(myTrainClean)[j]))==1) {
        myTrainClean <- myTrainClean[ , -j]
      }
    }
  }
}

dim(myTrainClean)
```

```
## [1] 11776    60
```

```r
## Remove unneccessary columns
myTrainingNew <- myTrainClean[,8:length(myTrainClean)]

## Remove near zero variables
nearZero <- nearZeroVar(myTrainingNew,saveMetrics=TRUE)
nearZero
```

```
##                      freqRatio percentUnique zeroVar   nzv
## roll_belt             1.071298    8.67017663   FALSE FALSE
## pitch_belt            1.026549   13.87567935   FALSE FALSE
## yaw_belt              1.052632   14.41915761   FALSE FALSE
## total_accel_belt      1.059689    0.23777174   FALSE FALSE
## gyros_belt_x          1.072591    1.06997283   FALSE FALSE
## gyros_belt_y          1.157659    0.55197011   FALSE FALSE
## gyros_belt_z          1.098113    1.35869565   FALSE FALSE
## accel_belt_x          1.004184    1.33322011   FALSE FALSE
## accel_belt_y          1.163842    1.12092391   FALSE FALSE
## accel_belt_z          1.077213    2.43716033   FALSE FALSE
## magnet_belt_x         1.046512    2.45414402   FALSE FALSE
## magnet_belt_y         1.078481    2.36922554   FALSE FALSE
## magnet_belt_z         1.047101    3.63451087   FALSE FALSE
## roll_arm             44.340426   19.42085598   FALSE FALSE
## pitch_arm            83.400000   22.40149457   FALSE FALSE
## yaw_arm              32.061538   21.34001359   FALSE FALSE
## total_accel_arm       1.047882    0.55197011   FALSE FALSE
## gyros_arm_x           1.155477    5.31589674   FALSE FALSE
## gyros_arm_y           1.442675    3.10801630   FALSE FALSE
## gyros_arm_z           1.204545    1.91915761   FALSE FALSE
## accel_arm_x           1.019231    6.40285326   FALSE FALSE
## accel_arm_y           1.227642    4.41576087   FALSE FALSE
## accel_arm_z           1.115385    6.36888587   FALSE FALSE
## magnet_arm_x          1.060000   11.09035326   FALSE FALSE
## magnet_arm_y          1.094340    7.23505435   FALSE FALSE
## magnet_arm_z          1.093750   10.59782609   FALSE FALSE
## roll_dumbbell         1.000000   87.40658967   FALSE FALSE
## pitch_dumbbell        2.233766   85.54687500   FALSE FALSE
## yaw_dumbbell          1.026667   86.99898098   FALSE FALSE
## total_accel_dumbbell  1.034524    0.35665761   FALSE FALSE
## gyros_dumbbell_x      1.032967    1.92764946   FALSE FALSE
## gyros_dumbbell_y      1.273239    2.20788043   FALSE FALSE
## gyros_dumbbell_z      1.045333    1.68987772   FALSE FALSE
## accel_dumbbell_x      1.041667    3.43919837   FALSE FALSE
## accel_dumbbell_y      1.051282    3.83831522   FALSE FALSE
## accel_dumbbell_z      1.244604    3.38824728   FALSE FALSE
## magnet_dumbbell_x     1.018182    8.91644022   FALSE FALSE
## magnet_dumbbell_y     1.142857    6.94633152   FALSE FALSE
## magnet_dumbbell_z     1.071429    5.55366848   FALSE FALSE
## roll_forearm         11.974359   14.80129076   FALSE FALSE
## pitch_forearm        61.394737   21.10224185   FALSE FALSE
## yaw_forearm          15.657718   14.13043478   FALSE FALSE
## total_accel_forearm   1.135318    0.56895380   FALSE FALSE
## gyros_forearm_x       1.000000    2.36073370   FALSE FALSE
## gyros_forearm_y       1.073394    6.01222826   FALSE FALSE
## gyros_forearm_z       1.116838    2.42017663   FALSE FALSE
## accel_forearm_x       1.166667    6.49626359   FALSE FALSE
## accel_forearm_y       1.116667    8.20312500   FALSE FALSE
## accel_forearm_z       1.136842    4.69599185   FALSE FALSE
## magnet_forearm_x      1.122449   11.95652174   FALSE FALSE
## magnet_forearm_y      1.319149   15.20040761   FALSE FALSE
## magnet_forearm_z      1.108108   13.34069293   FALSE FALSE
## classe                1.469065    0.04245924   FALSE FALSE
```
### Random Decision Forest

```r
## Random Forest
set.seed(123)
modelFit <- randomForest(classe~.,data=myTrainingNew)
print(modelFit)
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = myTrainingNew) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.65%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3345    2    0    0    1 0.0008960573
## B   14 2259    6    0    0 0.0087757789
## C    0   13 2039    2    0 0.0073028238
## D    0    0   24 1903    3 0.0139896373
## E    0    0    3    9 2153 0.0055427252
```

```r
## Cross Validation
testPrediction <- predict(modelFit,dataTesting,type="class")

trainingResults <- predict(modelFit,myTrainingNew)
trainingAccuracy <- sum(trainingResults==myTrainingNew$classe)/length(trainingResults)
paste("Accuracy on training set =",trainingAccuracy)
```

```
## [1] "Accuracy on training set = 1"
```

```r
validationResults <- predict(modelFit, newdata=myTest)
validationAccuracy <- sum(validationResults==myTest$classe)/length(validationResults)
paste("Accuracy on validation set =",validationAccuracy)
```

```
## [1] "Accuracy on validation set = 0.992352791231201"
```

```r
testResults <- predict(modelFit,newdata=dataTesting)
print("Classifications on the test set:");testResults
```

```
## [1] "Classifications on the test set:"
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

```r
heat.tree <- function(tree, low.is.green = FALSE, ...) 
  { 
    # dots args passed to prp
    y <- tree$frame$yval
    if(low.is.green)
      y <- -y
    max <- max(y)
    min <- min(y)
    cols <- rainbow(99, end = .36)[
            ifelse(y > y[1], (y-y[1]) * (99-50) / (max-y[1]) + 50,
                  (y-min) * (50-1) / (y[1]-min) + 1)]
    prp(tree, branch.col = cols, box.col = cols, ...)
  }

treeModel <- rpart(classe ~ ., data=myTrainingNew,method="anova")
heat.tree(treeModel,type=2,varlen=2,fallen.leaves=TRUE)
```

![plot of chunk dataVisualization](figure/dataVisualization-1.png)
