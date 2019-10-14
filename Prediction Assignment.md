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
## roll_belt             1.051188    8.59375000   FALSE FALSE
## pitch_belt            1.071429   13.65489130   FALSE FALSE
## yaw_belt              1.064103   14.42764946   FALSE FALSE
## total_accel_belt      1.062168    0.24626359   FALSE FALSE
## gyros_belt_x          1.088089    1.01902174   FALSE FALSE
## gyros_belt_y          1.134269    0.54347826   FALSE FALSE
## gyros_belt_z          1.109405    1.34171196   FALSE FALSE
## accel_belt_x          1.000000    1.34171196   FALSE FALSE
## accel_belt_y          1.129670    1.17187500   FALSE FALSE
## accel_belt_z          1.073446    2.43716033   FALSE FALSE
## magnet_belt_x         1.023364    2.46263587   FALSE FALSE
## magnet_belt_y         1.020305    2.36073370   FALSE FALSE
## magnet_belt_z         1.062500    3.61752717   FALSE FALSE
## roll_arm             49.341463   19.59069293   FALSE FALSE
## pitch_arm            74.962963   22.44395380   FALSE FALSE
## yaw_arm              32.629032   21.62024457   FALSE FALSE
## total_accel_arm       1.009363    0.55197011   FALSE FALSE
## gyros_arm_x           1.068852    5.29891304   FALSE FALSE
## gyros_arm_y           1.445483    3.11650815   FALSE FALSE
## gyros_arm_z           1.141956    1.97860054   FALSE FALSE
## accel_arm_x           1.090000    6.41983696   FALSE FALSE
## accel_arm_y           1.100000    4.40726902   FALSE FALSE
## accel_arm_z           1.162500    6.40285326   FALSE FALSE
## magnet_arm_x          1.051724   11.08186141   FALSE FALSE
## magnet_arm_y          1.107143    7.17561141   FALSE FALSE
## magnet_arm_z          1.126984   10.54687500   FALSE FALSE
## roll_dumbbell         1.267606   87.36413043   FALSE FALSE
## pitch_dumbbell        2.144444   84.90149457   FALSE FALSE
## yaw_dumbbell          1.267606   86.76120924   FALSE FALSE
## total_accel_dumbbell  1.072639    0.36514946   FALSE FALSE
## gyros_dumbbell_x      1.030220    1.94463315   FALSE FALSE
## gyros_dumbbell_y      1.337209    2.26732337   FALSE FALSE
## gyros_dumbbell_z      1.043478    1.59646739   FALSE FALSE
## accel_dumbbell_x      1.025000    3.38824728   FALSE FALSE
## accel_dumbbell_y      1.006579    3.82982337   FALSE FALSE
## accel_dumbbell_z      1.300752    3.37975543   FALSE FALSE
## magnet_dumbbell_x     1.047170    8.79755435   FALSE FALSE
## magnet_dumbbell_y     1.209091    6.87839674   FALSE FALSE
## magnet_dumbbell_z     1.233010    5.59612772   FALSE FALSE
## roll_forearm         11.829146   14.74184783   FALSE FALSE
## pitch_forearm        53.454545   20.99184783   FALSE FALSE
## yaw_forearm          16.680851   14.02853261   FALSE FALSE
## total_accel_forearm   1.187671    0.57744565   FALSE FALSE
## gyros_forearm_x       1.111821    2.38620924   FALSE FALSE
## gyros_forearm_y       1.093023    6.09714674   FALSE FALSE
## gyros_forearm_z       1.059016    2.47112772   FALSE FALSE
## accel_forearm_x       1.169492    6.60665761   FALSE FALSE
## accel_forearm_y       1.216667    8.21161685   FALSE FALSE
## accel_forearm_z       1.086957    4.61956522   FALSE FALSE
## magnet_forearm_x      1.040816   12.04144022   FALSE FALSE
## magnet_forearm_y      1.185185   15.27683424   FALSE FALSE
## magnet_forearm_z      1.000000   13.34918478   FALSE FALSE
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
##         OOB estimate of  error rate: 0.74%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3345    3    0    0    0 0.0008960573
## B   18 2256    5    0    0 0.0100921457
## C    0   15 2034    5    0 0.0097370983
## D    0    0   28 1900    2 0.0155440415
## E    0    0    2    9 2154 0.0050808314
```

```r
dataModel <- rpart(classe ~ .,data=myTrainingNew,method="class")
## dataPrediction <- predict(dataModel,myTest,type="class")
rpart.plot(dataModel,main="Figure 1: Classification",extra=100,under=TRUE,faclen=0)
```

![plot of chunk dataVisualization](figure/dataVisualization-1.png)

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
heat.tree(main="Figure 2: Heat Tree" ,treeModel,type=2,varlen=2,fallen.leaves=TRUE)
```

![plot of chunk dataVisualization](figure/dataVisualization-2.png)

```r
## Cross Validation
```
