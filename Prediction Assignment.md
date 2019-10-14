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
## roll_belt             1.060391    8.55129076   FALSE FALSE
## pitch_belt            1.218182   13.83322011   FALSE FALSE
## yaw_belt              1.031847   14.38519022   FALSE FALSE
## total_accel_belt      1.026721    0.24626359   FALSE FALSE
## gyros_belt_x          1.042840    1.08695652   FALSE FALSE
## gyros_belt_y          1.136621    0.56046196   FALSE FALSE
## gyros_belt_z          1.027307    1.34171196   FALSE FALSE
## accel_belt_x          1.045852    1.32472826   FALSE FALSE
## accel_belt_y          1.144912    1.12941576   FALSE FALSE
## accel_belt_z          1.033708    2.43716033   FALSE FALSE
## magnet_belt_x         1.054545    2.54755435   FALSE FALSE
## magnet_belt_y         1.112272    2.43716033   FALSE FALSE
## magnet_belt_z         1.028269    3.66847826   FALSE FALSE
## roll_arm             51.000000   19.52275815   FALSE FALSE
## pitch_arm            92.772727   22.56283967   FALSE FALSE
## yaw_arm              30.000000   21.41644022   FALSE FALSE
## total_accel_arm       1.022770    0.55197011   FALSE FALSE
## gyros_arm_x           1.057047    5.27343750   FALSE FALSE
## gyros_arm_y           1.465116    3.11650815   FALSE FALSE
## gyros_arm_z           1.160000    1.92764946   FALSE FALSE
## accel_arm_x           1.009346    6.45380435   FALSE FALSE
## accel_arm_y           1.238095    4.39877717   FALSE FALSE
## accel_arm_z           1.160000    6.41134511   FALSE FALSE
## magnet_arm_x          1.226415   11.10733696   FALSE FALSE
## magnet_arm_y          1.020000    7.20957880   FALSE FALSE
## magnet_arm_z          1.015152   10.52989130   FALSE FALSE
## roll_dumbbell         1.050000   87.82269022   FALSE FALSE
## pitch_dumbbell        2.250000   85.67425272   FALSE FALSE
## yaw_dumbbell          1.166667   87.12635870   FALSE FALSE
## total_accel_dumbbell  1.069221    0.34816576   FALSE FALSE
## gyros_dumbbell_x      1.054598    1.96161685   FALSE FALSE
## gyros_dumbbell_y      1.311429    2.21637228   FALSE FALSE
## gyros_dumbbell_z      1.075630    1.60495924   FALSE FALSE
## accel_dumbbell_x      1.042328    3.43070652   FALSE FALSE
## accel_dumbbell_y      1.054795    3.86379076   FALSE FALSE
## accel_dumbbell_z      1.194245    3.36277174   FALSE FALSE
## magnet_dumbbell_x     1.028302    8.86548913   FALSE FALSE
## magnet_dumbbell_y     1.360825    6.89538043   FALSE FALSE
## magnet_dumbbell_z     1.173077    5.63009511   FALSE FALSE
## roll_forearm         11.674877   14.77581522   FALSE FALSE
## pitch_forearm        56.452381   21.13620924   FALSE FALSE
## yaw_forearm          16.458333   14.18987772   FALSE FALSE
## total_accel_forearm   1.150970    0.59442935   FALSE FALSE
## gyros_forearm_x       1.074534    2.31827446   FALSE FALSE
## gyros_forearm_y       1.106481    6.08865489   FALSE FALSE
## gyros_forearm_z       1.186851    2.40319293   FALSE FALSE
## accel_forearm_x       1.160000    6.58967391   FALSE FALSE
## accel_forearm_y       1.135593    8.31351902   FALSE FALSE
## accel_forearm_z       1.020619    4.62805707   FALSE FALSE
## magnet_forearm_x      1.217391   12.14334239   FALSE FALSE
## magnet_forearm_y      1.142857   15.26834239   FALSE FALSE
## magnet_forearm_z      1.058824   13.39164402   FALSE FALSE
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
##         OOB estimate of  error rate: 0.69%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3345    1    1    0    1 0.0008960573
## B   10 2257   12    0    0 0.0096533567
## C    0   15 2038    1    0 0.0077896787
## D    0    0   27 1900    3 0.0155440415
## E    0    0    2    8 2155 0.0046189376
```

```r
dataModel <- rpart(classe ~ .,data=myTrainingNew,method="class")
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
# Prediction 1
testPrediction <- predict(modelFit,myTest,type="class")
confusionMatrix(testPrediction,myTest$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2231   14    0    0    0
##          B    0 1502    8    0    0
##          C    1    2 1357   20    2
##          D    0    0    3 1265    4
##          E    0    0    0    1 1436
## 
## Overall Statistics
##                                           
##                Accuracy : 0.993           
##                  95% CI : (0.9909, 0.9947)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9911          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9996   0.9895   0.9920   0.9837   0.9958
## Specificity            0.9975   0.9987   0.9961   0.9989   0.9998
## Pos Pred Value         0.9938   0.9947   0.9819   0.9945   0.9993
## Neg Pred Value         0.9998   0.9975   0.9983   0.9968   0.9991
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1914   0.1730   0.1612   0.1830
## Detection Prevalence   0.2861   0.1925   0.1761   0.1621   0.1832
## Balanced Accuracy      0.9985   0.9941   0.9940   0.9913   0.9978
```

```r
# Prediction 2
modelFit2 <- randomForest(classe ~. ,data=myTrainingNew,method="class")
testPrediction2 <- predict(dataModel,myTest,type="class")
confusionMatrix(testPrediction2,myTest$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1976  306   41  130   48
##          B   62  834   61   33   94
##          C   55  157 1080  198  166
##          D   79  111   96  808   87
##          E   60  110   90  117 1047
## 
## Overall Statistics
##                                          
##                Accuracy : 0.7322         
##                  95% CI : (0.7223, 0.742)
##     No Information Rate : 0.2845         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.6601         
##                                          
##  Mcnemar's Test P-Value : < 2.2e-16      
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8853   0.5494   0.7895   0.6283   0.7261
## Specificity            0.9065   0.9605   0.9111   0.9431   0.9411
## Pos Pred Value         0.7901   0.7694   0.6522   0.6842   0.7353
## Neg Pred Value         0.9521   0.8988   0.9535   0.9283   0.9385
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2518   0.1063   0.1376   0.1030   0.1334
## Detection Prevalence   0.3188   0.1382   0.2111   0.1505   0.1815
## Balanced Accuracy      0.8959   0.7550   0.8503   0.7857   0.8336
```
