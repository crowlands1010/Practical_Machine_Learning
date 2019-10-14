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
## roll_belt             1.064286    8.55978261   FALSE FALSE
## pitch_belt            1.106195   13.68885870   FALSE FALSE
## yaw_belt              1.041009   14.30027174   FALSE FALSE
## total_accel_belt      1.109937    0.23777174   FALSE FALSE
## gyros_belt_x          1.072115    1.04449728   FALSE FALSE
## gyros_belt_y          1.180498    0.53498641   FALSE FALSE
## gyros_belt_z          1.047486    1.39266304   FALSE FALSE
## accel_belt_x          1.074786    1.29925272   FALSE FALSE
## accel_belt_y          1.126214    1.18885870   FALSE FALSE
## accel_belt_z          1.152263    2.42017663   FALSE FALSE
## magnet_belt_x         1.008969    2.48811141   FALSE FALSE
## magnet_belt_y         1.170270    2.36922554   FALSE FALSE
## magnet_belt_z         1.093525    3.60903533   FALSE FALSE
## roll_arm             45.488889   19.42085598   FALSE FALSE
## pitch_arm            78.769231   22.66474185   FALSE FALSE
## yaw_arm              31.492308   21.34001359   FALSE FALSE
## total_accel_arm       1.038391    0.56046196   FALSE FALSE
## gyros_arm_x           1.025806    5.28192935   FALSE FALSE
## gyros_arm_y           1.585859    3.12500000   FALSE FALSE
## gyros_arm_z           1.071875    1.96161685   FALSE FALSE
## accel_arm_x           1.017857    6.50475543   FALSE FALSE
## accel_arm_y           1.138462    4.40726902   FALSE FALSE
## accel_arm_z           1.049383    6.41983696   FALSE FALSE
## magnet_arm_x          1.018519   11.07336957   FALSE FALSE
## magnet_arm_y          1.018868    7.26902174   FALSE FALSE
## magnet_arm_z          1.013889   10.59782609   FALSE FALSE
## roll_dumbbell         1.050633   87.74626359   FALSE FALSE
## pitch_dumbbell        2.325301   85.52989130   FALSE FALSE
## yaw_dumbbell          1.136986   87.32167120   FALSE FALSE
## total_accel_dumbbell  1.097411    0.36514946   FALSE FALSE
## gyros_dumbbell_x      1.007937    1.99558424   FALSE FALSE
## gyros_dumbbell_y      1.268012    2.26732337   FALSE FALSE
## gyros_dumbbell_z      1.025281    1.60495924   FALSE FALSE
## accel_dumbbell_x      1.077320    3.42221467   FALSE FALSE
## accel_dumbbell_y      1.088435    3.85529891   FALSE FALSE
## accel_dumbbell_z      1.172414    3.39673913   FALSE FALSE
## magnet_dumbbell_x     1.028846    8.80604620   FALSE FALSE
## magnet_dumbbell_y     1.281553    6.92934783   FALSE FALSE
## magnet_dumbbell_z     1.028302    5.58763587   FALSE FALSE
## roll_forearm         11.043269   14.78430707   FALSE FALSE
## pitch_forearm        62.027027   21.13620924   FALSE FALSE
## yaw_forearm          14.711538   14.10495924   FALSE FALSE
## total_accel_forearm   1.145288    0.57744565   FALSE FALSE
## gyros_forearm_x       1.049689    2.38620924   FALSE FALSE
## gyros_forearm_y       1.111111    6.05468750   FALSE FALSE
## gyros_forearm_z       1.066225    2.38620924   FALSE FALSE
## accel_forearm_x       1.230769    6.54721467   FALSE FALSE
## accel_forearm_y       1.237288    8.18614130   FALSE FALSE
## accel_forearm_z       1.089109    4.67051630   FALSE FALSE
## magnet_forearm_x      1.021739   11.96501359   FALSE FALSE
## magnet_forearm_y      1.372549   15.26834239   FALSE FALSE
## magnet_forearm_z      1.000000   13.40013587   FALSE FALSE
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
##         OOB estimate of  error rate: 0.76%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3344    3    0    0    1 0.001194743
## B   17 2253    9    0    0 0.011408513
## C    0   17 2030    7    0 0.011684518
## D    0    0   26 1903    1 0.013989637
## E    0    1    1    6 2157 0.003695150
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
##          A 2231    7    0    0    0
##          B    1 1510    9    0    0
##          C    0    1 1359   10    2
##          D    0    0    0 1273    3
##          E    0    0    0    3 1437
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9954          
##                  95% CI : (0.9937, 0.9968)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9942          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9996   0.9947   0.9934   0.9899   0.9965
## Specificity            0.9988   0.9984   0.9980   0.9995   0.9995
## Pos Pred Value         0.9969   0.9934   0.9905   0.9976   0.9979
## Neg Pred Value         0.9998   0.9987   0.9986   0.9980   0.9992
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1925   0.1732   0.1622   0.1832
## Detection Prevalence   0.2852   0.1937   0.1749   0.1626   0.1835
## Balanced Accuracy      0.9992   0.9966   0.9957   0.9947   0.9980
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
##          A 2051  304   24  104   38
##          B   50  892  108   65   96
##          C   66  174 1123  119  146
##          D   40   88   92  850   84
##          E   25   60   21  148 1078
## 
## Overall Statistics
##                                           
##                Accuracy : 0.764           
##                  95% CI : (0.7544, 0.7733)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.7002          
##                                           
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9189   0.5876   0.8209   0.6610   0.7476
## Specificity            0.9163   0.9496   0.9220   0.9537   0.9603
## Pos Pred Value         0.8136   0.7366   0.6898   0.7366   0.8093
## Neg Pred Value         0.9660   0.9057   0.9606   0.9348   0.9441
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2614   0.1137   0.1431   0.1083   0.1374
## Detection Prevalence   0.3213   0.1543   0.2075   0.1471   0.1698
## Balanced Accuracy      0.9176   0.7686   0.8715   0.8073   0.8540
```
