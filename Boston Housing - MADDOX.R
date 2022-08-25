#Team 9
#Artemis Maddox

#libraries needed
library(mlbench)
library(caret)
library(corrplot)
library(AppliedPredictiveModeling)
library(xlsx)

#data source
data("BostonHousing")
dim(BostonHousing)
#[1] 506  14
View(BostonHousing)

#write to csv
write.csv(BostonHousing, file = "BostHous_Team9.csv")

#import 
BostonHousing_Team9 <- read.csv("BostHous_Team9.csv")
dim(BostonHousing_Team9)
#[1] 506  15
View(BostonHousing_Team9)

##Note:
#column "X" of row numbers from original data has been added, but is unneeded
#so I removed it
BostonHousing_Team9 <- BostonHousing_Team9[,2:15]


#omit rows with missing values
newBost <- na.omit(BostonHousing_Team9)
dim(newBost)
#[1] 506  14
View(newBost)

#select x and y
taxPredictors <- subset(newBost, select = -tax)
taxOutcome <- subset(newBost, select = "tax")

#split data 75% train 25% test
set.seed(1234)
trainingRows <- createDataPartition(newBost$tax,
                                    p = 0.75,   # 75 % into training data set
                                    list = FALSE)

trainPred <- taxPredictors[trainingRows,]
trainOut <- taxOutcome[trainingRows,]

testPred <- taxPredictors[-trainingRows,]  
testOut <- taxOutcome[-trainingRows,] 

#explore data
library(skimr)

skimmed1 <- skim(trainPred)

skimmed2 <- skim(trainOut)

skimmed1
skimmed2
##Output:
#> skimmed1
#-- Data Summary ------------------------
#  Values         
#Name                       trainPredictors
#Number of rows             380            
#Number of columns          13             
#_______________________                   
#Column type frequency:                    
#  numeric                  13             
#________________________                  
#Group variables            None           
#
#-- Variable type: numeric -------------------------------------------------------------------------------------------------------
#  # A tibble: 13 x 11
#  skim_variable n_missing complete_rate     mean     sd       p0      p25     p50     p75    p100 hist 
#* <chr>             <int>         <dbl>    <dbl>  <dbl>    <dbl>    <dbl>   <dbl>   <dbl>   <dbl> <chr>
#1 crim                  0             1   3.81    8.77   0.00906   0.0870   0.324   4.05   89.0   ▇▁▁▁▁
#2 zn                    0             1  11.1    23.7    0         0        0       3.12  100     ▇▁▁▁▁
#3 indus                 0             1  11.3     6.84   0.74      5.56     9.9    18.1    27.7   ▆▆▁▇▁
#4 chas                  0             1   0.0632  0.244  0         0        0       0       1     ▇▁▁▁▁
#5 nox                   0             1   0.559   0.115  0.385     0.463    0.538   0.631   0.871 ▇▇▆▅▁
#6 rm                    0             1   6.29    0.703  3.56      5.88     6.20    6.62    8.78  ▁▂▇▂▁
#7 age                   0             1  70.0    28.2    2.9      45.3     81.0    94.8   100     ▁▂▂▂▇
#8 dis                   0             1   3.70    2.12   1.13      2.04     2.95    4.82   12.1   ▇▃▂▁▁
#9 rad                   0             1   9.87    8.85   1         4        5      24      24     ▇▂▁▁▃
#10 ptratio               0             1  18.5     2.16  12.6      17.4     19.1    20.2    22     ▁▃▅▃▇
#11 b                     0             1 355.     93.2    0.32    373.     391.    396.    397.    ▁▁▁▁▇
#12 lstat                 0             1  12.9     7.30   1.73      7.17    11.7    17.2    38.0   ▇▇▅▂▁
#13 medv                  0             1  22.4     9.30   5        16.5     21      25      50     ▃▇▅▁▁
#> 


#> skimmed2
#-- Data Summary ------------------------
#  Values    
#Name                       trainYield
#Number of rows             380       
#Number of columns          1         
#_______________________              
#Column type frequency:               
#  numeric                  1         
#________________________             
#Group variables            None      
#
#-- Variable type: numeric -------------------------------------------------------------------------------------------------------
#  # A tibble: 1 x 11
#  skim_variable n_missing complete_rate  mean    sd    p0   p25   p50   p75  p100 hist 
#* <chr>             <int>         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>
#  1 data                  0             1  412.  172.   187   279   330   666   711 ▇▇▃▁▇
#> 

#correlate with corrl'n plot
correlations <- cor(trainPred[,1:13])
corrplot(correlations, method="circle")
corrplot(correlations, method="number")

##High Positive Correlation:
#nox~indus : 0.77
#age~nox : 0.73
#zn~dis : 0.69
#medv~rm : 0.68

##High Negative Correlation:
#dis~indus : -0.70
#dis~nox : -0.76
#dis~age : -0.77
#medv~lstat : -0.73


#resample with boot_all

control <- trainControl(method="boot_all", number=10, repeats=3)


#linear regression model w/ preprocessing
#preprocess methods: descriptions from the preProcess {caret} R Documentation
#
#first center (subtracting the data mean), 
#then scale (divide data by standard deviation), 
#then YeoJohnson (a Box-Cox normalizing transformation that allows for 0 or negative values)
#then corr (to filter out highly correlated predictors)
#
#once the data has been normalized and correlations filtered, k-nearest neighbors
#imputation fits bagged tree models to each predictor
#then zv identifies numeric predictor columns with a single value 
#(meaning zero variance) and excludes them from further calculations
#finally nzv does the same, applying nearZeroVar to exclude "near zer-variance" predictors.

set.seed(7)
LR <- train(x = trainPred, y = trainOut, 
            method="lm", 
            preProc=c("center", "scale", "YeoJohnson", "corr",  
                      "knnImpute", "zv", "nzv"), 
            trControl=control)
LR
##Output:
#> LR
#Linear Regression 
#
#380 samples
#13 predictor
#
#Pre-processing: centered (13), scaled (13), Yeo-Johnson transformation (9),
#nearest neighbor imputation (13) 
#Resampling: Bootstrapped (10 reps) 
#Summary of sample sizes: 380, 380, 380, 380, 380, 380, ... 
#Resampling results:
#  
#  RMSE      Rsquared   MAE       RMSE_632  Rsquared_632  MAE_632   RMSE_OptBoot
#69.15676  0.8347446  51.12698  68.09001  0.840651      50.40538  67.70664    
#Rsquared_OptBoot  MAE_OptBoot
#0.8431622         49.77606   
#
#Tuning parameter 'intercept' was held constant at a value of TRUE
#> 




#Team 9
#Week 2
#Artemis Maddox

##///
#From Week 1:
library(mlbench)
library(caret)
library(corrplot)
library(AppliedPredictiveModeling)
library(xlsx)
#From Ch6:
### Relationship Modeling
library(AppliedPredictiveModeling)
library(elasticnet)
library(lars)
library(pls)
library(stats)
library(MASS)
library(caret)
library(lattice)
##From Ch7:
# Install packages such as caret, earth, kernlab, and nnet
library (caret)
library (earth)
library(kernlab)

##///

library(mlbench)
#data source
data("BostonHousing")
dim(BostonHousing)
#[1] 506  14
View(BostonHousing)

#write to csv
write.csv(BostonHousing, file = "BostHous_Team9.csv")

#import 
BostonHousing_Team9 <- read.csv("BostHous_Team9.csv")
dim(BostonHousing_Team9)
#[1] 506  15
View(BostonHousing_Team9)

##Note:
#column "X" of row numbers from original data has been added, but is unneeded
#so I removed it
BostonHousing_Team9 <- BostonHousing_Team9[,2:15]

##------------------------Linear Regression------------------------##
#~~~Gotta separate our trains and tests:

##Note: This is from Week 1, as is the data import. I had issues using BostonHousing
#       as is, so went ahead and reused what I know works.


#omit rows with missing values
newBost <- na.omit(BostonHousing_Team9)
dim(newBost)
#[1] 506  14
View(newBost)

#select x and y
taxPredictors <- subset(newBost, select = -tax)
taxOutcome <- subset(newBost, select = "tax")

#split data 75% train 25% test
set.seed(1234)
trainingRows <- createDataPartition(newBost$tax,
                                    p = 0.75,   # 75 % into training data set
                                    list = FALSE)

trainPred <- taxPredictors[trainingRows,]
trainOut <- taxOutcome[trainingRows,]

testPred <- taxPredictors[-trainingRows,]  
testOut <- taxOutcome[-trainingRows,] 

#Resample with boot_all:

control <- trainControl(method="boot_all", number=10, repeats=3)

#~~~Then regress in a linear fashion:

set.seed(7)
LR <- train(x = trainPred, y = trainOut, 
            method="lm", 
            preProc=c("center", "scale", "YeoJohnson", "corr",  
                      "knnImpute", "zv", "nzv"), 
            trControl=control)
LR


##------------------------------Partial Least Squares---------------------------##
#(4 variations: like pls, simpls, widekernelpls, oscorepls)

ctrl_A <- trainControl(method = "cv")

#Tuning parameter: ++++++++++ Option A (Do nothing) ++++++++++++
# By default, Caret automatically tunes hyperparameters for 3 values.

set.seed(999)
plsTune <- train(x = trainPred, y = trainOut,
                 method = "pls",
                 trControl = ctrl_A)
plsTune

simplsTune <- train(x = trainPred, y = trainOut,
                    method = "simpls",
                    trControl = ctrl_A)
simplsTune

widekernTune <- train(x = trainPred, y = trainOut,
                      method = "widekernelpls",
                      trControl = ctrl_A)
widekernTune

oscoreTune <- train(x = trainPred, y = trainOut,
                    method = "oscorepls",
                    trControl = ctrl_A)
#Error: Model oscorepls is not in caret's built-in library

??oscorepls


##----------------------------Penalized Regression Models-----------------------##
#(3) (like ridge regression, lasso regression, elasticnet regression)

set.seed(999)
ctrl_10 <- trainControl(method = "cv", number=10)

set.seed(999)
lmTune0 <- train(x = trainPred, y = trainOut,
                 method = "lm",
                 trControl = ctrl_10)

lmTune0

summary(lmTune0)

lmPred1 <- predict(lmTune0  , testPred)
head(lmPred1)

lmValues1 <- data.frame(obs = testOut, pred = lmPred1)
defaultSummary(lmValues1)

##-------------------Multivariate Adaptive Regression Splines----------------##
set.seed(999)
marsTune <- train(x = trainPred, y = trainOut,
                  method = "earth",
                  tuneGrid = expand.grid(degree = 1:2, nprune = 2:38),
                  trControl = ctrl_10)

marsTune
plot(marsTune)
varImp(marsTune) 

marsImp <- varImp(marsTune, scale = FALSE)

plot(marsImp, top = 25)

##-----------------------------Support Vector Machine-----------------------##
#Radial
set.seed(999)
svmRTune <- train(x = trainPred, y = trainOut,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneLength = 14,
                  trControl = ctrl_10)

svmRTune

plot(svmRTune, scales = list(x = list(log = 2)))
svmRTune$finalModel

##I'm confused as to if this part needs adjusting:
svmGrid <- expand.grid(degree = 1:2, 
                       scale = c(0.01, 0.005, 0.001), 
                       C = 2^(-2:5))

#Polynomial
set.seed(999)
svmPTune <- train(x = trainPred, y = trainOut,
                  method = "svmPoly",
                  preProc = c("center", "scale"),
                  tuneGrid = svmGrid,
                  trControl = ctrl_10)

svmPTune

plot(svmPTune, 
     scales = list(x = list(log = 2), 
                   between = list(x = .5, y = 1)))         


##------------------------------K-Nearest Neighbors-----------------------##
#Remove near-zero variance predictors:
knnDescr <- trainPred[, -nearZeroVar(trainPred)]

set.seed(999)
knnTune <- train(x = knnDescr, y = trainOut,
                 method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(k = 1:20),
                 trControl = ctrl_10)

knnTune

plot(knnTune)  # Similar to Fig 7.10 on Page 161.

# Maddox Week 6 Draft 1

#Install libraries
library(mlbench)
library(caret)
library(corrplot)
library(AppliedPredictiveModeling)
library(glmnet)
library(earth)
library(kernlab)
library(DALEX)

#Load built-in data set BostonHousing from mlbench package
BHData = BostonHousing
head(BHData)

### Create a control function that will be used across models. 

library(caret)
set.seed(100)
ctrl <- trainControl(method = "cv", number=10)
#Separate target/outcome variable from the rest of the data

#Select/separate all rows (independent variables) 
#excluding medv (target: median value of owner occupied homes) column
predictors <- subset(BHData, select = -medv)
predictors
#Select/separate the target variable (medv)
MedianValue <- subset(BHData, select = "medv")
MedianValue

#Split the data set into training (75%) & test set. 

set.seed(9) #set random number to 9 for group 9
trainingRows <- createDataPartition(MedianValue$medv,
                                    p = 0.75,   # 75 % into training data set
                                    list = FALSE)
#make training predictors, test predictors 
# and the outcome variable for training and test
trainPredictors <- predictors[trainingRows,]
trainMedianValue <- MedianValue[trainingRows,]
testPredictors <- predictors[-trainingRows,]
testMedianValue <- MedianValue[-trainingRows,] 

##############################################################################
### Random Forest

set.seed(100)
#running with ntree = 5
rfTune5 <- train(x = trainPredictors, y = trainMedianValue,
                 method = "rf",
                 tuneLength=5,
                 ntree = 5, 
                 trControl = ctrl)
rfTune5
plot(rfTune5)

#running with ntree = 1000
rfTuneT <- train(x = trainPredictors, y = trainMedianValue,
                 method = "rf",
                 tuneLength=5,
                 ntree = 1000, 
                 trControl = ctrl)
rfTuneT
plot(rfTuneT)

rfImp <- varImp(rfTuneT, scale = FALSE)
rfImp


################################################################################
### Boosting

library(gbm)
gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                       n.trees = seq(100, 1000, by = 50), 
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 20)
set.seed(100)
#training the model
gbmTune <- train(x = trainPredictors, y = trainMedianValue,
                 method = "gbm",
                 tuneLength=5,
                 trControl = ctrl)

gbmTune
summary(gbmTune)
plot(gbmTune, auto.key = list(columns = 4, lines = TRUE))

gbmImp <- varImp(gbmTune, scale = FALSE)
gbmImp


################################################################################
### Cubist

cbGrid <- expand.grid(committees = c(1:10, 20, 50, 75, 100), 
                      neighbors = c(0, 1, 5, 9))
set.seed(100)
#training the model
cubistTune <- train(trainPredictors, trainMedianValue,
                    method= "cubist",
                    trControl = ctrl)
cubistTune
summary(cubistTune)
plot(cubistTune, auto.key = list(columns = 4, lines = TRUE)) 

#Variables of importance

cbImp <- varImp(cubistTune, scale = FALSE)
cbImp


################################################################################
### DALEX Stuff

library(DALEX)

###Note: I'm using rfTune5 for debugging as it is faster to create
##Also: adding the label changes the model label, but both with and without function
#The DALEX instructions said "To create an explainer for these models it is enough 
#to use explain() function with the model, data and y parameters."

explain(rfTune5, label = "rf", data = BHData, y = predictors)
splainMeRF <- explain(rfTune5, data = BHData, y = predictors)

explain(gbmTune, label = "gbm", data = BHData, y = predictors)
splainMeGBM <- explain(gbmTune, data = BHData, y = predictors)

explain(cubistTune, label = "cube", data = BHData, y = predictors)
splainMeCUBE <- explain(cubistTune, data = BHData, y = predictors)

### Model Performance

Perf_RF <- model_performance(splainMeRF)
Perf_RF

Perf_GBM <- model_performance(splainMeGBM)
Perf_GBM

Perf_CUBE <- model_performance(splainMeCUBE)
Perf_CUBE

plot(Perf_RF, Perf_GBM, Perf_CUBE)

plot(Perf_RF, Perf_GBM, Perf_CUBE, geom = "boxplot")


### Variable Importance DALEX

VarImp_RF <- model_parts(splainMeRF, loss_function = loss_root_mean_square)
VarImp_RF

VarImp_GBM <- model_parts(splainMeGBM, loss_function = loss_root_mean_square)
VarImp_GBM

VarImp_CUBE <- model_parts(splainMeCUBE, loss_function = loss_root_mean_square)
VarImp_CUBE

plot(VarImp_RF, VarImp_GBM, VarImp_CUBE)
##Error Message:
##Error: Can't draw more than one boxplot per group. Did you forget aes(group = ...)?
##Run `rlang::last_error()` to see where the error occurred.


##################
### Partial Dependence Plot
### Example code:
pdp_regr_rf <- model_profile(explainer_regr_rf, variable = "construction.year"
                             , type = "partial")
pdp_regr_gbm <- model_profile(explainer_regr_gbm, variable = "construction.yea
r", type = "partial")
pdp_regr_nn <- model_profile(explainer_regr_nn, variable = "construction.year"
                             , type = "partial")
plot(pdp_regr_rf, pdp_regr_gbm, pdp_regr_nn)

### Boston Housing Code: Need to pick variable
pdpRF <- model_profile(splainMeRF, variable = "", type = "partial")
pdpRF

pdpGBM <- model_profile(splainMeGBM, variable = "", type = "partial")
pdpGBM

pdpCUBE <- model_profile(splainMeCUBE, variable = "", type = "partial")
pdpCUBE

##################
### Accumulated Local Efforts
### Example code:
ale_regr_rf <- model_profile(explainer_regr_rf, variable = "construction.year"
                             , type = "accumulated")
ale_regr_gbm <- model_profile(explainer_regr_gbm, variable = "construction.yea
r", type = "accumulated")
ale_regr_nn <- model_profile(explainer_regr_nn, variable = "construction.year"
                             , type = "accumulated")
plot(ale_regr_rf, ale_regr_gbm, ale_regr_nn)
### For categorical variables:
mpp_regr_rf <- model_profile(explainer_regr_rf, variable = "district", type =
                               "partial")
mpp_regr_gbm <- model_profile(explainer_regr_gbm, variable = "district", type
                              = "partial")
mpp_regr_nn <- model_profile(explainer_regr_nn, variable = "district", type =
                               "partial")
plot(mpp_regr_rf, mpp_regr_gbm, mpp_regr_nn)

### Boston Housing Code: Need to pick variable
ALE_RF <- model_profile(splainMeRF, variable = "", type = "accumulated")
ALE_RF

ALE_GBM <- model_profile(splainMeGBM, variable = "", type = "accumulated")
ALE_GBM

ALE_CUBE <- model_profile(splainMeCUBE, variable = "", type = "accumulated")
ALE_CUBE