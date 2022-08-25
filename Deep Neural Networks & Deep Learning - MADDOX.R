### Artemis Maddox
### Week 7 HW

################################################################################
############ 1. Deep Neural Networks (Numeric Response Variable) ###############

# Lecture Video: https://youtu.be/SrQw_fWo4lw 

# Libraries
library(keras)
library(mlbench) 
library(dplyr)
library(magrittr)
library(neuralnet)
library(caret)

# Data
data("cars")
data <- cars
str(data)
View(data)

data %<>% mutate_if(is.integer, as.numeric)

# Neural Network Visualization
n <- neuralnet(Price ~ Mileage+Cylinder+Doors+Cruise+Sound+Leather+Buick+Cadillac
               +Chevy+Pontiac+Saab+Saturn+convertible+coupe+hatchback+sedan+wagon,
               data = data,
               hidden = c(10,5),
               linear.output = F,
               lifesign = 'full',
               rep=1)
plot(n,
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

# Matrix
data <- as.matrix(data)
dimnames(data) <- NULL

# Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
training <- data[ind==1, 2:18] #predictor variables
test <- data[ind==2, 2:18]
trainingtarget <- data[ind==1, 1] #outcome variable
testtarget <- data[ind==2, 1]

# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

### First Step in Model Analysis:
# Create Model: @7:20 in YouTube lecture
model_A <- keras_model_sequential()
model_A %>% 
  layer_dense(units = 5, activation = 'relu', input_shape = c(17)) %>% #hidden layer: 5 neurons; input layer: 17 variables
  layer_dense(units = 1) #output layer: 1 neuron
### The above model has few layers/neurons in order to run fast and validate the method. 
#These will be increased in the Fine-Tune Stage. 


# Compile
model_A %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')

# Fit Model
mymodel_A <- model_A %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate
model_A %>% evaluate(test, testtarget)
pred_A <- model_A %>% predict(test)
mean((testtarget-pred_A)^2) #MSE Mean Squared Error
plot(testtarget, pred_A) #Scatterplot of Error

### Second Step in Model Analysis:
# Fine-Tune Model 
## First Run: A Few More Neurons and Layers (see @11:50 in video)
model_A1 <- keras_model_sequential()
model_A1 %>% 
  layer_dense(units = 10, activation = 'relu', input_shape = c(17)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 5, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)
summary(model_A1)

# Compile
model_A1 %>% compile(loss = 'mse',
                  optimizer = optimizer_rmsprop(lr = 0.001), 
                  #small for greatest reduction in error
                  metrics = 'mae')

# Fit Model
mymodel_A1 <- model_A1 %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate
model_A1 %>% evaluate(test, testtarget)
pred_A1 <- model_A1 %>% predict(test)
mean((testtarget-pred_A1)^2) #MSE Mean Squared Error
plot(testtarget, pred_A1) #Scatterplot of Error

## Second Run: A Full Lobe of Brain (see @14:10)
model_A2 <- keras_model_sequential()
model_A2 %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(17)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)
summary(model_A2)

# Compile
model_A2 %>% compile(loss = 'mse',
                     optimizer = optimizer_rmsprop(lr = 0.001), 
                     #small for greatest reduction in error
                     metrics = 'mae')

# Fit Model
mymodel_A2 <- model_A2 %>%
  fit(training,
      trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

# Evaluate
model_A2 %>% evaluate(test, testtarget)
pred_A2 <- modelA2 %>% predict(test)
mean((testtarget-pred_A2)^2) #MSE Mean Squared Error
plot(testtarget, pred_A2) #Scatterplot of Error

### Use the MSE and scatterplot results from each model to see how increasing 
# neruons and layers decreases error, and calibrate the values for model_A2 
# as you feel best for maximum error reduction for computation power 
# (if you feel they need changing)






################################################################################
#### 2. Deep Learning (Multilayer Perceptron for Multiclass Classification) ####

# Lecture Video: https://youtu.be/hd81EH1g1bE

library(AppliedPredictiveModeling) #for 'hepatic' dataset

# Install Packages

install.packages("keras")

library(keras)
install_keras()

# Data Bio
data("hepatic")
dataB <- bio
dataB[, 185] <- injury
str(dataB)
View(dataB)

# Response Variable is Injury, a separate factor with 3 levels. 
# This has been added to dataB as V185

dataB %<>% mutate_if(is.integer, as.numeric)
dataB <- as.matrix(dataB)
dimnames(dataB) <- NULL

# Normalize
dataB[, 1:184] <- normalize(dataB[, 1:184])
dataB[, 185] <- as.numeric(dataB[, 185]) -1
summary(dataB)
str(dataB)

# Data Partition
set.seed(1234)
indB <- sample(2, nrow(dataB), replace = T, prob = c(0.7, 0.3))
trainingB <- dataB[ind==1, 1:184] #predictor variables
testB <- dataB[ind==2, 1:184]
trainingtarget_B <- data[ind==1, 185] #outcome variable Injury
testtarget_B <- data[ind==2, 185]

# One Hot Encoding
trainLabels <- to_categorical(trainingtarget_B)
testLabels <- to_categorical(testtarget_B)
print(testLabels)

# Create Sequential Model: Again, fewer neurons and layers to validate functionality
# YouTube video @8:00
model_B <- keras_model_sequential()
model_B %>% 
  layer_dense(units = 8, activation = 'relu', input_shape = c(184)) %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model_B)

#Compile
model_B %>%
  compile(loss = 'categorical_crossentropy',
          optimizer = 'adam',
          metrics = 'accuracy')

# Fit Model
# This is what visually shows your error. See @13:00 in the video
history_B <- model_B %>% 
  fit(trainingB,
      trainLabels,
      epoch = 200,
      batch_size = 32,
      validation_split = 0.2)
plot(history_B) 

# Evaluate Model with Test Data
# Shows Loss and Accuracy Ratings
# @15:40
model_B %>%
  evaluate(testB, testLabels)

# Prediction & Confusion Matrix - Test Data
# Shows misclassification 
prob_B <- model_B %>%
          predict_proba(testB)
pred_B <- model_B %>%
          predict_classes(testB)
table_B <- table(Predicted = pred_B, Actual = testtarget_B)

# Table of outputs @18:30
cbind(prob_B, pred_B, testtarget_B)


# Fine Tune Model: Add more neurons 
# @20:20
model_B2 <- keras_model_sequential()
model_B2 %>% 
  layer_dense(units = 50, activation = 'relu', input_shape = c(184)) %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model_B2)

# Compile
model_B2 %>%
  compile(loss = 'categorical_crossentropy',
          optimizer = 'adam',
          metrics = 'accuracy')

# Fit Model
history_B2 <- model_B2 %>%
  fit(trainingB,
      trainLabels,
      epoch = 200,
      batch_size = 32,
      validation_split = 0.2)
plot(history_B2)

# Evaluate Model with Test Data
model_B2 %>%
  evaluate(testB, testLabels)

# Prediction & Confusion Matrix - Test Data
# Shows misclassification 
prob_B2 <- model_B2 %>%
  predict_proba(testB)
pred_B2 <- model_B2 %>%
  predict_classes(testB)
table_B2 <- table(Predicted = pred_B2, Actual = testtarget_B)

# Table of outputs
cbind(prob_B2, pred_B2, testtarget_B)

# Fine Tune Model: More Layers 
model_B3 <- keras_model_sequential()
model_B3 %>% 
  layer_dense(units = 50, activation = 'relu', input_shape = c(184)) %>%
  layer_dense(units = 8, activation = 'relu',) %>%
  layer_dense(units = 3, activation = 'softmax')
summary(model_B3)

# Compile
model_B3 %>%
  compile(loss = 'categorical_crossentropy',
          optimizer = 'adam',
          metrics = 'accuracy')

# Fit Model
history_B3 <- model_B3 %>%
  fit(trainingB,
      trainLabels,
      epoch = 200,
      batch_size = 32,
      validation_split = 0.2)
plot(history_B3)

#Evaluate Model with Test Data
model_B3 %>%
  evaluate(testB, testLabels)

# Prediction & Confusion Matrix - Test Data
# Shows misclassification 
prob_B3 <- model_B3 %>%
  predict_proba(testB)
pred_B3 <- model_B3 %>%
  predict_classes(testB)
table_B3 <- table(Predicted = pred_B3, Actual = testtarget_B)

# Table of outputs
cbind(prob_B3, pred_B3, testtarget_B)

# Model Comparison Side-by-Side
table_B
model_B

table_B2
model_B2

table_B3
model_B3

#### Again feel free to adjust the neurons and layers as you see fit.
