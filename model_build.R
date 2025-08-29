library(readr)
library(data.table)
library(ggplot2)
library(dplyr)
library(RSpectra)
library(readxl)
library(readr)
setwd("C:/Users/User/Downloads")
metadata=read_xlsx("Metadata (1).xlsx")

NFT=read.csv("NFT.csv")



data_cfdna_meta=read.csv("C:/Users/User/Downloads/gastric0720.csv")




train=data_cfdna_meta[which(data_cfdna_meta$Cohort=="Train"),]

test=data_cfdna_meta[which(data_cfdna_meta$Cohort=="Test"),]




# Select columns that start with TMD
tmd_columns <-c("SampleID","Label", c(grep("^CNA", names(train), value = TRUE),
                                      grep("^MOTIF_END", names(train), value = TRUE),grep("^GWMD", names(train), value = TRUE),
                                      grep("^flen", names(train), value = TRUE),grep("^TMD", names(train), value = TRUE)))
train=train[,tmd_columns]
test=test[,tmd_columns]

library(caret)
# Assuming 'train_tmd' contains only the columns you're interested in
set.seed(1234) # For reproducibility
folds <- createFolds(train$Label, k = 3)



# Loop over each fold
for(i in seq_along(folds)) {
  # Create training and test datasets for the current fold
  train_indices <- folds[[i]]
  test_indices <- setdiff(1:nrow(train), train_indices)
  
  # Specify training and test data
  train_data <- train[test_indices, -1]
  test_data <- train[train_indices, -1]
  
  
  train_data$Label=as.factor(train_data$Label)
  
  # Specify the models to train
  models <- list(
    gbm = list(method = "gbm", tuneLength = 3),
    svm = list(method = "svmLinear", tuneLength = 3),
    rf = list(method = "rf", tuneLength = 3),
    logistic = list(method = "glm", family = "binomial")
  )
  
  # Loop over each model
  library(caret)
  library(e1071)  # For SVM models
  library(randomForest)  # Random forest models
  library(gbm)  # GBM models
  
  # Define the control and tuning grid for RF
  train_control_rf <- trainControl(method = "cv", number = 10)
  tuneGrid_rf <- expand.grid(mtry = c(2, 3, 4))
  
  # Train the RF model
  rf_model <- train(Label ~ ., data = train_data,
                    method = "rf",
                    trControl = train_control_rf,
                    tuneGrid = tuneGrid_rf)
  # Define the control and tuning grid for GBM
  train_control_gbm <- trainControl(method = "cv", number = 10)
  tuneGrid_gbm <- expand.grid(interaction.depth = c(1, 3, 5),
                              n.trees = 100,
                              shrinkage = 0.1,
                              n.minobsinnode = 10)
  
  # Train the GBM model
  gbm_model <- train(Label ~ ., data = train_data,
                     method = "gbm",
                     trControl = train_control_gbm,
                     tuneGrid = tuneGrid_gbm)
  
  # Train the Logistic Regression model
  logistic_model <- train(Label ~ ., data = train_data,
                          method = "glm",
                          family = "binomial",
                          trControl = trainControl(method = "cv", number = 10))
  
  # Define the control and tuning grid for SVM
  # Assuming train_data is already defined
  # Define train control
  train_control_svm <- trainControl(method = "cv", number = 10)
  
  # Define the tuning grid. Adjust the values based on your dataset and needs.
  tuneGrid_svm <- expand.grid(C = 2^(-5:2), sigma = 2^(-15:3))
  
  # Train the SVM model with RBF kernel
  svm_model <- train(Label ~ ., data = train_data,
                     method = "svmRadial",
                     trControl = train_control_svm,
                     tuneGrid = tuneGrid_svm,
                     preProcess = "scale") # Scaling features can be important for SVM
  
  
  # Refit RF model with the best hyperparameters on the entire training data
  final_rf_model <- randomForest(Label ~ ., data = train_data,
                                 mtry = rf_model$bestTune$mtry,
                                 ntree = 500)  # You can specify ntree or any other RF parameters as needed
  
  
  # Make predictions with the RF model
  predictions_rf <- predict(final_rf_model, newdata = test_data,"prob")
  
  pred_rf=ifelse(predictions_rf[,1]>0.5,0,1)
  
  # Optionally, evaluate performance
  accuracy_rf <- confusionMatrix(factor(pred_rf,levels = c(0,1)),factor(test_data$Label,levels = c(0,1)))
  
  
  
  # Refit GBM model with the best hyperparameters on the entire training data
  final_gbm_model <- gbm(Label ~ ., data = train_data,
                         distribution = "bernoulli",
                         n.trees = gbm_model$bestTune$n.trees,
                         interaction.depth = gbm_model$bestTune$interaction.depth,
                         shrinkage = gbm_model$bestTune$shrinkage,
                         n.minobsinnode = gbm_model$bestTune$n.minobsinnode)
  
  
  
  # Make predictions with the GBM model
  predictions_gbm <- predict(gbm_model, newdata = test_data,"prob")
  pred_gbm=ifelse(predictions_gbm[,1]>0.5,0,1)
  
  # Optionally, evaluate performance
  accuracy_gbm <- confusionMatrix(factor(pred_gbm,levels = c(0,1)),factor(test_data$Label,levels = c(0,1)))
  
  
  # Make predictions with the Logistic Regression model
  predictions_logistic <- predict(logistic_model, newdata = test_data, type="prob")
  predictions_logistic_class <- ifelse(predictions_logistic[,1] > 0.5, "0", "1") # Adjust class names as needed
  
  # Optionally, evaluate performance
  accuracy_lg <- confusionMatrix(factor(predictions_logistic_class,levels = c(0,1)),factor(test_data$Label,levels = c(0,1)))
  
  # Refit SVM model with the best hyperparameters on the entire training data
  final_svm_model <- svm(Label ~ ., data = train_data,
                         kernel = "radial",
                         C = svm_model$bestTune$C,
                         sigma = svm_model$bestTune$sigma,
                         scaled = TRUE,type="C-classification", probability = TRUE)  # Note: ksvm() from kernlab may offer more flexibility for SVMs
  
  # Making predictions with the final SVM model
  predictions_svm <- predict(final_svm_model, newdata = test_data, probability = TRUE)
  
  svm_probs <- attr(predictions_svm, "probabilities")
  pred_svm <- ifelse(svm_probs[,1] > 0.5, "0", "1") # Adjust class names as needed
  
  # Optionally, evaluate performance
  accuracy_svm <- confusionMatrix(factor(pred_svm,levels = c(0,1)),factor(test_data$Label,levels = c(0,1)))
  
  
  if(i==1){
    prob_out_rf=data.frame(Reference=test_data$Label,SampleID=train$SampleID[train_indices],predictions_rf)
    prob_out_svm=data.frame(Reference=test_data$Label,SampleID=train$SampleID[train_indices],svm_probs)
    prob_out_lg=data.frame(Reference=test_data$Label,SampleID=train$SampleID[train_indices],predictions_logistic)
    prob_out_gbm=data.frame(Reference=test_data$Label,SampleID=train$SampleID[train_indices],predictions_gbm)
  }else{
    prob_out_rf_i=data.frame(Reference=test_data$Label,SampleID=train$SampleID[train_indices],predictions_rf)
    prob_out_svm_i=data.frame(Reference=test_data$Label,SampleID=train$SampleID[train_indices],svm_probs)
    prob_out_lg_i=data.frame(Reference=test_data$Label,SampleID=train$SampleID[train_indices],predictions_logistic)
    prob_out_gbm_i=data.frame(Reference=test_data$Label,SampleID=train$SampleID[train_indices],predictions_gbm)
    
    prob_out_rf=rbind(prob_out_rf,prob_out_rf_i)
    prob_out_svm=rbind(prob_out_svm,prob_out_svm_i)
    prob_out_lg=rbind(prob_out_lg,prob_out_lg_i)
    prob_out_gbm=rbind(prob_out_gbm,prob_out_gbm_i)
  }
  
  
}




write.csv(prob_out_rf,"All_RF_Gastric_Train_Prob2_V8_2107.csv")
write.csv(prob_out_svm,"All_SVM_Gastric_Train_Prob2_V8_2107.csv")
write.csv(prob_out_lg,"All_LG_Gastric_Train_Prob2_V8_2107.csv")
write.csv(prob_out_gbm,"Allsummary(_GBM_Gastric_Train_Prob2_V8_2107.csv")







# Specify training and test data
train_data <- train[, -1]
test_data <- test[, colnames(train_data)]


train_data$Label=as.factor(train_data$Label)

# Specify the models to train
models <- list(
  gbm = list(method = "gbm", tuneLength = 3),
  svm = list(method = "svmLinear", tuneLength = 3),
  rf = list(method = "rf", tuneLength = 3),
  logistic = list(method = "glm", family = "binomial")
)

# Loop over each model
library(caret)
library(e1071)  # For SVM models
library(randomForest)  # Random forest models
library(gbm)  # GBM models

# Define the control and tuning grid for RF
train_control_rf <- trainControl(method = "cv", number = 10)
tuneGrid_rf <- expand.grid(mtry = c(2, 3, 4))

# Train the RF model
rf_model <- train(Label ~ ., data = train_data,
                  method = "rf",
                  trControl = train_control_rf,
                  tuneGrid = tuneGrid_rf)
# Define the control and tuning grid for GBM
train_control_gbm <- trainControl(method = "cv", number = 10)
tuneGrid_gbm <- expand.grid(interaction.depth = c(1, 3, 5),
                            n.trees = 100,
                            shrinkage = 0.1,
                            n.minobsinnode = 10)

# Train the GBM model
gbm_model <- train(Label ~ ., data = train_data,
                   method = "gbm",
                   trControl = train_control_gbm,
                   tuneGrid = tuneGrid_gbm)

# Train the Logistic Regression model
logistic_model <- train(Label ~ ., data = train_data,
                        method = "glm",
                        family = "binomial",
                        trControl = trainControl(method = "cv", number = 10))

# Define the control and tuning grid for SVM
# Assuming train_data is already defined
# Define train control
train_control_svm <- trainControl(method = "cv", number = 10)

# Define the tuning grid. Adjust the values based on your dataset and needs.
tuneGrid_svm <- expand.grid(C = 2^(-5:2), sigma = 2^(-15:3))

# Train the SVM model with RBF kernel
svm_model <- train(Label ~ ., data = train_data,
                   method = "svmRadial",
                   trControl = train_control_svm,
                   tuneGrid = tuneGrid_svm,
                   preProcess = "scale") # Scaling features can be important for SVM


# Refit RF model with the best hyperparameters on the entire training data
final_rf_model <- randomForest(Label ~ ., data = train_data,
                               mtry = rf_model$bestTune$mtry,
                               ntree = 500)  # You can specify ntree or any other RF parameters as needed


# Make predictions with the RF model
predictions_rf <- predict(final_rf_model, newdata = test_data,"prob")

pred_rf=ifelse(predictions_rf[,1]>0.5,0,1)

# Optionally, evaluate performance
accuracy_rf <- confusionMatrix(factor(pred_rf,levels = c(0,1)),factor(test_data$Label,levels = c(0,1)))



# Refit GBM model with the best hyperparameters on the entire training data
final_gbm_model <- gbm(Label ~ ., data = train_data,
                       distribution = "bernoulli",
                       n.trees = gbm_model$bestTune$n.trees,
                       interaction.depth = gbm_model$bestTune$interaction.depth,
                       shrinkage = gbm_model$bestTune$shrinkage,
                       n.minobsinnode = gbm_model$bestTune$n.minobsinnode)



# Make predictions with the GBM model
predictions_gbm <- predict(gbm_model, newdata = test_data,"prob")
pred_gbm=ifelse(predictions_gbm[,1]>0.5,0,1)

# Optionally, evaluate performance
accuracy_gbm <- confusionMatrix(factor(pred_gbm,levels = c(0,1)),factor(test_data$Label,levels = c(0,1)))


# Make predictions with the Logistic Regression model
predictions_logistic <- predict(logistic_model, newdata = test_data, type="prob")
predictions_logistic_class <- ifelse(predictions_logistic[,1] > 0.5, "0", "1") # Adjust class names as needed

# Optionally, evaluate performance
accuracy_lg <- confusionMatrix(factor(predictions_logistic_class,levels = c(0,1)),factor(test_data$Label,levels = c(0,1)))

# Refit SVM model with the best hyperparameters on the entire training data
final_svm_model <- svm(Label ~ ., data = train_data,
                       kernel = "radial",
                       C = svm_model$bestTune$C,
                       sigma = svm_model$bestTune$sigma,
                       scaled = TRUE,type="C-classification", probability = TRUE)  # Note: ksvm() from kernlab may offer more flexibility for SVMs

# Making predictions with the final SVM model
predictions_svm <- predict(final_svm_model, newdata = test_data, probability = TRUE)

svm_probs <- attr(predictions_svm, "probabilities")
pred_svm <- ifelse(svm_probs[,1] > 0.5, "0", "1") # Adjust class names as needed

# Optionally, evaluate performance
accuracy_svm <- confusionMatrix(factor(pred_svm,levels = c(0,1)),factor(test_data$Label,levels = c(0,1)))


prob_out_rf=data.frame(Reference=test_data$Label,SampleID=test$SampleID,predictions_rf)
prob_out_svm=data.frame(Reference=test_data$Label,SampleID=test$SampleID,svm_probs)
prob_out_lg=data.frame(Reference=test_data$Label,SampleID=test$SampleID,predictions_logistic)
prob_out_gbm=data.frame(Reference=test_data$Label,SampleID=test$SampleID,predictions_gbm)

write.csv(prob_out_rf,"All_RF_Gastric_Test_Prob_V8_2107.csv")
write.csv(prob_out_svm,"All_SVM_Gastric_Test_Prob_V8_2107.csv")
write.csv(prob_out_lg,"All_LG_Gastric_Test_Prob_V8_2107.csv")
write.csv(prob_out_gbm,"All_GBM_Gastric_Test_Prob_V8_2107.csv")
