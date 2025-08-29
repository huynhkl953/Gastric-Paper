
train_EM=read_csv("MOTIF_END_SVM_Gastric_Train_Prob2_V8_0909.csv")
train_CNA=read_csv("CNA_RF_Gastric_Train_Prob2_V8_0909.csv")
train_TMD=read_csv("TMD_RF_Gastric_Train_Prob2_V8_0909.csv")
train_GWMD=read_csv("GWMD_RF_Gastric_Train_Prob2_V8_0909.csv")
train_Flen=read_csv("Flen_SVM_Gastric_Train_Prob2_V8_0909.csv")
train_All=read_csv("All_GBM_Gastric_Train_Prob2_V8_0909.csv")

library(ggplot2)
library(pROC)

# Define a function to calculate ROC, AUC, and its 95% CI
calculate_roc_ci <- function(data, marker) {
  roc_obj <- roc(data$Reference, data$X1)  # Assuming X1 is the predicted probability for the positive class
  auc_val <- auc(roc_obj)
  ci_val <- ci.auc(roc_obj)
  
  roc_df <- data.frame(
    Sensitivity = rev(roc_obj$sensitivities), 
    Specificity = rev(1 - roc_obj$specificities), 
    Marker = sprintf("%s (AUC = %.2f, 95%% CI = %.2f - %.2f)", marker, auc_val, ci_val[1], ci_val[3])
  )
  
  return(roc_df)
}

# Calculate ROC and 95% CI for each dataset
roc_EM <- calculate_roc_ci(train_EM, "EM")
roc_CNA <- calculate_roc_ci(train_CNA, "CNA")
roc_TMD <- calculate_roc_ci(train_TMD, "TMD")
roc_GWMD <- calculate_roc_ci(train_GWMD, "GWMD")
roc_Flen <- calculate_roc_ci(train_Flen, "Flen")
roc_All <- calculate_roc_ci(train_All, "All")

# Combine all ROC data
roc_data <- rbind(roc_EM, roc_CNA, roc_TMD, roc_GWMD, roc_Flen, roc_All)

# Plot the ROC curves with AUC and 95% CI in the legend
ggplot(roc_data, aes(x = Specificity, y = Sensitivity, color = Marker)) +
  geom_line(size = 1) +
  labs(title = "",
       x = "1 - Specificity",
       y = "Sensitivity") +
  theme_classic(base_size = 15) +
  scale_color_manual(values = c("EM (AUC = 0.85, 95% CI = 0.80 - 0.90)" = "blue", 
                                "CNA (AUC = 0.82, 95% CI = 0.76 - 0.87)" = "red", 
                                "TMD (AUC = 0.77, 95% CI = 0.71 - 0.83)" = "green", 
                                "GWMD (AUC = 0.85, 95% CI = 0.80 - 0.90)" = "purple", 
                                "Flen (AUC = 0.79, 95% CI = 0.73 - 0.85)" = "orange", 
                                "All (AUC = 0.90, 95% CI = 0.86 - 0.94)" = "brown")) +
  theme(legend.position = "right")


test_EM=read_csv("MOTIF_END_SVM_Gastric_Test_Prob_V8_0909.csv")
test_CNA=read_csv("CNA_RF_Gastric_Test_Prob_V8_0909.csv")
test_TMD=read_csv("TMD_RF_Gastric_Test_Prob_V8_0909.csv")
test_GWMD=read_csv("GWMD_RF_Gastric_Test_Prob_V8_0909.csv")
test_Flen=read_csv("Flen_SVM_Gastric_Test_Prob_V8_0909.csv")
test_All=read_csv("All_GBM_Gastric_Test_Prob_V8_0909.csv")



# Define a function to calculate ROC and return a data frame for ggplot
calculate_roc <- function(data, marker, pred_col = "X1") {
  roc_obj <- roc(data$Reference, data[[pred_col]]) # Adjust pred_col if necessary
  roc_df <- data.frame(
    Sensitivity = rev(roc_obj$sensitivities), 
    Specificity = rev(1 - roc_obj$specificities), 
    Marker = marker
  )
  return(roc_df)
}

# Calculate ROC for each test dataset, specify the column with predictions
roc_EM <- calculate_roc(test_EM, "EM", "X1")
roc_CNA <- calculate_roc(test_CNA, "CNA", "X1")
roc_TMD <- calculate_roc(test_TMD, "TMD", "X1")
roc_GWMD <- calculate_roc(test_GWMD, "GWMD", "X1")
roc_All <- calculate_roc(test_All, "All", "predictions_xgb")
roc_Flen <- calculate_roc(test_Flen, "Flen", "X1") # Adjusted for different column name

# Combine all ROC data
roc_data <- rbind(roc_EM, roc_CNA, roc_TMD, roc_GWMD, roc_Flen, roc_All)

# Plot the ROC curves
ggplot(roc_data, aes(x = Specificity, y = Sensitivity, color = Marker)) +
  geom_line(size = 1) +
  labs(title = "",
       x = "1 - Specificity",
       y = "Sensitivity") +
  theme_classic(base_size = 15) +
  scale_color_manual(values = c("EM" = "blue", "CNA" = "red", "TMD" = "green", 
                                "GWMD" = "purple", "Flen" = "orange", "All" = "brown")) +
  theme(legend.position = "right")







train_EM=read_csv("MOTIF_END_SVM_Gastric_Train_Prob2_V8_0909.csv")
train_CNA=read_csv("CNA_RF_Gastric_Train_Prob2_V8_0909.csv")
train_TMD=read_csv("TMD_RF_Gastric_Train_Prob2_V8_0909.csv")
train_GWMD=read_csv("GWMD_RF_Gastric_Train_Prob2_V8_0909.csv")
train_Flen=read_csv("Flen_SVM_Gastric_Train_Prob2_V8_0909.csv")
train_All=read_csv("All_GBM_Gastric_Train_Prob2_V8_0909.csv")


test_EM=read_csv("MOTIF_END_SVM_Gastric_Test_Prob_V8_0909.csv")
test_CNA=read_csv("CNA_RF_Gastric_Test_Prob_V8_0909.csv")
test_TMD=read_csv("TMD_RF_Gastric_Test_Prob_V8_0909.csv")
test_GWMD=read_csv("GWMD_RF_Gastric_Test_Prob_V8_0909.csv")
test_Flen=read_csv("Flen_SVM_Gastric_Test_Prob_V8_0909.csv")
test_All=read_csv("All_GBM_Gastric_Test_Prob_V8_0909.csv")



library(pROC)

# Function to find the cutoff that achieves 0.95 specificity in training data
calculate_cutoff_and_metrics <- function(train_data, test_data, marker) {
  # Convert Reference columns to factors (ensuring control = 0 and case = 1)
  train_data$Reference <- factor(train_data$Reference, levels = c(0, 1))
  test_data$Reference <- factor(test_data$Reference, levels = c(0, 1))
  
  # ROC for training data
  roc_train <- roc(train_data$Reference, train_data$X0)  # Assuming X1 is the predicted probability for the positive class
  
  # Find the threshold corresponding to 95% specificity in training data
  cutoff_index <- which.max(round(roc_train$specificities,2) >= 0.95)  # Get the first threshold with specificity >= 0.95
  cutoff <- roc_train$thresholds[cutoff_index]
  
  # Get the sensitivity at this cutoff in training data
  train_sens <- roc_train$sensitivities[cutoff_index]
  train_spec <- roc_train$specificities[cutoff_index]
  
  # Apply the cutoff to the test data
  roc_test <- roc(test_data$Reference, test_data$X0)
  test_sens <- sensitivity(reference=test_data$Reference, data=as.factor(ifelse(test_data$X0 >= cutoff,0,1)), positive = "1")
  test_spec <- specificity(reference=test_data$Reference, data=as.factor(ifelse(test_data$X0 >= cutoff,0,1)), negative = "0")
  
  # Calculate 95% CI for sensitivity in the training data
  ci_sens_train <- ci.se(roc_train, specificities = 0.95)
  
  # Output all the results
  result <- list(
    cutoff = cutoff,
    train_sens = train_sens,
    train_spec = train_spec,
    test_sens = test_sens,
    test_spec = test_spec,
    ci_train_sens = ci_sens_train
  )
  
  return(result)
}


calculate_cutoff_and_metrics_GBM <- function(train_data, test_data, marker) {
  # Convert Reference columns to factors (ensuring control = 0 and case = 1)
  train_data$Reference <- factor(train_data$Reference, levels = c(0, 1))
  test_data$Reference <- factor(test_data$Reference, levels = c(0, 1))
  
  # ROC for training data
  roc_train <- roc(response=train_data$Reference, predictor=train_data$X1)  # Assuming X1 is the predicted probability for the positive class
  
  # Find the threshold corresponding to 95% specificity in training data
  cutoff_index <- which.max(round(roc_train$specificities,2) > 0.95)  # Get the first threshold with specificity >= 0.95
  cutoff <- roc_train$thresholds[cutoff_index]
  
  # Get the sensitivity at this cutoff in training data
  train_sens <- roc_train$sensitivities[cutoff_index]
  train_spec <- roc_train$specificities[cutoff_index]
  
  # Apply the cutoff to the test data
  roc_test <- roc(response=test_data$Reference, predictor=test_data$predictions_xgb)
  test_sens <- sensitivity(reference=test_data$Reference, data=as.factor(ifelse(test_data$predictions_xgb >= 1-cutoff,1,0)), positive = "1")
  test_spec <- specificity(reference=test_data$Reference, data=as.factor(ifelse(test_data$predictions_xgb >= 1-cutoff,1,0)), negative = "0")
  
  # Calculate 95% CI for sensitivity in the training data
  ci_sens_train <- ci.se(roc_train, specificities = 0.95)
  
  # Output all the results
  result <- list(
    cutoff = cutoff,
    train_sens = train_sens,
    train_spec = train_spec,
    test_sens = test_sens,
    test_spec = test_spec,
    ci_train_sens = ci_sens_train
  )
  
  return(result)
}
# Load your train and test data as mentioned
# Assuming you have already loaded train_EM, test_EM, etc.

# Calculate metrics for each dataset
results_EM <- calculate_cutoff_and_metrics(train_data = train_EM, test_data = test_EM,marker =  "EM")
results_CNA <- calculate_cutoff_and_metrics(train_CNA, test_CNA, "CNA")
results_TMD <- calculate_cutoff_and_metrics(train_TMD, test_TMD, "TMD")
results_GWMD <- calculate_cutoff_and_metrics(train_GWMD, test_GWMD, "GWMD")
results_Flen <- calculate_cutoff_and_metrics(train_Flen, test_Flen, "Flen")
results_All <- calculate_cutoff_and_metrics_GBM(train_data =train_All,test_data =  test_All, marker ="All")

# Print or save the results
results_EM
results_CNA
results_TMD
results_GWMD
results_Flen
results_All




library(ggplot2)

# Prepare the data for plotting
plot_data <- data.frame(
  Dataset = rep(c("EM", "CNA", "TMD", "GWMD", "Flen", "All"), each = 4),
  Type = rep(c("Train", "Train", "Test", "Test"), times = 6),
  Metric = rep(c("Sensitivity", "Specificity"), times = 12),
  Value = c(
    results_EM$train_sens, results_EM$train_spec, results_EM$test_sens, results_EM$test_spec,
    results_CNA$train_sens, results_CNA$train_spec, results_CNA$test_sens, results_CNA$test_spec,
    results_TMD$train_sens, results_TMD$train_spec, results_TMD$test_sens, results_TMD$test_spec,
    results_GWMD$train_sens, results_GWMD$train_spec, results_GWMD$test_sens, results_GWMD$test_spec,
    results_Flen$train_sens, results_Flen$train_spec, results_Flen$test_sens, results_Flen$test_spec,
    results_All$train_sens, results_All$train_spec, results_All$test_sens, results_All$test_spec
  ),
  Lower_CI = c(
    results_EM$ci_train_sens[1], NA, NA, NA,
    results_CNA$ci_train_sens[1], NA, NA, NA,
    results_TMD$ci_train_sens[1], NA, NA, NA,
    results_GWMD$ci_train_sens[1], NA, NA, NA,
    results_Flen$ci_train_sens[1], NA, NA, NA,
    results_All$ci_train_sens[1], NA, NA, NA
  ),
  Upper_CI = c(
    results_EM$ci_train_sens[3], NA, NA, NA,
    results_CNA$ci_train_sens[3], NA, NA, NA,
    results_TMD$ci_train_sens[3], NA, NA, NA,
    results_GWMD$ci_train_sens[3], NA, NA, NA,
    results_Flen$ci_train_sens[3], NA, NA, NA,
    results_All$ci_train_sens[3], NA, NA, NA
  )
)

plot_data$Dataset=factor(plot_data$Dataset,levels = c("EM","CNA","Flen","GWMD","TMD","All"))
plot_data$Type=factor(plot_data$Type,levels = c("Train","Test"))
plot_data$Metric=factor(plot_data$Metric,levels = c("Sensitivity","Specificity"))

# Create the bar plot with error bars for 95% CI of sensitivity in the training set
ggplot(plot_data, aes(x = Dataset, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                position = position_dodge(0.7), width = 0.25, color = "black") +
  facet_wrap(~ Type, nrow = 1) +
  labs(title = "",
       y = "Value",
       x = "") +
  theme_classic(base_size = 20) +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Sensitivity" = "blue", "Specificity" = "red")) +
  geom_text(aes(label = round(Value, 2)), position = position_dodge(0.7), vjust = -0.5, size = 4)+ylim(0,1.1)





K_GBM=read_csv("All_GBM_Gastric_K_Prob_V8_0909.csv")
K2_GBM=read_csv("All_GBM_Gastric_K4_Prob_V8_0909.csv")
K3_GBM=read_csv("All_GBM_Gastric_K3_Prob_V8_0909.csv")

data_K=data.frame(pred_score=c(K_GBM$predictions_xgb[c(1,63)],K3_GBM$predictions_xgb),ref=c(ifelse(K_GBM$Reference%in%c("Gastric cancer","Gastric cancer (poorly differentiated signet ring cell adenocarcinoma ) with lymph node metastasis"),1,0)[c(1,63)],rep(0,nrow(K3_GBM))))

head(data_K)
pred=ifelse(data_K$pred_score> 1-0.7809258,0,1)

K3_GBM=read_csv("All_GBM_Gastric_K3_Prob_V8_0909.csv")
pred=ifelse(K3_GBM$predictions_xgb> 1-0.8009258,0,1)
table(pred)


confusionMatrix(factor(pred,levels = c(0,1)),factor(data_K$ref,levels = c(0,1)),positive = "1")

library(ggplot2)

# Confusion matrix values (True Negative, False Positive, False Negative, True Positive)
tn <- 49  # True Negative
fp <- 4  # False Positive
fn <- 0     # False Negative
tp <- 2     # True Positive

# Sensitivity and Specificity calculations
sensitivity <- tp / (tp + fn)  # Sensitivity
specificity <- tn / (tn + fp)  # Specificity

# Create a data frame with confusion matrix values and corresponding sensitivity/specificity
conf_matrix <- data.frame(
  Reference = factor(c("Healthy", "Healthy", "Gastric cancer", "Gastric cancer"), levels = c("Gastric cancer", "Healthy")),
  Prediction = factor(c("Healthy", "Gastric cancer", "Healthy", "Gastric cancer"), levels = c("Gastric cancer", "Healthy")),
  Count = c(tn, fp, fn, tp),
  Metric = c(specificity, NA, NA, sensitivity),  # Only color for Sensitivity and Specificity
  Label = c(
    paste0("TN = ", tn, "\nSpec = ", round(specificity, 2)),
    paste0("FP = ", fp),
    paste0("FN = ", fn),
    paste0("TP = ", tp, "\nSens = ", round(sensitivity, 2))
  )
)

# Plot the heatmap
ggplot(conf_matrix, aes(x = Reference, y = Prediction, fill = Metric)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), size = 5) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0, 1), na.value = "grey90", name = "Value") +
  labs(title = "",
       x = "Reference",
       y = "Prediction") +
  theme_classic(base_size = 15)




train_GBM=read_csv("All_GBM_Gastric_Train_Prob2_V8_0909.csv")
test_GBM=read_csv("All_GBM_Gastric_Test_Prob_V8_0909.csv")


data=data.frame(SampleID=c(train_GBM$SampleID,test_GBM$SampleID),Group=c(rep("train",nrow(train_GBM)),rep("test",nrow(test_GBM))),
                Score=c(train_GBM$X1,test_GBM$predictions_xgb))
GASTRIC_NEW_METADATA_adding_control_22_05_2_ <- read_excel("GASTRIC NEW METADATA adding control 22.05 (2).xlsx")

data_final=merge(GASTRIC_NEW_METADATA_adding_control_22_05_2_,data,"SampleID")

library(ggplot2)

data_final_sub=data_final[which(data_final$Gender!="NA"),]

# Plot the boxplot
# Plot the boxplot with jitter (dots) and custom colors
# If you don't have the ggpubr package, install it
# install.packages("ggpubr")

library(ggplot2)
library(ggpubr)

# Plot with statistical comparison
ggplot(data_final_sub, aes(x = Classification, y = Score, fill = Classification)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5, color = "black") +  # Adding jittered dots
  facet_wrap(~ Gender) +
  labs(title = "",
       x = "",
       y = "Score") +
  scale_fill_manual(values = c("Control" = "blue", "Gastric cancer" = "red")) +  # Custom colors for classifications
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +ylim(0,1.1)  # Adding statistical comparison


data_final_sub=data_final[which(data_final$Age!="NA"),]
data_final_sub$Group_Age=ifelse(as.numeric(data_final_sub$Age)>59,"Age > 59", "Age < 59")
ggplot(data_final_sub, aes(x = Classification, y = Score, fill = Classification)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5, color = "black") +  # Adding jittered dots
  facet_wrap(~ Group_Age) +
  labs(title = "",
       x = "",
       y = "Score") +
  scale_fill_manual(values = c("Control" = "blue", "Gastric cancer" = "red")) +  # Custom colors for classifications
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +ylim(0,1.1)  # Adding statistical comparison


DATA_GC <- read_excel("DATA_GC.xlsx")



library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

# Create groups for Tumor stages
DATA_GC <- DATA_GC %>%
  mutate(
    TumorGroup = case_when(
      `Tumor stage` %in% c("0", "IA", "IB", "IIA","IIB") ~ "Stage 0-2",
      `Tumor stage` %in% c("IIIA") ~ "Stage 3",
      `Tumor stage` %in% c("NA") ~ "Unknown",
      TRUE ~ "All Cancer"
    )
  )

# Calculate accuracy and n for each group
accuracy_data <- DATA_GC %>%
  group_by(TumorGroup) %>%
  summarise(
    Accuracy_SPOTMAS = mean(Label == SPOTMAS),
    Accuracy_HOTSPOT = mean(Label == HOTSPOT),
    Accuracy_SPOTMAS_HOTSPOT = mean(Label == (`SCORE+HOTSPOT`)),
    n = n()  # Count of samples in each group
  ) %>%
  # Combine all cancer data into a group
  bind_rows(
    tibble(
      TumorGroup = "All Cancer",
      Accuracy_SPOTMAS = mean(DATA_GC$Label == DATA_GC$SPOTMAS),
      Accuracy_HOTSPOT = mean(DATA_GC$Label == DATA_GC$HOTSPOT),
      Accuracy_SPOTMAS_HOTSPOT = mean(DATA_GC$Label == DATA_GC$`SCORE+HOTSPOT`),
      n = nrow(DATA_GC)
    )
  ) %>%
  # Gather the accuracy columns into a format suitable for plotting
  tidyr::gather(key = "Comparison", value = "Accuracy", Accuracy_SPOTMAS, Accuracy_HOTSPOT, Accuracy_SPOTMAS_HOTSPOT)

accuracy_data$

# Plot the barplot with n for each group
ggplot(accuracy_data, aes(x = TumorGroup, y = Accuracy, fill = Comparison)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Accuracy,2)), position = position_dodge(0.7), vjust = -0.5, size = 4) +
  labs(title = "Accuracy of Label vs SPOTMAS, HOTSPOT, and SPOTMAS + HOTSPOT",
       x = "Tumor Group",
       y = "Accuracy") +
  scale_fill_manual(values = c("Accuracy_SPOTMAS" = "blue", 
                               "Accuracy_HOTSPOT" = "red", 
                               "Accuracy_SPOTMAS_HOTSPOT" = "green")) +
  theme_classic(base_size = 15) +
  theme(legend.title = element_blank())


# Modify the Comparison and TumorGroup labels
accuracy_data <- accuracy_data %>%
  mutate(
    Comparison = case_when(
      Comparison == "Accuracy_SPOTMAS" ~ "SPOTMAS",
      Comparison == "Accuracy_HOTSPOT" ~ "HOTSPOT",
      Comparison == "Accuracy_SPOTMAS_HOTSPOT" ~ "SPOTMAS+HOTSPOT",
      TRUE ~ Comparison
    ),
    TumorGroup = paste0(TumorGroup, " (n=", n, ")")
  )

accuracy_data$TumorGroup=factor(accuracy_data$TumorGroup,levels = c("Stage 0-2 (n=15)","Stage 3 (n=16)","Unknown (n=28)","All Cancer (n=59)"))

# Plot the barplot with updated labels
ggplot(accuracy_data, aes(x = TumorGroup, y = Accuracy, fill = Comparison)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  labs(title = "",
       x = "",
       y = "Accuracy") +
  scale_fill_manual(values = c("SPOTMAS" = "blue", 
                               "HOTSPOT" = "red", 
                               "SPOTMAS+HOTSPOT" = "green")) +
  theme_classic(base_size = 20) +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = round(Accuracy,2)), position = position_dodge(0.7), vjust = -0.5, size = 6) +ylim(0,1.1) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) 


