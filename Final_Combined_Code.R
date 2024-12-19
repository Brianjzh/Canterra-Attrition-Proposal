# December 15, 2024
# Load Libraries-------------------------------------------------------------------------------
library(tidyverse) # tidyverse
library(ggplot2)
library(dplyr)
library(GGally) # for pairs plots with ggplot
library(caret)
library(rpart.plot)
library(mvrsquared)
library(readxl) # read excel files
library(openxlsx)
library(randomForest)
library(reshape2)
library(car)
library(broom)#convert outputs into tidy data frames
library(pROC) #Sampling-over and under, ROC and AUC curve
library(margins) # for marginal effects
library(ggcorrplot)#for visualizing correlation matrix
library(MASS)

### Reading and Cleaning Data ----------------------------------------------------------------
set.seed(111111)

data=read_excel("Employee_Data_Project.xlsx")
summary(data)

# check missing values
data |>
  summarize(
    across(everything(), function(x) sum(is.na(x)))
  )

data$Attrition <- as.factor(data$Attrition)
data$TotalWorkingYears <- as.numeric(data$TotalWorkingYears)
data$NumCompaniesWorked <- as.numeric(data$NumCompaniesWorked)
data$EnvironmentSatisfaction <- as.numeric(data$EnvironmentSatisfaction)
data$JobSatisfaction <- as.numeric(data$JobSatisfaction)

str(data)

na <- sum(data == "NA")
print(na)
data[data=="NA"] <- NA
data <- na.omit(data)

# Final check for missing values
apply(data, 2, anyNA)


### Creating a train/test split ------------------------------------------------
test_idx <- 
  createDataPartition(
    data$Attrition,
    p = 0.3,
    list = FALSE
  )

train <- data[-test_idx, ]

test <- data[test_idx, ]

table(train$Attrition)

###Pre-processing---------------------------------------------------------------

Attrition_idx <- c(
  which(train$Attrition == "Yes"),
  which(train$Attrition == "No") |>
    sample(468)
)

table(train$Attrition[Attrition_idx])

training_2<-train 

summary(train)


#### Heatmap w/ Numeric Data-----------------------------------------------------
# Step 1: Transform Attrition to numeric binary format
numeric_cols <- sapply(train, is.numeric)
train_numeric <- train[, numeric_cols]

# Compute the correlation matrix
cor_matrix <- cor(train_numeric, use = "complete.obs")

# Melt the correlation matrix 
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap
heatmap_plot <- ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap Including Attrition",
       x = "Variables",
       y = "Variables")

print(heatmap_plot)



####Individual Analysis------------------------------------------------------------
##### Attrition by Martial Status----
ggplot(train[Attrition_idx, ], aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar(position = "dodge") +  
  labs(
    title = "Attrition by Marital Status",
    x = "Marital Status",
    y = "Count",
    fill = "Attrition")+
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    position = position_dodge(width = 0.9),  
    vjust = -0.5  
  ) +
  theme_minimal(base_size = 14) 

##### Attrition by Income----
ggplot(data, aes(x = Attrition, y = Income, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Income Distribution by Attrition",
       x = "Attrition",
       y = "Income") +
  theme_minimal()


##### Attrition by Business Travel----
ggplot(train[Attrition_idx, ], aes(x = BusinessTravel, fill = Attrition)) +
  geom_bar(position = "dodge") +  
  labs(
    title = "Attrition by Business Travel",
    x = "Business Travel",
    y = "Count",
    fill = "Attrition")+
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5 
  ) +
  theme_minimal(base_size = 14)+ scale_fill_brewer(palette = "Set2")

##### Attrition by Years with Current Manager----
ggplot(train[Attrition_idx, ], aes(x = YearsWithCurrManager, fill = Attrition)) +
  geom_bar(position = "dodge") +  
  labs(
    title = "Attrition by Years with Current Manager",
    x = "Years with Current Manager",
    y = "Count",
    fill = "Attrition")+
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    position = position_dodge(width = 0.9),  
    vjust = -0.5  
  ) +
  theme_minimal(base_size = 14)+ scale_fill_brewer(palette = "Set3")

##### Attrition by Numbers of Companies Worked----
ggplot(train[Attrition_idx, ], aes(x = NumCompaniesWorked, fill = Attrition)) +
  geom_bar(position = "dodge") +  
  labs(
    title = "Attrition by Number of Companies Worked",
    x = "Number of Companies Worked",
    y = "Count",
    fill = "Attrition")+
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    position = position_dodge(width = 0.9),  
    vjust = -0.5  
  ) +
  theme_minimal(base_size = 14)+ scale_fill_brewer(palette = "Set1")

##### Attrition by Age----
ggplot(data, aes(x = Attrition, y = Age, fill = Attrition)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Attrition",
       x = "Attrition",
       y = "Age") +
  theme_minimal()



### Logistic Regression Model----------------------------------------------------------------

# Remove the outliers from the dataset
training_2_cleaned <- training_2 %>%
  mutate(
    Age_squared = Age^2,
    Income_squared = Income^2
  ) %>%
  filter(
    !(row_number() %in% c(552, 562, 593))
  ) %>%
  dplyr::select(
    -TotalWorkingYears,
    -StandardHours,
    -JobLevel,
    -DistanceFromHome,
    -Education,
    -EmployeeID,
    -Gender,
    -YearsAtCompany
  )

# Fit the first logistic regression model
f3_cleaned <- glm(
  Attrition ~ . + Age_squared + Income_squared,
  data = training_2_cleaned[Attrition_idx, ],
  family = binomial("logit")
)

# Display model summary
summary(f3_cleaned)

# Calculate Variance Inflation Factor (VIF)
vif_values <- vif(f3_cleaned)
print(vif_values)

f_roc3 <- tibble(
  actual = training_2_cleaned$Attrition,
  predicted = predict(f3_cleaned, training_2_cleaned, type = "response")
) |>
  roc("actual", "predicted")

plot(f_roc3)
f_roc3$auc
# ROC: 0.7275


# Compute marginal effects
marginal_effects_f3 <- margins(f3_cleaned)

# Summarize marginal effects
summary(marginal_effects_f3)

# visualize marginal effects
# plot 1----------
plot(marginal_effects_f3)

#plot 2 ---------
# Convert marginal effects summary to a data frame
marginal_effects_summary_f3 <- summary(marginal_effects_f3)

# Create a bar plot 
ggplot(marginal_effects_summary_f3, aes(x = reorder(factor, AME), y = AME, fill = AME > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Average Marginal Effects on Attrition Probability",
    x = "Predictors",
    y = "Marginal Effect (AME)"
  ) +
  scale_fill_manual(values = c("red", "blue"), guide = "none") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold") )

#plot 3---------
# confidence intervals to the marginal effects plot

ggplot(marginal_effects_summary_f3, aes(x = reorder(factor, AME), y = AME, color = AME > 0)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(
    title = "Marginal Effects with Confidence Intervals",
    x = "Predictors",
    y = "Marginal Effect (AME)"
  ) +
  scale_color_manual(values = c("red", "blue"), guide = "none") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold") )



## Decision Tree----------------------------------------------------------------------------
### Decision Tree Classification ----
# Train the model
f_income <- train(
  Attrition ~ .,
  data = training_2_cleaned[Attrition_idx, ] %>%
    drop_na(),
  method = "rpart",
  tuneGrid = expand.grid(cp = seq(0.001, 0.1, by = 0.01)),  # Tuning the complexity parameter (cp)
  trControl = trainControl(
    method = "cv", number = 10, # 10-fold cross-validation
    classProbs = TRUE,  # Enable probability predictions
    summaryFunction = twoClassSummary  # Use twoClassSummary to compute AUC
  ),
  metric = "ROC" # "ROC" gives us AUC & silences warning about Accuracy
)

# Display the results
print(f_income$results) # average across CV results for each tuning parameter

# Print the chosen cp
print(f_income$bestTune) # print chosen cp

# Cross-validation results for the chosen cp
print(f_income$resample) # cross validation results for the chosen cp

# Print the tree
rpart.plot(f_income$finalModel) # print the tree

# Variable importance
var_imp_income <- varImp(f_income) # variable importance

# Plot variable importance
plot(var_imp_income)



#______________________________________________________________________________________________________________________________________________________#

# Building a Decision Tree Classification Model --------------------------------------------------
f_attrition <- train(
  Attrition ~ .,
  data = train[Attrition_idx, ] |>
    dplyr::select(
      -EmployeeID
    ) |>
    drop_na(),
  method = "rpart",
  tuneGrid = expand.grid(cp = seq(0.001, 0.1, by = 0.01)),  # Tuning the complexity parameter (cp)
  trControl = trainControl(
    method = "cv", number = 10, # 10-fold cross validation
    classProbs = TRUE,  # Enable probability predictions
    summaryFunction = twoClassSummary  # Use twoClassSummary to compute AUC
  ),
  metric = "ROC" # "ROC" gives us AUC & silences warning about Accuracy
)

f_attrition$results # average across CV results for each tuning parameter

print(f_attrition$bestTune) # print chosen cp

f_attrition$resample # cross validation results for the chosen cp

rpart.plot(f_attrition$finalModel) # print the tree

var_imp_attrition <- varImp(f_attrition) # variable importance

plot(var_imp_attrition)

train[Attrition_idx, ] |> # look at the two most important variables by themselves
  ggplot(aes(y = Age |> log(), x = TotalWorkingYears, color = Attrition)) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~Attrition)

# Predictions and Conclusion for Decision Tree Classification Model ---------------------------------------------------
roc_attrition <- 
  roc(
    test$Attrition,
    predict(f_attrition, test, type = "prob")[["Yes"]]
  )

plot(roc_attrition)

roc_attrition$auc
# Conclusion from Decision Tree Model---------------------------------------------------------
# Top Values are Total Working Years, Age, Income  (Line 93) (THIS DOES NOT LOOK RIGHT WHY DOES IT KEEP ON CHANGING WTF FML) 
# The best model has a cp of 0.001
# The Models' ROC AUC scores range from 0.62 to 0.78  (Line 83)
# Average ROC AUC is 0.73, model does perform pretty good (Lines 100-109)
#_________________________________________________________________________________________________________________________________________________________#

### Bagged Tree Model-----------------------------------------------------------------
# build the model for missing income
bagged_model <- 
  train(
    Attrition ~ ., # formula
    data = train[Attrition_idx, ] |> # data
      dplyr::select(
        -EmployeeID
      ) |>
      drop_na(),
    method = "treebag",      # Bagged decision tree method
    trControl = trainControl(
      method = "cv", number = 10, # 10-fold cross validation
      classProbs = TRUE,  # Enable probability predictions
      summaryFunction = twoClassSummary  # Use twoClassSummary to compute AUC
    ),
    metric = "ROC" # "ROC" gives us AUC & silences warning about Accuracy
  ) 

bagged_model # summary

bagged_model$finalModel # number of trees

bagged_model$resample # cross validation results

vip <- varImp(bagged_model) # variable importance

vip

plot(vip)

# Fitting the model with fewer trees ---------------------------------------------------
bagged_model_10 <-
  train(
    Attrition ~ ., # formula
    data = train[Attrition_idx, ] |> # data
      dplyr::select(
        -EmployeeID
      ) |>
      drop_na(),
    method = "treebag",      # Bagged decision tree method
    nbagg = 10, # Adjust the number of trees to bag (default is 25)
    trControl = trainControl(
      method = "cv", number = 10, # 10-fold cross validation
      classProbs = TRUE,  # Enable probability predictions
      summaryFunction = twoClassSummary  # Use twoClassSummary to compute AUC
    ),
    metric = "ROC" # "ROC" gives us AUC & silences warning about Accuracy
  )   

bagged_model_10 # summary

bagged_model_10$finalModel # number of trees

bagged_model_10$resample # cross validation results

vip_10 <- varImp(bagged_model_10) # variable importance

vip_10

plot(vip_10)



# Comparing the two-------------------------------------------------------------
vip_for_plotting <- 
  rbind(
    tibble(
      var = rownames(vip$importance),
      vip$importance,
      num_trees = 25
    ),
    tibble(
      var = rownames(vip_10$importance),
      vip_10$importance,
      num_trees = 10
    ),
    tibble(
      var = rownames(var_imp_attrition$importance),
      var_imp_attrition$importance,
      num_trees = 1
    )
  )

vip_for_plotting |>
  ggplot(aes(y = var, x = Overall, fill = factor(num_trees))) +
  geom_bar(stat = "identity", position = "dodge")

# Comparing the Trees ---------------------------------------------------------
results_for_plotting <- 
  rbind(
    bagged_model$results[, -1],
    bagged_model_10$results[, -1],
    f_attrition$results[1, -1]
  ) |>
  mutate(
    num_trees = c(25, 10, 1)
  ) |>
  dplyr::select(
    ROC,
    Sens,
    Spec,
    num_trees
  ) |>
  pivot_longer(-num_trees) 

results_for_plotting |>
  ggplot(aes(x = value, y = name, fill = factor(num_trees))) +
  geom_bar(stat = "identity", position = "dodge") + 
  xlim(0, 1) + 
  ggtitle("Comparing Number of Bagged Trees")

# Predictions and Conclusions ----------------------------------------------------
test_preds <- predict(
  bagged_model, 
  test, 
  type = "prob"
) 

test_roc <- roc(
  test$Attrition,
  test_preds$Yes 
)

plot(test_roc)

test_roc$auc

test_preds_10 <- predict(
  bagged_model_10, 
  test, 
  type = "prob"
) 

test_roc_10 <- roc(
  test$Attrition,
  test_preds_10$Yes 
)

plot(test_roc_10)

test_roc_10$auc

test_preds_single <- predict(
  f_attrition, 
  test, 
  type = "prob"
) 

test_roc_single <- roc(
  test$Attrition,
  test_preds_single$Yes 
)

plot(test_roc_single)

test_roc_single$auc
# Conclusion--------------------------------------------------------------------
## Normal Bagged Tree 
# Average ROC AUC is 0.93 * SUPER GOOD YAY
# Sensitivity is between 0.77 and 0.87
# Specificity 0.85 to 0.95
# Most significant Variables are Age, Income, and TotalWorking Years (Lines 136 - 146 ^^^^)
## Bagged Tree Fewer Trees
# Average ROCK AUC 0.942 * better than before
# Sensitivity between 0.71 and 0.87
# Specificity between 0.84 and 0.95
# Most Significant Variables are Age, Income and Total Working Years again (Lines 167 - 177 ^^^^)
## Comparing the Number of bagged trees
# 25 Bags has highest everything and 1 bag has lowest everything
## Predictions and ROC Curve
# Bagged Model with 25 Tree has highest AUC 0.97
# Bagged Model with 10 Tree has 0.95
# Single Tree has 0.75
#_____________________________________________________________________________________________________________________________________________________________#

### Random Forest Method ---------------------------------------------------------
# Feature & Model Selection 
rf_model <- 
  train(
    Attrition ~ ., # formula
    data = train[Attrition_idx, ] |> # data
      dplyr::select(
        -EmployeeID
      ) |>
      drop_na(),
    method = "rf",      # Random Forest method                    # will fit 500 trees
    trControl = trainControl(
      method = "cv", number = 10, # 10-fold cross-validation
      classProbs = TRUE,  # Enable probability predictions
      summaryFunction = twoClassSummary  # Use twoClassSummary to compute AUC
    ),
    metric = "ROC" # "ROC" gives us AUC & silences warning about Accuracy
  )

rf_model # summary

rf_model$finalModel # details of the Random Forest model

rf_model$resample # cross-validation results

vip <- varImp(rf_model) # variable importance

vip

plot(vip)

# plot error based on number of trees
# can use it to assess number of trees we actually need (to speed up training time)
plot(rf_model$finalModel)

# Tuning ----------------------------------------------------------------------------------------
# Tune Random Forest: Adjust the number of trees and try different mtry values
rf_model_tuned <- 
  train(
    Attrition ~ ., # formula
    data = train[Attrition_idx, ] |> # data
      dplyr::select(
        -EmployeeID
      ) |>
      drop_na(),
    method = "rf",      # Random Forest method
    tuneGrid = expand.grid(mtry = c(2, 3, 4)), # Tune mtry 
    ntree = 100, # manually change the number of trees
    trControl = trainControl(
      method = "cv", number = 10, # 10-fold cross-validation
      classProbs = TRUE,  # Enable probability predictions
      summaryFunction = twoClassSummary  # Use twoClassSummary to compute AUC
    ),
    metric = "ROC" # "ROC" gives us AUC & silences warning about Accuracy
  )

plot(rf_model_tuned$finalModel)

rf_model_tuned # summary

rf_model_tuned$finalModel # details of the tuned Random Forest model

rf_model_tuned$resample # cross-validation results                        # OOB = more honmest than training set error less than K-Fold

vip_tuned <- varImp(rf_model_tuned) # variable importance

vip_tuned

plot(vip_tuned)

# Comparing the two----------------------------------------------------------------
vip_for_plotting <- 
  rbind(
    tibble(
      var = rownames(vip$importance),
      vip$importance,
      model = "Default RF"
    ),
    tibble(
      var = rownames(vip_tuned$importance),
      vip_tuned$importance,
      model = "Tuned RF"
    )
  )

vip_for_plotting |>
  ggplot(aes(y = var, x = Overall, fill = model)) +
  geom_bar(stat = "identity", position = "dodge")

results_for_plotting <- 
  rbind(
    rf_model$results |> mutate(model = "Default RF"),
    rf_model_tuned$results |> mutate(model = "Tuned RF")
  ) |>
  dplyr::select(
    ROC, Sens, Spec, model
  ) |>
  pivot_longer(-model)

results_for_plotting |>
  ggplot(aes(x = value, y = name, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") + 
  xlim(0, 1) + 
  ggtitle("Comparing Default and Tuned Random Forest")

# Predictions and Conclusions ------------------------------------------------------
test_preds <- predict(
  rf_model, 
  test, 
  type = "prob"
)

test_roc <- roc(
  test$Attrition,
  test_preds$Yes 
)

plot(test_roc)

test_roc$auc

test_preds_tuned <- predict(
  rf_model_tuned, 
  test, 
  type = "prob"
)

test_roc_tuned <- roc(
  test$Attrition,
  test_preds_tuned$Yes 
)

plot(test_roc_tuned)

test_roc_tuned$auc

# Conclusion--------------------------------------------------------------------
## Random Forest Method (Lines 310 - 324)
# Best MTRY is 2
# Average ROC is 0.95
# Sens: 0.75 - 0.9
# Specificity: 0.878-0.98
# OOB: 8.49% (Low)
# Confusion Matrix: NO Error: 12.44%, YES Error is 9.04% (better for predicting leaving)
# Cross Validations: ROC 0.95 - 0.99, Fold Sens: 0.898 Specificity: 0.98
# Important Var: Age, Income, Total Working Years

## Tuned Version
# MTRY is 4
# Average ROCK is between 0.937 - 0.971
# Sen: 0.799 - 0.891
# Spec: 0.877 to 0.959
# OOB: 9.71%
# Confusion Matrix: NO Error: 14.92% YES Error is 4.5%
# Important Var: Income, Total Working, Age

# Comparing the Two (Line 390)
# Tuned RF Seems to be slightly better, especially in Sens

# Predictions
# Test_Roc: 0.9733
# Tunned: 0.9723