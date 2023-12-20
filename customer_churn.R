df<- read.csv("~/3. Projects/project_SML/WA_Fn-UseC_-Telco-Customer-Churn.csv")
df <- df[,-1]

#Detecting and Removing Missing Values-----------------------------------------

#Identify rows with missing values.
rows_with_missing_values <- which(!complete.cases(df))

#Remove rows with missing values.
df <- df[-rows_with_missing_values, ]

#dataset without changing category names.
df_original<- df


# If you want to check for missing values in each column individually:
missing_columns <- colnames(df)[colSums(is.na(df)) > 0]

if (length(missing_columns) > 0) {
  cat("Columns with missing values:\n")
  cat(missing_columns, sep = ', ')
} else {
  cat("There are no columns with missing values.\n")
}

# Count the number of rows with tenure equal to 0
count_zero_tenure <- sum(df$tenure == 0)

# Print the result
cat("Number of rows with tenure equal to 0:", count_zero_tenure, "\n")


# Data Exploratory Analysis---- 

#Boxplot------------------------------------------------------------------------



# Numerical variables
numerical_vars <- c("tenure", "MonthlyCharges", "TotalCharges")

# Set wider margins
par(mar = c(4, 3, 2, 2) + 0.1)

# Create pairwise boxplots
par(mfrow = c(1, length(numerical_vars)))

for (var in numerical_vars) {
  boxplot(df[[var]] ~ df$Churn, 
          main = paste( var),
          xlab = "Churn Status",
          ylab = var,
          col = c("lightgreen", "lightcoral"),
          cex.main=1.5,
          cex.lab = 1.5)
}
#finding range------------------------------------------------------------------



# Find the range for each variable
range_tenure <- range(df$tenure)
range_MonthlyCharges <- range(df$MonthlyCharges)
range_TotalCharges <- range(df$TotalCharges)

# Print the ranges
cat("Range of 'tenure':", range_tenure, "\n")
cat("Range of 'MonthlyCharges':", range_MonthlyCharges, "\n")
cat("Range of 'TotalCharges':", range_TotalCharges, "\n")
 
 
#barplot 1----------------------------------------------------------------------

# Selected categorical predictors
selected_predictors <- c("gender", "SeniorCitizen", "Partner", "Dependents")

# Create a single layout for barplots
par(mfrow = c(2, 2),mar = c(2, 2, 2, 2))

# Create barplots for selected predictors
for (predictor in selected_predictors) {
  contingency_table <- table(df[[predictor]], df[[response_variable]])
  
# Convert to data frame for barplot
  bar_data <- as.data.frame(contingency_table)
  colnames(bar_data) <- c(predictor, "Churn", "Count")
  bar_data <- bar_data[order(bar_data[[predictor]]), ]
  
# Create barplot with larger y-axis limits and axis labels
  bp <- barplot(bar_data$Count, beside = TRUE, legend.text = TRUE,
                col = c("lightgreen", "lightcoral"),
                names.arg = rep("", nrow(bar_data)),  # Remove initial x-axis labels
                xlab = response_variable, ylab = "Count", ylim = c(0, max(bar_data$Count) * 1.2),
                main = paste(predictor, "by Churn Status"),
                axisnames = TRUE)
  
# Adding category labels at the center of each pair of bars
  for (i in 1:(length(bp)/2)) {
    midpoint <- mean(bp[(2 * i - 1):(2*i), ])
    text(x = midpoint, y = -240, labels = bar_data[[predictor]][2*i], pos = 1, xpd = TRUE, cex = 1.5)
  }
  
# Add counts on top of each bar with adjusted positioning
  text(bp, bar_data$Count, labels = bar_data$Count, pos = 3, offset = 0.5, cex = 1, col = "black")
  
}





#Barplot 2----------------------------------------------------------------------

# Selected categorical predictors
selected_predictors <- c("PhoneService", "MultipleLines", "InternetService", 
                         "OnlineSecurity", "OnlineBackup", "DeviceProtection")

# Create a single layout for barplots
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

# Create barplots for selected predictors
for (predictor in selected_predictors) {
  contingency_table <- table(df[[predictor]], df[[response_variable]])
  
  # Convert to data frame for barplot
  bar_data <- as.data.frame(contingency_table)
  colnames(bar_data) <- c(predictor, "Churn", "Count")
  bar_data <- bar_data[order(bar_data[[predictor]]), ]
  
  # Create barplot with larger y-axis limits and axis labels
  bp <- barplot(bar_data$Count, beside = TRUE, legend.text = TRUE,
                col = c("lightgreen", "lightcoral"),
                names.arg = rep("", nrow(bar_data)),  # Remove initial x-axis labels
                xlab = response_variable, ylab = "Count", ylim = c(0, max(bar_data$Count) * 1.2),
                main = paste(predictor, "by Churn Status"),
                axisnames = TRUE)
  
  # Adding category labels at the center of each pair of bars
  for (i in 1:(length(bp)/2)) {
    midpoint <- mean(bp[(2 * i - 1):(2*i), ])
    text(x = midpoint, y = -240, labels = bar_data[[predictor]][2*i], pos = 1, xpd = TRUE, cex = 1.5)
  }
  
  # Add counts on top of each bar with adjusted positioning
  text(bp, bar_data$Count, labels = bar_data$Count, pos = 3, offset = 0.5, cex = 1, col = "black")
  
}







#barplot 3----------------------------------------------------------------------



# Selected categorical predictors
selected_predictors <- c("TechSupport", "StreamingMovies", "StreamingTV", "Contract", "PaperlessBilling", "PaymentMethod")

# Create a single layout for barplots
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

# Create barplots for selected predictors
for (predictor in selected_predictors) {
  contingency_table <- table(df[[predictor]], df[[response_variable]])
  
# Convert to data frame for barplot
  bar_data <- as.data.frame(contingency_table)
  colnames(bar_data) <- c(predictor, "Churn", "Count")
  bar_data <- bar_data[order(bar_data[[predictor]]), ]
  
# Create barplot with larger y-axis limits and axis labels
  bp <- barplot(bar_data$Count, beside = TRUE, legend.text = TRUE,
                col = c("lightgreen", "lightcoral"),
                names.arg = rep("", nrow(bar_data)),  # Remove initial x-axis labels
                xlab = response_variable, ylab = "Count", ylim = c(0, max(bar_data$Count) * 1.2),
                main = paste(predictor, "by Churn Status"),
                axisnames = TRUE)
  
# Adding category labels at the center of each pair of bars
  for (i in 1:(length(bp)/2)) {
    midpoint <- mean(bp[(2 * i - 1):(2*i), ])
    text(x = midpoint, y = -240, labels = bar_data[[predictor]][2*i], pos = 1, xpd = TRUE, cex = 1.5)
  }
  
# Add counts on top of each bar with adjusted positioning
  text(bp, bar_data$Count, labels = bar_data$Count, pos = 3, offset = 0.5, cex = 1, col = "black")
  
}



#find all unique values of categorical variables.------------------------------

# Assuming df is your dataframe
factor_vars <- sapply(df, is.factor)

unique_values <- lapply(df[, factor_vars], levels)

# Print the unique values for each factor variable
for (i in seq_along(unique_values)) {
  cat("Variable:", names(unique_values)[i], "\n")
  print(unique_values[[i]])
  cat("\n")
}

#Abvriating the names of the category in categorical variables-----




library(dplyr)

# List of columns to exclude from conversion
exclude_columns <- c("tenure", "MonthlyCharges", "TotalCharges")

# Convert all character variables to factors, excluding specified columns
df <- df %>%
  mutate_at(vars(-exclude_columns), as.factor)

# convert np phone servies into nps
df <- df %>%
  mutate(MultipleLines = if_else(MultipleLines == "No phone service", "NPS", MultipleLines))
df <- df %>%
  mutate(
    InternetService = recode(InternetService, "Fiber optic" = "Fo"),
    OnlineSecurity = recode(OnlineSecurity, "No internet service" = "NIS"),
    OnlineBackup = recode(OnlineBackup, "No internet service" = "NIS"),
    DeviceProtection = recode(DeviceProtection, "No internet service" = "NIS")
  )

# for part 3


df <- df %>%
  mutate(
    Contract = recode(Contract, "Month-to-month" = "Mm", "One year" = "Oy", "Two year" = "Ty"),
    PaperlessBilling = recode(PaperlessBilling, "No" = "N", "Yes" = "Y"),
    TechSupport = recode(TechSupport, "No internet service" = "NIS"),
    StreamingTV = recode(StreamingTV, "No internet service" = "NIS"),
    StreamingMovies = recode(StreamingMovies, "No internet service" = "NIS"),
    PaymentMethod = recode(PaymentMethod, 
                           "Bank transfer (automatic)" = "Bk",
                           "Credit card (automatic)" = "Cc",
                           "Electronic check" = "Ec",
                           "Mailed check" = "Mck"))


#Co-relation between categorical Variables--------------------------------------


# Assuming the specified variables are columns in your dataset

# Create a list of categorical variables
categorical_vars <- c("gender", "SeniorCitizen", "Partner", "Dependents", 
                      "tenure", "PhoneService", "MultipleLines", 
                      "InternetService", "OnlineSecurity", "OnlineBackup", 
                      "DeviceProtection", "TechSupport", "StreamingTV", 
                      "StreamingMovies", "Contract", "PaperlessBilling")

# Create an empty matrix to store p-values
p_values <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars),
                   dimnames = list(categorical_vars, categorical_vars))

# Perform pairwise chi-squared tests
for (i in 1:(length(categorical_vars) - 1)) {
  for (j in (i + 1):length(categorical_vars)) {
    contingency_table <- table(df[[categorical_vars[i]]], df[[categorical_vars[j]]])
    chi_squared_test <- chisq.test(contingency_table)
    p_values[i, j] <- p_values[j, i] <- chi_squared_test$p.value
  }
}

# Apply Bonferroni correction
alpha <- 0.05
p_values_corrected <- p.adjust(as.vector(p_values), method = "bonferroni")
p_values_corrected_matrix <- matrix(p_values_corrected, nrow = length(categorical_vars),
                                    ncol = length(categorical_vars),
                                    dimnames = list(categorical_vars, categorical_vars))

# Display the results
print("Pairwise Chi-Squared Test Results with Bonferroni Correction:")
print(p_values_corrected_matrix)

# Assuming 'p_values_corrected_matrix' is the matrix of corrected p-values.

# Install and load necessary packages
install.packages("pheatmap")
library(pheatmap)

# Create a heatmap of the p-values
pheatmap(p_values_corrected_matrix,
         main = "Pairwise Chi-Squared Test P-Values",
         fontsize_row = 7, fontsize_col = 7,
         clustering_method = "complete",
         color = colorRampPalette(c("white", "blue"))(20))

#############   numerical variables


corrplot(cor_matrix, method = "number", type = "full", tl.col = "black", tl.srt = 45)



library(corrplot)
numerical_vars <- c("tenure", "MonthlyCharges", "TotalCharges")
corrplot(cor(numerical_vars),method="number",type="upper",tl.cex=0.7,number.cex=0.6)

#Splitting data into test and train-----------------------------------------------------------------
 
library(caret)

# Set seed for reproducibility
set.seed(123)

# Create an index for splitting the data
index <- createDataPartition(df[["Churn"]], p = 0.75, list = FALSE)

# Split the data
train_data <- df[index, ]
test_data <- df[-index, ]

#make count of the dimension of test and train.
dim(train_data)
dim(test_data)



#Feature selection--------------------------------------------------------------


library(randomForest)

response_variable <- "Churn"

# Create a formula for the classification model
formula <- as.formula(paste(response_variable, "~ ."))

# Fit a Random Forest model
rf_model <- randomForest(formula, data = train_data)

# Get feature importance scores
importance_scores <- importance(rf_model)

# Sort features by importance
sorted_features <- as.data.frame(importance_scores[order(-importance_scores[, "MeanDecreaseGini"]), , drop = FALSE])

# Create a bar plot of feature importance in rank order
ggplot(sorted_features, aes(x = reorder(rownames(sorted_features), -MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(title = "Feature Importance from Random Forest",
       x = "Features",
       y = "Mean Decrease Gini") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



###chi square for relevant variables.



categorical_variables <- c("gender","SeniorCitizen","Partner",   
                           "Dependents","PhoneService" ,"MultipleLines",
                           "InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport",     
                           "StreamingTV", "StreamingMovies" , "Contract","PaperlessBilling",)

# Initialize an empty data frame to store results
results_df <- data.frame(Variable = character(0), PValue = numeric(0), Significant = logical(0))

# Perform chi-square tests for each categorical variable
for (var in categorical_variables) {
  contingency_table <- table(df$Churn, df[[var]])
  chi_square_result <- chisq.test(contingency_table)
  
  # Store results in the data frame
  results_df <- rbind(results_df, data.frame(Variable = var,
                                             PValue = chi_square_result$p.value,
                                             Significant = chi_square_result$p.value < 0.05))
}

# Create a bar plot to visualize the results
ggplot(results_df, aes(x = reorder(Variable, -Significant), fill = Significant)) +
  geom_bar(stat = "count", color = "black") +
  scale_fill_manual(values = c("lightgray", "darkred")) +
  labs(title = "Significance of Categorical Variables",
       x = "Categorical Variables",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



#####significance for numeric variables.


numeric_variables <- c("tenure", "TotalCharges", "MonthlyCharges")

result_1 <- aov(df$MonthlyCharges ~ df$Churn)
result_2 <-aov(df$TotalCharges ~ df$Churn)
result_3 <- aov(df$tenure ~ df$Churn)


summary(result_1)
summary(result_2)
summary(result_3)



####### significance each categorical variable of chi-square tests for 

categorical_variables <- c("gender","SeniorCitizen","Partner",   
                           "Dependents","tenure" ,"PhoneService" ,"MultipleLines",
                           "InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport",     
                           "StreamingTV", "StreamingMovies" , "Contract","PaperlessBilling")

chi_square_results <- lapply(categorical_variables, function(var) {
  contingency_table <- table(df$Churn, df[[var]])
  chi_square_result <- chisq.test(contingency_table)
  return(data.frame(Variable = var,
                    ChiSquare = chi_square_result$statistic,
                    PValue = chi_square_result$p.value,
                    Relevant = chi_square_result$p.value < 0.05))
})

# Combine the results into a data frame
chi_square_results_df <- do.call(rbind, chi_square_results)

# Create a bar plot to visualize the results
ggplot(chi_square_results_df, aes(x = reorder(Variable, -ChiSquare), y = ChiSquare, fill = Relevant)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("lightgray", "darkred"), guide = FALSE) +
  labs(title = "Chi-Square Test for Categorical Variables",
       x = "Categorical Variables",
       y = "Chi-Square Statistic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))






#Model selection----------------------------------------------------------



# Load required libraries
library(caret)
library(randomForest)
library(xgboost)
library(e1071)
library(glmnet)

# Set seed for reproducibility
set.seed(123)

# Define the control parameters for 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Specify the formula excluding the variables to be dropped
formula1 <- Churn ~ .  -StreamingTV - OnlineBackup - DeviceProtection - gender - PhoneService

formula2<- Churn ~ . - gender - PhoneService

# Train logistic regression model using 5-fold cross-validation
logreg_model_corr_var_rm <- train(formula1, data = train_data, method = "glm", family = "binomial", trControl = ctrl)

# Gradient Boosting Model (XGBoost)
gbm_model <- train(formula2, data = train_data, method = "xgbTree", trControl = ctrl)

# Random Forest Model
rf_model <- train(formula2, data = train_data, method = "rf", trControl = ctrl)

# Naive Bayes Model
nb_model <- train(formula2, data = train_data, method = "naive_bayes", trControl = ctrl)

#decision tree
tree_model <- train(Churn ~ ., data = df, method = "rpart", trControl = ctrl)


# Extract accuracy from each model
tree_accuracy <- tree_model$results$Accuracy[1]
gbm_accuracy <- gbm_model$results$Accuracy[1]
rf_accuracy <- rf_model$results$Accuracy[1]
nb_accuracy <- nb_model$results$Accuracy[1]
logreg_model_corr_var_rm_accuracy <- logreg_model_corr_var_rm$results$Accuracy[1]


# Display accuracy for each model
cat("Decision Tree Accuracy:", tree_accuracy, "\n")
cat("Gradient Boosting Accuracy:", gbm_accuracy, "\n")
cat("Random Forest Accuracy:", rf_accuracy, "\n")
cat("Naive Bayes Accuracy:", nb_accuracy, "\n")
cat("Logistic Regression Accuracy:", logreg_model_corr_var_rm_accuracy, "\n")

library(ggplot2)

# Create a data frame with model names and corresponding accuracy
model_names <- c("Gradient Boosting", "Random Forest", "Naive Bayes", "Logistic Regression", "Decision Tree")
accuracies <- c(gbm_accuracy, rf_accuracy, nb_accuracy, logreg_model_corr_var_rm_accuracy, tree_accuracy)

accuracy_df <- data.frame(Model = model_names, Accuracy = accuracies)

# Plot the accuracy with dots
ggplot(accuracy_df, aes(x = Model, y = Accuracy, color = Model)) +
  geom_point(size = 7) +
  labs(title = "Model Accuracy Comparison",
       y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5,color = "black")




#Final model = Logistic Regression----------------------------------------------




# Make predictions on the test set
predictions <- predict(logreg_model_corr_var_rm, newdata = test_data, type = "prob")

# Convert predicted probabilities to binary predictions

binary_predictions <- ifelse(predictions[, "Yes"] > 0.5, "Yes", "No")
# Set levels for factor variables
levels(binary_predictions) <- levels(test_data$Churn)
# Create a confusion matrix
conf_matrix <- confusionMatrix(factor(binary_predictions), factor(test_data$Churn))


# find the accuracy
print(conf_matrix)


