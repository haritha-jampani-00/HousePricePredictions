library(gains)
library(pROC)
library(gplots)
library(readr)
library(GGally)
library(car)
library(lubridate)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(corrplot)
library(forecast)
options(scipen = 999)

#Reading the CSV file into R
housep <- read.csv("House_Price.csv", header = TRUE, stringsAsFactors = TRUE)
dim(housep)
head(housep)
View(housep)

# No missing Data and checking data types 
sum(is.na(housep))
summary(housep)
str(housep)

# Changing formats for some of the variables and calculating the difference between current year and the year that the house was built
#Making that calculation a new column called Buidling_age
housep$Sale_date <- as.Date(housep$Sale_date, format = "%m/%d/%Y")
current_year <- 2023
housep$Building_age <- current_year - housep$Build_year
housep$Type <- as.factor(housep$Type)
housep$Town <- as.factor(housep$Town)
housep$University <- as.factor(housep$University)
housep$Sale_month <- month(housep$Sale_date)

#Creating Dummy Variables and taking Multiple Occupancy as reference column 
table(housep$Type)
housep$SingleFamily <- ifelse(housep$Type == "Single Family", 1, 0)
housep$MultiFamily <- ifelse(housep$Type == "Multi Family", 1, 0)

# Convert 'Town' to dummy variables using model.matrix
table(housep$Town)
town_dummies <- model.matrix(~ Town - 1, data = housep)

# Bind the dummy variables with the original data (excluding the original 'Town' variable) to create a new data set called housep_model
housep_model <- cbind(housep[, !names(housep) %in% c("Town")], town_dummies)
#Getting values year and month values from Sale_date variable since the dataset only contains sales within a year period 
#By doing so will also improve the model 
housep_model$Sale_year <- year(housep$Sale_date)
housep_model <- housep_model %>% select(-University,-Sale_date,-Sale_year,-Record,-SingleFamily,-MultiFamily, -Building_age )
View(housep_model)

# Filtering out non-numeric columns
num_data <- select_if(housep, is.numeric)
num_data <- num_data[, !names(num_data) %in% c("Sale_amount","Record")]

# Assuming 'Sale_amount' is the response variable and is numeric
# Filter only numeric predictors
numeric_pre <- Filter(is.numeric, housep)
numeric_pre <- numeric_pre[, !names(numeric_pre) %in% c("Sale_amount","Record")]

# Calculate correlation matrix
cor_matrix <- cor(numeric_pre)
cor_matrix
corrplot(cor_matrix, method = "color",order = "hclust", tl.col = "black", tl.srt = 45)

# Creating box plots for each numeric variable
for (var in names(num_data)) {
  p <- ggplot(housep, aes_string(y = var)) + geom_boxplot() + labs(title = paste("Box Plot of", var), y = var, x = "")
  print(p)
}

# Function to remove outliers from a numerical variables
outliers_count <- function(data, column) {
  mean_value <- mean(data[[column]], na.rm = TRUE)
  stdev <- sd(data[[column]], na.rm = TRUE)
  lower_limit <- mean_value - 3 * stdev
  upper_limit <- mean_value + 3 * stdev
  data <- data[data[[column]] >= lower_limit & data[[column]] <= upper_limit, ]
  return(data)
}

# Apply the function to each numeric column and make it as a new dataset called cleaned_data where we removed the outliers that are 
#outside of 3Sds 
num_col <- sapply(housep, is.numeric)
cleaned_data <- housep

for (column in names(housep)[num_col]) {
  cleaned_data <- outliers_count(cleaned_data, column)
}

#Creating this to generate a scatter plot 
num_vars <- names(housep)[sapply(housep, is.numeric)]
num_vars <- num_vars[num_vars != "Sale_amount"]

# Loop through each numeric variable and create a scatter plot
for (var in num_vars) {
  p <- ggplot(housep, aes_string(x = var, y = 'Sale_amount', color = 'Type')) +
    geom_point() +
    labs(title = paste("Scatter Plot of", var, "vs Sale_amount by Type"),
         x = var,
         y = "Sale_amount")
  print(p)}

# Scatter plot with clean data 
for (var in num_vars) {
  p <- ggplot(cleaned_data, aes_string(x = var, y = 'Sale_amount', color = 'Type')) +
    geom_point() +
    labs(title = paste("Scatter Plot of", var, "vs Sale_amount by Type"),
         x = var,
         y = "Sale_amount")
  print(p)}

# Create a scatter plot with Type as the categorical variable
data <- select(cleaned_data, -Sale_date,-Town,-University,-Sale_amount,-Record)
ggpairs(data, aes(colour = Type, alpha = 0.4))
plot(cleaned_data$Sqft_home, cleaned_data$Sale_amount, 
     col = as.factor(cleaned_data$Type), 
     xlab = "Square Footage of Home", 
     ylab = "Sale Amount", 
     main = "Scatter Plot of Sale Amount vs Square Footage of Home by Type")

# Add a legend
legend("bottomright", legend = levels(as.factor(housep$Type)), 
       col = 1:length(levels(as.factor(housep$Type))), 
       pch = 1)
table(cleaned_data$Type)

#We have decided to use 70/30 splits for all of our models 
#Linear Models 
# for linear model 1 we use original data set which we named it as housep excluding University, and Beds variables 
#Removing University variable since there is multicollinearity with Town variable, and removing Beds because it is not significant 
#Adjusted R-squared for this model is 0.7178 
#RMSE for this model is 246462 and the ME is -6843.273
set.seed(1)
house1index <- createDataPartition(housep$Sale_amount,p = 0.7,list = FALSE)
train1 <- housep[house1index,]
valid1 <- housep[-house1index,]
Linear_model1 <- lm(Sale_amount ~ Sqft_home + Sqft_lot + Baths + Build_year+ Town + SingleFamily + MultiFamily + Sale_date, data = train1)
summary(Linear_model1)
predicted1 <- predict(Linear_model1, valid1)
accuracy(predicted1, valid1$Sale_amount)
residuals1 <- resid(Linear_model1)
length(residuals1)
plot(residuals1~train1$Sale_amount,xlab="Actual Sale Amount in $", ylab="Residuals")
abline(h = 0)

#for linear model 2 we use the cleaned data set where we removed all the outliers that are outside +/-3SDs
#Adjusted R-squared for this model is 0.7683
#RMSE for this model is 106202.8 and the ME is 1275.301
set.seed(1)
house2index <- createDataPartition(cleaned_data$Sale_amount,p = 0.7,list = FALSE)
train2 <- cleaned_data[house2index,]
valid2 <- cleaned_data[-house2index,]
Linear_model2 <- lm(Sale_amount ~ Sqft_home + Sqft_lot + Beds + Baths + Build_year+ Town + Sale_month, data = train2)
summary(Linear_model2)
predicted2 <- predict(Linear_model2, valid2)
accuracy(predicted2, valid2$Sale_amount)
residuals2 <- resid(Linear_model2)
length(residuals2)
plot(residuals2~train2$Sale_amount,xlab="Actual Sale Amount in $", ylab="Residuals")
abline(h = 0)

#### for linear model 3 we use the housep_model where we only use sale month instead of sale date, type of homes in
#terms of dummy variables, and town variable is also converted to dummy variable
# Assuming 'Sale_amount' is the response variable
#Adjusted R-squared for this model is 0.7181
#RMSE for this model is 247984.7 and the ME is -6602.476
set.seed(1)
house3index <- createDataPartition(housep_model$Sale_amount,p = 0.7,list = FALSE)
train3 <- housep_model[house3index,]
valid3 <- housep_model[-house3index,]
Linear_model3 <- lm(Sale_amount ~ ., data = train3)
summary(Linear_model3)
predicted3 <- predict(Linear_model3, valid3)
accuracy(predicted3, valid3$Sale_amount)
residuals3 <- resid(Linear_model3)
length(residuals3)
plot(residuals3~train3$Sale_amount, xlab="Actual Sale Amount in $", ylab="Residuals")
abline(h = 0)


######## Decision Tree Model 1 #############
#For model 1, we used the housep_model data set where it has dummy variables for towns, and only with sale month 
#Minimum CP to create the pruned tree is 0.003480911
#ME=451.2223, RMSE=221762.4, MAE=112907.2 , MPE=-25.42132 , MAPE=45.9297
options(scipen = 999)
set.seed(1)
housetree1 <- createDataPartition(housep_model$Sale_amount,p = 0.7,list = FALSE)
traintree1 <- housep_model[housetree1,]
validtree1 <- housep_model[-housetree1,]
set.seed(1)
default_tree1 <- rpart(Sale_amount ~., data = traintree1, method = "anova")
summary(default_tree1)
prp(default_tree1,type = 1,extra = 1,under = TRUE)
set.seed(1)
full_tree1 <- rpart(Sale_amount ~., data = traintree1, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree1, type = 1, extra = 1, under = TRUE)
full_tree1$cptable
printcp(full_tree1)
min_cp1 <- full_tree1$cptable[which.min(full_tree1$cptable[,"xerror"]),"CP"]
min_cp1
pruned_tree1 <- prune(full_tree1,cp = min_cp1)
prp(pruned_tree1, type = 1, extra = 1, under = TRUE)
printcp(pruned_tree1)
predicted_tree1 <- predict(pruned_tree1,validtree1)
predicted_tree1
accuracy(predicted_tree1,validtree1$Sale_amount)
full_tree1$cptable[which.min(full_tree1$cptable[,"xerror"]),"nsplit"]

######## Decision Tree Model 2 #############
#For model 2, we used the original dataset 
#Minimum CP for the pruned tree is 0.002174092
#ME=-2030.909, RMSE=218398.3, MAE=98414.63, MPE=-18.89625, MAPE=36.43376 
set.seed(1)
housetree2 <- createDataPartition(housep$Sale_amount,p = 0.7,list = FALSE)
traintree2 <- housep[housetree2,]
validtree2 <- housep[-housetree2,]
set.seed(1)
default_tree2 <- rpart(Sale_amount ~., data = traintree2, method = "anova")
summary(default_tree2)
prp(default_tree2,type = 1,extra = 1,under = TRUE)
set.seed(1)
full_tree2 <- rpart(Sale_amount ~., data = traintree2, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree2, type = 1, extra = 1, under = TRUE)
full_tree2$cptable
printcp(full_tree2)
min_cp2 <- full_tree2$cptable[which.min(full_tree2$cptable[,"xerror"]),"CP"]
min_cp2
pruned_tree2 <- prune(full_tree2,cp = min_cp2)
prp(pruned_tree2, type = 1, extra = 1, under = TRUE)
printcp(pruned_tree2)
predicted_tree2 <- predict(pruned_tree2,validtree2)
predicted_tree2
accuracy(predicted_tree2,validtree2$Sale_amount)
full_tree2$cptable[which.min(full_tree2$cptable[,"xerror"]),"nsplit"]

####### Decision Tree Model 3 #################
#For model 3, we used the data set with cleaned data that has values that are outside of +/- 3SDs removed
#Minimum CP for pruned tree is 0.001401632
#ME=13.54172, RMSE=108809.2, MAE=69035.05, MPE=-13.28724, MAPE=30.09282
set.seed(1)
housetree3 <- createDataPartition(cleaned_data$Sale_amount,p = 0.7,list = FALSE)
traintree3 <- cleaned_data[housetree3,]
validtree3 <- cleaned_data[-housetree3,]
set.seed(1)
default_tree3 <- rpart(Sale_amount ~., data = traintree3, method = "anova")
summary(default_tree3)
prp(default_tree3,type = 1,extra = 1,under = TRUE)
set.seed(1)
full_tree3 <- rpart(Sale_amount ~., data = traintree3, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree3, type = 1, extra = 1, under = TRUE)
full_tree3$cptable
printcp(full_tree3)
min_cp3 <- full_tree3$cptable[which.min(full_tree3$cptable[,"xerror"]),"CP"]
min_cp3
pruned_tree3 <- prune(full_tree3,cp = min_cp3)
prp(pruned_tree3, type = 1, extra = 1, under = TRUE)
printcp(pruned_tree3)
predicted_tree3 <- predict(pruned_tree3,validtree3)
predicted_tree3
accuracy(predicted_tree3,validtree3$Sale_amount)
full_tree3$cptable[which.min(full_tree3$cptable[,"xerror"]),"nsplit"]
