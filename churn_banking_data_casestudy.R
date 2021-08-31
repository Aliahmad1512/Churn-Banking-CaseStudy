#####################CASE STUDY-CHURN BANKING DATA###########################
########################CLASSIFICATION PROBLEM#######################

# Here the target variable "Churn" is a categorical in  nature. We will do a 
# logistic Regression 

# 1) Read the data-set in R studio
# Import the dataset

bank <- read.csv(choose.files())
bank

View(bank) ###### To see excel sheet in my rstudio #####
class(bank)
names(bank)

# 2) Dimension and summary checking of the data-set. 

dim(bank)
# dim() function returns Total dimension i.e. 
# both the number of rows and column in a dataframe.

nrow(bank)
ncol(bank)
# We can also use ncol() function to find the number of columns
# and nrow() to find the number of rows separately.

summary(bank)
# summary function returns some basic calculations for each column.

library(psych)
### stats package - "psych" ####

describe(bank)
# This function provides more deep dive statistics including
# standard deviation, mean absolute deviation, skew, etc


# 3) Structure check and conversion of variables

str(bank)

# to see how many features we can move to factor

lapply(bank,function(x)length(unique(x)))
####lapply-columnwise#####

# converting "Churn" to numeric

bank$Churn <- as.numeric(bank$Churn)

# converting "Num_loans", "Num_dependents", "Num_Savings_Acccts" to factors

bank$Num_loans <- as.factor(bank$Num_loans)
bank$Num_dependents <- as.factor(bank$Num_dependents)
bank$Num_Savings_Acccts <- as.factor(bank$Num_Savings_Acccts)

# Now again checking the structure of data
str(bank)

# 4) Data visualizations : Univariate analysis & Bivariate analysis

###### Univariate Analysis ######

# Multiple Continuous Variables
ColsForHist <- c("Age","MonthlyIncome", "Utilization.ratio")
par(mfrow=c(2,2))

library(RColorBrewer)
# library for colors

# Using loops to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(bank[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}

# Multiple Categorical Variables
ColsForBar <- c("Churn","Num_loans","Num_dependents","Num_Savings_Acccts")

par(mfrow=c(2,2)) 

# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(bank[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Paired"))
}

######## Bivariate Analysis ###########

# Relationship between target variable and predictors variables
# Categorical vs Continuous --- Box Plot
# Categorical vs Categorical -- Grouped Bar chart

############################################################
# Categorical vs Continuous analysis-- Boxplot
# Here "Age","MonthlyIncome", "Utilization.ratio" - continous variables
# and "Churn" - is categorical variable

par(mfrow=c(1,1))

boxplot(Age~Churn, data = bank, col=brewer.pal(8,"Accent"))

boxplot(MonthlyIncome~Churn, data = bank, col=brewer.pal(8,"Accent"))

boxplot(Utilization.ratio~Churn, data = bank, col=brewer.pal(8,"Accent"))

# Categorical vs Categorical analysis-- Grouped Bar chart
install.packages("ggplot2")
library(ggplot2) #### for creating graphics####

#Num_loans vs Churn
ggplot(bank, aes(fill=Churn, y=Churn, x=Num_loans)) + 
  geom_bar(position="stack", stat="identity")

#Num_dependents vs Churn
ggplot(bank, aes(fill=Churn, y=Churn, x=Num_dependents)) + 
  geom_bar(position="stack", stat="identity")

#Num_Savings_Acccts vs Churn
ggplot(bank, aes(fill=Churn, y=Churn, x=Num_Savings_Acccts)) + 
  geom_bar(position="stack", stat="identity")


# 5) Data pre-processing:
# i) check for the missing values

colSums(is.na(bank))
# The bank dataset does not have any missing values

####################################################################
# ii) Testing the significance of variable relationships of predictor variables with target variables using chi-square test.

# Relationship between target variable and predictors variable
# Categorical vs Continuous --- ANOVA
# Categorical vs Categorical -- Chi-square test

################ ANOVA TEST ##############################

# Continuous vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)
# Here continuous-"Age","MonthlyIncome", "Utilization.ratio" and
# categorical - "Churn"

# H0 Null hypothesis : Variables are not correlated

# Small P-Value < 5% - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value > - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

anova <- aov(Age~Churn, data = bank)
anova
summary(anova)
#Churn      Df=1, SumSq=435807, MeanSq=435807, F-value=2024, Pr(>F)<2e-16
#Residuals  Df=149998, SumSq=32293368, MeanSq=215

anova <- aov(MonthlyIncome~Churn, data = bank)
anova
summary(anova)
#Churn      Df=1, SumSq=6.877e+10, MeanSq=6.877e+10, F-value=5779, Pr(>F)<2e-16
#Residuals  Df=149998, SumSq=1.785e+12, MeanSq=1.190e+07

anova <- aov(Utilization.ratio~Churn, data = bank)
anova
summary(anova)
#Churn      Df=1, SumSq=920, MeanSq=920.2, F-value=8721, Pr(>F)<2e-16
#Residuals  Df=149998, SumSq=15826, MeanSq=0.1

## For "Age","MonthlyIncome","Utilization.ratio" -- P-value is low

################ CHI-SQUARE TEST ##############################

# Here categorical- "Num_loans","Num_dependents","Num_Savings_Acccts"
# categorical- "Churn"

# H0 Null hypothesis : Variables are not correlated

# Small P-Value - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

# We do cross tabulation as the input and gives you the result
Chisqcols <- c("Num_loans","Num_dependents","Num_Savings_Acccts")

# using for loop 
for(chi_cols in Chisqcols){
  CrossTabResult=table(bank[,c('Churn',chi_cols)])
  ChiResult=chisq.test(CrossTabResult)
  print(chi_cols)
  print(ChiResult)
}
# Hence, p-value is coming very low, 
# so we can reject the null hypothesis and 
# can conclude that these variables are correlated

# 7) Splitting of data into training and test sample 

library(caTools)

set.seed(123)
split <- sample.split(bank$Churn, SplitRatio = 0.75)
split

table(split)
# will show how many are true and false value

training <- subset(bank, split==TRUE)
nrow(training)
test <- subset(bank, split==FALSE)
nrow(test)

###################################################################
# 8) Building logistic regression model glm 

classi <- glm(Churn~., data = training, family = 'binomial')
classi
summary(classi)

# there are some variable which is not statically significant, 
# hence, we have to remove this.

classi1 <- glm(Churn~.-Num_dependents, data = training, family = 'binomial')
classi1
summary(classi1)

classi2 <- glm(Churn~.-Num_dependents-Num_Savings_Acccts, data = training, family = 'binomial')
classi2
summary(classi2)

classi3 <- glm(Churn~.-Num_dependents-Num_Savings_Acccts-Num_loans, data = training, family = 'binomial')
classi3
summary(classi3)

# the significant variables: "Utilization.ratio", "Age", "MonthlyIncome", "DebtRatio"

# Null deviance: 55215 -- system generated error without taking independent variable  
# Residual deviance: 43877 -- error with independent variable
# AIC: 43887  -- adjested r-square in logistic regression, akike information criterian

#Lower the AIC value, better the model

# 9) We can predict model by using test dataset

pred <- predict(classi3, newdata = test, type = "response")
pred

pred_thre_50 <- ifelse(pred>0.5,1,0)
pred_thre_50

# 10) Summarize the model validation test  

cm <- table(test$Churn, pred_thre_50)
cm
#########################################################
# Sensitivity / Recall = TP / (TP+FN)
# Accuracy = (TP+TN)/(TP+FP+FN+TN)
# Specificity = TN / (TN+FP)
# Precision/PPV = TP / (TP+FP)
# False Negative value = FN/(FN+TN)
# False Positive value = FP / (FP+TP)
# F1-Measures = (2*Recall*Precision)/(Recall+Precision)
#########################################################

# Accuracy = (TP+TN)/(TP+FP+FN+TN)
(34907+198)/(34907+87+2308+198)
#accuracy = 93%

# Library for confusion matrix
library(caret)
library(e1071)

confusionMatrix(cm)

# Accuracy : 0.9361
# Sensitivity/Recall : 0.93798         
# Specificity : 0.69474
# Pos Pred Value/Precision : 0.99751         
# Neg Pred Value : 0.07901 

# F1-Measures = (2*Recall*Precision)/(Recall+Precision)

F1_measures <- (2*0.93798*0.99751)/(0.93798+0.99751)
# F1-Measures/F1-score : 0.9668295

# we will go by -- threshold = 50%, accuracy - 93.61%

# Now ROC and AUC for the Sigmoid curve
# ROC curve plots the true positive rate against false positive rate

library(ROCR)

rocprediction <- prediction(test$Churn, pred_thre_50)
rocprediction

rocperformance <- performance(rocprediction,'tpr','fpr')
rocperformance

plot(rocperformance, col='red', print.cutoffs.at=seq(0.1, by=.1))
abline(a=0,b=1)

########################################################
#######LOGISTIC REGRESSION MODEL COMPLETED##############




