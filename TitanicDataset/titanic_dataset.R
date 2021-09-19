#####################################################################
####################### Classification Case Study ###################
#####################################################################

########################## LOGISTIC REGRESSION ######################
#####################################################################

#In case of a classification problem:  the target variable is a categorical in nature.
#We'll be doing Logistic Regression.

#1) Identify the Problem Statement, What are you trying to solve?
#here the ask is:-
#"if the titanic ship is once again built, and people are boarded, and the
#journey starts again, then can you help us identify whether the person whom we
#are boarding will survive another crash or not?"

#2) Import the dataset
## Here, the target variable is "Survived" which is categorical in nature.

titanic <- read.csv(choose.files())

titanic

# Basic EDA

View(titanic)
head(titanic)
tail(titanic)
class(titanic)
names(titanic)

dim(titanic)
# dim() function returns Total dimension i.e. 
# both the number of rows and column in a dataframe.
nrow(titanic)
ncol(titanic)
# We can also use ncol() function to find the number of columns
# and nrow() to find the number of rows separately.

# Structure check of the variables
str(titanic)

summary(titanic)

library(psych)  ### stats package - "psych" ####

describe(titanic)
# This function provides more deep dive statistics including
# standard deviation, mean absolute deviation, skew, etc

# to see how many features we can move to factor
lapply(titanic,function(x)length(unique(x)))

# lets convert "Survived" to be numeric

titanic$Survived <- as.numeric(titanic$Survived)

# lets convert "Pclass", "Sex", "Embarked" to be factors

titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# now again checking the structure of data
str(titanic)

#3) Data Pre-processing
#To check missing values
## find the missing value by using visualization ##
install.packages("Amelia")
library(Amelia)

missmap(titanic, main="TITANIC - Finding Missing Data",
        col=c("red", "black"), legend = F)

#### to check where is missing data to see in charater or factor data ###
colSums(titanic=="") 
colSums(titanic=="")/nrow(titanic) #77%- Cabin, 0.2 % Embarked

colSums(is.na(titanic))
colSums(is.na(titanic))/nrow(titanic)#19.86% we will impute the missing value

boxplot(titanic$Age) 
mean(titanic$Age, na.rm = T)
titanic$Age[which(is.na(titanic$Age))] <- 29.69912
colSums(is.na(titanic))

#we will mode function to handle missing value in Embarked 
getmode <- function(titanic) {
  uniqv <- unique(titanic)
  uniqv[which.max(tabulate(match(titanic, uniqv)))]
}
getmode(titanic$Embarked)
#replace with mode 
titanic$Embarked[titanic$Embarked==""]<- getmode(titanic$Embarked)
colSums(titanic=="") #done with missing missing embarked categorise 

# Now there is no missing values

#To remove the column
titanic <- titanic[,-c(1,4,9,11)] 
str(titanic)

# Checking for the presence of outliers in variables using boxplot.

boxplot(titanic$Age) 
quantile(titanic$Age, seq(0,1,0.02))
titanic$Age <- ifelse(titanic$Age>55,55, titanic$Age)
titanic$Age <- ifelse(titanic$Age<5,5, titanic$Age)
boxplot(titanic$Age)

`#Outlier Treatment Done

# 4) Data visualizations : Univariate analysis & Bivariate analysis

################## Univariate Analysis ##################

# Multiple Continuous Variables
ColsForHist=c("Age","Fare")

#Splitting the plot window into four parts
par(mfrow=c(2,1))

# library to generate professional colors
library(RColorBrewer) 

# looping to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(titanic[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}

# Multiple Categorical features
ColsForBar=c("Survived","Pclass","Embarked")

#Splitting the plot window into four parts
par(mfrow=c(2,2))

# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(titanic[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Spectral"))
}

################ Bivariate Analysis ###################

# Relationship between target variable and predictors variables
# Categorical vs Continuous --- Box Plot
# Categorical vs Categorical -- Grouped Bar chart

# Categorical vs Continuous analysis-- Boxplot
#Here, "Age", "Fare"- continuous
#Survived - Categorical

par(mfrow=c(1,1))

boxplot(Age~Survived, data = titanic, col=brewer.pal(8,"Accent"))

boxplot(Fare~Survived, data = titanic, col=brewer.pal(8,"Accent"))

# Categorical vs Categorical analysis-- Grouped Bar chart

install.packages("ggplot2")
library(ggplot2) #### for creating graphics####

#Pclass vs Survived
ggplot(titanic, aes(fill=Survived, y=Survived, x=Pclass)) + 
  geom_bar(position="stack", stat="identity")

#Embarked vs Survived
ggplot(titanic, aes(fill=Survived, y=Survived, x=Embarked)) + 
  geom_bar(position="stack", stat="identity")


# Relationship between target variable and predictors variable
# Categorical vs Continuous --- ANOVA
# Categorical vs Categorical -- Chi-square test

################ ANOVA TEST ##############################
# Continuous vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)

# H0 Null hypothesis : Variables are not correlated
# Small P-Value < 5% - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value > 5% - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

anova <- aov(Age~Survived, data = titanic)
anova
summary(anova)

anova <- aov(Fare~Survived, data = titanic)
anova
summary(anova)

################ CHI-SQUARE TEST ##############################
# Categorical vs Categorical
# H0 Null hypothesis : Variables are not correlated

# Small P-Value - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

# We do cross tabulation as the input and gives you the result

Chisqcols=c("Pclass","Sex","Embarked")

for(chi_cols in ColsForBar ){
  CrossTabResult=table(titanic[,c('Survived',chi_cols)])
  ChiResult=chisq.test(CrossTabResult)
  print(ColsForBar)
  print(ChiResult)
}

#H0:the two columns are not correlated

#p is very low, 
#so we reject the null and conclude that these two columns are correlated


#5) Split the data into training and testing 

library(caTools)

set.seed(101)
split<- sample.split(titanic$Survived, SplitRatio = 0.80)
split

table(split)
# will show how many are true and false value

training <- subset(titanic, split==TRUE)
nrow(training)
test <- subset(titanic, split==FALSE)
nrow(test)


#6) Building logistic regression model glm 

classi <- glm(Survived~., data = training, family = 'binomial')
classi
summary(classi)

# there are some variable which is not statically significant, 
# hence, we have to remove this.

classi1 <- glm(Survived~.-Parch-Fare-Embarked, data = training, family = 'binomial')
classi1
summary(classi1)

####################################################################################
# Null deviance: 949.9 -- system generated error without taking independent variable
# Residual deviance: 632.4 -- error with independent variable
# AIC: 644.4 -- adjested r-square in logistic regression, akike information criterian
#####################################################################################

#Lower the AIC value, better the model

#7) We can predict model by using test dataset

pred <- predict(classi1, newdata = test, type = "response")
pred

pred_thre_50 <- ifelse(pred>0.5,1,0)

cm <- table(test$Survived, pred_thre_50)
cm

########################################################
# Sensitivity / Recall = TP / (TP+FN)
# Accuracy = (TP+TN)/(TP+FP+FN+TN)
# Specificity = TN / (TN+FP)
# Precision/PPV = TP / (TP+FP)
# False Negative value = FN/(FN+TN)
# False Positive value = FP / (FP+TP)
# F1-Measures = (2*Recall*Precision)/(Recall+Precision)
########################################################

# Accuracy = (TP+TN)/(TP+FP+FN+TN)
(92+47)/(92+18+21+47)

#accuracy = 78%

# Library for confusion matrix
library(caret)
library(e1071)

confusionMatrix(cm)
# 78.09% accuracy

##################################################################
# Accuracy : 0.7809
# Sensitivity/Recall : 0.8142        
# Specificity : 0.7231          
# Pos Pred Value/Precision : 0.8364         
# Neg Pred Value : 0.6912          
# Balanced Accuracy : 0.7686
# F1-Measures/F1-score = (2*Recall*Precision)/(Recall+Precision)

F1_measures <- (2*0.8142*0.8364)/(0.8142+0.8364)
#F1-measures : 0.8251507
##################################################################

###############################################################################
# Maximum Likelihood Estimitation (MLE) concept -- Check for a better accuracy
###############################################################################

# set.seed(101)
# splitratio = 0.80

# threshold = 50%, accuracy - 78.09%

pred_thre_60 <- ifelse(pred>=0.6,1,0)
cm <- table(test$Survived, pred_thre_60)
confusionMatrix(cm)

# threshold = 50%, accuracy - 78.09%
# threshold = 60%, accuracy - 80.34%

pred_thre_70 <- ifelse(pred>=0.7,1,0)
cm <- table(test$Survived, pred_thre_70)
confusionMatrix(cm)

# threshold = 50%, accuracy - 78.09%
# threshold = 60%, accuracy - 80.34%
# threshold = 70%, accuracy - 80.9%

pred_thre_40 <- ifelse(pred>=0.4,1,0)
cm <- table(test$Survived, pred_thre_40)
confusionMatrix(cm)

# threshold = 50%, accuracy - 78.09%
# threshold = 60%, accuracy - 80.34%
# threshold = 70%, accuracy - 80.9%
# threshold = 40%, accuracy - 78.65%

pred_thre_80 <- ifelse(pred>=0.8,1,0)
cm <- table(test$Survived, pred_thre_80)
confusionMatrix(cm)

# threshold = 50%, accuracy - 78.09%
# threshold = 60%, accuracy - 80.34%
# threshold = 70%, accuracy - 80.9%
# threshold = 40%, accuracy - 78.65%
# threshold = 80%, accuracy - 79.02%


## We will choose the accuracy where:
##threshold = 70%, accuracy - 80.9%

#################################################################
# Accuracy : 0.809
# Sensitivity/Recall : 0.7714         
# Specificity : 0.9474          
# Pos Pred Value/Precision : 0.9818         
# Neg Pred Value : 0.5294          
# Balanced Accuracy : 0.8594
# F1-Measures/F1-score = (2*Recall*Precision)/(Recall+Precision)

F1-measures <- (2*0.7714*0.9818)/(0.7714+0.9818)
#F1-measures : 0.863975
##################################################################

# Now ROC and AUC for the Sigmoid curve.
# ROC curve plots the true positive rate against false positive rate.

library(ROCR)   # library for Roc curve

rocprediction <- prediction(test$Survived, pred_thre_70)
rocprediction

rocperformance <- performance(rocprediction,'tpr','fpr')

rocperformance

plot(rocperformance, col='red', print.cutoffs.at=seq(0.1, by=.1))
abline(a=0,b=1)

#########################################################################################
#####################LOGISTIC REGRESSION MODEL COMPLETED#################################
#########################################################################################
