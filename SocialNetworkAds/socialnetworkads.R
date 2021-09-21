########################################################################################
##################### CASE STUDY- Social Network Ads ################################
########################################################################################

# Logistic Regression

# In case of a classification problem: the target variable is a categorical in nature.
# We will do Logistic Regression and Decision Tree


# Import the dataset

mydata1 <- read.csv(choose.files())
mydata1

head(mydata1)

str(mydata1)

mydata1 <- mydata1[,c(3,4,5)]
summary(mydata1)

# finding missing value

colSums(is.na(mydata1))

# heatmap visualisation 

library(corrplot)

corrplot(cor(mydata1), method = 'color')

# split the data into training and test

library(caTools)

set.seed(123)
split <- sample.split(mydata1$Purchased, SplitRatio = 0.75)
split 
table(split)

training <- subset(mydata1, split==TRUE)
nrow(training)
test <- subset(mydata1, split==FALSE)
nrow(test)


#############################################################################

# Building the Logistic Regression Model (generalised linear model : glm)

classifier <- glm(Purchased~., data = training, family = 'binomial')
classifier
summary(classifier)

#Null deviance: system generated error without taking independent variable
#Residual deviance: error with independent variable
#AIC: adjested r-square in Logistic regression 

#Lower the AIC value, better the model

# Predictions : predict model by using test dataset

pred <- predict(classifier, newdata = test, type='response')
pred

pred_thre_50 <- ifelse(pred>0.5,1,0)
pred_thre_50

cm <- table(test$Purchased,pred_thre_50)
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

(57+26)/(57+7+10+26)
#accuracy - 83%

install.packages("caret")# known for confusion matrix
library(caret)

install.packages("e1071")
library(e1071)

confusionMatrix(cm)

#################################################################
# Accuracy : 0.83
# Sensitivity/Recall : 0.8507         
# Specificity : 0.7879          
# Pos Pred Value/Precision : 0.8906         
# Neg Pred Value : 0.7222          
# Balanced Accuracy : 0.8193
# F1-Measures/F1-score = (2*Recall*Precision)/(Recall+Precision)

F1_measures <- (2*0.8507*0.8906)/(0.8507+0.8906)
# F1-Measures/F1-score : 0.8701929
##################################################################

# Multicollinearity check: 

library(car)
vif(classifier)
# So no multicollinearity found


# Now ROC and AUC for the Sigmoid curve
# ROC curve plots the true positive rate against false positive rate

install.packages("ROCR")
library(ROCR)

rocprediction <- prediction(test$Purchased, pred_thre_50)
rocprediction

rocperformance <- performance(rocprediction,'tpr','fpr')
rocperformance

plot(rocperformance, col='red', print.cutoffs.at=seq(0.1, by=.1))
abline(a=0,b=1)

#########################################################################################
#####################LOGISTIC REGRESSION MODEL ##########################################
#########################################################################################



