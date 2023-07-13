# Clear the workspace
rm(list=ls())

# Use menu /Session/Set Working Directory/Choose Directory Or command below to set working directory
# This assumes that the script file is located in the root folder of the project
(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

# 1a Load the data bank.csv into RStudio as a data.frame and name the data.frame as b
b <- read.csv("bank.csv", stringsAsFactors = FALSE)

# examin b
str(b)  # you should have a data.frame with 4,521 observations and 17 variables

# 1b Convert the following variables to the factor type: job, marital, education, default, housing, loan, contact, month, campaign, poutcome, y. 
selectedColumns <- c("job", "marital", "education", "default", "housing", 
                     "loan", "contact", "month", "campaign", "poutcome", "y")
b[, selectedColumns] <- lapply(b[, selectedColumns], function(x){as.factor(x)})

# 1c
# now we will split the data into testing and training data sets
# we will first randomly select 2/3 of the rows
set.seed(123)
train <- sample(1:nrow(b), nrow(b)*(2/3))

# Use the train index set to split the dataset
# b.train for building the model
# b.test for testing the model
b.train <- b[train,]   #3014 Observations
b.test <- b[-train,]   #1507 observations

# 2a
# Building decision tree with rpart
library(rpart)

# Grow tree 
fit <- rpart(y ~ ., 
             data=b.train, 
             method="class", 
             control=rpart.control(xval=10, minsplit=50),
             parms=list(split="gini"))

# 2b
# Plot the tree
library(rpart.plot)

rpart.plot(fit, type = 1, extra = 4, main="Classification Tree for Customer purchasing CD", cex=0.47)

# 2c
# Define the confusion matrix cm for the test dataset b.test
b.pred <- predict(fit, b.test, type="class")
b.actual <- b.test$y
cm <- table(b.pred,b.actual) #confusion matrix

# 2d
#  Extract True Positive, True Negative, False Positive, and False Negative from the confusion matrix cm, and define four corresponding variables as tp, tn, fp and fn
tp <- cm[2,2]
tn <- cm[1,1]
fp <- cm[2,1]
fn <- cm[1,2]

# 2e
# Use tp, tn, fp and fn to compute the following statistics: False Positive Rate, False Negative Rate, Specificity, Sensitivity, and Accuracy. 

#False Positive Rate
FPR <- fp/(tn+fp)  #0.04484305

#False Negative Rate
FNR <- fn/(fn+tp)  #0.6745562

#Specificity
TNR <- tn/(tn+fp)  #0.955157

#Sensitivity
TPR <- tp/(tp+fn)  #0.3254438

#Accuracy
accuracy <- (tp+tn)/(tn+fp+fn+tp)   #0.8845388

