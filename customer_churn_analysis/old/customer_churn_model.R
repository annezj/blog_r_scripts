#
# Machine learning model to predict customer churn
#
rm(list=ls())
library(data.table)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)


datadir="/Users/annejones/Documents/blog/datasets/churn/"
outdir="/Users/annejones/Documents/blog/blog_r_scripts/customer_churn_analysis/output/"
#
# 1. Load the TELCO customer churn data
# This sample dataset from IBM gives information about a set of customers
# for a telecommunications business, indicating the length of time they've been a customer,
# ("tenure", in months), if they left the service within the last month ("Churn" - a binary variable)
# together with 18 other covariates describing each customer.
# 
df.tel=fread(paste(datadir,"WA_Fn-UseC_-Telco-Customer-Chur-Table 1.csv",sep=''), data.table=F)


# tidy up variables
df.vars<-df.tel[,2:20]
df.vars$SeniorCitizen=as.factor(df.vars$SeniorCitizen)
# shorten some strings and tidy up resulting data frame
df.vars <- as.data.frame(lapply(df.vars, function(x) { gsub("No phone service", "No service", x)}), stringsAsFactors = F)
df.vars <- as.data.frame(lapply(df.vars, function(x) { gsub("No internet service", "No service", x)}),stringsAsFactors = F)
df.vars <- as.data.frame(lapply(df.vars, function(x) { gsub("Month-to-month", "Month", x)}),stringsAsFactors = F)
df.vars <- as.data.frame(lapply(df.vars, function(x) { gsub("\\(automatic\\)", "", x)}),stringsAsFactors = F)
df.vars <- as.data.frame(lapply(df.vars, function(x) { gsub("Mailed check", "Mailed\ncheck", x)}),stringsAsFactors = F)
df.vars <- as.data.frame(lapply(df.vars, function(x) { gsub("Credit card", "Credit\ncard", x)}),stringsAsFactors = F)
df.vars <- as.data.frame(lapply(df.vars, function(x) { gsub("Bank transfer", "Bank\ntransfer", x)}),stringsAsFactors = F)
df.vars <- as.data.frame(lapply(df.vars, function(x) { gsub("Electronic check", "Electronic\ncheck", x)}),stringsAsFactors = T)

df.vars$tenure=as.integer(as.character(df.vars$tenure))
df.vars$MonthlyCharges=as.numeric(as.character(df.vars$MonthlyCharges))
df.vars$TotalCharges=as.numeric(as.character(df.vars$tenure))

# do we have missing values - no, we're all clear
sum(is.na(df.vars))

df.tel[,2:20]=df.vars

df.tel$contract_expired=F
df.tel$contract_expired[which(df.tel$Contract=="Month" & df.tel$tenure>=1)]=T
df.tel$contract_expired[which(df.tel$Contract=="One year" & df.tel$tenure>=12)]=T
df.tel$contract_expired[which(df.tel$Contract=="Two year" & df.tel$tenure>=24)]=T

# Load the caret and random forest packages
library(caret)

# Note data are not balanced:
table(df.tel$Churn)

# undersampling: subset the majority class. Oversampling: repeat sample the minority class

#
# Split the data into training (80 %), test (10 %) and validation (10 %) sets
# train to train the models and tune hyperparameters using cross validation
# test to compare different ML models
# validation for final estimation of model performance
#
train_test_index=createDataPartition(df.tel$Churn, p=0.9, list=F)
validation_data = df.tel[-train_test_index,c(2:19,22,21)]
traintest=df.tel[train_test_index, c(2:19,22, 21)]
train_index=createDataPartition(traintest$Churn, p=0.88889, list=F)
train_data=traintest[train_index,]
test_data=traintest[-train_index,]

# Compare: Naive Bayes, 



# Try out some candidate ML algorithms
# Evaluate using the area under the ROC curve
control = trainControl(method="repeatedcv", number=10, repeats=3,
                       summaryFunction=twoClassSummary, classProbs=TRUE)
seed = 7
metric = "ROC"
preProcess=c("center", "scale")

# Try loads of different methods to see which might be OK, just use default settings initially
# Reset seed between each to make sure data subsets are the same

# todo - make sure you understand what each of these does! (or at least trim it down to good and bad ones for blog)
# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(Churn~., data=dataset, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(Churn~., data=dataset, method="glm", metric=metric, trControl=control)
# GLMNET - logistic regression with regularisation
set.seed(seed)
fit.glmnet <- train(Churn~., data=dataset, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(Churn~., data=dataset, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(Churn~., data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(Churn~., data=dataset, method="nb", metric=metric, trControl=control)
# CART - ERRORS - NOT RUN
set.seed(seed)
fit.cart <- train(Churn~., data=dataset, method="rpart", metric=metric, trControl=control)
# Bagged CART  ERRORS- NOT RUN
set.seed(seed)
fit.treebag <- train(Churn~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Churn~., data=dataset, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(Churn~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# Compare the results and select a subset of models to continue with
results <- resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb,
                          rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)
# Boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)


#
# Proceed with gbm (tree model), glmnet (logistic regression) and nb (simple/fast model for comparison)
# Try to improve the models

# Feature engineering
# Need to include contract expired
# Decompose categorical vars
# Convert numerical vars into accumulations or partitions e.g. months after contract expiry

# Feature selection on new feature list
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(dataset[,1:18], as.factor(dataset[,19]), sizes=c(1:18), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)

data.sub=dataset[,c(predictors(results), "Churn")]
data.sub$Churn=data.sub$Churn=="Yes"

control = trainControl(method="repeatedcv", number=10, repeats=3,
                       summaryFunction=twoClassSummary, classProbs=TRUE)
set.seed(seed)
# may be a more robust NB method - better on test set?
fit.nb2 <- train(Churn~., data=data.sub,
                 method="nb", metric=metric, trControl=control)


alldata <- df.tel[,c(2:19,23,21)]
cols.fac=c(1:4,6:17,19)
cols.num=c(5,18)
data.fac=alldata[,cols.fac]
data.fac$contract_expired=factor(data.fac$contract_expired)
data.num=alldata[,cols.num]


dmy=dummyVars(" ~ .", data = data.fac)
data.fac.expand = data.frame(predict(dmy, newdata = data.fac))
alldata.expand=cbind(data.fac.expand, data.num, alldata$Churn)
colnames(alldata.expand)[48]="Churn"

dataset=alldata.expand[validation_index,]



set.seed(seed)
fit.nb.full <- train(Churn~., data=dataset, method="nb", metric=metric, trControl=control)
set.seed(seed)
fit.glmnet.full <- train(Churn~., data=dataset, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
set.seed(seed)
fit.gbm.full <- train(Churn~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)

results2 <- resamples(list(glmnet=fit.glmnet.full,
                          nb=fit.nb.full,
                          gbm=fit.gbm.full))
# Table comparison
summary(results2)

testset=alldata.expand[-validation_index,]

# evaluate on test set
predictions.gbm=predict.train(fit.gbm.full, testset, type='raw')
predictions.nb=predict.train(fit.nb.full, testset, type='raw')
predictions.glmnet=predict.train(fit.glmnet.full, testset, type='raw')

confusionMatrix(predictions.gbm,testset[,"Churn"])

pr <- prediction(1*(predictions.gbm=="Yes"), 1*(testset$Churn=="Yes"))
auc <- performance(pr, measure = "auc")
auc=auc@y.values[[1]]
auc

pr <- prediction(1*(predictions.nb=="Yes"), 1*(testset$Churn=="Yes"))
auc <- performance(pr, measure = "auc")
auc=auc@y.values[[1]]
auc

pr <- prediction(1*(predictions.glmnet=="Yes"), 1*(testset$Churn=="Yes"))
auc <- performance(pr, measure = "auc")
auc=auc@y.values[[1]]
auc


# Can we improve predictions further using survival curve methods?


# Quantify churn with restricted mean survival time (RMST)
# This is the average lifetime or, in this case average time for which a customer is retained.
# The restriction is that we take
# mean time with an upper time limit (e.g. 1 year) over which we consider the
# mean survival, where the limit is chosen appropriatetly for the dataset
# For censored data, some individuals are below the time limit
# but fortunately the RMST is just the area under the survival curve for
# the time of interest, and therefore for right-censored data
# we can estimate RMST by the area under the K-M estimated survival curve.
# need to estimate RMST = area under survival curve
# rmean is the RMST
# here look at first 12 months and consider the contract
print(survfit(survival ~ Contract, data = df.tel), 
      print.rmean=getOption("survfit.print.rmean"), rmean = 12)
# So we see that for month-to-month contracts, the mean time to churn 
# over the first year is 9.72 months.
# Compared to (unsurprisingly) almost 12 months and 12 months for the contract customers
# What about over the first 2 years?
print(survfit(survival ~ Contract, data = df.tel), 
      print.rmean=getOption("survfit.print.rmean"), rmean = 24)
# Here, month to month customers last 17.5 months out of 24 on average (a little worse than
# during the first 12 months while the second year retention for one year customers is very
# good at 23.8 months out of 24.)
print(survfit(survival ~ Contract, data = df.tel), 
      print.rmean=getOption("survfit.print.rmean"), rmean = 36)
# Over three years the monthly customers drop a little again to 24/36 months (8 months out of 12)
# while the contract customers continue to do well at 35.4 and 36 months
print(survfit(survival ~ Contract, data = df.tel), 
      print.rmean=getOption("survfit.print.rmean"), rmean = 48)
# If we knew the cost of the contracts we could work out how best to 
# maximise revenue from these customers.


# Pseudo observations are a clever technique which allows us to transform the data,
# removing the censoring and therefore allowing more complicated regression models to be fitted.
# Here as an example create pseudo obs using the first 36 months of RMST
library(pseudo)
df.tel$pseudos <- pseudomean(df.tel$tenure, df.tel$Churn=="Yes", 36)
df.tel$id=1:nrow(df.tel)
# Now fit a regression model
# coefficients show impact on RMST over period
# also included the effect of continous in addition to categorical variables
summary(fit <- geese(pseudos ~ Contract + SeniorCitizen + gender,
                     data = df.tel, id = id, jack = TRUE, family = gaussian,
                     corstr = "independence", scale.fix = FALSE))
# So we can see for the variables considered in this model contract has the biggest impact over the
# first 36 months, and the other two variables have negligible effects.
#

# Comprehensive analysis using pseudo obs and decision trees

