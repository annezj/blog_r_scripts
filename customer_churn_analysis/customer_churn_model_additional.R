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

# Note data are not balanced so we will need to introduce under or oversampling at the training stage
table(df.tel$Churn)

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

# Compare: Naive Bayes (simple model), GLMnet (logistic regression) and a tree-based model (non-linear)

# Use area under the ROC curve to train on both hits and false alarms
control = trainControl(method="repeatedcv", number=10, repeats=3,
                       summaryFunction=twoClassSummary, classProbs=TRUE)

seed = 7
metric = "ROC"
preProcess=c("center", "scale")


# Let's not worry about the imbalance data yet, and just compare the methods

# GLMNET - logistic regression with regularisation
set.seed(seed)
fit.glmnet <- train(Churn~., data=train_data, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(Churn~., data=train_data, method="nb", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Churn~., data=train_data, method="rf", metric=metric, trControl=control)

# Compare the results and select a subset of models to continue with
results <- resamples(list(glmnet=fit.glmnet,nb=fit.nb,rf=fit.rf))
# Table comparison
summary(results)
# Boxplot comparison
bwplot(results)


# Performance overall is similar (mean ROC of 0.84 glmnet, 0.82 nb, 0.83 rf) note nb has lower sensitivity 
# (proportion of churns detected correctly) and higher specificity 
# (proportion of not churns detected correctly) while rf is the opposite. glmnet is between.
#

# Let's see what they did
print(fit.glmnet)
plot(fit.glmnet)
# The two hyperparameters are alpha and lambda
# alpha is 0.1 (close to ridge regression)
# lambda is 0.0032 (regularisation strength)
coeff=predict(fit.glmnet$finalModel, type = "coefficients", 
                            s=fit.glmnet$bestTune$lambda)
coeff=data.frame(name=coeff@Dimnames[1], value=coeff@x)
colnames(coeff)=c('name','value')
coeff[order(abs(coeff$value), decreasing = T),]
varImp(fit.glmnet)

#
# Note that the glmnet function within caret automatically creates dummy variables for categorical variables
# 
# so 30 variables were retained, with the most important being tenure (decrease probability of churning), 
# contract of two years (decrease probability of churning), fiber optic internet (increase probability of churning)
# contract expiry (increase probability of churning), one year contract (decrease probabilty of churning) and
# paperless billing (increase probability of churning)
#
# The fact that gender (for example) has been retained makes me worry that this model might be overfitting.
#

# What did the other models do?
print(fit.nb)
# Hyperparameters here are fL (0) and adjust (1)
# Try allowing them to be tuned
grid <- data.frame(fL=c(0,0.5,1.0), usekernel = TRUE, adjust=c(0,0.5,1.0))

fit.nb.2 <- train(Churn~., data=train_data, method="nb", metric=metric, 
                trControl=control,
                tuneGrid=grid)

grid <- data.frame(fL=c(0.5,0.75,1.0), usekernel = TRUE, adjust=c(0.5,0.75,1.0))
fit.nb.3 <- train(Churn~., data=train_data, method="nb", metric=metric, 
                  trControl=control,
                  tuneGrid=grid)
# So 0.5, 0.5 gives marginally better fit, but doesn't change anything drastically

print(fit.rf)
#
# Here hyperparameter mtry=2 which is the number of variables randomly sampled
# as candidates at each plit
# ntree (number of trees to grow) is not tuned and has default of 500
# we could try to increase it
#


# this is very slow! (could try fewer folds or reduce predictors)
fit.rf.2<-train(Churn~., data=train_data, method="rf", metric=metric, trControl=control,
                ntree=1000)

varImp(fit.rf)
# So the RF algorithm importance looks similar to the glmet but it's not quite the same

cbind(varImp(fit.rf)$importance, varImp(fit.glmnet)$importance)


#
# refine: try methods to balance the data before fitting
#

# Under sampling
control = trainControl(method="repeatedcv", number=10, repeats=3,
                       summaryFunction=twoClassSummary, classProbs=TRUE,
                       sampling="down")

set.seed(seed)
fit.glmnet.under <- train(Churn~., data=train_data, method="glmnet", 
                          metric=metric, preProc=c("center", "scale"), 
                          trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb.under <- train(Churn~., data=train_data, method="nb", metric=metric, 
                      trControl=control)
# Random Forest
set.seed(seed)
fit.rf.under <- train(Churn~., data=train_data, method="rf", metric=metric, 
                      trControl=control)

# Over sampling
control = trainControl(method="repeatedcv", number=10, repeats=3,
                       summaryFunction=twoClassSummary, classProbs=TRUE,
                       sampling="up")

set.seed(seed)
fit.glmnet.over <- train(Churn~., data=train_data, method="glmnet", 
                          metric=metric, preProc=c("center", "scale"), 
                          trControl=control)
set.seed(seed)
fit.nb.over <- train(Churn~., data=train_data, method="nb", metric=metric, 
                      trControl=control)
set.seed(seed)
fit.rf.over <- train(Churn~., data=train_data, method="rf", metric=metric, 
                      trControl=control)


# "SMOTE sampling
control = trainControl(method="repeatedcv", number=10, repeats=3,
                       summaryFunction=twoClassSummary, classProbs=TRUE,
                       sampling="smote")

set.seed(seed)
fit.glmnet.smote <- train(Churn~., data=train_data, method="glmnet", 
                         metric=metric, preProc=c("center", "scale"), 
                         trControl=control)
set.seed(seed)
fit.nb.smote <- train(Churn~., data=train_data, method="nb", metric=metric, 
                     trControl=control)
set.seed(seed)
fit.rf.smote <- train(Churn~., data=train_data, method="rf", metric=metric, 
                     trControl=control)

results <- resamples(list(glmnet.none=fit.glmnet,
                          glmnet.under=fit.glmnet.under,
                          glmnet.over=fit.glmnet.over,
                          glmnet.smote=fit.glmnet.smote,
                          nb.none=fit.nb,
                          nb.under=fit.nb.under,
                          nb.over=fit.nb.over,
                          nb.smote=fit.nb.smote,
                          rf.none=fit.rf,
                          rf.under=fit.rf.under,
                          rf.over=fit.rf.over,
                          rf.smote=fit.rf.smote
))

# Table comparison
summary(results)
# Boxplot comparison
bwplot(results)

#
# look at bias and variance on independent test set to compare models and 
# and reduce the predictor set for robustness?
predictions.nb=predict(fit.nb, test_data, type='raw')
predictions.glm=predict(fit.glmnet, test_data, type='raw')
predictions.rf=predict(fit.rf, test_data, type='raw')
predictions.glm.under=predict(fit.glmnet.under, test_data, type='raw')
predictions.nb.under=predict(fit.nb.under, test_data, type='raw')
predictions.rf.under=predict(fit.rf.under, test_data, type='raw')
predictions.glm.over=predict(fit.glmnet.over, test_data, type='raw')
predictions.nb.over=predict(fit.nb.over, test_data, type='raw')
predictions.rf.over=predict(fit.rf.over, test_data, type='raw')
predictions.glm.smote=predict(fit.glmnet.smote, test_data, type='raw')
predictions.nb.smote=predict(fit.nb.smote, test_data, type='raw')
predictions.rf.smote=predict(fit.rf.smote, test_data, type='raw')

test_classes=factor(test_data$Churn)

models=c(paste(rep(c("nb","glm","rf"),each=4),c("",".under",".over",".smote"),sep=''))
compare=data.frame()

for (name in models) {
  pred = get(paste0("predictions.", name))
  cm=confusionMatrix(pred, test_classes)
  pr = prediction(1*(pred=="Yes"), 1*(test_classes=="Yes"))
  auc = performance(pr, measure = "auc")
  auc=auc@y.values[[1]]
  compare=rbind(compare, data.frame(
           model=name,
           Sensitivity = cm$byClass["Sensitivity"],
           Specificity = cm$byClass["Specificity"],
           Precision = cm$byClass["Precision"],
           Recall = cm$byClass["Recall"],
           F1 = cm$byClass["F1"],
           Accuracy=cm$overall[1],
           ROCA = auc))
}

library(tidyr)
compare.melt=melt(compare, id.vars="model")
colnames(compare.melt)[2]="measure"
compare.melt %>%
  ggplot(aes(x = measure, y = value, color = model)) +
  scale_x_discrete(labels=colnames(compare)[-1])+
  geom_jitter(width = 0.2, alpha = 0.8, size = 3)+ theme_bw()

#
# Indicates variance of nb method is high - consistent because it is not
# regularised. We might do better with NB is we reduce the number of predictors
# Sampling methods are doing something bad to NB, and only improved precision. Why?
# Note different measures rank the models differently. Which do you choose? 
# It depends on the application (for another post)
#

# Tweak NB a little more by reducing the predictor set
control2 <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train_data[,1:19], as.factor(train_data[,20]), sizes=c(1:19), 
               rfeControl=control2)
# summarize the results
print(results)
# list the chosen features
predictors(results)

# ensemble the models?


#
# Evaluate performance on validation set
#











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

