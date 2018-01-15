# Adventures in survival analysis using R
# Survival analysis is an interesting topic of machine learning because it's applicable across
# any domain where "time to event" is of interest - from human or animal mortality to 
# customer churn and reliability of engineering components.
# By analysing the times for these events along with other information about 
# the individuals involved, we can understand which factors are most important in 
# affecting the event of interest, and then potentially develop targeted programmes to intervene.
# In this post I'm going to use the same analysis methods on three freely available datasets.
# 
# 1. Telco customer churn data from IBM https://www.ibm.com/communities/analytics/watson-analytics-blog/predictive-insights-in-the-telco-customer-churn-data-set/
# Additional data from https://www.knime.org/knime-applications/churn-prediction
# 2. Employee attrition data from http://www.scan-support.com/help/sample-data-sets
# 3. Divorce data from http://data.princeton.edu/wws509/datasets/#divorce
#
# A complication of survival analysis is that data are typically incomplete: we may have data recorded over a certain period,
# and therefore do not follow all individuals in the dataset until the event occurs for them. For these individuals, the
# times are "right-censored" i.e. we know the time-to-event is above a certain length of time, but not exactly how long.
#
# Luckily, the Kaplan Meier estimator [https://en.wikipedia.org/wiki/Kaplan%E2%80%93Meier_estimator] 
# provides a method of including information for both those individuals for which we have a record of 
# the event occurring, and those for which we only have right-censored data.
# 
# To better understand exactly how the K-M estimator works, there are some good simple examples 
# here [http://www.itl.nist.gov/div898/handbook/apr/section2/apr215.htm] and 
# here [http://www.pmean.com/08/SimpleKm.html].
# Kaplan-Meier is not the only method for surivial analysis, and requires exact times, but has the advantage of being 
# non-parametric. i.e. it is fitted empirically without making any assumptions about the distribution of
# the data.
#
# For each dataset I'm going to use K-M to estimate the survival function, defined as the probability that the event of 
# interest (e.g. a customer leaves) DOES NOT occur by a specific time.
#
# Resources (to add where appropriate or at end):
# Great tutorial on churn analysis: http://daynebatten.com/2015/02/customer-churn-survival-analysis/
# Note on survminer package for prettier visualisations : https://cran.r-project.org/web/packages/survminer/vignettes/Informative_Survival_Plots.html
# Helpful practical notes on all aspects of Machine Learning: https://machinelearningmastery.com/evaluate-machine-learning-algorithms-with-r/
#
rm(list=ls())
library(data.table)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)


# INITIAL DATA EXPLORATION

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

# Let's plot a quick K-M survival curve on all the data.
#
# Create a "survival object" for each observation - around 85% of customers remain for a year
df.tel$survival <- Surv(df.tel$tenure, df.tel$Churn == "Yes")
# Fit a basic survival curve using the data
fit <- survfit(survival ~ 1, data = df.tel)

ggsurvplot(fit, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.8,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')

# Now we want to understand how the suvival curve varies according to the covariates
# For example, we can fit separate curves for customers who are senior citizens and those who are not,
# and we find senior citizens churn more quickly than other customers:
fit2 <- survfit(survival ~ SeniorCitizen, data = df.tel)
ggsurvplot(fit2, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.3,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')


# Let's look at a summary of all the variables
# todo Describe this library
library(purrr)
# tidyr provides a neat alternative to melt
library(tidyr)

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

# let's plot a summary of each variable, first the numeric ones and then the categorical ones
df.vars %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(x=value,fill=key)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()+theme_bw()
ggsave(paste(outdir, "churn-variables-numeric.png", sep=''), height=2, width=8, units="in")

df.vars %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(x=value,fill=key)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()
ggsave(paste(outdir, "churn-variables-categorical.png", sep=''), height=6, width=12, units="in")

vars.cat=colnames(df.vars %>%keep(is.factor))
length(vars.cat)
vars.num=colnames(df.vars %>%keep(is.numeric))
length(vars.num)

# Now let's consider survival curves for all the categorical variables and see which have the biggest differences
log.rank.cat=data.frame()
for(icat in 1:length(vars.cat))
{
  diff=survdiff(formula = as.formula(paste('survival ~' , vars.cat[icat], sep='')), data = df.tel)
  p=pchisq(diff$chisq, df=(length(unique(df.tel[,vars.cat[icat]])))-1, lower.tail=F)
  tmp=data.frame(variable=vars.cat[icat], chisq=diff$chisq, p=p)
  log.rank.cat=rbind(log.rank.cat, tmp)
}
log.rank.cat[order(log.rank.cat$p, decreasing = T),]
# so all but PhoneService and gender have a curves that's significantly different at <1%

# Contract had the clearest effect
fit2 <- survfit(survival ~ Contract, data = df.tel)
ggsurvplot(fit2, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.3,xlim=c(0,25),
           break.x.by=1,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')
# Contract versus churn time - to we have a cliff edge at contract end?
# No we see only a small step at 12 months but also at 1 month for monthly contracts
# From this assume contract expiry occurs immediately at tenure =1, 12 or 24 months
# This might be a useful variable
df.tel$contract_expired=F
df.tel$contract_expired[which(df.tel$Contract=="Month" & df.tel$tenure>=1)]=T
df.tel$contract_expired[which(df.tel$Contract=="One year" & df.tel$tenure>=12)]=T
df.tel$contract_expired[which(df.tel$Contract=="Two year" & df.tel$tenure>=24)]=T
fit3 <- survfit(survival ~ contract_expired, data = df.tel)
ggsurvplot(fit3, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.3,xlim=c(0,25),
           break.x.by=1,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')
# The "Cox proportional hazards model" allows us to do regression on multiple covariates
# to fit the survival rate, which is assumed constant across different categories.
# Now test for non-proportional hazards
# p values gives statistical signifiance of non-proportional element
# (probability of getting result if hazards were purely proportional)
# if p<pthresh for statistical significance we cannot use simple proportional hazards model
# for that covariate.
cox.zph(fit.cox)
# No good
fit.cox=coxph(formula = survival ~ Contract + contract_expired, data = df.tel)
fit.cox
cox.zph(fit.cox)


# Explore redundancy and correlation among categorical variables

# Partner, Senior Citizen, Dependents - as expected
table(df.tel[,c("SeniorCitizen","Partner","Dependents")])

# Phone service, multiple lines - no phone service is contained within multiple lines
table(df.tel[,c("PhoneService","MultipleLines")])

# Internet service, tech support - supports is for internet only
table(df.tel[,c("InternetService","TechSupport")])

# Senior Citizen, Payment method - less likely to pay via mailed check
table(df.tel[,c("SeniorCitizen","PaymentMethod")])


# what about the numerical variables? Let's try Cox regression
coxph(formula = survival ~ MonthlyCharges, data = df.tel)
# 1.006177 So a $1 increase in Monthly charges increases churn slightly (by 0.6%) and this is statistically significant
ggplot(df.tel)+geom_point(aes(x=tenure, y=MonthlyCharges, color=TotalCharges))+theme_bw()

# How about total charges?
coxph(formula = survival ~ TotalCharges, data = df.tel)
# here's there's an issue with the data
hist(df.tel$TotalCharges)
ggplot(df.tel)+geom_point(aes(x=tenure, y=TotalCharges, color=TotalCharges))+theme_bw()
# so it's a linear function of tenure and we can remove it

# what can we say about those known to churn - look at who churned as a proportion of total
df.churn=melt(df.tel[which(df.tel$Churn=="Yes"), c('customerID',vars.cat)], id.vars='customerID')
ggplot(df.churn) + geom_bar(aes(x=value, fill=variable), position="stack")+theme_bw()+
  facet_wrap(~variable, nrow=4, scales='free')+ggtitle("Breakdown of churned customers")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste(outdir, "churn-breakdown.png", sep=''), height=8, width=8, units="in")

df.melt=melt(df.tel[, c('customerID',vars.cat,'Churn')], id.vars=c('customerID', "Churn"))
ggplot(df.melt) + geom_bar(aes(x=value, fill=Churn), position="stack")+theme_bw()+
  facet_wrap(~variable, nrow=4, scales='free')+ggtitle("Breakdown of all customers")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste(outdir, "category-breakdown.png", sep=''), height=8, width=8, units="in")


# Probability of churn conditional upon category values
# Use chain rule of probability: P(Churn/A)=P(Churn, A)/P(A)
# Note we have already transformed vars to factors but need to check the churned customers
# contain all levels of the factors
library(plyr)
df.freq=data.frame()
for(var in vars.cat)
{
  count.all=count(df.tel[,var])
  count.churn=count(df.tel[which(df.tel$Churn=="Yes"),var])
  pchurns=count.all
  # Use match to be cautious in case some levels are missing or vars are re-ordered
  pchurns[,2]=count.churn[match(count.all$x, count.churn$x),2]/count.all[,2]
  pchurns$variable=var
  df.freq=rbind(df.freq,pchurns)
}
churn.rate=nrow(df.tel[which(df.tel$Churn=="Yes"),])/nrow(df.tel)
ggplot(df.freq)+geom_bar(aes(x=x, y=freq, fill=variable), stat='identity')+theme_bw()+
  facet_wrap(~variable, nrow=4, scales='free')+ggtitle("Conditional churn probability")+
  scale_y_continuous(limits=c(0,0.5))+
  geom_hline(yintercept = churn.rate, linetype='dashed')+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+xlab('')+ylab('P(Churn/value)')
ggsave(paste(outdir, "churn-probability.png", sep=''), height=8, width=10, units="in")

# So e.g. monthly contract and electronix check (which may overlap) are high risk factors for churn


#
# e.g.
fit.cox<-coxph(survival~gender+SeniorCitizen+InternetService, data=df.tel)
# inspect fit for significance and coefficients
# for churn relative to base
# We can see gender is not significant
fit.cox
fit.cox<-coxph(survival~SeniorCitizen+InternetService, data=df.tel)
fit.cox
# The values of exp(coef) give us the relative churn for the different categories
# So we can see the Senior citizens churn 1.26 times faster than the baseline
# and that those with Fiber Optic internet churn 2.1 times faster.
# Those with no internet are much faster at 0.4 timesthe base churn.



# How about the other possible covariates?

# Unsurprisingly, contract has a big effect, but again it's not a simple proportional model.
fit <- survfit(survival ~ Contract, data = df.tel)
ggsurvplot(fit, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.3,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')

fit.cox<-coxph(survival~Contract, data=df.tel)
fit.cox
cox.zph(fit.cox)

#
# So let's try to build a machine learning model to predict churn
#


# Load the caret and random forest packages
library(caret)

# Split the data into training and a final validation set
#df.tel$Churn=as.factor(df.tel$Churn)
validation_index <- createDataPartition(df.tel$Churn, p=0.80, list=FALSE)
validation <- df.tel[-validation_index,c(2:19,21)]
# use the remaining 80% of data to training and testing the models
dataset <- df.tel[validation_index,c(2:19,21)]

# Caret balances the frequency of churn to match the whole dataset
length(which(validation$Churn=="Yes"))/nrow(validation)
length(which(dataset$Churn=="Yes"))/nrow(dataset)
churn.rate

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

