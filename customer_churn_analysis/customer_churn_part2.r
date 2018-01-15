library(data.table)
library(survival)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
rm(list=ls())
# Load the TELCO customer churn data  .csv file conversion :
#df.tel=fread("/Users/annejones/Documents/blog/datasets/churn/WA_Fn-UseC_-Telco-Customer-Chur-Table 1.csv", data.table=F)
datadir="/Users/annejones/Documents/blog/datasets/churn/"
df.tel=fread(paste(datadir,"WA_Fn-UseC_-Telco-Customer-Chur-Table 1.csv",sep=''), data.table=F)

# Tidy up
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

# Check for missing values - no, we're all clear
sum(is.na(df.vars))
df.tel[,2:20]=df.vars

head(df.tel[,1:11], n=3)
head(df.tel[,12:21], n=3)

df.tel=df.tel[,c(2:19, 21)]
df.tel$contract_expired=F
df.tel$contract_expired[which(df.tel$Contract=="Month" & df.tel$tenure>=1)]=T
df.tel$contract_expired[which(df.tel$Contract=="One year" & df.tel$tenure>=12)]=T
df.tel$contract_expired[which(df.tel$Contract=="Two year" & df.tel$tenure>=24)]=T

table(df.tel$Churn)/nrow(df.tel)

library(caret)
seed = 7
set.seed(seed)
train_index=createDataPartition(df.tel$Churn, p=0.8, list=F)
train_data=df.tel[train_index,c(1:18,20,19)]
test_data=df.tel[-train_index,c(1:18,20,19)]

# Set up for training to optimise ROC area
control = trainControl(method="repeatedcv", number=10, repeats=3,
                       summaryFunction=twoClassSummary, classProbs=TRUE)

metric = "ROC"
preProcess=c("center", "scale")

# GLMNET - logistic regression with regularisation
set.seed(seed)
fit.glmnet <- train(Churn~., data=train_data, method="glmnet", metric=metric, preProc=preProcess, trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(Churn~., data=train_data, method="nb", metric=metric, preProc=preProcess, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Churn~., data=train_data, method="rf", metric=metric, preProc=preProcess, trControl=control)

#
# Get the ROC areas for predictions on the test set
# Note to get full predictive power from the classifier it's important to get the 
# probability from the predictions, and not just use the raw results
# which will predict a binary response based on probabilities above or below 0.5.
#
library(pROC)
compare=data.frame()
rocs=list()
i=1
for (name in c("glmnet", "nb", "rf")) 
{
  fit = get(paste0("fit.",name))
  predictions=predict(fit, test_data, type="prob")
  roc.obj=roc(test_data$Churn=="Yes", predictions$Yes)
  aucval=auc(roc.obj)[1]
  compare=rbind(compare, data.frame(model=name, auc=auc(roc.obj)[1], 
                                    auc.lci95=ci.auc(roc.obj)[1],
                                    auc.uci95=ci.auc(roc.obj)[3]))
  rocs[[i]]=roc.obj
  i=i+1
}
print(compare, digits=3)
ggroc(rocs)+scale_color_discrete("model", labels=modelnames)+theme_bw()+
  geom_segment(data=data.frame(x=1, y=0, xend=0, yend=1), 
               aes(x=x, y=y, xend=xend, yend=yend), color='black')
ggroc(rocs)+scale_color_discrete("model", labels=modelnames)+theme_bw()+
  geom_segment(data=data.frame(x=1, y=0, xend=0, yend=1), 
               aes(x=x, y=y, xend=xend, yend=yend), color='black')
outdir="/Users/annejones/Documents/blog/blog_r_scripts/customer_churn_analysis/output/"
ggsave(paste(outdir, "churn-roc-std.png", sep=''), height=3, width=4, units="in")


coeff=predict(fit.glmnet$finalModel, type = "coefficients", 
              s=fit.glmnet$bestTune$lambda)
coeff=data.frame(name=coeff@Dimnames[1], value=coeff@x)
colnames(coeff)=c('name','value')
coeff[order(abs(coeff$value), decreasing = T),]
varImp(fit.glmnet)


# plot learning curves
# Need to dummify predictor vars first
dmy = dummyVars(" ~ .", data = train_data[,1:(ncol(train_data)-1)])
dmy.df = data.frame(predict(dmy, newdata = train_data))
dmy.df=cbind(dmy.df, data.frame(Churn=factor(train_data$Churn)))
lcd=learing_curve_dat(dat=dmy.df, 
                      outcome="Churn",method="glmnet",
                      metric=metric, trControl=trainControl(
                        classProbs=T, summaryFunction = twoClassSummary))

ggplot(lcd, aes(x = Training_Size, y = ROC, color = Data)) + 
  geom_smooth(method = loess, span = .8) + 
  theme_bw()
ggsave(paste(outdir, "learning-curve-glmnet.png", sep=''), height=3, width=4, units="in")

lcd.rf=learing_curve_dat(dat=train_data, 
                      outcome="Churn",method="rf",
                      metric=metric, trControl=trainControl(
                        classProbs=T, summaryFunction = twoClassSummary))
ggplot(lcd.rf, aes(x = Training_Size, y = ROC, color = Data)) + 
  geom_smooth(method = loess, span = .8) + 
  theme_bw()
ggsave(paste(outdir, "learning-curve-rf.png", sep=''), height=3, width=4, units="in")


lcd.nb=learing_curve_dat(dat=dmy.df, 
                         outcome="Churn",method="nb",
                         metric=metric, trControl=trainControl(
                           classProbs=T, summaryFunction = twoClassSummary))

ggplot(lcd.nb, aes(x = Training_Size, y = ROC, color = Data)) + 
  geom_smooth(method = loess, span = .8) + 
  theme_bw()
ggsave(paste(outdir, "learning-curve-nb.png", sep=''), height=3, width=4, units="in")
