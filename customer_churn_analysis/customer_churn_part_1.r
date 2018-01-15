library(data.table)
library(survival)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
rm(list=ls())
# Load the TELCO customer churn data from my .csv file conversion here:
df.tel=fread("/Users/annejones/Documents/blog/datasets/churn/WA_Fn-UseC_-Telco-Customer-Chur-Table 1.csv", data.table=F)
head(df.tel)


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
df.vars$TotalCharges=as.numeric(as.character(df.vars$tenure))

# Check for missing values - no, we're all clear
sum(is.na(df.vars))

df.tel[,2:20]=df.vars


# Plot a summary of each variable, first the numeric ones and then the categorical ones
df.tel %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(x=value,fill=key)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()+theme_bw()

df.tel %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(x=value,fill=key)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

# fit survival curves


df.tel$survival <- Surv(df.tel$tenure, df.tel$Churn == "Yes")
fit <- survfit(survival ~ 1, data = df.tel)

png(paste(outdir, "churn-surv1.png", sep=''), height=6, width=6, units="in", res=300)
ggsurvplot(fit, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.8,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')
dev.off()


fit2 <- survfit(survival ~ SeniorCitizen, data = df.tel)
png(paste(outdir, "churn-surv2.png", sep=''), height=6, width=6, units="in", res=300)
ggsurvplot(fit2, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.3,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')
dev.off()


fit.cox<-coxph(survival~SeniorCitizen, data=df.tel)
fit.cox
cox.zph(fit.cox)


png(paste(outdir, "cox-zph-SC.png", sep=''), height=6, width=6, units="in", res=300)
plot(cox.zph(fit.cox))
dev.off()


# Consider each categorical covariate in turn
vars.cat=colnames(df.vars %>%keep(is.factor))
log.rank.cat=data.frame()
for(icat in 1:length(vars.cat))
{
  diff=survdiff(formula = as.formula(paste('survival ~' , vars.cat[icat], sep='')), data = df.tel)
  p=pchisq(diff$chisq, df=(length(unique(df.tel[,vars.cat[icat]])))-1, lower.tail=F)
  tmp=data.frame(variable=vars.cat[icat], chisq=diff$chisq, p=p)
  log.rank.cat=rbind(log.rank.cat, tmp)
}
log.rank.cat[order(log.rank.cat$p, decreasing = T),]

# Contract has the biggest effect
fit3 <- survfit(survival ~ Contract, data = df.tel)
png(paste(outdir, "churn-surv3.png", sep=''), height=6, width=10, units="in", res=300)
ggsurvplot(fit3, data=df.tel, risk.table=F, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.3,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')
dev.off()

# Numerical vars (only one is valid)
coxph(formula = survival ~ MonthlyCharges, data = df.tel)

# Conditional probability plot
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