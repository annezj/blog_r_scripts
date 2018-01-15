# Adventures in survival analysis using R
# Survival analysis is an interesting topic of machine learning because it's applicable across
# any domain where "time to event" is of interest - from human or animal mortality to 
# customer churn and reliability of engineering components.
# I always think it's fun to see how the same underlying mathematics can be used for diverse applications.
# In this post I'm going to use the same analysis methods on three freely available datasets.
# Resources:
# Great tutorial on churn analysis: http://daynebatten.com/2015/02/customer-churn-survival-analysis/
# Note on survminer package for prettier visualisations : https://cran.r-project.org/web/packages/survminer/vignettes/Informative_Survival_Plots.html
# Kaplan Meier simple examples: http://www.itl.nist.gov/div898/handbook/apr/section2/apr215.htm and http://www.pmean.com/08/SimpleKm.html
# Datasets:
# Telco customer churn data from IBM https://www.ibm.com/communities/analytics/watson-analytics-blog/predictive-insights-in-the-telco-customer-churn-data-set/
# Additional data from https://www.knime.org/knime-applications/churn-prediction
# Employee attrition data from http://www.scan-support.com/help/sample-data-sets
# Divorce data from http://data.princeton.edu/wws509/datasets/#divorce
# 
#
# Advantages of fitting Kaplan Meier:
# - non-parametric; it is fitted empirically
# Disadvantages:
# - exact times are required
#
#
rm(list=ls())
library(data.table)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)


# load and explore the data

datadir="/Users/annejones/Documents/blog/datasets/churn/"
outdir="/Users/annejones/Documents/blog/blog_r_scripts/customer_churn_analysis/output/"
#
# 1. The TELCO customer churn data
# The variables relevant to the survival analysis are "Churn" and tenure.
# There are 18 other covariates describing the customer
df.tel=fread(paste(datadir,"WA_Fn-UseC_-Telco-Customer-Chur-Table 1.csv",sep=''), data.table=F)

#
# Basic analysis using the survival package
#
# Create a "survival object" for each observation
df.tel$survival <- Surv(df.tel$tenure, df.tel$Churn == "Yes")
# Fit a basic survival curve using the data
fit <- survfit(survival ~ 1, data = df.tel)

# Plot the survival curve
plot(fit, lty = 1, mark.time = FALSE, ylim=c(.75,1), xlab = 'Tenure', ylab = 'Percent remaining')
# prettier plotting
ggsurvplot(fit, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.8,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')

# Now look at covariates
fit1 <- survfit(survival ~ gender, data = df.tel)
ggsurvplot(fit1, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.3,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')
fit2 <- survfit(survival ~ SeniorCitizen, data = df.tel)
ggsurvplot(fit2, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.3,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')
fit3 <- survfit(survival ~ InternetService, data = df.tel)
ggsurvplot(fit3, data=df.tel, risk.table=T, conf.int = T, 
           break.time.by=12, ggtheme=theme_bw(),
           censor.shape=124, conf.int.alpha=0.3,
           ylim=c(0.5,1.0),xlab='Tenure (months)',ylab='Probability of remaining')


#
# Use Log Rank test to compare curves for groups
# p-value is probability of obtaining these results if the two cuves were from same population
#
survdiff(survival ~ gender, data = df.tel)
survdiff(survival ~ SeniorCitizen, data = df.tel)
survdiff(survival ~ InternetService, data = df.tel)

# Now start to model multiple predictors
# using regression for Cox proportional hazards
# which assumes constant churn across different categories
fit.cox<-coxph(survival~gender+SeniorCitizen, data=df.tel)
# inspect fit for significance and coefficients
# for churn relative to base
fit.cox

# now test for non-proportional hazards
# p values gives statistical signifiance of non-proportional element
# (probability of getting result if hazards were purely proportional)
# if p<pthresh cannot use simple proportional hazards model
cox.zph(fit.cox)

# Quantify churn with restricted mean survival time (RMST)
# take mean time to churn with upper time limite (e.g.1 year)
# for censored data, need to estimate RMST = area under survival curve
# rmean is the RMST
# here look at first 12 months
print(fit1, print.rmean=getOption("survfit.print.rmean"), rmean = 12)
print(fit2, print.rmean=getOption("survfit.print.rmean"), rmean = 12)
print(fit3, print.rmean=getOption("survfit.print.rmean"), rmean = 12)

# analysis with pseudo observations - for up to 10,000 obs
library(pseudo)
df.tel$pseudos <- pseudomean(df.tel$tenure, df.tel$Churn=="Yes", 12)
df.tel$id=1:nrow(df.tel)
# fit a regression model
# coefficients show impact on RMST over period
# also included the effect of continous in addition to categorical variables
summary(fit <- geese(pseudos ~ gender + SeniorCitizen,
                     data = df.tel, id = id, jack = TRUE, family = gaussian,
                     corstr = "independence", scale.fix = FALSE))


