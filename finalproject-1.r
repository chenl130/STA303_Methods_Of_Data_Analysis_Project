#read dataset
data=read.csv("/Users/liangchen/Desktop/diabetes_update.csv",stringsAsFactors =FALSE)
#dechotomize response variable
data$readmitted
data$readmitted[data$readmitted=="NO"]=0
data$readmitted[data$readmitted==">30"]=1
data$readmitted[data$readmitted=="<30"]=1
data$readmitted

#general information of given dataset
data1=data[,-c(7,12,13)]
summary(data1)
hist(as.numeric(data1$readmitted))
barplot(prop.table(table(data1$gender)))
barplot(prop.table(table(data1$race)))
barplot(prop.table(table(data1$age)))

library(tidyverse)
#set seed with student number
set.seed(1005735126)
#random select 20000 observations from dataset
aa=sample(data$patient_nbr, size =20000,replace=FALSE)
aa
test <- data[data$patient_nbr %in% aa,]
test
train <- data[!data$patient_nbr %in% aa,]
train

#chi-square test to check which dummy variables can omit 
#p-value of age<0.05
chisq.test(data$readmitted,data$age)
#p-value of race<0.05
chisq.test(data$readmitted,data$race)
#p-value of repaglinide<0.05
chisq.test(data$readmitted,data$repaglinide)
#p-value of nateglinide>0.05
chisq.test(data$readmitted,data$nateglinide)
#p-value of chlorpropamide >0.05
chisq.test(data$readmitted,data$chlorpropamide)
#p-value of glimepiride>0.05
chisq.test(data$readmitted,data$glimepiride)
#p-value of acetohexamide>0.05
chisq.test(data$readmitted,data$acetohexamide)
#won't consider variables with most values being N/A, can directly remove them 
train=train[,-c(7,12,13)]
#remove missing values
train=na.omit(train)

hist(as.numeric(train$readmitted))
barplot(prop.table(table(train$gender)))


#fit glm model with chosen dummy variables and continuous variables
model1 <- glm(as.numeric(train$readmitted)~ race+ age + gender  +Length.of.Stay+num_lab_procedures+num_procedures+num_medications+number_outpatient+number_emergency+number_inpatient+number_diagnoses+repaglinide+insulin, family=binomial, data=train); 
summary(model1)
library(MASS)
#stepwise with BIC
m1<-stepAIC(model1, direction = c("both"),trace = 1,k=log(20000))
summary(m1)

#fit glmm models 
library(lme4)
#mixed model 2 m2
m2 = glmer(as.numeric(readmitted) ~ 1 +(1|patient_nbr), data =train, family=binomial)
summary(m2)
#mixed model 3 m3
m3 = glmer(as.numeric(readmitted)~ race+ age + gender+Length.of.Stay+num_lab_procedures+num_procedures+number_outpatient+number_emergency+number_inpatient+number_diagnoses+repaglinide+insulin+(1|patient_nbr), family=binomial, data=train)
summary(m3)
#compare fix effect of the two nested models to choose a better glmm model
anova(m3,m2)

#model diagnostics
library(ggplot2)
#residual plot
resid = residuals(m1)
fitted = fitted(m1)
plot(fitted,resid)
#qq plot
qqnorm(resid)
qqline(resid)

#create tibble
dd <- tibble(resid = residuals(m1), fitted = fitted(m1))
plot1 <- dd %>% ggplot(aes(sample=resid)) + stat_qq()
plot2 <- dd %>% ggplot(aes(x=fitted,y=resid)) + geom_point(alpha=0.3) +geom_hline(yintercept=0) + labs(x="Fitted", y="Residuals")
plot1
plot2

#internal validation using cross-validation
library(rms)
lrm.final <- lrm(as.numeric(readmitted) ~ age + num_lab_procedures + 
                   num_procedures + number_outpatient + number_emergency + number_inpatient + 
                   number_diagnoses + insulin, data = train, x=TRUE, y=TRUE, model=T)
#cross validation
cross.calib <- calibrate(lrm.final, method="crossvalidation", B=10)
plot(cross.calib, las=1, xlab = "Prediction Probability")

#ROC curve
library(pROC)
pred.prob <- predict(m1, type = "response")
pred.prob
roc_logit <- roc(train$readmitted~pred.prob, na.action = "na.pass")
## The True Positive Rate ##
TPR <- roc_logit$sensitivities
## The False Positive Rate ##
FPR <- 1 - roc_logit$specificities
plot(FPR, TPR, xlim = c(0,1), ylim = c(0,1), type = 'l', lty = 1, lwd = 2,col = 'red') 
abline(a = 0, b = 1, lty = 2, col = 'blue')
text(0.7,0.4,label = paste("AUC = ", round(auc(roc_logit),2)))
#AUC of the model we finally chose
auc(roc_logit)

#Perform prediction on test data to check validation of model
pred.prob <- predict(lrm.final, newdata = train, type = "fitted") 
test$pred.prob <- predict(model1, newdata = test, type = "response",na.rm=TRUE) 
deciles <- quantile(test$pred.prob, probs = seq(0,1, by =0.1),na.rm=TRUE) 

test$decile <- findInterval(test$pred.prob, deciles, rightmost.closed = T)
pred.prob <- tapply(test$pred.prob, test$decile, mean)
obs.prob <- tapply(as.numeric(test$readmitted), test$decile, mean)
## The plot ##
par(family = 'serif')
plot(pred.prob, obs.prob, type = "l", ylab = "Observed", xlab = "Predicted", xlim = c(0,1), ylim = c(0,1)) 
abline(a=0, b=1)


