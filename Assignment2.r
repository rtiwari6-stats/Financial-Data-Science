setwd("~/UW/CFRM-502/Assignment2/")

#question 1
library(quantmod)
getSymbols(c("AMRMX","IWD","IWF", "IWO","VONG"), from="2011-01-03", 
           to="2016-12-29", src="yahoo")
dat <- na.locf(merge(AMRMX$AMRMX.Adjusted,IWD$IWD.Adjusted,IWF$IWF.Adjusted,
                     IWO$IWO.Adjusted,VONG$VONG.Adjusted))[index(AMRMX)]

dat = data.frame(date=index(dat), coredata(dat))


#Question 1.a
# compute daily log returns
dat$AMRMX_logreturns = c(diff(log(dat$AMRMX.Adjusted)),0)
dat$IWD_logreturns = c(diff(log(dat$IWD.Adjusted)),0)
dat$IWF_logreturns = c(diff(log(dat$IWF.Adjusted)),0)
dat$IWO_logreturns = c(diff(log(dat$IWO.Adjusted)), 0)
dat$VONG_logreturns = c(diff(log(dat$VONG.Adjusted)), 0)

#Scatter plots and correlation
library("corrplot")
logreturns = subset(dat, select=c("AMRMX_logreturns", "IWD_logreturns", "IWF_logreturns",
                                  "IWO_logreturns", "VONG_logreturns"))
corr_matrix = cor(logreturns)
corrplot(corr_matrix, method = "number")
pairs(logreturns, pch=19)

#question 1.b
lm_model = lm(AMRMX_logreturns ~., logreturns)
summary(lm_model)
#correlation between fitted and observed values
cor(dat$AMRMX_logreturns, fitted(lm_model))

#question 1.c
lm_model_aov = anova(lm_model)
lm_model_aov
sum(lm_model_aov$`Sum Sq`) - lm_model_aov$

#question 1.d
lm_model_reduced = lm(AMRMX_logreturns ~ 0 + IWD_logreturns + IWF_logreturns, logreturns)
summary(lm_model_reduced)
anova(lm_model_reduced, lm_model)

#question 1.e
#single ETF
lm_1 = lm(AMRMX_logreturns ~ IWD_logreturns, logreturns)
summary(lm_1)
lm_2 = lm(AMRMX_logreturns ~ IWF_logreturns, logreturns)
summary(lm_2)
lm_3 = lm(AMRMX_logreturns ~ IWO_logreturns, logreturns)
summary(lm_3)
lm_4 = lm(AMRMX_logreturns ~ VONG_logreturns, logreturns)
summary(lm_4)

#2 ETFs
lm_5 = lm(AMRMX_logreturns ~ IWD_logreturns + IWF_logreturns, logreturns)
summary(lm_5)
lm_6 = lm(AMRMX_logreturns ~ IWD_logreturns + IWO_logreturns, logreturns)
summary(lm_6)
lm_7 = lm(AMRMX_logreturns ~ IWD_logreturns + VONG_logreturns, logreturns)
summary(lm_7)
lm_8 = lm(AMRMX_logreturns ~ IWF_logreturns + IWO_logreturns, logreturns)
summary(lm_8)
lm_9 = lm(AMRMX_logreturns ~ IWF_logreturns + VONG_logreturns, logreturns)
summary(lm_9)
lm_10 = lm(AMRMX_logreturns ~ IWO_logreturns + VONG_logreturns, logreturns)
summary(lm_10)

#3 ETFs
lm_11 = lm(AMRMX_logreturns ~ IWD_logreturns + IWF_logreturns + IWO_logreturns, logreturns)
summary(lm_11)
lm_12 = lm(AMRMX_logreturns ~ IWD_logreturns + IWO_logreturns + VONG_logreturns, logreturns)
summary(lm_12)
lm_13 = lm(AMRMX_logreturns ~ IWD_logreturns + IWF_logreturns + VONG_logreturns, logreturns)
summary(lm_13)
lm_14 = lm(AMRMX_logreturns ~ IWF_logreturns + IWO_logreturns + VONG_logreturns, logreturns)
summary(lm_14)

#question 1.f
BIC(lm_1)
BIC(lm_2)
BIC(lm_3)
BIC(lm_4)
BIC(lm_5)
BIC(lm_6)
BIC(lm_7)
BIC(lm_8)
BIC(lm_9)
BIC(lm_10)
BIC(lm_11)
BIC(lm_12)
BIC(lm_13)
BIC(lm_14)
BIC(lm_model)

library(leaps)
m = regsubsets(AMRMX_logreturns ~., logreturns, nvmax=5, method = "seqrep")
m
which.min(summary(m)$bic) #model 11

#question 1.g
library(car)
vif(lm_model)
vif(lm_11)

#question 2
dat = read.table("HW2-data.txt",header=TRUE)
dat =dat[order(dat$T),]
EMP = -diff(log(dat$price))/diff(dat$T) 
MAT = dat$T[-1]

plot(MAT, EMP)

#question 2.a
emp_mat_lm = lm(EMP ~ MAT)
summary(emp_mat_lm)
plot(EMP ~ MAT)
lines(sort(MAT), fitted(emp_mat_lm)[order(MAT)])

#question 2.b
plot(emp_mat_lm, 5)
plot(emp_mat_lm, 4)

#question 2.c
plot(emp_mat_lm, 1)
plot(emp_mat_lm, 3)
plot(emp_mat_lm, 2)

#question 2.d
emp_mat_poly_lm = lm(EMP ~ MAT + I(MAT ^ 2) + I(MAT ^ 3))
summary(emp_mat_poly_lm)
plot(EMP ~ MAT)
lines(sort(MAT), fitted(emp_mat_poly_lm)[order(MAT)])

#question 2.e
plot(emp_mat_poly_lm, 5)
plot(emp_mat_poly_lm, 4)

plot(emp_mat_poly_lm, 1)
plot(emp_mat_poly_lm, 3)
plot(emp_mat_poly_lm, 2)

#question 2.f
vif(emp_mat_poly_lm)
