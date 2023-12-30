setwd("~/UW/CFRM-502/Assignment1")

customlag = function(x,n) {
  c(rep(NA,n),head(x,-n))
}

#read the input file
wip = read.csv("TCB_MZTT_1959_2020.csv", header = T)
pdf(file='plots.pdf')
sink('output.txt')
par(mfrow=c(2,2))

#add other columns
wip$DUE = wip$A0M043- customlag(wip$A0M043, 1)

wip$LEI = wip$G0M910
wip$LLEI = log(wip$G0M910)
wip$L1LEI = customlag(wip$LLEI, 1)
wip$DLLEI = wip$LLEI-wip$L1LEI
wip$L1LEI = customlag(wip$DLLEI, 1)
wip$L6LEI = customlag(wip$DLLEI, 6)
wip$L12LEI = customlag(wip$DLLEI, 12)

wip$WKUCL = wip$A0M005
wip$LWKUCL = log(wip$A0M005)
wip$L1LWKUCL = customlag(wip$LWKUCL, 1)
wip$DLWKUCL = wip$LWKUCL-wip$L1LWKUCL
wip$L1WKUCL = customlag(wip$DLWKUCL, 1)
wip$L6WKUCL = customlag(wip$DLWKUCL, 6)
wip$L12WKUCL = customlag(wip$DLWKUCL, 12)

#simple linear regressions

#LEI models
LEI_m = lm(DUE ~ LEI, data=wip)
summary(LEI_m)
plot(LEI_m, main = "DUE~LEI")

DLLEI_m = lm(DUE ~ DLLEI, data=wip)
summary(DLLEI_m)
plot(DLLEI_m, main = "DUE~DLLEI")

L1LEI_m = lm(DUE ~ DLLEI + L1LEI, data=wip)
summary(L1LEI_m)
plot(L1LEI_m, main = "DUE~DLLEI + L1LEI")

L6LEI_m = lm(DUE ~ DLLEI + L6LEI, data=wip)
summary(L6LEI_m)
plot(L6LEI_m, main = "DUE~DLLEI + L6LEI")

L12LEI_m = lm(DUE ~ DLLEI + L12LEI, data=wip)
summary(L12LEI_m)
plot(L12LEI_m, main = "DUE~DLLEI + L12LEI")

#WKCUL models
WKUCL_m = lm(DUE ~ WKUCL, data=wip)
summary(WKUCL_m)
plot(WKUCL_m, main = "DUE~WKUCL")

DLWKUCL_m = lm(DUE ~ DLWKUCL, data=wip)
summary(DLWKUCL_m)
plot(DLWKUCL_m, main = "DUE~DLWKUCL")

L1WKUCL_m = lm(DUE ~ DLWKUCL + L1WKUCL, data=wip)
summary(L1WKUCL_m)
plot(L1WKUCL_m, main = "DUE~DLWKUCL + L1WKUCL")

L6WKUCL_m = lm(DUE ~ DLWKUCL + L6WKUCL, data=wip)
summary(L6WKUCL_m)
plot(L6WKUCL_m, main = "DUE~DLWKUCL + L6WKUC")

L12WKUCL_m = lm(DUE ~ DLWKUCL + L12WKUCL, data=wip)
summary(L12WKUCL_m)
plot(L12WKUCL_m, main = "DUE~DLWKUCL + L12WKUC")

#multiple regression
mlr = lm(DUE ~ L1LEI + L1WKUCL, data = wip)
summary(mlr)
plot(mlr, main = "DUE~L1LEI+L1WKUCL")

sink()
dev.off()
