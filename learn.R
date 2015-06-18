?confusionMatrix
?findCorrelation
?varImp
?rfe
#模型比较
?resamples
?reshape
?scale
?smoothScatter
?cancor


#!!!!!!!!!
?ts
?decompose
#decompose先去掉趋势项，再拟合季节项
?stl
#stl先拟合季节项再去掉趋势项
###我觉得这两者都不好啊！！！为什么不能相互迭代呢？先拟合季节项，然后从残差中学习趋势项，在去掉趋势项，在残差中重新学习季节项，依次下去

?forecast

?rstudent


memory.limit()
memory.size(F)
memory.size(T)


library(date)



library(nutshell)


library(TSA)
win.graph(width=4.875, height=3,pointsize=8)
 data(ar1.s); plot(ar1.s,ylab=expression(Y[t]),type='o')


library(tseries)

?tsdiag

















library(TSA)
data(airmiles)
air.m1=arimax(log(airmiles),order=c(0,1,1),
              seasonal=list(order=c(0,1,1),period=12),
              xtransf=data.frame(I911=1*(seq(airmiles)==69),
                                 I911=1*(seq(airmiles)==69)),transfer=list(c(0,0),c(1,0)),
              xreg=data.frame(Dec96=1*(seq(airmiles)==12),
                              Jan97=1*(seq(airmiles)==13),Dec02=1*(seq(airmiles)==84)),
              method='ML')














?seasadj
?filter
?rstandard

data(color)
m1.color=arima(color,order=c(1,0,0))
m1.color
plot(rstandard(m1.color),ylab ='Standardized Residuals',type='o');
abline(h=0)








library(TSA)
data(CREF)
plot(CREF)

?adf.test
?pp.test

library(TSTutorial)

sx=ts(myTot$total_purchase_amt,frequency=7,start=c(1,1))
data(Turismes)
TSTutorial(Turismes,student=TRUE,report=list(report=FALSE))






set.seed(1235678)
garch01.sim=garch.sim(alpha=c(.01,.9),n=500)
plot(garch01.sim,type='l',ylab=expression(r[t]), xlab='t')

install.packages("fGarch")

library(TSA)
data(oil.price)

m1.oil=arima(log(oil.price),order=c(0,1,1))
acf(abs(rstandard(m1.oil)))
pacf(abs(rstandard(m1.oil)))
eacf(abs(rstandard(m1.oil)))
acf(rstandard(m1.oil)^2)
pacf(rstandard(m1.oil)^2)
eacf(rstandard(m1.oil)^2)
m2=garchFit(formula=~arma(0,1)+garch(1,1),data=diff(log(oil.price)),include.mean=F)
plot((residuals(m2)-mean(residuals(m2)))/sd(residuals(m2)),type="b")
plot(residuals(m2),type="b")


install.packages("perARMA")
library(perARMA)

install.packages("seas")
library(seas)
library(help="seas")

install.packages("tpr")
library(tpr)
library(help="tpr")

install.packages("dynlm")
library(dynlm)

data("AirPassengers", package = "datasets")
ap <- log(AirPassengers)
ap_fm <- dynlm(ap ~ trend(ap) + season(ap))
summary(ap_fm)
plot(AirPassengers)
plot(ap_fm$residuals)
plot(predict(ap_fm))
