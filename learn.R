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





stlf(sx,h=30,s.window="periodic",robust=TRUE)$mean

temp=stl(sx,s.window="periodic",robust=TRUE)
forecast(temp,h=30)$mean
s=temp$time.series[,1]
t=sx-s
plot(forecast(s,h=30))
plot(forecast(t,h=30))
forecast(s,h=30)$mean+forecast(t,h=30)$mean



Temp=weekdays(myTot$report_date)%in%c("星期六","星期日")+0
plot(244:427,myTot$consume_amt[244:427],type="l")
temp=(strftime(myTot$report_date,format="%d")=="01")
temp2=(1:427)[temp]
abline(v=temp2)
points(1:427,(myTot$consume_amt*Temp),type="p")
points(1:427,(myTot$consume_amt*jiaqi*(1-Temp)),type="p",col="red")
points(1:427,(myTot$consume_amt*buxiu),type="p",col="green")



temp1=read.csv("result/naive.csv",header=FALSE)
temp2=read.csv("result/eighteenth.csv",header=FALSE)

plot(temp1[,2],type="l")
points(temp2[,2],type="l",col="red")
























myTot=TT
Tot=myTot[244:427,]
for(i in 2:17){
        plot(Tot[,i],type="l",ylab=i)
}

total_Pred=rep(0,17*30)
dim(total_Pred)=c(30,17)
for(i in 2:17){
        sx=ts(Tot[,i],frequency=7,start=c(1,1))
        myStl=stl(sx,s.window="periodic",robust=TRUE)
        myPred=forecast(myStl,h=30)
        plot(myPred)
        total_Pred[,i]=myPred$mean
}
total_Pred=as.data.frame(total_Pred)
names(total_Pred)=names(Tot)

temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
sep2=format(sep,format="%Y%m%d")

total_Pred$report_date=sep2

P=total_Pred$purchase_bal_amt+total_Pred$purchase_bank_amt+total_Pred$share_amt
R=total_Pred$tftobal_amt+total_Pred$tftocard_amt+total_Pred$category1+total_Pred$category2+total_Pred$category3+total_Pred$category4

out=data.frame(sep2,P,R)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])
write.table(out,"newResult/eighth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)








myTot=TT

#############假期特征
jiaqitemp=c("2013-09-19","2013-09-20","2013-09-21","2013-10-01","2013-10-02","2013-10-03","2013-10-04","2013-10-05",
            "2013-10-06","2013-10-07","2014-01-01","2014-01-31","2014-02-01","2014-02-02","2014-02-03","2014-02-04",
            "2014-02-05","2014-02-06","2014-04-05","2014-04-06","2014-04-07","2014-05-01","2014-05-02","2014-05-03",
            "2014-05-31","2014-06-01","2014-06-02","2014-09-06","2014-09-07","2014-09-08")
jiaqitemp=as.Date(jiaqitemp)

jiaqi=myTot$report_date %in% jiaqitemp+0
temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
yuceJiaqi=sep %in% jiaqitemp+0

buxiutemp=c("2013-09-22","2013-09-29","2013-10-12","2014-01-26","2014-02-08","2014-05-04","2014-09-28")
buxiutemp=as.Date(buxiutemp)
buxiu=myTot$report_date %in% buxiutemp+0
#########################
temp=weekdays(myTot$report_date)%in%c("星期六","星期日")+0
plot(myTot$direct_purchase_amt,type="l")
points((myTot$direct_purchase_amt*temp),type="p")
#points((myTot$total_redeem_amt*((myTot$report_date=="2013-11-11")*1)),type="p",col="red")
points((myTot$direct_purchase_amt*jiaqi),type="p",col="red")
points((myTot$direct_purchase_amt*buxiu),type="p",col="green")

sx=ts(myTot$direct_purchase_amt,frequency=7,start=c(1,1))
autoFit=auto.arima(sx,d=0,D=1,trace=TRUE)
#ARIMA(1,0,0)(0,1,2)
plot(forecast(autoFit,h=30)$residual)
points((forecast(autoFit,h=30)$residual*jiaqi*(1-temp)),type="p")
points(forecast(autoFit,h=30)$residual*jiaqi*temp,type="p",col="blue")

points(forecast(autoFit,h=30)$residual*buxiu,type="p",col="red")

##非周末假期的偏移
jiaqiBias=sum(forecast(autoFit,h=30)$residual*jiaqi*(1-temp))/sum(jiaqi*(1-temp))
##补休的偏移
buxiuBias=sum(forecast(autoFit,h=30)$residual*buxiu)/sum(buxiu)
##############
direct_purchase_amt_1=as.numeric(forecast(autoFit,h=30)$mean)
direct_purchase_amt_1[8]=direct_purchase_amt_1[8]+jiaqiBias
direct_purchase_amt_1[28]=direct_purchase_amt_1[28]+buxiuBias



plot(myTot$total_redeem_amt,type="l")
points((myTot$total_redeem_amt*temp),type="p")
#points((myTot$total_redeem_amt*((myTot$report_date=="2013-11-11")*1)),type="p",col="red")
points((myTot$total_redeem_amt*jiaqi),type="p",col="red")
points((myTot$total_redeem_amt*buxiu),type="p",col="green")

sx=ts(myTot$total_redeem_amt,frequency=7,start=c(1,1))
autoFit=auto.arima(sx,d=0,D=1,trace=TRUE)
#ARIMA(1,0,2)(0,1,1) with drift
plot(forecast(autoFit,h=30)$residual)
points((forecast(autoFit,h=30)$residual*jiaqi*(1-temp)),type="p")
points(forecast(autoFit,h=30)$residual*jiaqi*temp,type="p",col="blue")

points(forecast(autoFit,h=30)$residual*buxiu,type="p",col="red")

##非周末假期的偏
jiaqiBias=sum(forecast(autoFit,h=30)$residual*jiaqi*(1-temp))/sum(jiaqi*(1-temp))
##补休的偏移
buxiuBias=sum(forecast(autoFit,h=30)$residual*buxiu)/sum(buxiu)
###################
total_redeem_amt_1=as.numeric(forecast(autoFit,h=30)$mean)
total_redeem_amt_1[8]=total_redeem_amt_1[8]+jiaqiBias
total_redeem_amt_1[28]=total_redeem_amt_1[28]+buxiuBias


###最后预测收益。。。

sx=ts(myTot$share_amt,frequency=7,start=c(1,1))
autoFit=auto.arima(sx,d=1,D=1,trace=TRUE)
#ARIMA(1,1,1)(0,1,1)
share_amt=as.numeric(forecast(autoFit,h=30)$mean)


P=direct_purchase_amt_1+share_amt
R=total_redeem_amt_1


temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,P,R)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])

write.table(out,"newResult/eighteenth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)


plot(eighteenth[,2],type="o",col="blue")
points(out[,2],type="o",col="red")












sx=ts(myTot$total_purchase_amt[244:427],frequency=7,start=c(1,1))
plot(244:427,sx,type="l")
myStl=stl(sx,s.window="periodic",robust=TRUE)
plot(forecast(myStl,h=30))
myPurchase=as.numeric(forecast(myStl,h=30)$mean)


sx=ts(myTot$transfer_amt[244:427],frequency=7,start=c(1,1))
myStl=stl(sx,s.window="periodic",robust=TRUE)
f1=forecast(myStl,h=30)$mean



sx=ts(myTot$consume_amt[244:427],frequency=7,start=c(1,1))
myStl=stl(sx,s.window="periodic",robust=TRUE)
f2=forecast(myStl,h=30)$mean

myRedeem=as.numeric(f1)+as.numeric(f2)




temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,myPurchase,myRedeem)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])

write.table(out,"newResult/stl.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)

