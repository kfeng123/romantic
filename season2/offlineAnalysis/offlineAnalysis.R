library("forecast")
myTot=read.csv("myTot.csv",header=FALSE)
myTot=myTot[,-4]
names(myTot)=c("report_date","purchase","redeem")
myTot$purchase=as.numeric(myTot$purchase)
myTot$redeem=as.numeric(myTot$redeem)
myTot$report_date=as.integer(myTot$report_date)
myTot$report_date=as.Date(as.character(myTot$report_date),format="%Y%m%d")

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
#周末
xingqi=strftime(myTot$report_date,format="%w")
zhoumo=as.numeric(xingqi%in%c("0","6"))

#月初，每月1号
yuechu=(strftime(myTot$report_date,format="%d")=="01")+0
#########################
sx=ts(myTot$redeem,frequency=7,start=c(1,1))

plot(275:427,myTot$redeem[275:427],type="o")
temp=(strftime(myTot$report_date,format="%d")=="01")
temp2=(1:427)[temp]
abline(v=temp2)
points(1:427,(myTot$redeem*(cycle(sx)==1 +0)),type="p")
points(1:427,(myTot$redeem*jiaqi),type="p",col="red")
points(1:427,(myTot$redeem*buxiu),type="p",col="green")       





#研究残差
##相对误差
myD=data.frame(purchase=sx,seasonaldummy(sx))
myD=myD[275:427,]
lmFit=lm(purchase~S1+S2+S3+S4+S5+S6,data=myD)
temp=ts(1:7,frequency=7,start=c(1,1))
temp=seasonaldummy(temp)
predict(lmFit,data.frame(temp))
hist(residuals(lmFit))
hist(residuals(lmFit)/fitted.values(lmFit))
plot(residuals(lmFit),type="o")
plot(residuals(lmFit)/fitted.values(lmFit),type="l")
temp=(strftime(myTot$report_date,format="%d")=="01")
temp2=(1:427)[temp]
abline(v=temp2-274)
points((residuals(lmFit)/fitted.values(lmFit)*zhoumo[275:427]),type="p")
points((residuals(lmFit)/fitted.values(lmFit)*jiaqi[275:427]),type="p",col="red")
points((residuals(lmFit)/fitted.values(lmFit)*buxiu[275:427]),type="p",col="green")       


plot(275:427,stl(ts(residuals(lmFit),frequency=7,start=c(1,1)),s.window="period")$time.series[,2],type="l")
abline(v=temp2)

myD=data.frame(purchase=sx,seasonaldummy(sx),yuechu,jiaqi,buxiu)
myD=myD[275:427,]
lmFit=lm(purchase~.,myD)

temp=ts(1:30,frequency=7,start=c(1,1))
Pseason=seasonaldummy(temp)

toP=data.frame(Pseason,yuechu=(1:30%in%1) +0,jiaqi=(1:30%in%8) +0,buxiu=(1:30%in%28) +0)
plot(predict(lmFit,toP),type="o")


tuhao=read.csv("tuhao.csv",header=FALSE)
tuhao=tuhao[,c(2,9)]
names(tuhao)=c("report_date","redeem")
tuhao$report_date=as.Date(as.character(tuhao$report_date),format="%Y%m%d")
a=rep(0,427-275+1)
for (i in 1:length(tuhao$report_date)){
        temp=as.integer(tuhao$report_date[i]-as.Date("20140401",format="%Y%m%d")+1)
        a[temp]=a[temp]+tuhao$redeem[i]
}
plot(275:427,myTot$redeem[275:427]-a,type="l")       

plot(myTot$redeem/redeemNum[,2],type="l")





dayu20k=read.csv("dayu20k.csv",header=FALSE)
plot(dayu20k[,2],type="l")
plot(myTot$redeem[275:427]-dayu20k[,2],type="l")
plot(myTot$redeem[275:427],type="l")



(myTot$redeem[279]+myTot$redeem[335])/2
(myTot$redeem[280]+myTot$redeem[336])/2
(myTot$redeem[281]+myTot$redeem[337])/2


