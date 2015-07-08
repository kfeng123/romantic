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
#########################

plot(myTot$purchase,type="l")
points((myTot$purchase*zhoumo),type="p")
#points((myTot$total_redeem_amt*((myTot$report_date=="2013-11-11")*1)),type="p",col="red")
points((myTot$purchase*jiaqi),type="p",col="red")
points((myTot$purchase*buxiu),type="p",col="green")

sx=ts(myTot$purchase,frequency=7,start=c(1,1))
#autoFit=auto.arima(sx,d=0,D=1,trace=TRUE)
#plot(forecast(autoFit,h=30)$residual)
##points((forecast(autoFit,h=30)$residual*jiaqi*(1-zhoumo)),type="p")
#points(forecast(autoFit,h=30)$residual*jiaqi*zhoumo,type="p",col="blue")
#points(forecast(autoFit,h=30)$residual*buxiu,type="p",col="red")
myStl=stl(sx,s.window="periodic",robust=TRUE)
plot(forecast(myStl,h=30))
