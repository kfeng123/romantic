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
xingqi=format(myTot$report_date,format="%w")
zhoumo=as.numeric(xingqi%in%c("0","6"))

#月初，每月1号
yuechu=(strftime(myTot$report_date,format="%d")=="01")+0
#########################
sx=ts(myTot$purchase,frequency=7,start=c(1,1))

plot(1:200,myTot$purchase[1:200],type="l")
temp=(strftime(myTot$report_date,format="%d")=="01")
temp2=(1:427)[temp]
abline(v=temp2)
points(1:427,(myTot$purchase*(cycle(sx)==1 +0)),type="p")
points(1:427,(myTot$purchase*jiaqi),type="p",col="red")
points(1:427,(myTot$purchase*buxiu),type="p",col="green")       


new_Tot$purchase=new_Tot$total_purchase_amt
plot(1:427,new_Tot$purchase[1:427],type="o")
temp=(strftime(new_Tot$report_date,format="%d")=="01")
temp2=(1:427)[temp]
abline(v=temp2)
points(1:427,(new_Tot$purchase*(cycle(sx)==1 +0)),type="p")
points(1:427,(new_Tot$purchase*jiaqi),type="p",col="red")
points(1:427,(new_Tot$purchase*buxiu),type="p",col="green")  



#研究残差
##相对误差

myD=data.frame(purchase=sx,seasonaldummy(sx))
myD=myD[275:427,]

lmFit=lm(purchase~S1+S2+S3+S4+S5+S6,data=myD)
temp=ts(1:7,frequency=7,start=c(1,1))
temp=seasonaldummy(temp)
plot(residuals(lmFit),type="o")
temp=(strftime(myTot$report_date,format="%d")=="01")
temp2=(1:427)[temp]
abline(v=temp2-274)
points((residuals(lmFit)*zhoumo[275:427]),type="p")
points((residuals(lmFit)*jiaqi[275:427]),type="p",col="red")
points((residuals(lmFit)*buxiu[275:427]),type="p",col="green")       




###purchase模型
sx=ts(myTot$purchase,frequency=7,start=c(1,1))
myD=data.frame(purchase=sx,seasonaldummy(sx),yuechu,buxiu)
myD$jiaqiN=jiaqi*(1-zhoumo)
myD$jiaqi6=(1:427 %in% c(279,335))+0
myD$jiaqi7=(1:427 %in% c(280,336))+0
myD=myD[275:427,]
lmFit=lm(purchase~.,myD)
plot(residuals(lmFit),type="o")
temp=(strftime(myTot$report_date,format="%d")=="01")
temp2=(1:427)[temp]
abline(v=temp2-274)
points((residuals(lmFit)*zhoumo[275:427]),type="p")
points((residuals(lmFit)*jiaqi[275:427]),type="p",col="red")
points((residuals(lmFit)*buxiu[275:427]),type="p",col="green")       


temp=ts(1:30,frequency=7,start=c(1,1))
Pseason=seasonaldummy(temp)
toP=data.frame(Pseason,yuechu=(1:30%in%1) +0,jiaqi6=(1:30%in%6)+0,jiaqi7=(1:30%in%7)+0,jiaqiN=(1:30%in%8) +0,buxiu=(1:30%in%28) +0)
plot(predict(lmFit,toP),type="o")



#redeem模型
sx=ts(myTot$redeem,frequency=7,start=c(1,1))
myD=data.frame(redeem=sx,seasonaldummy(sx),buxiu)
myD$jiaqiN=jiaqi*(1-zhoumo)
myD$jiaqi6=(1:427 %in% c(279,335))+0
myD$jiaqi7=(1:427 %in% c(280,336))+0
myD$jihao=as.numeric(format(myTot$report_date,format="%d"))
#假期前1天特征
myD$qian1=(1:427 %in% c(278,304,334))+0
#假期前2天特征
myD$qian2=(1:427 %in% c(277,303,333))+0
myD=myD[275:427,]
lmFit=lm(redeem~.,myD)
plot(residuals(lmFit),type="o")
temp=(strftime(myTot$report_date,format="%d")=="01")
temp2=(1:427)[temp]
abline(v=temp2-274)
points((residuals(lmFit)*zhoumo[275:427]),type="p")
points((residuals(lmFit)*jiaqi[275:427]),type="p",col="red")
points((residuals(lmFit)*buxiu[275:427]),type="p",col="green")       


temp=ts(1:30,frequency=7,start=c(1,1))
Pseason=seasonaldummy(temp)
toP=data.frame(Pseason,jiaqi6=(1:30%in%6)+0,jiaqi7=(1:30%in%7)+0,jiaqiN=(1:30%in%8) +0,buxiu=(1:30%in%28) +0,jihao=1:30,qian1=(1:30%in%c(5,30))+0,qian2=(1:30%in%c(4,29))+0)
redeem=predict(lmFit,toP)


plot(redeem,type="l")
temp=read.csv("7_13.csv",header=FALSE)
points(temp[,3],col="red",type="l")
##修正6、 7号节日
(myTot$redeem[279]+myTot$redeem[335])/2
(myTot$redeem[280]+myTot$redeem[336])/2
(myTot$redeem[281]+myTot$redeem[337])/2



old_Tot=read.csv("old_Tot.csv")
new_Tot=read.csv("new_Tot.csv")


plot(old_Tot$purchase)

