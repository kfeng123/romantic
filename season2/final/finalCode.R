library("forecast")
###############################################
#读取数据

#season2 part2数据
#10w用户
finalTot=read.csv("finalTot.csv",header=FALSE)
for (i in 2:17){
        finalTot[,i]=as.numeric(finalTot[,i])
}
names(finalTot)=c("report_date","tbalance","ybalance","total_purchase_amt"
                  ,"direct_purchase_amt","purchase_bal_amt"
                  ,"purchase_bank_amt","total_redeem_amt","consume_amt","transfer_amt"
                  ,"tftobal_amt","tftocard_amt","share_amt"
                  ,"category1","category2","category3","category4")
finalTot$report_date=as.Date(as.character(finalTot$report_date),format="%Y%m%d")


#season2 part1数据
#10w用户
part1=read.csv("season2part1.csv",header=FALSE)
part1=part1[,-4]
part1[,3]=as.numeric(part1[,3])
names(part1)=c("report_date","total_purchase_amt","total_redeem_amt")

plot(finalTot$total_redeem_amt,type="l")
lines(part1$total_redeem_amt,col="red")




##################################################
#####构建特征
jiaqitemp=c("2013-09-19","2013-09-20","2013-09-21","2013-10-01","2013-10-02","2013-10-03","2013-10-04","2013-10-05",
            "2013-10-06","2013-10-07","2014-01-01","2014-01-31","2014-02-01","2014-02-02","2014-02-03","2014-02-04",
            "2014-02-05","2014-02-06","2014-04-05","2014-04-06","2014-04-07","2014-05-01","2014-05-02","2014-05-03",
            "2014-05-31","2014-06-01","2014-06-02","2014-09-06","2014-09-07","2014-09-08")
jiaqitemp=as.Date(jiaqitemp)

jiaqi=finalTot$report_date %in% jiaqitemp+0
temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
yuceJiaqi=sep %in% jiaqitemp+0
buxiutemp=c("2013-09-22","2013-09-29","2013-10-12","2014-01-26","2014-02-08","2014-05-04","2014-09-28")
buxiutemp=as.Date(buxiutemp)
buxiu=finalTot$report_date %in% buxiutemp+0
#周末
xingqi=format(finalTot$report_date,format="%w")
zhoumo=as.numeric(xingqi%in%c("0","6"))
#月初，每月1号
yuechu=(strftime(finalTot$report_date,format="%d")=="01")+0
#####训练集特征构建
temp=ts(finalTot$total_purchase_amt,frequency=7,start=c(1,1))
#周1到周6dummy，每月1号dummy，补休日dummy
myFeature=data.frame(seasonaldummy(temp),yuechu,buxiu)
#周一到周五的节日dummy
myFeature$jiaqiN=jiaqi*(1-zhoumo)
#周六节日dummy
myFeature$jiaqi6=(1:427 %in% c(279,335))+0
#周日节日dummy
myFeature$jiaqi7=(1:427 %in% c(280,336))+0
#几号
myFeature$jihao=as.numeric(format(finalTot$report_date,format="%d"))
#假期前1天特征
myFeature$qian1=(1:427 %in% c(278,304,334))+0
#假期前2天特征
myFeature$qian2=(1:427 %in% c(277,303,333))+0
#1:427特征
myFeature$t=1:427
###########测试集特征构建
temp=ts(1:30,frequency=7,start=c(1,1))
Pseason=seasonaldummy(temp)
toP=data.frame(Pseason
               ,yuechu=(1:30%in%1)+0
               ,buxiu=(1:30%in%28)+0
               ,jiaqiN=(1:30%in%8)+0
               ,jiaqi6=(1:30%in%6)+0
               ,jiaqi7=(1:30%in%7)+0
               ,jihao=1:30
               ,qian1=(1:30%in%c(5,30))+0
               ,qian2=(1:30%in%c(4,29))+0
               ,t=428:457)

#################模型
library("mgcv")
myFeature$purchase=(part1$total_purchase_amt+finalTot$total_purchase_amt)/2
lmFit=gam(purchase~S1+S2+S3+S4+S5+S6+yuechu+buxiu+jiaqiN+jiaqi6+jiaqi7+s(jihao)+s(t),data=myFeature[275:427,],family=Gamma(link="log"))
lines(exp(predict(lmFit,toP)),type="o",col="blue")

plot(residuals(lmFit),type="o")
temp=(strftime(finalTot$report_date,format="%d")=="01")
temp2=(1:427)[temp]
abline(v=temp2-274)
points((residuals(lmFit)*zhoumo[275:427]),type="p")
points((residuals(lmFit)*jiaqi[275:427]),type="p",col="red")
points((residuals(lmFit)*buxiu[275:427]),type="p",col="green")       

