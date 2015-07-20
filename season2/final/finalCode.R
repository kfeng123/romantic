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

#old1数据
#3w用户
old1=read.csv("old1.csv")

#old2数据
#3w用户
old2=read.csv("old2.csv")
old2=old2[,-1]
names(old2)=c("report_date","total_purchase_amt","total_redeem_amt")

#中位数处理，去掉了大土豪的数据
medianTot=read.csv("medianTot.csv")
medianTot=medianTot[,-1]
###########探索性数据分析
myPlotTot=function(hhh){
        plot(1:427,hhh[1:427],type="l")
        temp=(strftime(finalTot$report_date,format="%d")=="01")
        temp2=(1:427)[temp]
        abline(v=temp2)
        sx=ts(finalTot$total_purchase_amt,frequency=7,start=c(1,1))
        points(1:427,(hhh*(cycle(sx)==1 +0)),type="p")
        points(1:427,(hhh*jiaqi),type="p",col="red")
        points(1:427,(hhh*buxiu),type="p",col="green")
}
myPlotTot(finalTot$total_purchase_amt)
myPlotTot(medianTot$total_purchase_amt)
myPlotTot(part1$total_purchase_amt)
myPlotTot((finalTot$total_purchase_amt+part1$total_purchase_amt+old1$total_purchase_amt+old2$total_purchase_amt)/2.6)
myPlotTot(old1$total_purchase_amt)
myPlotTot(old2$total_purchase_amt)
plot(finalTot$total_purchase_amt[275:427],type="l")
lines(medianTot$total_purchase_amt[275:427]*94626/30000,col="red")
plot(finalTot$total_purchase_amt-medianTot$total_purchase_amt*94626/30000,type="l")

myPlotTot(finalTot$total_redeem_amt)
myPlotTot(medianTot$total_redeem_amt)
myPlotTot(part1$total_redeem_amt)
myPlotTot((finalTot$total_redeem_amt+part1$total_redeem_amt+old1$total_redeem_amt+old2$total_redeem_amt)/2.6)
myPlotTot(old1$total_redeem_amt)
myPlotTot(old2$total_redeem_amt)



plot(finalTot$total_redeem_amt,type="l")
lines(part1$total_redeem_amt,col="red")
lines((finalTot$total_redeem_amt+part1$total_redeem_amt+old1$total_redeem_amt+old2$total_redeem_amt)/2.6,col="red")

plot(old1$total_redeem_amt,type="l")
lines(old2$total_redeem_amt,col="red")

plot(finalTot$total_purchase_amt,type="l")
lines((finalTot$total_purchase_amt+part1$total_purchase_amt+old1$total_purchase_amt+old2$total_purchase_amt)/2.6,col="red")



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

#################purchase模型
library("mgcv")
formulaStr="purchase~S1+S2+S3+S4+S5+S6+yuechu+buxiu+jiaqiN+jiaqi6+jiaqi7+s(t)"
#final
myFeature$purchase=finalTot$total_purchase_amt
finalFit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
purchase=exp(predict(finalFit,toP))
myPlotRes(finalFit)
plot(purchase,type="l")
myPlotTot(finalTot$total_purchase_amt)



#part1
myFeature$purchase=part1$total_purchase_amt
part1Fit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
#26w人全部数据
myFeature$purchase=(finalTot$total_purchase_amt+part1$total_purchase_amt+old1$total_purchase_amt+old2$total_purchase_amt)/2.6
totalFit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
#median处理过后的数据
myFeature$purchase=medianTot$total_purchase_amt*94626/30000
medianFit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
#################redeem模型
formulaStr="redeem~S1+S2+S3+S4+S5+S6+buxiu+jiaqiN+jiaqi6+jiaqi7+qian1+qian2+s(jihao)"
#final
myFeature$redeem=finalTot$total_redeem_amt
finalFit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
redeem=exp(predict(finalFit,toP))
redeem[28]=1000000000
plot(redeem,type="l")
plot(purchase,type="l")













#part1
myFeature$redeem=part1$total_redeem_amt
part1Fit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
#26w人全部数据
myFeature$redeem=(finalTot$total_redeem_amt+part1$total_redeem_amt+old1$total_redeem_amt+old2$total_redeem_amt)/2.6
totalFit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
############画图
plot(exp(predict(finalFit,toP)),type="o")
lines(exp(predict(part1Fit,toP)),type="o",col="blue")
lines(exp(predict(totalFit,toP)),type="o",col="red")
lines(exp(predict(medianFit,toP)),type="o",col="red")

myPlotRes=function(lmFit){
        plot(residuals(lmFit),type="o")
        temp=(strftime(finalTot$report_date,format="%d")=="01")
        temp2=(1:427)[temp]
        abline(v=temp2-274)
        points((residuals(lmFit)*zhoumo[275:427]),type="p")
        points((residuals(lmFit)*jiaqi[275:427]),type="p",col="red")
        points((residuals(lmFit)*buxiu[275:427]),type="p",col="green")       
}
myPlotRes(finalFit)
myPlotRes(part1Fit)
myPlotRes(totalFit)

plot












########第一天提交

#################purchase模型
library("mgcv")
formulaStr="purchase~S1+S2+S3+S4+S5+S6+yuechu+buxiu+jiaqiN+jiaqi6+jiaqi7"
#final
myFeature$purchase=finalTot$total_purchase_amt
finalFit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
purchase=exp(predict(finalFit,toP))
purchase[30]=purchase[30]*0.8
purchase[29]=purchase[29]*0.9
purchase[28]=900000000
purchase[8]=480000000

#################redeem模型
formulaStr="redeem~S1+S2+S3+S4+S5+S6+buxiu+jiaqiN+jiaqi6+jiaqi7+qian1+qian2+s(jihao)"
#final
myFeature$redeem=finalTot$total_redeem_amt
finalFit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
redeem=exp(predict(finalFit,toP))
redeem[28]=1000000000
plot(redeem,type="l")
plot(purchase,type="l")

write.csv(as.integer(purchase),"purchase1.csv",row.names=FALSE)
write.csv(as.integer(redeem),"redeem1.csv",row.names=FALSE)




########第二天提交

#################purchase模型
library("mgcv")
formulaStr="purchase~S1+S2+S3+S4+S5+S6+yuechu+buxiu+jiaqiN+jiaqi6+jiaqi7+s(jihao)"
#final
myFeature$purchase=finalTot$total_purchase_amt
finalFit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
purchase=exp(predict(finalFit,toP))
plot(purchase,type="l",col="red")
lines(finalTot$total_purchase_amt[400:427])
myPlotTot(finalTot$total_purchase_amt)
purchase[30]=purchase[30]*0.8
purchase[29]=purchase[29]*0.9

myFeature$shang=(finalTot$total_purchase_amt/gam(purchase~s(t),data=myFeature)$fitted.values)
tempFit=lm(shang~S1+S2+S3+S4+S5+S6,data=myFeature)
plot(myFeature$shang,type="l")
myPlotTot(myFeature$shang-tempFit$fitted.values)
lines(tempFit$fitted.values,col="red")
#################redeem模型
formulaStr="redeem~S1+S2+S3+S4+S5+S6+buxiu+jiaqiN+jiaqi6+jiaqi7+s(jihao)"
#final
myFeature$redeem=finalTot$total_redeem_amt
finalFit=gam(as.formula(formulaStr),data=myFeature[275:427,],family=Gamma(link="log"))
redeem=exp(predict(finalFit,toP))
myPlotTot(finalTot$total_redeem_amt)

myFeature$shang=(finalTot$total_redeem_amt/gam(redeem~s(t),data=myFeature)$fitted.values)
tempFit=lm(shang~S1+S2+S3+S4+S5+S6,data=myFeature)
plot(myFeature$shang,type="l")
myPlotTot(myFeature$shang-tempFit$fitted.values)
lines(tempFit$fitted.values,col="red")

plot(myFeature$shang-tempFit$fitted.values,type="o")


write.csv(as.integer(purchase),"purchase2.csv",row.names=FALSE)
write.csv(as.integer(redeem),"redeem2.csv",row.names=FALSE)
