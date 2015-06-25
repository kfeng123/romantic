
library(caret)
library(plyr)
library(forecast)
#用户信息表
user_profile=read.csv("data/user_profile_table.csv",fileEncoding="UTF-8")
#用户申购赎回数据表 
user_balance=read.csv("data/user_balance_table.csv")
user_balance$report_date=as.Date(as.character(user_balance$report_date),format="%Y%m%d")
user_balance$tBalance=as.numeric(user_balance$tBalance)
user_balance$total_purchase_amt=as.numeric(user_balance$total_purchase_amt)
user_balance$total_redeem_amt=as.numeric(user_balance$total_redeem_amt)
for(i in 15:18){
        user_balance[is.na(user_balance[,i]),i]=0
}
#按日期排序
temp=order(user_balance$report_date)
user_balance=user_balance[temp,]

#收益率表
interest=read.csv("data/mfd_day_share_interest.csv")
#拆借率
shibor=read.csv("data/mfd_bank_shibor.csv")

####### tempData ################
#myBigTable=read.csv("tempData/myBigTable.csv")
#mySmallTable=ddply(myBigTable[,-1],.(sex,city,constellation),colwise(mean))
######################  LET THE HACKING BEGIN #############

#分析用户
user_Long=read.csv("tempData/user_Long.csv",stringsAsFactors=FALSE)
user_Long$start=as.Date(user_Long$start,format="%Y-%m-%d")
user_Long$end=as.Date(user_Long$end,format="%Y-%m-%d")
hou=as.Date("2014-08-31",format="%Y-%m-%d")
qian=as.Date("2013-07-01",format="%Y-%m-%d")
temp=as.numeric(user_Long$end-user_Long$start)
hist(user_Long$start)
summary(temp[temp!=0&user_Long$end!=hou])


#目标：推测余额宝投资到哪种拆借品种
shibor$mfd_date=as.Date(as.character(shibor$mfd_date),format="%Y%m%d")
interest$mfd_date=as.Date(as.character(interest$mfd_date),format="%Y%m%d")
SI=merge(x=interest,y=shibor)




#预测9月收益率
#lmFit=lm(mfd_daily_yield~.-mfd_date-mfd_7daily_yield,data=SI)
#smoothScatter(SI$mfd_daily_yield,SI$Interest_1_M)
#smoothScatter(SI$mfd_daily_yield,SI$Interest_3_M)
lmFit=lm(mfd_daily_yield~Interest_1_M+Interest_3_M,data=SI)
predict.lm(lmFit,SI)
  #预测9月银行间拆借利率
##1_M
plot(SI$mfd_date-SI$mfd_date[1],SI$Interest_1_M,type="l")

##3_M
plot(SI$mfd_date-SI$mfd_date[1],SI$Interest_3_M,type="l")


plot(SI$mfd_date,SI$mfd_daily_yield,type="l")

lines(SI$mfd_date,predict.lm(lmFit,SI),lty="dotted")
#这里有问题，残差明显是有趋势的


#shibor只有周一到周五的数据。解决这个问题的方法有两种：一、按照星期，建立7个模型。二、填补周末数据，只建立一个模型。













#naive

Tot=ddply(user_balance,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日

Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))


week=ddply(Tot[350:427,],.(weekday),function(D){
        purchase=mean(D$total_purchase_amt)
        redeem=mean(D$total_redeem_amt)
        temp=data.frame(purchase,redeem)
        return(temp)
})
sep=as.Date("20140901",format="%Y%m%d")
temp=sep
for(i in 2:30){
        temp=temp+1
        sep=c(sep,temp)
}
temp=as.numeric(strftime(sep,format("%w")))
out=week[match(temp,week$weekday),c(2,3)]
sep2=format(sep,format="%Y%m%d")
out=data.frame(sep2,out)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])

write.table(out,"result/naive.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)




##time series

Tot=ddply(user_balance,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日
Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))


tsPurchase=ts(Tot$total_purchase_amt[351:427],frequency=7,start=c(1,7))

#decompose
print(tsPurchase,calendar=TRUE)
plot(tsPurchase)
decom=decompose(tsPurchase,type="additive")
plot(decom)
plot(decom$figure,type="l")
plot(week$purchase[c(2,3,4,5,6,7,1)],type="l")


#stl
mystl=stl(tsPurchase,s.window="periodic")
plot(mystl)
head(mystl$time.series)
head(mystl$weights)
temp=tsPurchase-mystl$time.series[,1]

plot(forecast(mystl,h=30))
plot(as.vector(forecast(mystl,h=30)$mean),type="l")
lines(out[,2],type="l")

#用stl和forecast来预测
tsPurchase=ts(Tot$total_purchase_amt[351:427],frequency=7,start=c(1,7))
purchaseStl=stl(tsPurchase,s.window="periodic")
purchasePred=forecast(purchaseStl,h=30)$mean

tsRedeem=ts(Tot$total_redeem_amt[351:427],frequency=7,start=c(1,7))
redeemStl=stl(tsRedeem,s.window="periodic")
redeemPred=forecast(redeemStl,h=30)$mean


temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29

sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,purchasePred,redeemPred)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])

write.table(out,"result/stl.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)











##########第三次提交，加油！！


nv=user_profile$user_id[user_profile$sex==0]
nan=user_profile$user_id[user_profile$sex==1]
user_balance_nv=user_balance[user_balance$user_id%in% nv,]
user_balance_nan=user_balance[user_balance$user_id%in% nan,]
Tot=ddply(user_balance_nv,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日
Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))

#用stl和forecast来预测
tsPurchase=ts(Tot$total_purchase_amt[351:427],frequency=7,start=c(1,7))
purchaseStl=stl(tsPurchase,s.window="periodic",robust)
purchasePred=forecast(purchaseStl,h=30)$mean

tsRedeem=ts(Tot$total_redeem_amt[351:427],frequency=7,start=c(1,7))
redeemStl=stl(tsRedeem,s.window="periodic",robust)
redeemPred=forecast(redeemStl,h=30)$mean



Tot=ddply(user_balance_nan,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日
Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))

#用stl和forecast来预测
tsPurchase=ts(Tot$total_purchase_amt[351:427],frequency=7,start=c(1,7))
purchaseStl=stl(tsPurchase,s.window="periodic",robust)
purchasePred2=forecast(purchaseStl,h=30)$mean

tsRedeem=ts(Tot$total_redeem_amt[351:427],frequency=7,start=c(1,7))
redeemStl=stl(tsRedeem,s.window="periodic",robust)
redeemPred2=forecast(redeemStl,h=30)$mean

r=redeemPred+redeemPred2
p=purchasePred+purchasePred2


temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29

sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,p,r)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])

write.table(out,"result/nan_nv.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)












#第四次提交，即使是我，也一定可以做的更好，加油！！
Tot=ddply(user_balance,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日
Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))


tsPurchase=ts(Tot$total_purchase_amt[1:427],frequency=7,start=c(1,7))
plot(tsPurchase)

which.max(tsPurchase)
#213天最多
plot(tsPurchase[210:230],type="l")
Tot$report_date[213]



#收益与流量
#yieldAndTot=merge(interest,Tot,by.x="mfd_date",by.y="report_date")
#lmFit=lm(total_purchase_amt~mfd_daily_yield,data=yieldAndTot)
#plot(yieldAndTot$mfd_daily_yield,yieldAndTot$total_purchase_amt)
#abline(lmFit)
#线性关系很强，但是因为有异常值，估计可能得学习下回归，用稳健回归什么的
#purchaseStl=stl(tsPurchase,s.window="periodic",robust)
#theTrend=purchaseStl$time.series[,2]
#yieldAndTot=data.frame(yieldAndTot,theTrend)
#lmFit=lm(theTrend~mfd_daily_yield,data=yieldAndTot)
#plot(yieldAndTot$mfd_daily_yield,yieldAndTot$theTrend)
#abline(lmFit)

#temp=as.numeric(lmFit$residuals>0)
#plot(temp)
#tail(yieldAndTot$mfd_date[temp==0])
#发现趋势在2014年2月7日改变了，所以以后使用2014年2月10日开始的数据，因为那天星期一
#2014年2月10日是第225个数据
#yieldAndTot=data.frame(yieldAndTot,changePoint=rep(c(0,1),c(224,427-224)))
#plot(yieldAndTot$mfd_daily_yield,col=yieldAndTot$changePoint)

###先去调收益的影响，再拟合时间序列
Tot=Tot[225:427,]
interest=interest[225:427,]
interest$mfd_date=as.Date(as.character(interest$mfd_date),format="%Y%m%d")
yieldAndTot=merge(interest,Tot,by.x="mfd_date",by.y="report_date")
tsPurchase=ts(Tot$total_purchase_amt,frequency=7,start=c(1,7))
purchaseStl=stl(tsPurchase,s.window="periodic",robust)
theTrend=purchaseStl$time.series[,2]
yieldAndTot=data.frame(yieldAndTot,theTrend)
lmFit=lm(theTrend~mfd_daily_yield,data=yieldAndTot)
plot(yieldAndTot$mfd_daily_yield,yieldAndTot$theTrend)
abline(lmFit)
plot(lmFit$residuals)

theFitted=lmFit$fitted.values

##去掉收益的影响
theData=Tot$total_purchase_amt-theFitted


temp=ts(theData,frequency=7,start=c(1,7))
theStl=stl(temp,s.window="periodic")
#plot(forecast(theStl,h=30))
A=forecast(theStl,h=30)$mean

#预测未来30天的收益，就用每天
temp=ts(yieldAndTot$mfd_daily_yield,frequency=7,start=c(1,7))
syStl=stl(temp,s.window="periodic")
B=forecast(syStl,h=30)$mean
C=predict.lm(lmFit,data.frame(mfd_daily_yield=B))
purc=A+C





yieldAndTot=merge(interest,Tot,by.x="mfd_date",by.y="report_date")
tsRedeem=ts(Tot$total_redeem_amt,frequency=7,start=c(1,7))
redeemStl=stl(tsRedeem,s.window="periodic",robust)
theTrend=redeemStl$time.series[,2]
yieldAndTot=data.frame(yieldAndTot,theTrend)
lmFit=lm(theTrend~mfd_daily_yield,data=yieldAndTot)
plot(yieldAndTot$mfd_daily_yield,yieldAndTot$theTrend)
abline(lmFit)
plot(lmFit$residuals)

theFitted=lmFit$fitted.values

##去掉收益的影响
theData=Tot$total_redeem_amt-theFitted


temp=ts(theData,frequency=7,start=c(1,7))
theStl=stl(temp,s.window="periodic")
#plot(forecast(theStl,h=30))
A=forecast(theStl,h=30)$mean

#预测未来30天的收益，就用每天
temp=ts(yieldAndTot$mfd_daily_yield,frequency=7,start=c(1,7))
syStl=stl(temp,s.window="periodic")
B=forecast(syStl,h=30)$mean
C=predict.lm(lmFit,data.frame(mfd_daily_yield=B))
rede=A+C

temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29

sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,purc,rede)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])

write.table(out,"result/fourth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)















##第五次提交，是之前的改版吧
nv=user_profile$user_id[user_profile$sex==0]
nan=user_profile$user_id[user_profile$sex==1]
user_balance_nv=user_balance[user_balance$user_id%in% nv,]
user_balance_nan=user_balance[user_balance$user_id%in% nan,]
Tot=ddply(user_balance_nv,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日
Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))

#用stl和forecast来预测
tsPurchase=ts(Tot$total_purchase_amt[225:427],frequency=7,start=c(1,7))
purchaseStl=stl(tsPurchase,s.window="periodic",robust=TRUE)
purchasePred=forecast(purchaseStl,h=30)$mean

tsRedeem=ts(Tot$total_redeem_amt[225:427],frequency=7,start=c(1,7))
redeemStl=stl(tsRedeem,s.window="periodic",robust=TRUE)
redeemPred=forecast(redeemStl,h=30)$mean



Tot=ddply(user_balance_nan,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日
Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))

#用stl和forecast来预测
tsPurchase=ts(Tot$total_purchase_amt[225:427],frequency=7,start=c(1,7))
purchaseStl=stl(tsPurchase,s.window="periodic",robust=TRUE)
purchasePred2=forecast(purchaseStl,h=30)$mean

tsRedeem=ts(Tot$total_redeem_amt[225:427],frequency=7,start=c(1,7))
redeemStl=stl(tsRedeem,s.window="periodic",robust=TRUE)
redeemPred2=forecast(redeemStl,h=30)$mean

r=redeemPred+redeemPred2
p=purchasePred+purchasePred2


temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29

sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,p,r)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])

write.table(out,"result/fifth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)


##第六次提交，是之前的改版吧
nv=user_profile$user_id[user_profile$sex==0]
nan=user_profile$user_id[user_profile$sex==1]
user_balance_nv=user_balance[user_balance$user_id%in% nv,]
user_balance_nan=user_balance[user_balance$user_id%in% nan,]
Tot=ddply(user_balance_nv,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日
Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))

#用stl和forecast来预测
tsPurchase=ts(Tot$total_purchase_amt[225:427],frequency=7,start=c(1,7))
purchaseStlw=stl(tsPurchase,s.window="periodic",robust=TRUE)
purchasePredw=forecast(purchaseStlw,h=30)$mean
purchaseStl=stl(tsPurchase,s.window="periodic",robust)
purchasePred=forecast(purchaseStl,h=30)$mean


tsRedeem=ts(Tot$total_redeem_amt[225:427],frequency=7,start=c(1,7))
redeemStlw=stl(tsRedeem,s.window="periodic",robust=TRUE)
redeemPredw=forecast(redeemStlw,h=30)$mean
redeemStl=stl(tsRedeem,s.window="periodic",robust)
redeemPred=forecast(redeemStl,h=30)$mean



Tot=ddply(user_balance_nan,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日
Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))

#用stl和forecast来预测
tsPurchase=ts(Tot$total_purchase_amt[225:427],frequency=7,start=c(1,7))
purchaseStlw=stl(tsPurchase,s.window="periodic",robust=TRUE)
purchasePred2w=forecast(purchaseStlw,h=30)$mean
purchaseStl=stl(tsPurchase,s.window="periodic",robust)
purchasePred2=forecast(purchaseStl,h=30)$mean

tsRedeem=ts(Tot$total_redeem_amt[225:427],frequency=7,start=c(1,7))
redeemStlw=stl(tsRedeem,s.window="periodic",robust=TRUE)
redeemPred2w=forecast(redeemStlw,h=30)$mean
redeemStl=stl(tsRedeem,s.window="periodic",robust)
redeemPred2=forecast(redeemStl,h=30)$mean

r=redeemPred+redeemPred2
p=purchasePred+purchasePred2
rw=redeemPredw+redeemPred2w
pw=purchasePredw+purchasePred2w

r=c(r[1:15],rw[16:30])
p=c(p[1:15],pw[16:30])


temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29

sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,p,r)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])


write.table(out,"result/sixth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)



##第七次提交，与第六次是相反的
nv=user_profile$user_id[user_profile$sex==0]
nan=user_profile$user_id[user_profile$sex==1]
user_balance_nv=user_balance[user_balance$user_id%in% nv,]
user_balance_nan=user_balance[user_balance$user_id%in% nan,]
Tot=ddply(user_balance_nv,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日
Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))

#用stl和forecast来预测
tsPurchase=ts(Tot$total_purchase_amt[225:427],frequency=7,start=c(1,7))
purchaseStlw=stl(tsPurchase,s.window="periodic",robust=TRUE)
purchasePredw=forecast(purchaseStlw,h=30)$mean
purchaseStl=stl(tsPurchase,s.window="periodic",robust)
purchasePred=forecast(purchaseStl,h=30)$mean


tsRedeem=ts(Tot$total_redeem_amt[225:427],frequency=7,start=c(1,7))
redeemStlw=stl(tsRedeem,s.window="periodic",robust=TRUE)
redeemPredw=forecast(redeemStlw,h=30)$mean
redeemStl=stl(tsRedeem,s.window="periodic",robust)
redeemPred=forecast(redeemStl,h=30)$mean



Tot=ddply(user_balance_nan,.(report_date),function(D){
        temp1=sum(D$tBalance)
        temp2=sum(D$total_purchase_amt)
        temp3=sum(D$total_redeem_amt)
        temp4=length(unique(D$user_id))
        temp=data.frame(tBalance=temp1,total_purchase_amt=temp2,total_redeem_amt=temp3,num=temp4)
        return(temp)
})
Tot=data.frame(Tot,weekday=strftime(Tot$report_date,format("%w")))
#0是周日
Tot=data.frame(Tot,numDay=as.numeric(Tot$report_date-Tot$report_date[1]))

#用stl和forecast来预测
tsPurchase=ts(Tot$total_purchase_amt[225:427],frequency=7,start=c(1,7))
purchaseStlw=stl(tsPurchase,s.window="periodic",robust=TRUE)
purchasePred2w=forecast(purchaseStlw,h=30)$mean
purchaseStl=stl(tsPurchase,s.window="periodic",robust)
purchasePred2=forecast(purchaseStl,h=30)$mean

tsRedeem=ts(Tot$total_redeem_amt[225:427],frequency=7,start=c(1,7))
redeemStlw=stl(tsRedeem,s.window="periodic",robust=TRUE)
redeemPred2w=forecast(redeemStlw,h=30)$mean
redeemStl=stl(tsRedeem,s.window="periodic",robust)
redeemPred2=forecast(redeemStl,h=30)$mean

r=redeemPred+redeemPred2
p=purchasePred+purchasePred2
rw=redeemPredw+redeemPred2w
pw=purchasePredw+purchasePred2w

r=c(rw[1:15],r[16:30])
p=c(pw[1:15],p[16:30])


temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29

sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,p,r)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])


write.table(out,"result/seventh.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)
