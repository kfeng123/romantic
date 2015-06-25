library(caret)
library(plyr)
library(forecast)
library(TSA)
library(tseries)
library(tsoutliers)
#用户信息表
user_profile=read.csv("newData/user_profile_table.csv",fileEncoding="UTF-8")
#用户申购赎回数据表 
user_balance=read.csv("newData/user_balance_table.csv")
user_balance$report_date=as.Date(as.character(user_balance$report_date),format="%Y%m%d")
for(i in 15:18){
        user_balance[is.na(user_balance[,i]),i]=0
}
for(i in 3:18){
        user_balance[,i]=as.numeric(user_balance[,i])
}
#按日期排序
temp=order(user_balance$report_date)
user_balance=user_balance[temp,]

#收益率表
interest=read.csv("newData/mfd_day_share_interest.csv")
#拆借率
shibor=read.csv("newData/mfd_bank_shibor.csv")





#################换数据后第一次提交，加油！！！！！！
myTot=ddply(user_balance,.(report_date),function(D){
        colwise(sum)(D[,c(-1,-2)])
})
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
################################purchase!!!!!!!!!!!!##########################
sx=ts(myTot$total_purchase_amt[244:427],frequency=7,start=c(1,1))
plot(244:427,sx,type="l")

myStl=stl(sx,s.window="periodic",robust=TRUE)
mySeason=myStl$time.series[,1]
myT=sx-mySeason
############特征
#几号
jihao=as.numeric(strftime(myTot$report_date,format="%d"))
#假期dummy
Temp=weekdays(myTot$report_date)%in%c("星期六","星期日")+0
aaa=jiaqi*(1-Temp)
#补休
buxiu
####月份dummy
sanyue=(strftime(myTot$report_date,format="%m")=="03")+0
siyue=(strftime(myTot$report_date,format="%m")=="04")+0
wuyue=(strftime(myTot$report_date,format="%m")=="05")+0
liuyue=(strftime(myTot$report_date,format="%m")=="06")+0
qiyue=(strftime(myTot$report_date,format="%m")=="07")+0
bayue=(strftime(myTot$report_date,format="%m")=="08")+0

#月初：7月1号和8月1号
yuechu=( as.character(myTot$report_date)%in%c("2014-07-01","2014-08-01") )+0

###################
###去掉季节项的数据，加上特征
myD=data.frame(total_purchase_amt=myT,jihao=jihao[244:427],jiaqi=aaa[244:427],buxiu=buxiu[244:427],riqi=244:427,sanyue=sanyue[244:427],siyue=siyue[244:427],wuyue=wuyue[244:427],liuyue=liuyue[244:427],qiyue=qiyue[244:427],bayue=bayue[244:427],yuechu=yuechu[244:427])
myLmFit=lm(total_purchase_amt~sanyue:jihao+siyue:jihao+wuyue:jihao+sanyue+siyue+wuyue+jiaqi+buxiu,data=myD)
plot(244:427,myD$total_purchase_amt,type="o")
points(244:427,myLmFit$fitted.values,type="l",col="red")
summary(myLmFit)
##########预测
toP=data.frame(jihao=1:30,jiaqi=(1:30%in%8)+0,buxiu=(1:30%in%28)+0,riqi=428:457,sanyue=rep(0,30),siyue=rep(0,30),wuyue=rep(0,30),liuyue=rep(0,30),qiyue=rep(0,30),bayue=rep(0,30),yuechu=(1:30 %in% 1)+0)
A=(stlf(mySeason,h=30)$mean)
B=predict(myLmFit,toP)
myAutoFit=auto.arima(myLmFit$residuals,trace=TRUE)
C=predict(myAutoFit,n.ahead=30)$pred
myPurchase=as.numeric(A)+as.numeric(B)+as.numeric(C)
#############################################redeem!!!!!!!!!##############

sx=ts(myTot$transfer_amt[244:427],frequency=7,start=c(1,1))
myStl=stl(sx,s.window="periodic",robust=TRUE)
f1=forecast(myStl,h=30)$mean
r1=forecast(myStl,h=30)$residuals


sx=ts(myTot$consume_amt[244:427],frequency=7,start=c(1,1))
myStl=stl(sx,s.window="periodic",robust=TRUE)
f2=forecast(myStl,h=30)$mean
r2=forecast(myStl,h=30)$residuals

#假期
(244:427-243)[as.logical(aaa)[244:427]]
#38 62 63 94
((r1+r2)[38]-(r1+r2)[37]+(r1+r2)[63]-(r1+r2)[61]+(r1+r2)[62]-(r1+r2)[61]+(r1+r2)[94]-(r1+r2)[93])/4
#-107042255
#补休
(244:427-243)[as.logical(buxiu)[244:427]]
(r1+r2)[65]-(r1+r2)[64]
#172417267

myRedeem=as.numeric(f1)+as.numeric(f2)
myRedeem[8]=myRedeem[8]-107042255
myRedeem[28]=myRedeem[28]+172417267



temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,myPurchase,myRedeem)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])

write.table(out,"newResult/first.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)

#换数据后第二次提交，昨天的成绩特别差，只有101.05，今天如果没有突破，有可能就进不了第二赛季了，不管怎样，保持一个平常心，加油~

#不做新的分析了，把之前的方法再做一遍
first=read.csv("newResult/first.csv",header=FALSE)

naive=read.csv("newResult/naive.csv",header=FALSE)
eighth=read.csv("newResult/eighth.csv",header=FALSE)
eighteenth=read.csv("newResult/eighteenth.csv",header=FALSE)
stl=read.csv("newResult/stl.csv",header=FALSE)

plot(first[,2],type="o")
points(naive[,2],type="o",col="red")
points(eighth[,2],type="o",col="blue")
points(eighteenth[,2],type="o",col="gray")
points(stl[,2],type="o",col="green")


temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,(naive[,2]+stl[,2]+eighteenth[,2])/3,(naive[,3]+stl[,3]+eighteenth[,3])/3)
out[8,2]=eighteenth[8,2]
out[8,3]=eighteenth[8,3]
out[28,2]=eighteenth[28,2]
out[28,3]=eighteenth[28,3]
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])

write.table(out,"newResult/second.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)

plot(first[,3],type="o")
points(naive[,3],type="o",col="red")
points(eighth[,3],type="o",col="red")
points(eighteenth[,3],type="o",col="red")
points(stl[,3],type="o",col="red")


points(out[,3],type="o",col="purple")


myTot=ddply(user_balance,.(report_date),function(D){
        colwise(sum)(D[,c(-1,-2)])
})




#用户信息表
old_user_profile=read.csv("data/user_profile_table.csv",fileEncoding="UTF-8")
#用户申购赎回数据表 
old_user_balance=read.csv("data/user_balance_table.csv")
old_user_balance$report_date=as.Date(as.character(old_user_balance$report_date),format="%Y%m%d")
for(i in 15:18){
        old_user_balance[is.na(old_user_balance[,i]),i]=0
}
for(i in 3:18){
        old_user_balance[,i]=as.numeric(old_user_balance[,i])
}
#按日期排序
temp=order(old_user_balance$report_date)
old_user_balance=old_user_balance[temp,]

oldMyTot=ddply(old_user_balance,.(report_date),function(D){
        colwise(sum)(D[,c(-1,-2)])
})





A=list()
B=list()
for(i in 1:50){
        A[[i]]=sample(30000,30000/4)
        B[[i]]=sample(30000,30000/4)        
}
D1=ddply(user_balance,.(report_date),function(D){
        temp=list()
        for(i in 1:50){
                temp1=D[D$user_id%in%A[[i]],]
                temp[[i]]=colwise(sum)(temp1[,c(-1,-2)])
        }
        temp2=ldply(temp)
        colwise(median)(temp2)
})
#D2=ddply(old_user_balance,.(report_date),function(D){
#        temp=list()
#        for(i in 1:10){
#                temp1=D[D$user_id%in%A[[i]],]
#                temp[[i]]=colwise(sum)(temp1[,c(-1,-2)])
#        }
##        temp2=ldply(temp)
 #       colwise(median)(temp2)
#})

plot(D1$total_redeem_amt[244:427]*4,type="o")
points(myTot$total_redeem_amt[244:427],type="o",col="red")

#plot(D2$total_purchase_amt[244:427]*10,type="l")
#points(oldMyTot$total_purchase_amt[244:427],type="l",col="red")


temp=user_balance[user_balance$report_date=="2014-08-30",]
which.max(temp$total_purchase_amt)
#2309
temp[2309,]
#user_id 为15118的大土豪....他在2014年8月31号购买了100万...
#user_id 为5330的是一个小土豪，在8月30号买了大概30多万吧..
temp=user_balance$direct_purchase_amt[user_balance$user_id==5330]

user_balance$total_redeem_amt[user_balance$user_id==15118]


temp=user_balance[!(user_balance$user_id%in%c(5330,15118)),]
TT=ddply(temp,.(report_date),function(D){
        colwise(sum)(D[,c(-1,-2)])
})
