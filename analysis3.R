library(caret)
library(plyr)
library(forecast)
library(TSA)
library(tseries)
library(tsoutliers)
#用户信息表
user_profile=read.csv("data/user_profile_table.csv",fileEncoding="UTF-8")
#用户申购赎回数据表 
user_balance=read.csv("data/user_balance_table.csv")
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
interest=read.csv("data/mfd_day_share_interest.csv")
#拆借率
shibor=read.csv("data/mfd_bank_shibor.csv")





#########第17次提交，以全新的面貌，fighting！！！！

#####分析用户
#length(unique(user_balance$user_id))
#28366
temp=apply(user_balance[,c(-1,-2)],MARGIN=1,FUN=sum)
user_balance=user_balance[temp!=0,]
#length(unique(user_balance$user_id))
#15537

##去掉share的user_balance
###发现一天内同时买卖的人数特别多，说明有严重的相关性
noshare_user_balance=user_balance[user_balance$direct_purchase_amt!=0|user_balance$total_redeem_amt!=0,]


user_long=ddply(noshare_user_balance,.(user_id),function(temp){
        temp2=data.frame(start=min(temp$report_date),end=max(temp$report_date),times=nrow(temp),
                         max_purchase=max(temp$direct_purchase_amt),
                         max_redeem=max(temp$total_redeem_amt),
                         mean_purchase=mean(temp$direct_purchase_amt),
                         mean_redeem=mean(temp$total_redeem_amt),
                         weekend_number=sum(weekdays(temp$report_date)%in%c("星期六","星期日")+0)/as.integer(max(temp$report_date)-min(temp$report_date)+1)
                         )
        return(temp2)
})
colwise(mean)(user_long[,c(-1,-2,-3)])
set.seed(1)
myKmeans=kmeans(user_long[,c(-1,-2,-3)],centers=2)
cluster_1=user_long$user_id[myKmeans$cluster==1]
cluster_2=user_long$user_id[myKmeans$cluster==2]

##################################类1为土豪，类2为屌丝！！！之后要注意下，有可能会两类调换！！！！！！！！！！！！！！！！


myTot=ddply(noshare_user_balance,.(report_date),function(D){
        D=D[D$user_id%in%cluster_1,]
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
##发下第8天为负数了。。。。还是shrink到0比较好
direct_purchase_amt_1[8]=0

sx=ts(myTot$total_redeem_amt,frequency=7,start=c(1,1))
autoFit=auto.arima(sx,d=0,D=1,trace=TRUE)
#ARIMA(1,0,2)(0,1,2) with drift
plot(forecast(autoFit,h=30)$residual)
points((forecast(autoFit,h=30)$residual*jiaqi*(1-temp)),type="p")
##非周末假期的偏移,土豪这个比较多!!!!!!!!!!!
jiaqiBias=sum(forecast(autoFit,h=30)$residual*jiaqi*(1-temp))/sum(jiaqi*(1-temp))
##补休的偏移
buxiuBias=sum(forecast(autoFit,h=30)$residual*buxiu)/sum(buxiu)
###################
total_redeem_amt_1=as.numeric(forecast(autoFit,h=30)$mean)
total_redeem_amt_1[8]=total_redeem_amt_1[8]+jiaqiBias
total_redeem_amt_1[28]=total_redeem_amt_1[28]+buxiuBias



#######################################################

myTot=ddply(noshare_user_balance,.(report_date),function(D){
        D=D[D$user_id%in%cluster_2,]
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
#########################
temp=weekdays(myTot$report_date)%in%c("星期六","星期日")+0
plot(myTot$direct_purchase_amt,type="l")
points((myTot$direct_purchase_amt*temp),type="p")
#points((myTot$total_redeem_amt*((myTot$report_date=="2013-11-11")*1)),type="p",col="red")
points((myTot$direct_purchase_amt*jiaqi),type="p",col="red")
points((myTot$direct_purchase_amt*buxiu),type="p",col="green")
sx=ts(myTot$direct_purchase_amt,frequency=7,start=c(1,1))
autoFit=auto.arima(sx,d=0,D=1,trace=TRUE)
#ARIMA(2,0,1)(0,1,1)
plot(forecast(autoFit,h=30)$residual)
points((forecast(autoFit,h=30)$residual*jiaqi*(1-temp)),type="p")
points(forecast(autoFit,h=30)$residual*buxiu,type="p",col="red")
##非周末假期的偏移
jiaqiBias=sum(forecast(autoFit,h=30)$residual*jiaqi*(1-temp))/sum(jiaqi*(1-temp))
##补休的偏移
buxiuBias=sum(forecast(autoFit,h=30)$residual*buxiu)/sum(buxiu)
##############
direct_purchase_amt_2=as.numeric(forecast(autoFit,h=30)$mean)
direct_purchase_amt_2[8]=direct_purchase_amt_2[8]+jiaqiBias
direct_purchase_amt_2[28]=direct_purchase_amt_2[28]+buxiuBias



sx=ts(myTot$total_redeem_amt,frequency=7,start=c(1,1))
autoFit=auto.arima(sx,d=0,D=1,trace=TRUE)
#ARIMA(1,0,1)(0,1,1) with drift
plot(forecast(autoFit,h=30)$residual)
points((forecast(autoFit,h=30)$residual*jiaqi*(1-temp)),type="p")
points(forecast(autoFit,h=30)$residual*buxiu,type="p",col="red")
##非周末假期的偏移
jiaqiBias=sum(forecast(autoFit,h=30)$residual*jiaqi*(1-temp))/sum(jiaqi*(1-temp))
##补休的偏移
buxiuBias=sum(forecast(autoFit,h=30)$residual*buxiu)/sum(buxiu)
###################
total_redeem_amt_2=as.numeric(forecast(autoFit,h=30)$mean)
total_redeem_amt_2[8]=total_redeem_amt_2[8]+jiaqiBias
total_redeem_amt_2[28]=total_redeem_amt_2[28]+buxiuBias



###最后预测收益。。。
myTot=ddply(noshare_user_balance,.(report_date),function(D){
        colwise(sum)(D[,c(-1,-2)])
})

sx=ts(myTot$share_amt,frequency=7,start=c(1,1))
autoFit=auto.arima(sx,d=1,D=1,trace=TRUE)
#ARIMA(1,1,1)(0,1,1)
share_amt=as.numeric(forecast(autoFit,h=30)$mean)


P=direct_purchase_amt_1+direct_purchase_amt_2+share_amt
R=total_redeem_amt_1+total_redeem_amt_2


temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,P,R)
out[6,-1]=out[6,-1]*0.9
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])


write.table(out,"result/seventeenth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)


##########第18次提交,加油！！！！不分类，做预测


myTot=ddply(noshare_user_balance,.(report_date),function(D){
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


P=direct_purchase_amt_1+direct_purchase_amt_2+share_amt
R=total_redeem_amt_1+total_redeem_amt_2


temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
sep2=format(sep,format="%Y%m%d")

out=data.frame(sep2,P,R)
out[6,-1]=out[6,-1]*0.9
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])


##########第19次提交,加油！！！！

out=read.csv("result/eighth.csv",header=FALSE)
temp1=read.csv("result/eighteenth.csv",header=FALSE)

out[8,]=temp1[8,]
out[28,]=temp1[28,]

write.table(out,"result/nighteenth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)



##########第20次提交,加油！！！！

#####分析用户
temp=apply(user_balance[,c(-1,-2)],MARGIN=1,FUN=sum)
user_balance=user_balance[temp!=0,]

##去掉share的user_balance
noshare_user_balance=user_balance[user_balance$direct_purchase_amt!=0|user_balance$total_redeem_amt!=0,]

user_long=ddply(noshare_user_balance,.(user_id),function(temp){
        temp2=data.frame(start=min(temp$report_date),end=max(temp$report_date),times=nrow(temp),
                         max_purchase=max(temp$direct_purchase_amt),
                         max_redeem=max(temp$total_redeem_amt),
                         mean_purchase=mean(temp$direct_purchase_amt),
                         mean_redeem=mean(temp$total_redeem_amt),
                         weekend_number=sum(weekdays(temp$report_date)%in%c("星期六","星期日")+0)/as.integer(max(temp$report_date)-min(temp$report_date)+1)
        )
        return(temp2)
})
colwise(mean)(user_long[,c(-1,-2,-3)])
set.seed(1)
myKmeans=kmeans(user_long[,c(-1,-2,-3)],centers=2)
cluster_1=user_long$user_id[myKmeans$cluster==1]
cluster_2=user_long$user_id[myKmeans$cluster==2]
#########################################


myTot=ddply(noshare_user_balance,.(report_date),function(D){
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
#########################

sx=ts(myTot$total_redeem_amt,frequency=7,start=c(1,1))
temp=ts(myTot$total_redeem_amt[300:427],frequency=7,start=c(1,1))
autoFit=auto.arima(temp^2,d=1,D=1,trace=TRUE)
#ARIMA(1,0,2)(0,1,2)
tsdiag(autoFit)

stats::arima(sx,order=c(2,0,2),seasonal=list(order=c(0,1,2),period=7),xreg=jiaqi)
      