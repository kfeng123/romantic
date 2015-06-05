library(caret)
library(plyr)
library(forecast)
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

####### tempData ################
#myBigTable=read.csv("tempData/myBigTable.csv")
#mySmallTable=ddply(myBigTable[,-1],.(sex,city,constellation),colwise(mean))
######################  LET THE HACKING BEGIN #############

###################第八次提交，LET'S HARDCODING!!!!!!!!
Tot=ddply(user_balance,.(report_date),function(D){
       colwise(sum)(D[,c(-1,-2)])

})
Tot=Tot[225:427,]
for(i in 2:17){
        plot(Tot[,i],type="l",ylab=i)
}

total_Pred=rep(0,17*30)
dim(total_Pred)=c(30,17)
for(i in 2:17){
        sx=ts(Tot[,i],frequency=7,start=c(1,7))
        myStl=stl(sx,s.window="periodic",robust=TRUE)
        myPred=forecast(myStl,h=30)
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
write.table(out,"result/eighth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)





###################第九次提交，是第八次的分了男女
nv=user_profile$user_id[user_profile$sex==0]
nan=user_profile$user_id[user_profile$sex==1]
user_balance_nv=user_balance[user_balance$user_id%in% nv,]
user_balance_nan=user_balance[user_balance$user_id%in% nan,]



Tot=ddply(user_balance_nan,.(report_date),function(D){
        colwise(sum)(D[,c(-1,-2)])
        
})
Tot=Tot[225:427,]
for(i in 2:17){
        plot(Tot[,i],type="l",ylab=i)
}

total_Pred=rep(0,17*30)
dim(total_Pred)=c(30,17)
for(i in 2:17){
        sx=ts(Tot[,i],frequency=7,start=c(1,7))
        myStl=stl(sx,s.window="periodic",robust=TRUE)
        myPred=forecast(myStl,h=30)
        total_Pred[,i]=myPred$mean
}
total_Pred=as.data.frame(total_Pred)
names(total_Pred)=names(Tot)
Pnan=total_Pred$purchase_bal_amt+total_Pred$purchase_bank_amt+total_Pred$share_amt
Rnan=total_Pred$tftobal_amt+total_Pred$tftocard_amt+total_Pred$category1+total_Pred$category2+total_Pred$category3+total_Pred$category4

Tot=ddply(user_balance_nv,.(report_date),function(D){
        colwise(sum)(D[,c(-1,-2)])
        
})
Tot=Tot[225:427,]
for(i in 2:17){
        plot(Tot[,i],type="l",ylab=i)
}

total_Pred=rep(0,17*30)
dim(total_Pred)=c(30,17)
for(i in 2:17){
        sx=ts(Tot[,i],frequency=7,start=c(1,7))
        myStl=stl(sx,s.window="periodic",robust=TRUE)
        myPred=forecast(myStl,h=30)
        total_Pred[,i]=myPred$mean
}
total_Pred=as.data.frame(total_Pred)
names(total_Pred)=names(Tot)
Pnv=total_Pred$purchase_bal_amt+total_Pred$purchase_bank_amt+total_Pred$share_amt
Rnv=total_Pred$tftobal_amt+total_Pred$tftocard_amt+total_Pred$category1+total_Pred$category2+total_Pred$category3+total_Pred$category4



temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
sep2=format(sep,format="%Y%m%d")
out=data.frame(sep2,Pnan+Pnv,Rnan+Rnv)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])
write.table(out,"result/nineth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)



#############第11次提交，把第8次log变换试一下
Tot=ddply(user_balance,.(report_date),function(D){
        colwise(sum)(D[,c(-1,-2)])
        
})
Tot=Tot[225:427,]
total_Pred=rep(0,17*30)
dim(total_Pred)=c(30,17)
for(i in 2:17){
        sx=ts(log(Tot[,i]+1),frequency=7,start=c(1,7))
        myStl=stl(sx,s.window="periodic",robust=TRUE)
        myPred=forecast(myStl,h=30)
        total_Pred[,i]=myPred$mean
}
total_Pred=as.data.frame(total_Pred)
names(total_Pred)=names(Tot)

temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
sep2=format(sep,format="%Y%m%d")

total_Pred$report_date=sep2

P=exp(total_Pred$purchase_bal_amt)+exp(total_Pred$purchase_bank_amt)+exp(total_Pred$share_amt)
R=exp(total_Pred$tftobal_amt)+exp(total_Pred$tftocard_amt)+exp(total_Pred$category1)+exp(total_Pred$category2)+exp(total_Pred$category3)+exp(total_Pred$category4)

out=data.frame(sep2,P,R)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])
write.table(out,"result/eleventh.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)


######复出，第12次提交，好好做！
Tot=ddply(user_balance,.(report_date),function(D){
        temp1=colwise(sum)(D[,c(-1,-2)])
        temp2=nrow(D)
        data.frame(temp1,num=temp2)
})
Tot=Tot[225:427,]

numSeries=ts(Tot$num,frequency=7,start=c(1,7))
numStl=stl(numSeries,s.window="periodic",robust=TRUE)
predNum=forecast(numStl,h=30)$mean

total_Pred=rep(0,17*30)
dim(total_Pred)=c(30,17)
for(i in 2:17){
        sx=ts(Tot[,i]/numSeries,frequency=7,start=c(1,7))
        myStl=stl(sx,s.window="periodic",robust=TRUE)
        myPred=forecast(myStl,h=30)
        total_Pred[,i]=myPred$mean
}
total_Pred=as.data.frame(total_Pred)
names(total_Pred)=names(Tot[1:17])

temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
sep2=format(sep,format="%Y%m%d")

total_Pred$report_date=sep2

P=(total_Pred$purchase_bal_amt+total_Pred$purchase_bank_amt+total_Pred$share_amt)*predNum
R=(total_Pred$tftobal_amt+total_Pred$tftocard_amt+total_Pred$category1+total_Pred$category2+total_Pred$category3+total_Pred$category4)*predNum

out=data.frame(sep2,P,R)
out[,2]=as.integer(out[,2])
out[,3]=as.integer(out[,3])
write.table(out,"result/twelfth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)

######第13次提交
Tot=ddply(user_balance,.(report_date),function(D){
        colwise(sum)(D[,c(-1,-2)])
        
})
Tot=Tot[225:427,]
total_Pred=rep(0,17*30)
dim(total_Pred)=c(30,17)
for(i in 2:17){
        sx=ts(log(Tot[,i]+1),frequency=7,start=c(1,1))
        diffsx=diff(sx)
        myStl=stl(diffsx,s.window="periodic",robust=TRUE)
        myPred=forecast(myStl,h=30)
        temp=diffinv(myPred$mean,xi=tail(sx,1))
        total_Pred[,i]=exp(temp[-1])
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
write.table(out,"result/thirteenth.csv",row.names=FALSE,sep=",",dec=".",col.names=FALSE,quote=FALSE)
