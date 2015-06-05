setwd("C:/Users/ThinkCentre/Desktop/romantic")
library(caret)
library(plyr)
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
sum(user_balance[,16])
#按日期排序
temp=order(user_balance$report_date)
user_balance=user_balance[temp,]

#收益率表
interest=read.csv("data/mfd_day_share_interest.csv")
#拆借率
shibor=read.csv("data/mfd_bank_shibor.csv")

## 预处理
#生成所有日期
tempDate=as.Date(c("20130701"),format="%Y%m%d")
temp=as.Date(c("20130702"),format="%Y%m%d")
endDate=as.Date(c("20140831"),format="%Y%m%d")
repeat{
        tempDate=c(tempDate,temp)
        
        
        if(temp==endDate){
                break
        }
        else{
                temp=temp+1
        }
}

D=ddply(user_balance,.(user_id),.fun=function(tD){
        temp=rep(0,times=length(tempDate))
        dim(temp)=c(length(tempDate),1)
        temp=as.data.frame(temp)
        row.names(temp)=as.character(tempDate)
        temp2=match(as.character(tD$report_date),row.names(temp))
        temp[temp2,1]=tD$tBalance
        temp=t(temp)
        temp=as.data.frame(temp)
        return(temp)
        
})
write.csv(D,"tempData/high_dimention.csv",row.names=FALSE)

myReshape=function(myName,dd,tD){
        temp=rep(0,times=length(tempDate))
        dim(temp)=c(length(tempDate),1)
        temp=as.data.frame(temp)
        tempname=paste(myName,strftime(tempDate,format("%Y.%m.%d")),sep="")
        row.names(temp)=tempname
        temp2=match(as.character(tD$report_date),as.character(tempDate))
        temp[temp2,1]=dd
        temp=t(temp)
        temp=as.data.frame(temp)
        return(temp)
}
#去掉user_balance里没用的信息
#length(unique(user_balance$user_id))
#28366
temp=apply(user_balance[,c(-1,-2)],MARGIN=1,FUN=sum)
user_balance=user_balance[temp!=0,]
#length(unique(user_balance$user_id))
#15537

D2=ddply(user_balance,.(user_id),.fun=function(tD){
        #tBalance
        Temp1=myReshape("tBalance",tD$tBalance,tD)
        #direct_purchase_amt
        Temp2=myReshape("tPurchase",tD$direct_purchase_amt,tD)
        #purchase_bal_amt
        Temp3=myReshape("balPurchase",tD$purchase_bal_amt,tD)
        #purchase_bank_amt
        Temp4=myReshape("bankPurchase",tD$purchase_bank_amt,tD)
        #consume_amt
        Temp5=myReshape("consume",tD$consume_amt,tD)
        #transfer_amt
        Temp6=myReshape("transfer",tD$transfer_amt,tD)
        #tftobal_amt
        Temp7=myReshape("tftobal",tD$tftobal_amt,tD)
        #tftocard_amt
        Temp8=myReshape("tftocard",tD$tftocard_amt,tD)
        #share_amt
        Temp9=myReshape("share",tD$share_amt,tD)
        #category1
        Temp10=myReshape("c1",tD$category1,tD)
        #category2
        Temp11=myReshape("c2",tD$category2,tD)
        #category3
        Temp12=myReshape("c3",tD$category3,tD)
        #category4
        Temp13=myReshape("c4",tD$category4,tD)
        temp=data.frame(Temp1,Temp2,Temp3,Temp4,Temp5,Temp6,Temp7,Temp8,Temp9,Temp10,Temp11,Temp12,Temp13)
        return(temp)
})
write.csv(D2,"tempData/high_dimention2.csv",row.names=FALSE)

D1=ddply(user_balance,.(user_id),.fun=function(tD){
        #tBalance
        temp=myReshape("tBalance",tD$tBalance,tD)
        return(temp)
})
write.csv(D1,"tempData/tBalance.csv",row.names=FALSE)

D2=ddply(user_balance,.(user_id),.fun=function(tD){
        #direct_purchase_amt
        temp=myReshape("tPurchase",tD$direct_purchase_amt,tD)
        return(temp)
})
write.csv(D2,"tempData/tPurchase.csv",row.names=FALSE)

D3=ddply(user_balance,.(user_id),.fun=function(tD){
        #purchase_bal_amt
        temp=myReshape("balPurchase",tD$purchase_bal_amt,tD)
        return(temp)
})
write.csv(D3,"tempData/balPurchase.csv",row.names=FALSE)

D4=ddply(user_balance,.(user_id),.fun=function(tD){
        #purchase_bank_amt
        temp=myReshape("bankPurchase",tD$purchase_bank_amt,tD)
        return(temp)
})
write.csv(D4,"tempData/bankPurchase.csv",row.names=FALSE)

D5=ddply(user_balance,.(user_id),.fun=function(tD){
        #consume_amt
        temp=myReshape("consume",tD$consume_amt,tD)
        return(temp)
})
write.csv(D5,"tempData/consume.csv",row.names=FALSE)

D6=ddply(user_balance,.(user_id),.fun=function(tD){
        #transfer_amt
        temp=myReshape("transfer",tD$transfer_amt,tD)
        return(temp)
})
write.csv(D6,"tempData/transfer.csv",row.names=FALSE)

D7=ddply(user_balance,.(user_id),.fun=function(tD){
        #tftobal_amt
        temp=myReshape("tftobal",tD$tftobal_amt,tD)
        return(temp)
})
write.csv(D7,"tempData/tftobal.csv",row.names=FALSE)

D8=ddply(user_balance,.(user_id),.fun=function(tD){
        #tftocard_amt
        temp=myReshape("tftocard",tD$tftocard_amt,tD)
        return(temp)
})
write.csv(D8,"tempData/tftocard.csv",row.names=FALSE)

D9=ddply(user_balance,.(user_id),.fun=function(tD){
        #share_amt
        temp=myReshape("share",tD$share_amt,tD)
        return(temp)
})
write.csv(D9,"tempData/share.csv",row.names=FALSE)

D10=ddply(user_balance,.(user_id),.fun=function(tD){
        #category1
        temp=myReshape("c1",tD$category1,tD)
        return(temp)
})
write.csv(D10,"tempData/c1.csv",row.names=FALSE)

D11=ddply(user_balance,.(user_id),.fun=function(tD){
        #category2
        temp=myReshape("c2",tD$category2,tD)
        return(temp)
})
write.csv(D11,"tempData/c2.csv",row.names=FALSE)

D12=ddply(user_balance,.(user_id),.fun=function(tD){
        #category3
        temp=myReshape("c3",tD$category3,tD)
        return(temp)
})
write.csv(D12,"tempData/c3.csv",row.names=FALSE)

D13=ddply(user_balance,.(user_id),.fun=function(tD){
        #category4
        temp=myReshape("c4",tD$category4,tD)
        return(temp)
})
write.csv(D13,"tempData/c4.csv",row.names=FALSE)

myBigTable=data.frame(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13)
myBigTable=merge(myBigTable,user_profile)
write.csv(myBigTable,"tempData/myBigTable.csv",row.names=FALSE)


#用户加入和退出时间
user_long=ddply(user_balance,.(user_id),function(temp){
        temp2=data.frame(start=min(temp$report_date),end=max(temp$report_date))
        return(temp2)
})
write.csv(user_long,"tempData/user_Long.csv",row.names=FALSE)
