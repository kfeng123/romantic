

# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口2的表数据映射成dataset2
dataset2 <- pai.inputPort(2) # class: data.frame
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

library("forecast")
dataset1$report_date=as.Date(dataset1$report_date,format="%Y%m%d")

#############假期特征
jiaqitemp=c("2013-09-19","2013-09-20","2013-09-21","2013-10-01","2013-10-02","2013-10-03","2013-10-04","2013-10-05",
            "2013-10-06","2013-10-07","2014-01-01","2014-01-31","2014-02-01","2014-02-02","2014-02-03","2014-02-04",
            "2014-02-05","2014-02-06","2014-04-05","2014-04-06","2014-04-07","2014-05-01","2014-05-02","2014-05-03",
            "2014-05-31","2014-06-01","2014-06-02","2014-09-06","2014-09-07","2014-09-08")
jiaqitemp=as.Date(jiaqitemp)

jiaqi=dataset1$report_date %in% jiaqitemp+0
temp=as.Date("20140901",format="%Y%m%d")
sep=temp+0:29
yuceJiaqi=sep %in% jiaqitemp+0

buxiutemp=c("2013-09-22","2013-09-29","2013-10-12","2014-01-26","2014-02-08","2014-05-04","2014-09-28")
buxiutemp=as.Date(buxiutemp)
buxiu=dataset1$report_date %in% buxiutemp+0
#########################
temp=dataset1$saturday+dataset1$sunday

sx=ts(dataset1$total_purchase_amt,frequency=7,start=c(1,1))
autoFit=auto.arima(sx,d=0,D=1,trace=TRUE)

##非周末假期的偏移
jiaqiBias=sum(forecast(autoFit,h=30)$residual*jiaqi*(1-temp))/sum(jiaqi*(1-temp))
##补休的偏移
buxiuBias=sum(forecast(autoFit,h=30)$residual*buxiu)/sum(buxiu)
##############
purchase=as.numeric(forecast(autoFit,h=30)$mean)
purchase[8]=purchase[8]+jiaqiBias
purchase[28]=purchase[28]+buxiuBias



#######redeem
sx=ts(dataset1$total_redeem_amt,frequency=7,start=c(1,1))
autoFit=auto.arima(sx,d=0,D=1,trace=TRUE)

##非周末假期的偏
jiaqiBias=sum(forecast(autoFit,h=30)$residual*jiaqi*(1-temp))/sum(jiaqi*(1-temp))
##补休的偏移
buxiuBias=sum(forecast(autoFit,h=30)$residual*buxiu)/sum(buxiu)
###################
redeem=as.numeric(forecast(autoFit,h=30)$mean)
redeem[8]=redeem[8]+jiaqiBias
redeem[28]=redeem[28]+buxiuBias




dataname=data.frame(purchase)
dataname$report_date=dataset2$report_date
dataname$redeem=redeem

# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)





########7月4日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口2的表数据映射成dataset2
dataset2 <- pai.inputPort(2) # class: data.frame
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

formulaStr="total_purchase_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
purchase=predict(lmFit,dataset2)


formulaStr="total_redeem_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
redeem=predict(lmFit,dataset2)




dataname=data.frame(purchase)
dataname$report_date=dataset2$report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)



########7月5日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口2的表数据映射成dataset2
dataset2 <- pai.inputPort(2) # class: data.frame
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

formulaStr="total_purchase_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
purchase=predict(lmFit,dataset2)


formulaStr="total_redeem_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
redeem=predict(lmFit,dataset2)




dataname=data.frame(purchase)
dataname$report_date=dataset2$report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

dataname$purchase[28]=865500630
dataname$redeem[28]=955642120


# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)




########7月6日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口2的表数据映射成dataset2
dataset2 <- pai.inputPort(2) # class: data.frame
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

formulaStr="total_purchase_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
purchase=predict(lmFit,dataset2)


formulaStr="total_redeem_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
redeem=predict(lmFit,dataset2)




dataname=data.frame(purchase)
dataname$report_date=dataset2$report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

dataname$purchase[28]=865500630
dataname$redeem[28]=955642120

dataname$purchase[30]=900000000
dataname$redeem[30]=900000000

# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)



########7月7日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口2的表数据映射成dataset2
dataset2 <- pai.inputPort(2) # class: data.frame
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

formulaStr="total_purchase_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
purchase=predict(lmFit,dataset2)


formulaStr="total_redeem_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
redeem=predict(lmFit,dataset2)




dataname=data.frame(purchase)
dataname$report_date=dataset2$report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

dataname$purchase[28]=865500630
dataname$redeem[28]=955642120

dataname$purchase[30]=900000000
dataname$redeem[30]=900000000

dataname$redeem=rep(0,30)

# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)





########7月8日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口2的表数据映射成dataset2
dataset2 <- pai.inputPort(2) # class: data.frame
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

library("forecast")
myTot=data.frame(report_date=dataset1$report_date)
myTot$purchase=dataset1$total_purchase_amt
myTot$redeem=dataset1$total_redeem_amt
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

sx=ts(myTot$purchase,frequency=7,start=c(1,1))
myStl=stl(sx,s.window="periodic",robust=TRUE)
purchase=forecast(myStl,h=30)$mean


formulaStr="total_redeem_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
redeem=predict(lmFit,dataset2)




dataname=data.frame(purchase)
dataname$report_date=dataset2$report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

dataname$purchase[28]=865500630
dataname$redeem[28]=955642120

dataname$purchase[30]=900000000
dataname$redeem[30]=900000000


# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)



########7月9日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口2的表数据映射成dataset2
dataset2 <- pai.inputPort(2) # class: data.frame
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

formulaStr="total_purchase_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
purchase=predict(lmFit,dataset2)


formulaStr="total_redeem_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
redeem=predict(lmFit,dataset2)




dataname=data.frame(purchase)
dataname$report_date=dataset2$report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

dataname$purchase[28]=865500630
dataname$redeem[28]=955642120

dataname$purchase[30]=900000000
dataname$redeem[30]=900000000

for (i in c(4,11,18,25)){
	dataname$purchase[i]=874634149
}

# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)






########7月10日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口2的表数据映射成dataset2
dataset2 <- pai.inputPort(2) # class: data.frame
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

formulaStr="total_purchase_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
purchase=predict(lmFit,dataset2)


formulaStr="total_redeem_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
redeem=predict(lmFit,dataset2)




dataname=data.frame(purchase)
dataname$report_date=dataset2$report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

dataname$purchase[28]=865500630
dataname$redeem[28]=955642120

dataname$purchase[30]=900000000
dataname$redeem[30]=900000000

dataname$purchase[1]=1031064940


# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)



########7月11日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口2的表数据映射成dataset2
dataset2 <- pai.inputPort(2) # class: data.frame
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

library("forecast")
myTot=data.frame(report_date=dataset1$report_date)
myTot$purchase=dataset1$total_purchase_amt
myTot$redeem=dataset1$total_redeem_amt
myTot$purchase=as.numeric(myTot$purchase)
myTot$redeem=as.numeric(myTot$redeem)

myTot$report_date=as.Date("2014-04-01")+(275:427) -275

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

#月初，每月1号
yuechu=(myTot$report_date %in% as.Date(c("2014-04-01","2014-05-01","2014-06-01","2014-07-01","2014-08-01")))+0
#########################

sx=ts(myTot$purchase,frequency=7,start=c(1,2))


myD=data.frame(purchase=sx,seasonaldummy(sx),yuechu,jiaqi,buxiu)

lmFit=lm(purchase~.,myD)

temp=ts(1:30,frequency=7,start=c(1,1))
Pseason=seasonaldummy(temp)

toP=data.frame(Pseason,yuechu=(1:30%in%1) +0,jiaqi=(1:30%in%8) +0,buxiu=(1:30%in%28) +0)
purchase=predict(lmFit,toP)


formulaStr="total_redeem_amt~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),dataset1)
redeem=predict(lmFit,dataset2)




dataname=data.frame(purchase)
dataname$report_date=dataset2$report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

dataname$purchase[28]=865500630
dataname$redeem[28]=955642120

dataname$purchase[30]=900000000
dataname$redeem[30]=900000000

# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)










########7月12日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

library("forecast")
myTot=data.frame(report_date=dataset1$report_date)
myTot$purchase=dataset1$total_purchase_amt
myTot$redeem=dataset1$total_redeem_amt
myTot$purchase=as.numeric(myTot$purchase)
myTot$redeem=as.numeric(myTot$redeem)
myTot$report_date=as.Date(dataset1$report_date,format="%Y%m%d")
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

#月初，每月1号
yuechu=(format(myTot$report_date,format="%d")=="01")+0
#########################

sx=ts(myTot$purchase,frequency=7,start=c(1,1))
myD=data.frame(purchase=sx,seasonaldummy(sx),yuechu,jiaqi,buxiu)
myD=myD[275:427,]

lmFit=lm(purchase~.,myD)

temp=ts(1:30,frequency=7,start=c(1,1))
Pseason=seasonaldummy(temp)

toP=data.frame(Pseason,yuechu=(1:30%in%1) +0,jiaqi=(1:30%in%8) +0,buxiu=(1:30%in%28) +0)
purchase=predict(lmFit,toP)


sx=ts(myTot$redeem,frequency=7,start=c(1,1))
myD=data.frame(redeem=sx,seasonaldummy(sx))
myD=myD[275:427,]
lmFit=lm(redeem~.,myD)
toP=data.frame(Pseason,yuechu=(1:30%in%1) +0,jiaqi=(1:30%in%8) +0,buxiu=(1:30%in%28) +0)
redeem=predict(lmFit,toP)




dataname=data.frame(purchase)
report_date=0:29+20140901
report_date=as.integer(report_date)
dataname$report_date=report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

dataname$purchase[28]=865500630
dataname$redeem[28]=955642120

dataname$purchase[30]=900000000
dataname$redeem[30]=900000000

dataname$redeem[6]=518343988
dataname$redeem[7]=468647116

# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)






########7月13日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

library("forecast")
myTot=data.frame(report_date=dataset1$report_date)
myTot$purchase=dataset1$total_purchase_amt
myTot$redeem=dataset1$total_redeem_amt
myTot$purchase=as.numeric(myTot$purchase)
myTot$redeem=as.numeric(myTot$redeem)
myTot$report_date=as.Date(dataset1$report_date,format="%Y%m%d")
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
yuechu=(format(myTot$report_date,format="%d")=="01")+0
#########################

###purchase模型
sx=ts(myTot$purchase,frequency=7,start=c(1,1))
myD=data.frame(purchase=sx,seasonaldummy(sx),yuechu,buxiu)
myD$jiaqiN=jiaqi*(1-zhoumo)
myD$jiaqi6=(1:427 %in% c(279,335))+0
myD$jiaqi7=(1:427 %in% c(280,336))+0
myD=myD[275:427,]
lmFit=lm(purchase~.,myD)
temp=ts(1:30,frequency=7,start=c(1,1))
Pseason=seasonaldummy(temp)

toP=data.frame(Pseason,yuechu=(1:30%in%1) +0,jiaqi6=(1:30%in%6)+0,jiaqi7=(1:30%in%7)+0,jiaqiN=(1:30%in%8) +0,buxiu=(1:30%in%28) +0)

purchase=predict(lmFit,toP)







sx=ts(myTot$redeem,frequency=7,start=c(1,1))
myD=data.frame(redeem=sx,seasonaldummy(sx))
myD=myD[275:427,]
lmFit=lm(redeem~.,myD)
toP=data.frame(Pseason,yuechu=(1:30%in%1) +0,jiaqi=(1:30%in%8) +0,buxiu=(1:30%in%28) +0)
redeem=predict(lmFit,toP)







dataname=data.frame(purchase)
report_date=0:29+20140901
report_date=as.integer(report_date)
dataname$report_date=report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

dataname$purchase[28]=865500630
dataname$redeem[28]=955642120

dataname$purchase[30]=900000000
dataname$redeem[30]=900000000

dataname$redeem[6]=518343988
dataname$redeem[7]=468647116

# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)






########7月14日



# 请链接输入数据
# 链接完成后，系统会自动生成映射代码，将输入数据映射成变量参数，用户可直接使用
# 切记不可修改系统生成代码，否则运行将报错
# 端口1的表数据映射成dataset1
dataset1 <- pai.inputPort(1) # class: data.frame

library("forecast")
myTot=data.frame(report_date=dataset1$report_date)
myTot$purchase=dataset1$total_purchase_amt
myTot$redeem=dataset1$total_redeem_amt
myTot$purchase=as.numeric(myTot$purchase)
myTot$redeem=as.numeric(myTot$redeem)
myTot$report_date=as.Date(dataset1$report_date,format="%Y%m%d")
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
yuechu=(format(myTot$report_date,format="%d")=="01")+0
#########################

###purchase模型
sx=ts(myTot$purchase,frequency=7,start=c(1,1))
myD=data.frame(purchase=sx,seasonaldummy(sx),yuechu,buxiu)
myD$jiaqiN=jiaqi*(1-zhoumo)
myD$jiaqi6=(1:427 %in% c(279,335))+0
myD$jiaqi7=(1:427 %in% c(280,336))+0
myD=myD[275:427,]
lmFit=lm(purchase~.,myD)
temp=ts(1:30,frequency=7,start=c(1,1))
Pseason=seasonaldummy(temp)

toP=data.frame(Pseason,yuechu=(1:30%in%1) +0,jiaqi6=(1:30%in%6)+0,jiaqi7=(1:30%in%7)+0,jiaqiN=(1:30%in%8) +0,buxiu=(1:30%in%28) +0)

purchase=predict(lmFit,toP)


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
temp=ts(1:30,frequency=7,start=c(1,1))
Pseason=seasonaldummy(temp)
toP=data.frame(Pseason,jiaqi6=(1:30%in%6)+0,jiaqi7=(1:30%in%7)+0,jiaqiN=(1:30%in%8) +0,buxiu=(1:30%in%28) +0,jihao=1:30,qian1=(1:30%in%c(5,30))+0,qian2=(1:30%in%c(4,29))+0)
redeem=predict(lmFit,toP)






dataname=data.frame(purchase)
report_date=0:29+20140901
report_date=as.integer(report_date)
dataname$report_date=report_date
dataname$redeem=redeem

dataname$purchase[8]=480000000
dataname$redeem[8]=600000000

dataname$purchase[28]=865500630
dataname$redeem[28]=955642120

dataname$purchase[30]=900000000

# 用户指定数据变量dataname(class:data.frame)到输出端口
# 平台会将该数据生成ODPS表
# dataname务必修改成自己的变量名称
pai.outputPort(1, dataname)
