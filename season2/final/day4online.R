dataset1 <- pai.inputPort(1) # class: data.frame

library("forecast")
###############################################
#读取数据
finalTot=dataset1
for (i in 2:17){
        finalTot[,i]=as.numeric(finalTot[,i])
}
names(finalTot)=c("report_date","tbalance","ybalance","total_purchase_amt"
                  ,"direct_purchase_amt","purchase_bal_amt"
                  ,"purchase_bank_amt","total_redeem_amt","consume_amt","transfer_amt"
                  ,"tftobal_amt","tftocard_amt","share_amt"
                  ,"category1","category2","category3","category4")
finalTot$report_date=as.Date(as.character(finalTot$report_date),format="%Y%m%d")
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
myFeature$qian1=(1:427 %in% c(80,184,278,304,334))+0
#1:427特征
myFeature$t=1:427

myFeature$l2=(strftime(finalTot$report_date,format="%d")=="02")+0
myFeature$l3=(strftime(finalTot$report_date,format="%d")=="03")+0
myFeature$l4=(strftime(finalTot$report_date,format="%d")=="04")+0
myFeature$l5=(strftime(finalTot$report_date,format="%d")=="05")+0
myFeature$l6=(strftime(finalTot$report_date,format="%d")=="06")+0
myFeature$l7=(strftime(finalTot$report_date,format="%d")=="07")+0
myFeature$l8=(strftime(finalTot$report_date,format="%d")=="08")+0
myFeature$l9=(strftime(finalTot$report_date,format="%d")=="09")+0
myFeature$l10=(strftime(finalTot$report_date,format="%d")=="10")+0
myFeature$l11=(strftime(finalTot$report_date,format="%d")=="11")+0
myFeature$l12=(strftime(finalTot$report_date,format="%d")=="12")+0
myFeature$l13=(strftime(finalTot$report_date,format="%d")=="13")+0
myFeature$l14=(strftime(finalTot$report_date,format="%d")=="14")+0
myFeature$l15=(strftime(finalTot$report_date,format="%d")=="15")+0
myFeature$l16=(strftime(finalTot$report_date,format="%d")=="16")+0
myFeature$l17=(strftime(finalTot$report_date,format="%d")=="17")+0
myFeature$l18=(strftime(finalTot$report_date,format="%d")=="18")+0
myFeature$l19=(strftime(finalTot$report_date,format="%d")=="19")+0
myFeature$l20=(strftime(finalTot$report_date,format="%d")=="20")+0
myFeature$l21=(strftime(finalTot$report_date,format="%d")=="21")+0
myFeature$l22=(strftime(finalTot$report_date,format="%d")=="22")+0
myFeature$l23=(strftime(finalTot$report_date,format="%d")=="23")+0
myFeature$l24=(strftime(finalTot$report_date,format="%d")=="24")+0
myFeature$l25=(strftime(finalTot$report_date,format="%d")=="25")+0
myFeature$l26=(strftime(finalTot$report_date,format="%d")=="26")+0
myFeature$l27=(strftime(finalTot$report_date,format="%d")=="27")+0
myFeature$l28=(strftime(finalTot$report_date,format="%d")=="28")+0
myFeature$l29=(strftime(finalTot$report_date,format="%d")=="29")+0
myFeature$l30=(strftime(finalTot$report_date,format="%d")=="30")+0

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
               ,qian1=(1:30%in%c(5))+0
               ,l2=(1:30%in%2)+0
               ,l3=(1:30%in%3)+0
               ,l4=(1:30%in%4)+0
               ,l5=(1:30%in%5)+0
               ,l6=(1:30%in%6)+0
               ,l7=(1:30%in%7)+0
               ,l8=(1:30%in%8)+0
               ,l9=(1:30%in%9)+0
               ,l10=(1:30%in%10)+0
               ,l11=(1:30%in%11)+0
               ,l12=(1:30%in%12)+0
               ,l13=(1:30%in%13)+0
               ,l14=(1:30%in%14)+0
               ,l15=(1:30%in%15)+0
               ,l16=(1:30%in%16)+0
               ,l17=(1:30%in%17)+0
               ,l18=(1:30%in%18)+0
               ,l19=(1:30%in%19)+0
               ,l20=(1:30%in%20)+0
               ,l21=(1:30%in%21)+0
               ,l22=(1:30%in%22)+0
               ,l23=(1:30%in%23)+0
               ,l24=(1:30%in%24)+0
               ,l25=(1:30%in%25)+0
               ,l26=(1:30%in%26)+0
               ,l27=(1:30%in%27)+0
               ,l28=(1:30%in%28)+0
               ,l29=(1:30%in%29)+0
               ,l30=(1:30%in%30)+0
               ,t=428:457)
#################模型
##########purchase
formulaStr="purchase~S1+S2+S3+S4+S5+S6+yuechu+buxiu+jiaqiN+jiaqi6+jiaqi7+l2+l3+l4+l5+l6+l7+l8+l9+l10+l11+l12+l13+l14+l15+l16+l17+l18+l19+l20+l21+l22+l23+l24+l25+l26+l27+l28+l29+l30"
myFeature$purchase=finalTot$total_purchase_amt
finalFit=lm(as.formula(formulaStr),data=myFeature[275:427,])
purchase=predict(finalFit,toP)
purchase[30]=purchase[30]*0.8
purchase[29]=purchase[29]*0.9
#################redeem模型
formulaStr="redeem~S1+S2+S3+S4+S5+S6+buxiu+jiaqiN+jiaqi6+jiaqi7+qian1+l2+l3+l4+l5+l6+l7+l8+l9+l10+l11+l12+l13+l14+l15+l16+l17+l18+l19+l20+l21+l22+l23+l24+l25+l26+l27+l28+l29+l30"

myFeature$redeem=finalTot$total_redeem_amt-finalTot$tftocard_amt
finalFit=lm(as.formula(formulaStr),data=myFeature[275:427,])
redeemPart1=predict(finalFit,toP)

formulaStr="redeem~S1+S2+S3+S4+S5+S6+buxiu+jiaqiN+jiaqi6+jiaqi7+qian1+l2+l3+l4+l5+l6+l7+l8+l9+l10+l11+l12+l13+l14+l15+l16+l17+l18+l19+l20+l21+l22+l23+l24+l25+l26+l27+l28+l29+l30"
myFeature$redeem=finalTot$tftocard_amt
finalFit=lm(as.formula(formulaStr),data=myFeature[275:427,])
redeemPart2=predict(finalFit,toP)

redeem=redeemPart1+redeemPart2

dataname=data.frame(purchase)
report_date=0:29+20140901
report_date=as.integer(report_date)
dataname$report_date=report_date
dataname$redeem=redeem

pai.outputPort(1, dataname)
