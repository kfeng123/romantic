d2=read.csv("7_2.csv",header=FALSE)
d3=read.csv("7_3.csv",header=FALSE)
d4=read.csv("7_4.csv",header=FALSE)
d5=read.csv("7_5.csv",header=FALSE)
d6=read.csv("7_6.csv",header=FALSE)
d7=read.csv("7_7.csv",header=FALSE)
d8=read.csv("7_8.csv",header=FALSE)
plot(d6[,2],type="o")
points(d8[,2],type="o",col="red")
points(myTot[400:427,2],type="o",col="blue")

temp=(strftime(myTot$report_date,format="%w")=="4")
www=myTot[temp,]
mean(www[40:61,2])
mean(www[50:61,2])
#874634149
plot(www[,2])

myTot$monday=(strftime(myTot$report_date,format="%w")=="1")
myTot$tuesday=(strftime(myTot$report_date,format="%w")=="2")
myTot$wednesday=(strftime(myTot$report_date,format="%w")=="3")
myTot$thursday=(strftime(myTot$report_date,format="%w")=="4")
myTot$friday=(strftime(myTot$report_date,format="%w")=="5")
myTot$saturday=(strftime(myTot$report_date,format="%w")=="6")

formulaStr="purchase~monday+tuesday+wednesday+thursday+friday+saturday"
lmFit=lm(as.formula(formulaStr),myTot[275:427,])
purchase=predict(lmFit,dataset2)