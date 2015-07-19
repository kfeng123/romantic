d2=read.csv("7_2.csv",header=FALSE)
d3=read.csv("7_3.csv",header=FALSE)
d4=read.csv("7_4.csv",header=FALSE)
d5=read.csv("7_5.csv",header=FALSE)
d6=read.csv("7_6.csv",header=FALSE)
d7=read.csv("7_7.csv",header=FALSE)
d8=read.csv("7_8.csv",header=FALSE)
d9=read.csv("7_9.csv",header=FALSE)
d10=read.csv("7_10.csv",header=FALSE)
d11=read.csv("7_11.csv",header=FALSE)
d12=read.csv("7_12.csv",header=FALSE)
d13=read.csv("7_13.csv",header=FALSE)
d14=read.csv("7_14.csv",header=FALSE)
d15=read.csv("7_15.csv",header=FALSE)

myTot=read.csv("offlineAnalysis/myTot.csv",header=FALSE)
plot(d13[,2],type="o")
points(d13[,2],type="o",col="red")
points(d13[,2],type="o",col="blue")


points(finalTot$total_redeem_amt[400:427],type="o",col="blue")

points(finalTot$total_redeem_amt[365:392],type="o",col="blue")

