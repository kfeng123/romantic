dataset1 <- pai.inputPort(1) # class: data.frame
user_balance=dataset1

temp=apply(user_balance[,c(-1,-2)],MARGIN=1,FUN=sum)
user_balance=user_balance[temp!=0,]

###提取特征


pai.outputPort(1, dataname)
