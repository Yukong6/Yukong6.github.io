#155页练习二第一题

height<-c(161.3,162.2,164.9,165.3,165.5,166.5,166.6,168,168.1,168.3,168.4,170.1,170.1,171.2,171.3,172.1,172.6,174.4,175.3,175.5)
weight<-c(53.7,55.3,57.5,57.5,58.3,58.6,58.8,58.8,59.2,61.4,62.2,62.4,62.8,63,63.3,64.4,64.7,64.9,66.1,67.5)
tab<-data.frame(身高=height,体重=weight)
tab
library("ggpubr")
ggscatterhist(data = tab,x="身高",y="体重",
              size = 1,color = "red",
              margin.plot = "boxplot",
              margin.params = list(fill = "lightblue",color = "blue"))

cor(tab$身高,tab$体重)


#101，5
A<-c(164,167,168,165,170,165,164,168,164,162,163,166,167,166,165)
B<-c(129,130,129,130,131,130,129,127,128,128,127,128,128,125,132)
C<-c(125,126,126,127,126,128,127,126,127,127,125,126,116,126,125)
df<-data.frame(方法A=A,方法B=B,方法C=C)
df
library("pastecs")
round(stat.desc(df),4)
mean<-apply(df, 2, mean)#按列就算平均数
sd<-apply(df,2,sd)#按列计算标准差
cv<-sd/mean#计算离散系数
df<-data.frame(平均数=mean,标准差=sd,离散系数=cv)
round(df,4)