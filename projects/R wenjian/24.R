#求环比增长率和定基增长率
year<-c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
person_money<-c(12008,14074,15586,17220,18857,20801,22969,25245,27504,27438)
tab<-data.frame(年份=year,居民消费水平=person_money)
g1=round(exp(diff(log(tab[,2]))-1)*100,2)#计算环比增长率，结果保留两位小数
g2=round((tab[,2]/tab[1,2,]-1)*100,2)#计算定基增长率
data.frame(tab[,1,2],环比增长率=c(NA,g1),定基增长率=replace(g2,g2==0,NA))

#求平均增长率
g_bar<-(((tab[10,2]/tab[1,2])^(1/9))-1)*100
round(g_bar,2)

#2021
27438*(1+0.0897) 
#2022
27438*(1+0.0897)^2

#预测方法的选择与评估
year<-c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
lr<-c(1200,1750,2938,3125,3250,3813,4616,4125,5386,5313,6250,5623,6000,6563,6682,7500)
cl<-c(25,84,124,214,216,354,420,514,626,785,1006,1526,2156,2927,4195,6692)
cb<-c(27,60,73,121,126,172,218,227,254,223,226,232,200,181,153,119)
xs<-c(189,233,213,230,223,240,208,209,208,198,223,195,202,227,254,222)
tab<-data.frame(年份=year,净利润=lr,产量=cl,管理成本=cb,销售价格=xs)
df<-ts(tab,start = 2006)
par(mfrow=c(2,2),mai=c(0.65,0.65,0.3,0.1),lab=c(15,6,1),cex=0.8,cex.main=1,font.main=1)
plot(df[,2],type="o",xlab="时间",ylab="净利润",main="(a)净利润")
plot(df[,3],type="o",xlab="时间",ylab="产量",main="(b)产量")
plot(df[,4],type="o",xlab="时间",ylab="管理成本",main="(c)管理成本")
plot(df[,5],type="o",xlab="时间",ylab="销售价格",main="(d)销售价格")

#确定模型参数α和系数a
df<-ts(tab,start = 2006)
pforecase<-HoltWinters(df[,5],beta = FALSE,gamma = FALSE)
#alpha为平滑系数，beta表示HoltWinters
#平滑的参数，当beta = FALSE,gamma = FALSE的时候
#表示做简单字数平滑预测
#a 228,3773是模型的系数
pforecase

#计算历史拟合值
pforecase$fitted
#绘制观测值和拟合图
par(mai=c(0.7,0.7,0.1,0.1),cex=0.8,lab=c(19,5,1))
res<-df[,5]-pforecase$fitted#计算残差
plot(res[,1],type="o",xlab="时间",ylab="残差")
abline(h=0,lty=2,col="red")



par(mai=c(0.7,0.7,0.1,0.1),cex=0.8,lab=c(19,5,1))
plot(res[,1],type="o",xlab="时间",ylab="销售价格")
lines(df[,1][-1],pforecase$fitted[,1],type = "o",lty=2,col="blue")
legend(x="topleft",legend = c("观测值","拟合值"),lty=1:2,col = c(1,4),fill = c(1,4),
       box.col="grey80",inset=0.01,cex0.8)

#计算拟合值的残差，并绘制残差图检测拟合值拟合效果
par(mai=c(0.7,0.7,0.1,0.1),cex=0.8,lab=c(19,5,1))
res<-df[,5]-pforecase$fitted#计算残差
plot(res[,1],type="o",xlab="时间",ylab="残差")
abline(h=0,lty=2,col="red")

#2022年销售价格的预测值
#install.packages("forecast")
#library("forecast")
pforecast1 <- forecast(pforecase, h=1)
pforecast1

#绘制实际值和预测值图
plot(pforecast1,type="o",xlab="时间",ylab="销售价格",main="")
#拟合一元回归方程
fit<-lm(净利润~年份,data=tab)
summary(fit)
#计算各年的预测值
predata<-predict(fit,data.frame(年份=2006:2022))
predata
#计算拟合残差
res<-fit$res
res
#计算拟合观测值和预测值
plot(2006:2022,predata,type = "o",lty=2,col="blue",xlab="时间",ylab="利润率")
legend(x="topleft",legend = c("观测值","预测值"),lty=1:2,cex = 0.8,fill = c(1,4))
abline(v=2021,lty=6,col=2)
#绘制残差图
par(mai=c(0.85,0.7,0.3,0.1),cex=0.8,lab=c(19,5,1))
fit<-lm(净利润~年份,data=tab)
plot(fit,which=1)


year<-c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
lr<-c(1200,1750,2938,3125,3250,3813,4616,4125,5386,5313,6250,5623,6000,6563,6682,7500)
cl<-c(25,84,124,214,216,354,420,514,626,785,1006,1526,2156,2927,4195,6692)
cb<-c(27,60,73,121,126,172,218,227,254,223,226,232,200,181,153,119)
xs<-c(189,233,213,230,223,240,208,209,208,298,223,195,202,227,254,222)
tab<-data.frame(年份=year,净利润=lr,产量=cl,管理成本=cb,销售价格=xs)
#指数曲线的拟合
df<-ts(tab,start = 2006)
y<-log(df[,3])#对产量数据取对数
x<- 1:16#设置自变量的值
fit<-lm(y~x)#拟合曲线模型
fit
#计算指数
exp(3.7046)
#曲线拟合方程 y = 40.63379*e^0.3106t
#历史数据预测2022年的产量
predata<-exp(predict(fit,data.frame(x=1:17)))
predata
#各年预测残差
predata<-exp(predict(fit,data.frame(x=1:16)))
predata<-ts(predata,start = 2006)
residuals<-df[,3]-predata
residuals
#绘制实际值和预测值的图
par(mai=c(0.7,0.7,0.15,0.1),cex = 0.8,lab=c(19,5,1))
predata<-exp(predict(fit,data.frame(x=1:17)))
plot(2006:2022,predata,type="o",lty=2,col=4,xlab = "时间",ylab="产量")
points(df[,1],df[,3],type = "o",pch=19)
legend(x="topleft",legend = c("观测值","预测值"),lty=1:2,col = c(1,4),fill = c(1,4),cex = 0.8)
abline(v=2021,lty=6,col=2)
#残差图
plot(2006:2021,residuals,type="o",lty=2,col=4,xlab = "时间",ylab="残差")
abline(h=0,lty=12,col=2)


#拟合二阶曲线模型
y<-tab[,4]
x<-1:16
fit<-lm(y~x+I(x^2))
fit

#二阶曲线预测值
predata<-predict(fit,data.frame(x = 1:17))
predata
#二阶曲线预测值的残差
residual<-fit$residuals
residual
#实际值和预测值的曲线图
par(mai=c(0.7,0.7,0.15,0.1),cex = 0.8,lab=c(19,5,1))
predata<-predict(fit,data.frame(x=1:17))
plot(2006:2022,predata,ylim=c(0,260),type = "o",lty=2,col="red",xlab = "时间",ylab="管理成本")
lines(tab[,1],tab[,4],type = "o",lty=1,col="blue")
abline(v=2021,lty=6,col=2)
legend(x="bottom",legend = c("观测值","预测值"),lty=1:2,col = c("black","blue","red"),
       fill = c("blue","red"))
#残差图
plot(2006:2021,residual,type="o",lty=2,col=4,xlab = "时间",ylab="残差")
abline(h=0,lty=12,col=2)


year<-c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
lr<-c(1200,1750,2938,3125,3250,3813,4616,4125,5386,5313,6250,5623,6000,6563,6682,7500)
cl<-c(25,84,124,214,216,354,420,514,626,785,1006,1526,2156,2927,4195,6692)
cb<-c(27,60,73,121,126,172,218,227,254,223,226,232,200,181,153,119)
xs<-c(189,233,213,230,223,240,208,209,208,298,223,195,202,227,254,222)
tab<-data.frame(年份=year,净利润=lr,产量=cl,管理成本=cb,销售价格=xs)
#移动平均
df<-ts(tab,start = 2006,end = 2021)
df
library("DescTools")
ma3<-MoveAvg(df[,5],order = 3,#3期移动平均
             align = "center",#结果中心对齐
             endrule = "NA")#没有数据的用NA代替
data.frame(年份=df[,1],销售价格=df[,5],ma3)

#绘制实际值和平滑值的图
par(mai=c(0.7,0.7,0.15,0.1),cex=0.58,lab=c(19,5,1))
plot(df[,1],df[,5],type="o",xlab="年份",ylab="销售价格")
lines(ma3,type="o",lty=2,col="red")
legend(x="topleft",legend=c("销售价格","ma3"),lty=c(1,2),
       col=c("black","red"),fill=c(1,2),
       cex=0.8)