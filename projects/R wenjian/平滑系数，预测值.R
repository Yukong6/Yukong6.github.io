year<-c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,
       2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
money<-c(100.7,99.2,101.2,103.9,101.8,101.5,104.8,105.9,99.3,103.3,
        105.4,102.6,102.6,102.0,101.4,102.0,101.6,102.1,102.9,102.5)
tab<-data.frame(年份=year,居民消费价格=money)
df<-ts(tab,start = 2021)
pforecase<-HoltWinters(df[,2],beta = FALSE,gamma = FALSE)
pforecase
#library("forecast")
pforecast1 <- forecast(pforecase, h=1)
pforecast1

plot(pforecast1,type="o",xlab="时间",ylab="居民消费价格",main="")

par(mai=c(0.7,0.7,0.1,0.1),cex=0.8,lab=c(19,5,1))
res<-df[,2]-pforecase$fitted#计算残差
plot(res[,1],type="o",xlab="时间",ylab="残差")
abline(h=0,lty=2,col="red")






year<-c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
Gdp<-c(487940.2,538580.0,592963.2,643563.1,688858.2,746395.1,832035.9,919281.1,986515.2,1015986.2)
tab<-data.frame(年份=year,生产总值=Gdp)
df<-ts(tab,start = 2021)

#求环比增长率和定基增长率
g1=round(exp(diff(log(tab[,2]))-1)*100,2)#计算环比增长率，结果保留两位小数
g2=round((tab[,2]/tab[1,2,]-1)*100,2)#计算定基增长率
data.frame(tab[,1,2],环比增长率=c(NA,g1),定基增长率=replace(g2,g2==0,NA))
#求平均增长率
g_bar<-(((tab[10,2]/tab[1,2])^(1/9))-1)*100
round(g_bar,2)

#计算各年的预测值
predata<-predict(fit,data.frame(年份=2011:2021))
predata

#绘制实际值和预测值图
plot(2011:2021,predata,type="o",lty=2,col="blue",xlab = "年份",ylab="生产总值")
points(df[,1],df[,2],type = "o",pch=19)
legend(x="topleft",legend = c("观测值","预测值"),lty=1:2,col = c(1,4),fill = c(1,4),cex = 0.8)
abline(v=2020,lty=6,col=2)
#绘制残差图
par(mai=c(0.85,0.7,0.3,0.1),cex = 0.8,lab=c(19,5,1))
fit<-lm(生产总值~年份,data = tab)
plot(fit,which = 1)






time<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
        20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35)
Closing_price<-c(33.82, 33.64, 34.00, 34.09, 34.27, 34.27, 34.00, 33.82, 33.91, 33.82, 33.55, 33.36, 
                 33.36, 33.18, 33.00, 32.64, 32.55, 32.64, 32.73, 32.45, 32.36, 32.00, 31.64, 32.09,
                 32.36, 32.36, 32.36, 32.64, 32.73, 32.45, 32.45, 32.27, 32.63, 33.00, 33.18)
tab = data.frame(时间=time,收益价格=Closing_price)
#拟合二阶曲线模型
y<-tab[,2]
x<-1:35
fit<-lm(y~x+I(x^2))
fit
# 拟合三阶曲线模型
y<-tab[,2]
x<-1:35
fit1<-lm(y~x+I(x^2)+I(x^3))
fit1

#二阶曲线预测值的残差
residual<-fit$residuals
residual
#残差图
plot(1:35,residual,type="o",lty=2,col=4,xlab = "时间",ylab="收益价格")
abline(h=0,lty=12,col=2)
#三阶曲线预测值的残差

#残差图
