tab=data.frame(六月份每天货物配送量=c(18.4,23.3,27.3,25.9,24.1,27.1,23.0,24.0,27.3,30.4,
     27.5,22.2,24.8,22.9,22.9,22.5,20.1,22.7,22.0,25.0,
     25.0,31.1,26.2,22.4,23.8,29.3,25.9,29.1,25.2,22.7))
#平均数
mean(tab$六月份每天货物配送量)
#中位数
median(tab$六月份每天货物配送量)
#四分数
quantile(tab$六月份每天货物配送量,probs = c(0.25,0.75),type=7)
#第80个百分位数
quantile(tab$六月份每天货物配送量,probs = c(0.01,0.9),type=7)
#众数
#library("DescTools")
Mode(tab$六月份每天货物配送量)
#极差
R<-max(tab$六月份每天货物配送量)-min(tab$六月份每天货物配送量)
R
#四分位差
IQR(tab$六月份每天货物配送量,type=7)
#方差
var(tab$六月份每天货物配送量)   
#标准差
sd(tab$六月份每天货物配送量)
#偏度系数和峰度系数
skewness(tab$六月份每天货物配送量,type=2)
kurtosis(tab$六月份每天货物配送量,type=2)
#标准分数
scale(tab$六月份每天货物配送量)