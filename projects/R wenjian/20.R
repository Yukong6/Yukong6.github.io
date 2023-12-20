#计算 p(x>80)的值
1-pnorm(80,mean = 50,sd=10)
#计算 p(20<=x<=80)就是用80的概率减去20的概率
pnorm(30,mean = 50,sd = 10)-pnorm(20,mean = 50,sd=10)

#计算标准正态分布
pnorm(-2,mean = 0,sd=1)
1-pnorm(1.5,mean = 0,sd=1)#大于1.5的概率

#计算标准正态分布分位数
#qnorm分位数函数
qnorm(0.025,mean = 0,sd=1)
#mean表示的是均值，标准差sd时的分位数
qnorm(0.95,mean = 0,sd=1)

#T分布
pt(-2,df=10)#自由度为10的，t值小于-2的概率
1-pt(3,df=15)#自由度为15的，t值大于3的概率

2*(1-pt(2.5,df=12))#自由度为12的，t值等于2.5的双尾概率

#计算分位数，t分布的
qt(0.025,df=25)#自由度为25，t分布左尾的概率为0.025的时t的值
qt(0.025,df=20)#自由度为20，t分布左双的概率为0.05时t的值

#求置信区间
#已知方差怎么求
install.packages("BSDA")
library("BSDA")
tab<-data.frame(食品重量=c(480.95,482.80,483.18,483.90,485.92,
                       486.44,486.74,486.96,487.21,487.44,487.71,
                       487.72,487.92,488.20,488.78,489.20,491.54,
                       491.83,492.79,493.75,494.75,495.18,495.90,
                       497.01,499.46,499.52,499.53,500.84,501.64,
                       501.70,502.02,502.52,502.66,502.86,504.42,
                       504.45,504.60,504.64,504.76,505.48,505.64,
                       505.77,506.16,506.29,506.64,506.69,506.75,
                       507.75,508.17,508.18))
z.test(tab$食品重量,mu=0,sigma.x = 5,conf.level = 0.95)#mu待检测均值，sigma.x为标准差
#不知道方差的时候怎么求置信区间
z.test(tab$食品重量,mu=0,sigma.x = sd(tab$食品重量),conf.level = 0.95)
#只让显示置信区间
z.test(tab$食品重量,mu=0,sigma.x = sd(tab$食品重量),conf.level = 0.95)$conf.int

df<-data.frame(使用寿命=c(10018,10638,9803,10488,11192,9727,9907,9234,10282,9037))
z.test(df$使用寿命,mu=0,sigma.x = 500,conf.level = 0.95)$conf.int
#未知标准差的时候
z.test(df$使用寿命,mu=0,sigma.x = sd(df$使用寿命),conf.level = 0.95)$conf.int

#总体比例的区间估值
n<-100
x<-65
p<-x/n
q<-qnorm(0.975)
LCI<-(p-q*sqrt(p*(1-p)/n))*100#计算置信下限
UCI<-(p+q*sqrt(p*(1-p)/n))*100#计算置信上限
data.frame(LCI,UCI)

#已知方差的条件下计算大样本检验
install.packages("TeachingDemos")
library("TeachingDemos")
#mu为假设的总体均值，alternative为备选假设选项，默认为双位检测，
#stdev为已知的总体标准差，n为样本量，conf.level为置信水平
z.test(255.8,mu=255,stdev = 5,n=40,alternative = "two.sided",
       conf.level = 0.95)
tab<-data.frame(蛋白质含量=c(2.96,2.96,2.92,3.01,2.96,2.95,3.07,2.86,2.95,
                        2.96,2.98,2.91,2.95,3.04,2.86,2.94,2.93,2.91,2.95,
                        2.91,2.84,3.13,3.02,3.02,2.94,2.98,2.92,3.06,3.06,
                        2.86,3.05,2.99,3.00,3.00,3.02,2.88,3.03,3.01,3.05,
                        2.98,2.98,3.00,2.93,2.98,3.05,2.97))
library("BSDA")
#已知总体方差的计算方式
z.test(tab$蛋白质含量,mu=3,sigma.x=0.07,alternative = "less",conf.level = 0.99)
#未知总体方差的计算方式
z.test(tab$蛋白质含量,mu=3,sigma.x=sd(tab$蛋白质含量),alternative = "less",
       conf.level = 0.99)

df<-data.frame(手机游戏时间=c(2.2,2.5,2.4,1.5,0.3,3.5,2.4,0.9,3.3,2.9,
                        2.8,1.6,2.2,3.8,4.0,1.8,3.0,1.7,0.8,3.4))
#已知方差
z.test(df$手机游戏时间,mu=2,sigma.x = 0.8,alternative = "greater",conf.level = 0.95)
#未知方差
z.test(df$手机游戏时间,mu=2,sigma.x = sd(df$手机游戏时间),alternative = "greater",
                                   conf.level = 0.95)
#总体比例的检验
n<-200
x<-170
p<-x/n
pi0<-0.8
z<-(p - pi0)/sqrt(pi0*(1-pi0)/n)
p_value<--1-pnorm(z)
data.frame(z,p_value)