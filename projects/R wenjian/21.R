library("BSDA")
tab<-data.frame(花费时间=c(53,20,79,43,63,48,66,49,52,88,49,
                       47,97,56,51,50,53,54,55,55,55,55,56,56,
                       57,57,57,57,57,57,57,58,58,58,58,58,59,60,
                       62,62,63,63,64,65,66,66,66,66,66,67))
#已知
z.test(tab$花费时间,mu=0,sigma.x = 20,conf.level = 0.95)$conf.int
#未知
z.test(tab$花费时间,mu=0,sigma.x = sd(tab$花费时间),conf.level = 0.90)$conf.int


##########
tab<-data.frame(pm2.5=c(81.6,86.6,80.0,85.8,78.6,58.3,68.7,73.2,
                        96.6,74.9,83.0,66.6,68.6,70.9,71.7,71.6,
                        77.3,76.1,92.2,72.4,61.7,75.6,85.5,72.5,
                        74.0,82.5,87.0,73.2,88.5,86.9,94.9,83.0))
z.test(tab$pm2.5,mu = 0,sigma.x = sd(tab$pm2.5),conf.level = 0.95)$conf.int
z.test(tab$pm2.5,mu=82,sigma.x = sd(tab$pm2.5),alternative = "less",
       conf.level = 0.95)

##########
td<-data.frame(金属重量=c(22.6,26.6,23.1,23.5,
                      27.0,25.3,28.6,24.5,
                      26.2,30.4,27.4,24.9,
                      25.8,23.2,26.9,26.1,
                      22.2,28.1,24.2,23.6))
z.test(td$金属重量,mu = 0,sigma.x = sd(td$金属重量),conf.level = 0.95)

z.test(td$金属重量,mu = 25,sigma.x = 5,alternative = "greater",conf.level = 0.99)
z.test(td$金属重量,mu=25,sigma.x = sd(td$金属重量),alternative = "greater",
       conf.level = 0.95)


###########

n<-550
x<-115
p<-x/n
pi0<-0.17
z<-(p - pi0)/sqrt(pi0*(1-pi0)/n)
p_value<--1-pnorm(z)
data.frame(z,p_value)
#属实



n<-500
x<-50
p<-x/n
pi0<-0.64
z<-(p - pi0)/sqrt(pi0*(1-pi0)/n)
p_value<--1-pnorm(z)
data.frame(z,p_value)