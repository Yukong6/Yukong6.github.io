bh<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
sr<-c(2100.47,2860.56,4143.96,4515.08,5237.88,5622.72,5769.49,
      6213.26,6470.69,6911.74,6936.50,7395.41,7673.79,8257.39,
      8545.80,9191.71,11100.32,11364.24,12217.61,12481.56)
zc<-c(813.36,916.46,987.02,1056.42,1150.39,1360.96,1551.58,
      1578.94,1592.87,1630.26,1691.16,1776.52,1914.39,1941.70,
      1951.71,1965.40,2250.06,2841.85,2961.95,2974.47)
tab<-data.frame(企业编号=bh,销售收入=sr,广告支出=zc)
tab
#install.packages("ggpubr")
library("ggpubr")
ggscatterhist(data = tab,x="广告支出",y="销售收入",
              size = 2,color = "blue4",
              margin.plot = "boxplot",
              margin.params = list(fill = "lightblue",color = "blue"))
              
                        
#相关系数
cor(tab$销售收入,tab$广告支出)

#回归模型的拟合
model<-lm(销售收入~广告支出,data = tab)
summary(model)

#计算回归系数的置信区间
confint(model,level = 0.95)

#输出模型的方差分析表
anova(model)

#绘制拟合图
model<-lm(销售收入~广告支出,data = tab)
plot(销售收入~广告支出,data=tab)
text(销售收入~广告支出,data=tab,labels=企业编号,cex=0.6,adj=c(-0.6,25),col="blue4")
abline(model,col="red",lwd=2)
n=nrow(tab)
#绘制线段
for(i in 1:n){segments(tab[i,3],tab[i,2],tab[i,3],model$fitted[i],col = 4)}
mtext(expression(hat(y)==-569.4359+(4.4804%)*%广告支出),cex=0.7,side=1,
                 line=-7.5,adj=0.75)
      
#回归预测，根据自变量求因变量
model<-lm(销售收入~广告支出,data = tab)
x0<-tab$广告支出
pre_model<-predict(model,data.frame(广告支出=x0))#计算点的预测值
res<-model$res#计算残差
df<-data.frame(tab,点预测值=pre_model,残差=res)
round(df,2)#保留两位小数
#当自变量x0=1000时候，收入预算是多少
x0 <- data.frame(广告支出 = 1000)
predict(model, newdata = x0)
#回归模型
plot(model,which = 1)#只输出第一幅图