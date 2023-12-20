zc=c("食品烟酒","衣着","居住","生活用品及服务","交通通信","教育文化娱乐","医疗保健","其他用品及服务")
bj=c(8751.4,1924.0,17163.1,2306.7,3925.2,3020.7,3755.0,880.0)
tj=c(9122.2,1860.4,7770.0,1804.1,4045.7,2530.6,2811.0,950.7)
sh=c(11515.1,1763.5,16465.1,2177.5,4677.1,3962.6,3188.7,1089.9)
cq=c(8618.8,1918.0,4970.8,1897.3,3290.8,2648.3,2445.3,675.1)
tab=data.frame(支出项目=zc,北京=bj,天津=tj,上海=sh,重庆=cq)
tab
#轮廓图
mat<-as.matrix(tab[,2:5])
rownames(mat)=tab[,1]
#library("DescTools")
par(mai=c(0.6,0.6,0.1,0.1),cex=0.7)
PlotLinesA(mat,xlab="消费项目",ylab="支出金额",args.legend=NA,
           col=1:4,pch=21,pch.col = 1,pch.bg = "white",pch.cex = 1)
legend(x="topright",legend = rownames(t(mat)),lty = 1,
       col = 1,box.col = "grey80",inset = 0.01,ncol = 1,cex = 0.8)

PlotLinesA(t(mat),xlab="消费项目",ylab="支出金额",args.legend=NA,
           col=1:4,pch=21,pch.col = 1,pch.bg = "white",pch.cex = 1)
legend(x="topright",legend = rownames(t(mat)),lty = 1,
       col = 1,box.col = "grey80",inset = 0.01,ncol = 1,cex = 0.8)

year<-c(2000,2001,2002,2003,0004,2005,2006,2007,2008,2009,2010,2011,
        2012,2013,2014,2015,2016,2017,2018,2019,2020)
city<-c(6972,1234,2597,2458,3468,2684,2364,7854,1598,9854,3264,
        25469,81452,65247,89939,57387,88488,66646,4522,56225,36355)
n<-c(6584,9999,6542,1564,9874,4652,1365,4569,8745,3265,1254,
     6544,4555,6555,26999,23654,45879,65478,16584,12658,65478)
df<-data.frame(年份=year,城市居民消费水平=city,农村居民消费水平=n)
df

#library("reshape2")
#library("ggplot2")
tab <- melt(df,id.vars = "年份",
            variable.name = "居民",value.name = "消费水平")
mytheme<-theme(legend.position = c(0.12,0.85),
               legend.text = element_text(size = "7"),
               axis.title = element_text(size = 10),
               axis.text = element_text(size=7))
ggplot(tab,aes(x=年份,y=消费水平,color=居民))+geom_line()+mytheme
#面积图
ggplot(tab,aes(x=年份,y=消费水平,fill=居民))+geom_area()+mytheme