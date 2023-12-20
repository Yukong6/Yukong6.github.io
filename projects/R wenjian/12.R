install.packages("DescTools")
library("DescTools")
install.packages("RColorBrewer")
library("RColorBrewer")
install.packages("waterfalls")
library("waterfalls")

zc=c("食品烟酒","衣着","居住","生活用品及服务","交通通信","教育文化娱乐","医疗保健","其他用品及服务")
bj=c(8751.4,1924.0,17163.1,2306.7,3925.2,3020.7,3755.0,880.0)
tj=c(9122.2,1860.4,7770.0,1804.1,4045.7,2530.6,2811.0,950.7)
sh=c(11515.1,1763.5,16465.1,2177.5,4677.1,3962.6,3188.7,1089.9)
cq=c(8618.8,1918.0,4970.8,1897.3,3290.8,2648.3,2445.3,675.1)
tab=data.frame(支出项目=zc,北京=bj,天津=tj,上海=sh,重庆=cq)
tab
par(mfrow=c(2,1),mai=c(0.6,1,0.3,0.1),cex=0.7,cex.main=1,font.main=1)
#horiz=TURE使用水平条形图xlab x轴的文本标签xlim x轴的取值范围
b1=barplot(tab$北京,horiz = TRUE,xlab = "支出金额",names=tab$支出项目,las=2,xlim = c(0,20000),las=1,col="skyblue",border = "blue",main = "北京各项支出水平条形图")
#往条形图上添加数值
BarText(tab$北京,b=b1,horiz = TRUE,cex=1,col="red")

cols=brewer.pal(4,"Set2")
value=c(8951.4,9122.2,11515.1,8618.8)
names=c("北京","天津","上海","重庆")
b2=barplot(value,names=names,col=cols,horiz=FALSE,xlab="地区",ylab="支出金额",ylim=c(0,15000),main="4个地区食品烟酒支出垂直条形图")
BarText(value,b=b2,horiz = FALSE,cex=1,pos="topin")

#帕累托图（***报错***）
par(mai=c(0.7,0.7,0.2,0.7),cex=0.7)
x=sort(tab$北京,decreasing = TRUE)#支出进行降序排序
palette=rev(brewer.pal(8,"Blues"))#设置连续性调色板
bar=barplot(x,xlab="支出项目",ylab = "支出金额",col=palette,ylim = c(0,20000),)
text(bar,x,labels=x,pos=3,col="red")
y=cumsum(x)/sum(x)#计算累计值
par(new=T)
plot(y,type="b",pch=15,axes=FALSE,xlab='',ylab = '',main='')
axis(side=4)
mtext("累计频率",side=4,line=3,cex=0.8)
text(labels="累计分布曲线",x=4.5,y=0.91,cex=1)

#簇状条形图（***报错***）
mat<-as.matrix(tab[2:5])
rownames(mat)<-tab[,1]
par(mfrow=c(2,1),mai=c(0.6,1,0.3,0.3),cex=0.7,cex.main=1,font.main=1)
col=brewer.pal(4,"Set2")
barplot(t(mat),col=col,horiz=TRUE,beside=TRUE,xlab="支出金额",names=tab$支出项目,las=2,xlim=c(0,20000),las=1,main="4个地区的各项支出水平簇状图"
        ,legend=rownames(t(mat)),args.legend = list(x=15000,y=38,ncol=1,cex=0.8,box.col="grey80"))

#堆积条形图
cols=rev(brewer.pal(8,"Reds"))
barplot(mat,col = cols,horiz = FALSE,beside = FALSE,xlab = "地区",ylab = "支出金额",main="4个地区的各项支出堆积垂直条形图",legend=rownames(mat),args.legend = list(x=4.8,y =45000,ncol=1,cex=0.8,box.col="grey80"))

#绘制百分比的条形图（***报错***）


#计算各项支出的百分比
p1=tab$北京/sum(tab$北京)*100
p2=tab$天津/sum(tab$天津)*100
p3=tab$上海/sum(tab$上海)*100
p4=tab$重庆/sum(tab$重庆)*100

#计算完成的百分比的结果做成矩阵
pd=data.frame(支出项目=tab[,1],北京=p1,天津=p2,上海=p3,重庆=p4)
maat=as.matrix(pd[,2:5])
rownames(mat)=pd[,1]
pd

#绘制堆积条形图
par(mai=c(0.7,0.7,0.6,0.2),cex=0.8,cex.main=1.1,font.main=1)
cols=rev(brewer.pal(8,"Set1"))#离散调色板
b1=barplot(mat,col=cols,horiz=FALSE,beside=FALSE,xlab="地区",ylab="支出百分比",main="4个地区支出的百分比",legend=rownames(mat),args.legend=list(x=4.8,y=115,ncol=4,cex=0.8,box.col="grey80"))
BarText(round(mat,2),b=b1,horiz = FALSE,cex=0.7,pos="mid")

#数据
zc=c("食品烟酒","衣着","居住","生活用品及服务","交通通信","教育文化娱乐","医疗保健","其他用品及服务")
bj=c(8751.4,1924.0,17163.1,2306.7,3925.2,3020.7,3755.0,880.0)
tj=c(9122.2,1860.4,7770.0,1804.1,4045.7,2530.6,2811.0,950.7)
sh=c(11515.1,1763.5,16465.1,2177.5,4677.1,3962.6,3188.7,1089.9)
cq=c(8618.8,1918.0,4970.8,1897.3,3290.8,2648.3,2445.3,675.1)
tab=data.frame(支出项目=zc,北京=bj,天津=tj,上海=sh,重庆=cq)
tab

#瀑布图

df=data.frame(支出项目=tab$支出项目,支出金额=tab$北京)#北京的数据
cols=brewer.pal(8,"Set1")
waterfall(.data=df,rect_text_labels = paste(df$支出金额),#设置矩形标签
          fill_colours = cols,calc_total = TRUE,total_rect_color = "pink",#设置总和矩形
          total_axis_text = paste("总支出",'\n',sum(df$支出金额)),total_rect_text_color = "black",
          rect_border = "grey50",fill_by_sign = FALSE)

#漏斗图
df<-tab[order(tab$北京,decreasing = FALSE),]#数据升序排序
par(mai=c(0.1,0.3,0.1,0.3),cex=0.8,font.main=1)
barplot(df$北京,horiz=TRUE,axes=FALSE,border=FALSE,
        col="steelblue",space=0.1,xlim=c(-17200,17200))
barplot(-df$北京,horiz=TRUE,axes=FALSE,border=FALSE,
        col="steelblue",space=0.1,xlim=c(-17200,17200),add=TRUE)
text(x=rep(-14000,8),y=seq(1,8,length.out=8),labels = df$支出项目,cex = 0.9)
text(x=rep(0.8),y=seq(1,8,length.out=8),labels = df$北京,cex = 0.8,col = "white")

#饼图
p1=round(tab$北京/sum(tab$北京)*100,2)#预留俩位小数
names=(tab$支出项目)
labs=paste(names," ",p1,"%",sep = "")#标题向量
pie(p1,labels = labs,
    init.angle = 90,#设置初始角度为90度
    radius = 1,#设置饼图半径为1
    main = "")#图的名字加不加无所谓

#绘制3D饼图
#install.packages("plotrix")
#install.packages("RColorBrewer")
#library("plotrix")
#library("RColorBrewer")
p2=round(tab$重庆/sum(tab$重庆)*100,2)
labs=paste(names,"","%",sep = "")
pie3D(p2,labels = labs,explode = 0,labelcex = 0.7,
      col = brewer.pal(8,"Set1"),main="")

#绘制环形图
p1=round(tab$北京/sum(tab$北京)*100,1)
p2=round(tab$天津/sum(tab$天津)*100,1)
p3=round(tab$上海/sum(tab$上海)*100,1)
p4=round(tab$重庆/sum(tab$重庆)*100,1)
names=tab$支出项目
labs1=paste(names,"",p1,"%",sep="")
labs2=paste(p2,"%",sep="")
labs3=paste(p3,"%",sep="")
labs4=paste(p4,"%",sep="")

par(mai=c(0.2,0.4,0.2,0.4),cex=0.7)
pie(p1,labels = labs1,init.angle = 90,col = brewer.pal(8,"Set3"),radius = 1)
par(new=TRUE)
pie(p2,labels = labs2,init.angle = 90,col = brewer.pal(8,"Set3"),radius = 0.8)
par(new=TRUE)
pie(p3,labels = labs3,init.angle = 90,col = brewer.pal(8,"Set3"),radius = 0.6)
par(new=TRUE)
pie(p4,labels = labs4,init.angle = 90,col = brewer.pal(8,"Set3"),radius = 0.4)
par(new=TRUE)
pie(1,labels = "",init.angle = 90,col = "white",border="white",radius = 0.2)

#树状图
#install.packages("reshape2")
#install.packages("treemap")
#library("reshape2")
#library("treemap")
tab
df<-melt(tab,id.vars="支出项目",variable.name="地区",
        value.name = "支出金额")
df
treemap(df,index = c("地区","支出项目"),
        vSize = "支出金额",
        type = "index",
        fontsize.labels = 9,
        position.legend = "bottom",
        title = "")

#旭日图
install.packages("d3r")
install.packages("sunburstR")
library("d3r")
library("sunburstR")
library("reshape2")
df<-melt(tab,id.vars="支出项目",variable.name="地区",
         value.name = "支出金额")
df<- data.frame(df[,2],df[,c(1,3)])
df
df_tree<-d3_nest(df,value_cols="支出金额")
sunburst(data=df_tree,valueField = "支出金额",
         count = TRUE,
         sumNodes = TRUE)

