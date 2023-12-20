par(mfrow=c(2,2),mai=c(0.5,0.5,0.3,0.1),cex=0.7,cex.axis=0.8,cex.main=0.8,font.main=1)
set.seed(123)
x=rnorm(100)
y=rexp(100)
plot(x,y,col=sample(c("black","red","green"),100,replace = TRUE),main="(a)散点图")
boxplot(x,y,col = 2:3,main = "(b)箱形图")
hist(x,col = "orange",ylab="y",main = "(c)直方图")
barplot(runif(5,10,20),col = 2:6,main = "(d)条形图")

layout(matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE),heights = c(2,1))
layout.show(3)

layout(matrix(c(1,1,1,2,3,4,4,3),nrow = 2,ncol = 3,byrow = TRUE),
       widths = c(3:1),heights = c(1,1))
layout.show(4)

layout(matrix(c(1,2,2,3,4,5,6,7,8),nrow = 3,ncol = 3,byrow = TRUE),
       widths = c(1:1),heights = c(1,1))
layout.show(8)

par(mfrow=c(2,3),mai=c(0.3,0.3,0.3,0.1),cex=0.7,mgp=c(1,1,0),cex.axis=0.7,cex.main=1,font.main=1)
x<-1:7
name=LETTERS[1:7]
barplot(x,names=name,col=rainbow(7),main="rainbow()")
barplot(x,names=name,col=rainbow(7,start = 0.1,end=0.5),main="rainbow(start=0.1,end=0.5)")
barplot(x,names=name,col=heat.colors(7),main="heat.colors()")
barplot(x,names=name,col=terrain.colors(6),main="terrain.colors()")
barplot(x,names=name,col=terrain.colors(7),main="terrain.colors()")
barplot(x,names=name,col=topo.colors(7),main="topo.colors()")
//barplot(x,names=name,col=cm.colors(7),main="cm.colors()")

install.packages("RColorBrewer")
library("RColorBrewer")

par(mfrow=c(2,3),mai=c(0.1,0.3,0.3,0,1),cex=0.6,font.main=1)
palette1<-brewer.pal(7,"Reds")
barplot(1:7,col=palette1,main = "(a)红色连续调色板")

palette2<-brewer.pal(7,"Set1")
barplot(1:7,col=palette2,main = "(b)离散调色板")