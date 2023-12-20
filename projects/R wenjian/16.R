zc=c("食品烟酒","衣着","居住","生活用品及服务","交通通信","教育文化娱乐","医疗保健","其他用品及服务")
bj=c(8751.4,1924.0,17163.1,2306.7,3925.2,3020.7,3755.0,880.0)
tj=c(9122.2,1860.4,7770.0,1804.1,4045.7,2530.6,2811.0,950.7)
sh=c(11515.1,1763.5,16465.1,2177.5,4677.1,3962.6,3188.7,1089.9)
cq=c(8618.8,1918.0,4970.8,1897.3,3290.8,2648.3,2445.3,675.1)
tab=data.frame(支出项目=zc,北京=bj,天津=tj,上海=sh,重庆=cq)
tab
#雷达图
install.packages("ggiraphExtra")
install.packages("ggplot2")
library("ggiraphExtra")
library("ggplot2")
ggRadar(data=tab,rescale=FALSE,aes(group=支出项目),#按照消费项目进行分组
        alpha = 0,size = 2)+#设置填充颜色透明度和节点大小
        theme(axis.text = element_text(size = 7),#设置坐标轴字体大小
              legend.position="right",#设置图例位置
              legend.text=element_text(size = 7))#设置图例字体大小
