#数据类型转换
#1.将数据框转换为向量
names <- c("刘文涛","王钰霄","天似玉","徐丽娜","丁文彬");
stat <- c(11,22,33,44,55)
math <- c(12,13,141,15,14)
econ <- c(34,35,67,89,09)
table <- data.frame(姓名 = names,统计学 = stat,数学 = math,经济学 = econ)
table
vector1<-as.vector(table$数学)
vector1
vector2<-as.vector(c(table$数学,table$经济学))
vector2
vector3<-as.vector(as.matrix(table[,2:4]))
vector3

#2.将数据框转换成矩阵
table
mat<-as.matrix(table[,2:4])#将数据框中的2到4列变成矩阵
mat
rownames(mat)<-table[,1]
mat

#3.短格式变成长格式
table
install.packages("reshape2")
library("reshape2")
df<-melt(table,vars="姓名",variable.name = "课程",value.name = "成绩")
df