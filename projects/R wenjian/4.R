#数据框，等价于Excel表格的显示方式
names <- c("刘文涛","王钰霄","天似玉","徐丽娜","丁文彬");
stat <- c(11,22,33,44,55)
math <- c(12,13,141,15,14)
econ <- c(34,35,67,89,09)
table <- data.frame(姓名 = names,统计学 = stat,数学 = math,经济学 = econ)
table

#查看数据能使用的函数
#head(table,2)#前两行数据的显示
#call(table,2)#后两行数据的显示
#nrow(table)#查看数据几行
#ncol(table)#查看数据几列
#dim(table)#同时查看行列的数
#class(table)#查看数据的类型
#str(table)#查看数据的组成结构

table$姓名
table[1,2]
table[,2:4]
table[,c(2,4)]
table[c(1,5),4]
table[c(1,5),]


names1 <- c("刘文","王钰","天似","徐丽","丁彬");
stat1 <- c(11,22,33,44,55)
math1 <- c(12,13,141,15,14)
econ1 <- c(34,35,67,89,09)
table1 <- data.frame(姓名 = names1,统计学 = stat1,数学 = math1,经济学 = econ1)
table1

table2 <- rbind(table,table1)#按行合并
table2

names2 <- c("刘文","王钰","天似","徐丽","丁彬",
            "刘文涛","王钰霄","天似玉","徐丽娜","丁文彬");
finance <- c(13,45,67,89,09,7,4,3,2,2)
accounting <-c(1,2,3,4,5,6,7,8,9,10)
table3 <- data.frame(姓名 = names2,金融 = finance,会计 =accounting)
table3

table4 <- cbind(table2,table3[,2:3])
table4

#排序
install.packages("dplyr")
library("dplyr")
arrange(table4,姓名)