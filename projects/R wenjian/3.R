#矩阵，二维数组
#使用matrix函数创建矩阵
c<-1:6;
#第一个元素是数据范围，第二个元素是表是矩阵有几行
#第三个元素是矩阵有几列，第四个元素是按行填充数据
mat <- matrix(c,nrow = 2,ncol = 3,byrow = TRUE)
mat
#后缀加行名
rownames(mat)<-c("甲","乙")
#添加列名字
colnames(mat)<-c("A","B","C")
mat