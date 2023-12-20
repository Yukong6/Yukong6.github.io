names=c("刘文涛","张青松","王宇翔","田思雨","张三","李四","王五","赵六")
sexs=c("男","男","女","男","男","男","女","女")
projects=c("会计学","金融学","金融学","管理学","管理学","会计学","金融学","金融学")
ds = data.frame(姓名=names,性别=sexs,专业=projects)
ds
#统计性别
#tb=table(ds$性别)
#tb
#统计专业
#tproject= table(ds$专业)
#tproject


#prop.table(tproject)*100
  
#生成性别和专业的二维列表 用table函数去生成
tb2 = table(ds$性别,ds$专业)
tb2

addmargins(tb2)

prop.table(tb2)*100

#对百分比联表加入边际和的统计
addmargins(prop.table(tb2)*100)

#生成二维联表，性别和专业（提供转换数据框的数据）
targetTb= table(ds$性别,ds$专业)
targetTb
#1.引入包 2.使用包里面的函数untable
library("DescTools")
#install.packages("DescTools")
#把列联表转换成原始数据框
d =Untable(targetTb)
d
#重新命名
da= data.frame(性别=d$var1,专业= d$var2)
da
head(da,3)
tail(da,3)
