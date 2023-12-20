a <- c("金融","数学","经济学","会计","金融")
f1 <- factor(a);#将a转化成因子
b <- as.numeric(f1);#将因子转换为数值
b

d <- c("好","很好","一般","差","很差")
f2 <- factor(d,ordered = TRUE,levels=c("很好","好","一般","差","很差"));
f2
x <- as.numeric(f2)
x