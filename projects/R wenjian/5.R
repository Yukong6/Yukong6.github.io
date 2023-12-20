number <- c(240,213,289,277,270,255,214,256,196,246,
            188,178,172,181,251,195,280,244,183,215,
            218,204,191,248,262,175,267,273,211,235,
            287,202,252,266,194,250,212,177,170,174)
table <- data.frame(销售额 = number)
table
install.packages("DescTools")
library("DescTools")
tab1<-Freq(table$销售额)#默认进行分组，求里面每个
#组的频数，百分比，累计频数，累计百分比
tab1

tab2 <- Freq(table$销售额,breaks = c(240,213,289,277,270,255,214,256,196,246),
             right=FALSE)
tab3 = data.frame(分组=tab2$level,频数=tab2$freq,频数百分比=tab2$perc*100,
                  累计频数=tab2$cumfreq,累计频数百分比 = tab2$cumperc*100)
tab3