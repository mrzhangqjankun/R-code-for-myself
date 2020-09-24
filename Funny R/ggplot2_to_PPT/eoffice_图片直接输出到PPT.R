##2020.7.13

##https://mp.weixin.qq.com/s/EhjfEz5pvvzlijvLcjCcxA
##不需要画图，直接把对象传给eoffice::topptx()，就可以直接出pptx文件

library(ggplotify)
library(eoffice);?topptx()

getwd()
setwd("E:/桌面/2017.8.11-8.17-内蒙采样/Results_2020.6/2. 去掉E2做Deblur和Uparse/Figure/")
f = "eoffice.pptx" #~/eoffice.pptx
p = as.ggplot(~plot(cars, cex.lab=2, cex.main=2,
                    xlab="biobabble", ylab="biobabble",
                    main = "Y叔叔演示专用"))
topptx(p, f)

#直接在R里打开
#remove.packages("rvcheck")
library(rvcheck)
o(f)
?o
