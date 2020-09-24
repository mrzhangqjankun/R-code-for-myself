##2018.6.9
##ggplot2版本的维恩图 
##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247485828&idx=1&sn=186ba5f19b511373549b023983ca6880&scene=21#wechat_redirect

#ggvenn是非常简单的，你只要给定一个matrix，这个matrix每一个column代表一个分组，
#数字0代表没有overlap，而非0代表overlap，数字的大小可以当做是overlap的加权。
#当然还支持别形式的输入，具体可以参考venneuler的文档。

rm(list = ls())
install.packages("venneuler")
library(venneuler)
set.seed(2018-6-09)
x <- matrix(sample(0:4, 40, TRUE, c(.5, .1, .1, .1, .1)), ncol=4)
colnames(x) <- LETTERS[1:4]
x
yyplot::ggvenn(x)
