##2019.11.16

#https://mp.weixin.qq.com/s/okjWi8Pn8kaOBIeXSI17pw

d = matrix(rnorm(4), ncol=2)
colnames(d) = c("A|B", "123")
write.csv(data.frame(d))

# |被去掉了，且数字前面加X

#其实如果我们不data.frame强转的话，一切都是OK的。
write.csv(d)


##如何能够避免这个坑
#用tibble
library(dplyr)
tibble::as_data_frame(d) %>% write.csv  # or write.table

