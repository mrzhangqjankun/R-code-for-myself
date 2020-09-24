#2019.5.29
#https://www.tidyverse.org/articles/2019/05/vroom-1-0-0/

p_load(vroom)
?vroom  ##读取文件速度超级快

##读入OTU，注意从Results_2019.2.21里面读
setwd('E:/桌面/2017.8.11-8.17-内蒙采样/Results_2019.2.21/')

OTU_dat=vroom("uparse/97/uparse_otu_table_combine.txt")
spec(OTU_dat) #查看每一列变量类型


file <- "https://raw.githubusercontent.com/r-lib/vroom/master/inst/extdata/mtcars.csv"
data <- vroom(file)

#压缩文件也可以直接读取