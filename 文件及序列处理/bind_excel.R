##2018.7.11
##EXCEL合并
https://mp.weixin.qq.com/s?__biz=MzA5NjU5NjQ4MA==&mid=2651159683&idx=1&sn=8cc4f43402de32de1859c5acea309826&chksm=8b5c7becbc2bf2fa72b3afe985745d12126b969080d727403ff3c05beb53cf4d6318106f148f&scene=0#rd

install.packages("xlsx")
library("xlsx")
filelist<-list.files(getwd())

MergeTable<-read.xlsx(filelist[1],sheetIndex=1,encoding="UTF-8")
for(i in 2:length(filelist))
{
  data<-read.xlsx(filelist[i],sheetIndex=1,encoding="UTF-8")
  MergeTable<-rbind(MergeTable,data)
}
write.xlsx(MergeTable,"M.xls", sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)


##改进.不知道行不行
MergeTable = c()
for(i in 1:length(filelist))
{
  data<-read.xlsx(filelist[i],sheetIndex=1,encoding="UTF-8")
  MergeTable<-rbind(MergeTable,data)
}
write.xlsx(MergeTable,"M.xls", sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)