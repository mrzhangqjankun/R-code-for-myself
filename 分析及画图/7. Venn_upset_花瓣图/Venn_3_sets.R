#R code
#Written by Kai Feng
##li 2017.7.12 learn

#install.packages("VennDiagram")
library(VennDiagram)
#setwd('D:/Kai/Desktop/16S测序分析-20160426/Uparse/Venn')
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/cca/')
rm(list=ls(all=TRUE))

otu.table=read.table('OTU.txt', sep = "\t",row.names = 1, header = T)

A1 <- which(otu.table[,1]>0)
B1 <- which(otu.table[,2]>0)
C1 <- which(otu.table[,3]>0)

length_A <- length(A1)
length_B <- length(B1)
length_C <- length(C1)

length_AB <- length(intersect(A1,B1))  ##intersect求交集
length_AC <- length(intersect(A1,C1))
length_BC <- length(intersect(B1,C1))

length_ABC <- length(intersect(intersect(A1,B1),C1))

venn.plot <- draw.triple.venn(
  area1 = length_A,
  area2 = length_B,
  area3 = length_C,
  n12 = length_AB,
  n13 = length_AC,
  n23 = length_BC,
  n123 = length_ABC,
  category = c("A-Pre", "A-Post", "A-Recovery"),
  fill = c("orange", "red", "blue"),
  lty = "solid",
  fontfamily = "serif",
  fontface = "bold",
  cex = 2,
  cat.cex = 2,
  cat.col = c("orange", "red", "blue")
)
tiff(filename = "Group A.tiff", compression = "lzw")
grid.draw(venn.plot)
dev.off()