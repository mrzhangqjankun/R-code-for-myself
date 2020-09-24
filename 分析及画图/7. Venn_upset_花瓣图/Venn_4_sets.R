#R code
#Written by Kai Feng
##li 2017.7.12 learn

library(VennDiagram)
#setwd('D:/Kai/Desktop/16S测序分析-20160426/Uparse/Venn')
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/cca/')
rm(list=ls(all=TRUE))

otu.table=read.table('OTU.txt', sep = "\t",row.names = 1, header = T)

A1 <- which(otu.table[,1]>0)
B1 <- which(otu.table[,2]>0)
C1 <- which(otu.table[,3]>0)
D1 <- which(otu.table[,4]>0)

length_A <- length(A1)
length_B <- length(B1)
length_C <- length(C1)
length_D <- length(D1)

#A1=which(A.table>0)
#B1=which(otu.table[,3]>0)
#length(intersect(A1,B1))

#rowSums(otu.table[,-1]>0)
#count1 = rowSums(otu.table[,-1]>0)
#length(count1 == 13)

length_AB <- length(intersect(A1,B1))
length_AC <- length(intersect(A1,C1))
length_AD <- length(intersect(A1,D1))
length_BC <- length(intersect(B1,C1))
length_BD <- length(intersect(B1,D1))
length_CD <- length(intersect(C1,D1))

length_ABC <- length(intersect(intersect(A1,B1),C1))
length_ABD <- length(intersect(intersect(A1,B1),D1))
length_ACD <- length(intersect(intersect(A1,C1),D1))
length_BCD <- length(intersect(intersect(B1,C1),D1))

length_ABCD <- length(intersect(intersect(A1,B1),intersect(C1,D1)))

venn.plot <- draw.quad.venn(
  area1 = length_A,
  area2 = length_D,
  area3 = length_B,
  area4 = length_C,
  n12 = length_AD,
  n13 = length_AB,
  n14 = length_AC,
  n23 = length_BD,
  n24 = length_CD,
  n34 = length_BC,
  n123 = length_ABD,
  n124 = length_ACD,
  n134 = length_ABC,
  n234 = length_BCD,
  n1234 = length_ABCD,
  category = c("A", "D", "B", "C"),
  fill = c("orange", "red", "green", "blue"),
  lty = "dashed",
  cex = 2,
  cat.cex = 2,
  cat.col = c("orange", "red", "green", "blue")
)
tiff(filename = "Quad_Venn_diagram.tiff", compression = "lzw")
grid.draw(venn.plot)
dev.off()

