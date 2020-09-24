## 2017.7.7 li learn
##http://www.omicshare.com/forum/forum.php?mod=viewthread&tid=1002
##使用R语言，实现批量差异分析（t检验或方差分析�?


rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/t test')

# 对于二代数据的表达差异分析，理论上应该reads counts进行计算�?
#如果手头没有reads count数据，而只有RPKM/FPKM值该怎么办？这个时候，就只能退而求其次，使用t检验和方差分析�?
#但注意，这两种检验是基于正态分布的检验方法，是不适用于二代数据的，对低丰度基因的检验会产生大量假阳性�?
#不到万不得已不要使用这类方法。如果非使用不可，可以：
#�?1）使用以下的R脚本进行批量差异检验（t检验或方差分析）；
#�?2）请将在两组样本中表达量RPKM值均低于1的基因过滤掉（t检验和方差分析在低丰度基因中，假阳性过高，P value不可靠）



# t 检验的代码如下�?

a=read.table("all_fpkm.txt",header=T,sep="\t") 
##head(a,2)看前两行

#预生�?2个长度与输入文件行数相同的全�?0的向量，将用于存储p value和差异倍数（log2FC�?
Pvalue<-c(rep(0,nrow(a)))     ##Rep(a,times= ),后边的数是重复前面数的次数�?
log2_FC<-c(rep(0,nrow(a)))  

# 2~4列是处理�?1,5~7列是处理�?2�?
#将使用循环对每一行进行t检�?
#如果某一行两组的标准差都等于0，将无法进行t检验，所以这些行使用NA替代
#每一行计算得到p value和log2FC将被加入原文件的后两列；
#计算log2FC时，每组均值加0.001，是为了防止分母�?0导致bug�?
for(i in 1:nrow(a)){
  if(sd(a[i,2:4])==0&&a[i,5:7]==0){
    Pvalue <- "NA"
    log2_FC<- "NA"
  }else{
    y=t.test(as.numeric(a[i,2:4]),as.numeric(a[i,5:7]))
    Pvalue<-y$p.value
    log2_FC<-log2((mean(as.numeric(a[i,2:4]))+0.001)/(mean(as.numeric(a[i,5:7]))+0.001))
  }
}
# 对p value进行FDR校正
fdr=p.adjust(Pvalue, "BH") 
# 在原文件后面加入log2FC，p value和FDR,�?3列；
out<-cbind(a,log2_FC,Pvalue,fdr) 
head(out)
#write.table(out,file="ttest.out.xls",quote=FALSE,sep="\t",row.names=FALSE)

#方差分析的代码如下，与t test相比，逻辑相同，只是有些细微的修改�?

a=read.table("all_fpkm.txt",header=T,sep="\t")  
#预生�?2个长度与输入文件行数相同的全�?0的向量，将用于存储p value和差异倍数（log2FC�?
Pvalue<-c(rep(0,nrow(a)))   
log2_FC<-c(rep(0,nrow(a)))  
#确定分组信息
type<-factor(c(rep("c",3),rep("m",3)))
type
# 2~4列是处理�?1,5~7列是处理�?2�?
#将使用循环对每一行进行方差检�?
#两组表达量都�?0的基因，不检验；
#每一行计算得到p value和log2FC将被加入原文件的后两列；
#计算log2FC时，每组均值加0.001，是为了防止分母�?0导致bug�?
for(i in 1:nrow(a)){
  if(sum(a[i,2:4])==0&&sum(a[i,5:7])==0){
    Pvalue[i] <- "NA"
    log2_FC[i]<- "NA"
  }else{
    y=aov(as.numeric(a[i,2:7])~type)
    Pvalue[i]<-summary(y)[[1]][,5][1]
    log2_FC[i]<-log2((mean(as.numeric(a[i,2:4]))+0.001)/(mean(as.numeric(a[i,5:7]))+0.001))
  }
}
# 对p value进行FDR校正
fdr=p.adjust(Pvalue, "BH") 
# 在原文件后面加入log2FC，p value和FDR,�?3列；
out<-cbind(a,log2_FC,Pvalue,fdr) 
out
#write.table(out,file="anova.out.xls",quote=FALSE,sep="\t",row.names=FALSE)

##列表内元素的引用可以用”[[]]�? , �? list1[[c(1,2,3)]]
##上述放于”[]”内的数字，称为下标。通过下标的变化，可以方便的访问向量、数据框、矩阵、列表内的各元素�?
##熟悉下标的用法对掌握循环结构是非常重要的�?
