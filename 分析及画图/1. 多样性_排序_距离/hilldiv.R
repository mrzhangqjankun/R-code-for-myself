##2019.4.22
##hilldiv

#https://github.com/anttonalberdi/hilldiv/

rm(list=ls());gc()

#remove.packages("Rcpp")
#install.packages("devtools")
# library(devtools)
# install_github("anttonalberdi/hilldiv")
library(hilldiv)
#dependencies: ggplot2, RColorBrewer, data.table, ape, ade4, iNEXT, iNextPD.
?RColorBrewer #调色板
?ape #系统发育
?ade4 #生态和环境数据
??hilldiv

#Load data
data(bat.diet.otutable) #OTU
data(bat.diet.hierarchy) #分组文件
data(bat.diet.tree)  #有根树

#Create simple objects
otu.table <- bat.diet.otutable
otu.vector <- bat.diet.otutable[,1]
names(otu.vector) <- rownames(otu.table)
hierarchy.table <- bat.diet.hierarchy
tree <- bat.diet.tree

##hill.div 输入OTU 或者向量，计算对应的Hill.如果有树的信息，则会计算基于系统发育的Hill
#Usage
#true.div(abund,qvalue,tree,type)
#qvalue:hill的阶数。q>=0
#type:abundance或者incidence
hill.div(otu.vector,0)
hill.div(otu.table,1)
hill.div(otu.table,qvalue=2)
hill.div(otu.table,1,tree)
hill.div(otu.table,1,tree,type="incidence")
hill.div(otu.table,qvalue=2,tree=tree)

##div.profile()   hill的阶数和其值的关系
#Usage
#div.profile(abund,qvalues,tree,values,hierarchy,level,colour,log)
#qvalue: default from 0 to 5. order=seq(from = 0, to = 5, by = (0.1))
#level: alpha或gamma。默认为gamma多样性。
#log:Hill是否取对数
# One sample
div.profile(otu.vector)
# Multiple individual samples (first 5 samples of the OTU table)
div.profile(otu.table[,c(1:5)])
# Multiple groups (aggregated samples)
div.profile(otu.table,hierarchy=hierarchy.table,colour=c("#35a849","#9d1923","#f7ab1b","#ed7125","#cc4323","#b6d134","#fcee21","#085ba7"))

##div.test() 多组之间的多样性比较
#Usage
#div.test(otutable,qvalue,hierarchy,tree)
# 先进行Shapiro and Barlett tests检验数据的正态性和均质性；再根据结果选择显著性检验的方法。
#两组：Student’s t-test or a Wilcoxon Rank Sum Test
#多组：ANOVA or a Kruskal-Wallis test 
?div.test()
#Contrast based on Hill numbers
div.test(otu.table,qvalue=0,hierarchy=hierarchy.table)
#Contrast based on phylogenetic Hill numbers
div.test(otu.table,qvalue=1,hierarchy=hierarchy.table,tree=tree)

##div.test.plot() 多组之间的多样性比较的图
#Usage
#div.test.plot(divtest,chart,colour)
#chart 可选择 boxplot 或 jitter 或 violin plot

contrast.div.q0 <- div.test(otu.table,qvalue=0,hierarchy=hierarchy.table)
colours <- c("#35a849","#9d1923","#f7ab1b","#ed7125","#cc4323","#b6d134","#fcee21","#085ba7")

#Box plot
div.test.plot(contrast.div.q0)
div.test.plot(contrast.div.q0,chart="box")

#Jitter plot
div.test.plot(contrast.div.q0,chart="jitter",colour=colours)

#Violin plot
div.test.plot(contrast.div.q0,chart="violin",colour=c("#35a849","#9d1923","#f7ab1b","#ed7125","#cc4323","#b6d134","#fcee21","#085ba7"))

##div.part 多样性分解。通过计算alpha和gamma多样性，用gamma/alpha得到beta多样性
#Usage
#div.part(otutable,qvalue,hierarchy,tree,type)

#Abundance-based
div.part(otu.table,qvalue=1)
div.part(otu.table,qvalue=1,type="abundance")
div.part(otu.table,qvalue=0,tree=tree)

#Incidence-based
div.part(otu.table,qvalue=0,type="incidence",hierarchy=hierarchy.table)


###beta.dis() 基于beta多样性；样本大小及hill的阶数进行相似性或不相似性分析。
#可计算 the Sørensen-type overlap (CqN), the Jaccard-type overlap (UqN), the Sørensen-type turnover-complement (VqN), and the Jaccard-type turnover-complement (SqN)
#Usage
#beta.dis(beta,qvalue,N,metric,type)
#N:样本数
#matrix: "C", "U", "V" or "S". 
#type:"similarity" or "dissimilarity".

#Using custom, beta-diversity, q-value and samples sizes
beta.dis(beta=4.5,qvalue=1,N=8)
beta.dis(beta=4.5,qvalue=1,N=8,metric="C",type="similarity")

#Computing from an div.part() derived object
div.part.object  <- div.part(otu.table,qvalue=0,tree=tree)
beta.dis(div.part.object)
beta.dis(div.part.object,metric="S",type="similarity")

###pair.dis() 成对样本之间的多样性分解
#Usage
#pair.dis(otutable,qvalue,tree,hierarchy,level,measure)
#level:1 or 2.1表示在所有样本之间计算不相似性；2表示在组间计算不相似性。默认1
pair.dis(otu.table,qvalue=0,hierarchy=hierarchy.table)
pair.dis(otu.table,qvalue=0,hierarchy=hierarchy.table,level="2")

###pair.dis.plot()  可视化
#Usage
#pair.dis.plot(distance,hierarchy,type,level,colour,magnify)
#NMDS chart, a qgraph plot or a heatmap/correlogram.
#type:NMDS or qgraph 

pair.div.q0.L2 <- pair.dis(otu.table[,sort(colnames(otu.table))],qvalue=0,hierarchy=hierarchy.table,level="2")
#Plot NMDS
pair.dis.plot(pair.div.q0.L2$L2_CqN,hierarchy=hierarchy.table,type="NMDS",level=2)
#Plot qgraph
pair.dis.plot(pair.div.q0.L2$L2_CqN,hierarchy=hierarchy.table,type="qgraph",level=2,magnify=TRUE)

###tss()
#Performs total sum scaling.
tss(otu.table)
tss(otu.vector)

###alpha.div
#Usage
#alpha.div(otutable,qvalue,weight)
alpha.div(otutable=otu.table,qvalue=0)


###################2019.9.1
#https://github.com/anttonalberdi/hilldiv/wiki/0.-Hill-numbers
library(devtools)
install_github("anttonalberdi/hilldiv")  #错误: (由警告转换成)程辑包'ggplot2'是用R版本3.6.1 来建造的

library(hilldiv)
