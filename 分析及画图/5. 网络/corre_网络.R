#https://mp.weixin.qq.com/s/eE3V7MFVZSzQwkVcsAAhQg
##改编
gc()
library(vegan)
library(reshape)
setwd("./data")
### 使用order等级门类做网络分析
comm.data<-read.csv("total_order_info.csv");head(comm.data)
comm.data<-comm.data[,-1]
dim(comm.data)
##去除read数目小于1407的样品
comm.data.read<-subset(comm.data, reads >= 1407)
dim(comm.data.read)#发现就剩下140个样品了

#抽平，将序列数目全部抽为1407条
comm.data<-cbind(comm.data.read[,c(1:4)],rrarefy(comm.data.read[,-c(1:4)],1407))

##选择需要的处理
trts<-as.vector((unique((comm.data$rep))))
trts<-trts[-c(4,5)]

results<-matrix(nrow=0,ncol=7)
options(warnings=-1)
### 网络构建主函数
#网络构建要忽略双0的情况，这里将物种丰度count数目小于1的相关忽略，方式假阳性。
#结果保存为igraph识别的邻接矩阵形式
#将全部分组的网络保存在一份表格中
for(a in 1:length(trts)){
  #pull the first element from the vector of treatments
  trt.temp<-trts[a]
  #subset the dataset for those treatments
  temp<-subset(comm.data, rep==trt.temp)
  
  #in this case the community data started at column 6, so the loop for co-occurrence has to start at that point
  for(b in 5:(dim(temp)[2]-1)){
    #every species will be compared to every other species, so there has to be another loop that iterates down the rest of the columns
    for(c in (b+1):(dim(temp)[2])){
      
      #summing the abundances of species of the columns that will be compared
      species1.ab<-sum(temp[,b])
      species2.ab<-sum(temp[,c])
      #if the column is all 0's no co-occurrence will be performed
      if(species1.ab >1 & species2.ab >1){
        test<-cor.test(temp[,b],temp[,c],method="spearman",na.action=na.rm)
        rho<-test$estimate
        p.value<-test$p.value
      }
      
      if(species1.ab <=1 | species2.ab <= 1){
        rho<-0
        p.value<-1
      }
      
      new.row<-c(trts[a],names(temp)[b],names(temp)[c],rho,p.value,species1.ab,species2.ab)
      results<-rbind(results,new.row)
      
    }
    
  }
  
  
  print(a/length(trts))
  
}

head(results)
results<-data.frame(data.matrix(results))
names(results)<-c("trt","taxa1","taxa2","rho","p.value","ab1","ab2")


#citation(package = "base", lib.loc = NULL)
#citation(package = "vegan", lib.loc = NULL)
