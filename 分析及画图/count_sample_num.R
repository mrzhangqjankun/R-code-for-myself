
setwd('E:/桌面')
rm(list=ls(all=TRUE))

f_dat = read.table("BTC-4R.txt",sep="\t",head=TRUE,row.names=1)

OTU.dat = t(f_dat)
valid.col = which(colSums(OTU.dat)>0)
OTU.dat = OTU.dat[,valid.col]#ȥ????????Ʒ????Ŀ??Ϊ0??OTU
OTU.dat[is.na(OTU.dat)] = 0

#??mixedsort 字符串排序

# for group #
library("gtools")
samp = rownames(OTU.dat)# samp ΪOTU.dat??????
grp1 = mixedsort(samp[grep("B1",samp)]); ##所有含B1的 挑出来
grp2 = mixedsort(samp[grep("B2",samp)]); 
grp3 = mixedsort(samp[grep("B3",samp)]); 
grp4 = mixedsort(samp[grep("T0",samp)]); 
grp5 = mixedsort(samp[grep("T1",samp)]); 
grp6 = mixedsort(samp[grep("C1",samp)]);
grp = list(grp1, grp2, grp3, grp4, grp5, grp6)
names(grp) = order1 = c("Beijing_MF", "Beijing_BF", "Beijing_BG", "Tibet_G0", "Tibet_G1",  "Changsha_G0")
grplist    = c(rep("Beijing_MF",21), rep("Beijing_BF",21), rep("Beijing_BG",21), rep("Tibet_G0",25), rep("Tibet_G1",25),  rep("Changsha_G0",21))
grp.order  = c(grp1, grp2, grp3, grp4, grp5, grp6)

samp_num = c(1,5,9,13,17,21)
richness.all = c()
for(i in 1:length(samp_num)){
  sample.num = samp_num[i] #ȷ??ȡ??????��
  richness.sn = c()
 ## for(j in length(grp)){   attention！！
    for(j in 1:length(grp)){
    grp.names = grp[[j]]#?????з??????в???
    samp.count  = length(grp.names)#ÿһ??????????Ʒ????��
    commu = OTU.dat[grp.names,]#???��?ȡһ????????OTU??Ϣ??????????Ʒ???ƣ???????OTU ID
    commu.t = t(commu)
    
    prob.num  = choose(samp.count, sample.num)#choose函数：排列组合中的C（x, y） 从x个元素中任取y个元素的子集数目。 只计算可能性，结果是一个数字。
    richness  = c()
    if(prob.num<100){
      
      comb.all = combn(samp.count, sample.num) ## comb 把所有可能列出来。
      for(k in 1:ncol(comb.all)){
        commu.ra   = as.data.frame(commu.t[,comb.all[,k] ])
        richness.ra = length(which(rowSums(commu.ra)>0))
        richness      = c(richness, richness.ra)
      }
      
    } else{
      
      for(k in 1:100){     ### 这个循环没有K啊。
        samp.rand = sample(samp.count, sample.num)
        commu.rand = commu[samp.rand,]#??ȡ?????п??ܵ???????
        richness.rand = length(which(colSums(commu.rand)>0))
        richness      = c(richness, richness.rand)
      }
      
    }
    
    rich.mean = mean(richness)
    rich.sd   = sd(richness)
    
    richness.sn = rbind(richness.sn, c(rich.mean, rich.sd))
  }
  
  richness.all = cbind(richness.all, richness.sn)
}

richness.all






library (ggplot2)
ggplot(data=f_dat, aes(x=counts,y=taxa,color=sites)) +
  geom_point(size=2) +
  geom_smooth(method = "lm",linetype=1 )+
  labs(title= "ITS", x="Log(Area)", y="Log(Taxa)")


  
  