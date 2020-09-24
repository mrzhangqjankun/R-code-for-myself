##li 2017.7.12 learn 

library(vegan)
library(Imap)
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/CCA')
#setwd('E:/Lab Meeting/all.R/wzy/Mantel and partial mantel')
rm(list=ls(all=TRUE))

#env_dat = read.csv("MantelEnv.txt", header=T, row.names=1)
env_dat = read.table("Env factor2017.6.28.txt", header=T, row.names=1)


# calculate geodis by latitude and longitude
n = nrow(env_dat)#n代表了样本的数量
geodis = matrix(0, n, n)#定义矩阵，空的矩阵，值为0，n行n列
for(i in c(1:n)){
  for(j in c(1:n)){
    geodis[i, j] = gdist(env_dat[i,1], env_dat[i,2], env_dat[j, 1], env_dat[j,2], units = "m") # Long1, Lat1, Long2, Lat2) 1和2分别代表纬度和经度所在的列，units是以m米为单位
  }
}
geodis[geodis==0] = 1   ; head(geodis,2)#如果地理距离计算得到的是0，我们赋值为1：因为所有采样点地理距离不可能完全一致，我们假设其距离为1米
geodis = as.dist(t(geodis))  ; head(geodis,2)

fg.dat = read.table("OTU.txt", sep="\t", header=T, row.names=1)

fg.dat[is.na(fg.dat)] = 0

BC.beta = vegdist(t(fg.dat[,-1]), method="bray") #[,-1]除开第一列,计算布雷柯蒂斯
JC.beta = vegdist(t(fg.dat[,-1]), method="jaccard")#binary变成0,和1

#calculate mantel test of microbial community and geodis
mantel.BC = mantel(geodis, BC.beta)
mantel.BC

mantel.JC = mantel(geodis, JC.beta)
mantel.JC


#calculate the relationship between community and each env_factor
env.select = env_dat
env.std = decostand(env.select, method = "standardize", MARGIN=2)#标准化到均值等于0，标准偏差等于1
envdis = dist(env.std)

report =c()#made a blank table

for(i in 3:ncol(env.std))#若为1:5就是1到5列
{
  envdis =vegdist(env.std[,i],method = "euclidean", na.rm=T)
  mantel.BC = mantel(envdis, BC.beta, na.rm=T)#每一列做一次
  
  #print(mantel.BC)适用于少量数据输出
  mantel.JC = mantel(envdis, JC.beta, na.rm=T)
  #print(mantel.JC)
  
  #输出五个变量 colnames(env.select)[i] mantel.BC$statistic, mantel.BC$signif，mantel.JC$statistic, mantel.JC$signif
  #print(paste(colnames(env.select)[i] mantel.BC$statistic, mantel.BC$signif，mantel.JC$statistic, mantel.JC$signif))
  
  report = rbind(report,c(colnames(env.select)[i], mantel.BC$statistic, mantel.BC$signif, mantel.JC$statistic, mantel.JC$signif))#rbind，r是row，每一行每一行的加入
}

report

#calculate partial mantel between community and each env_factor
env.select = env_dat
env.std = decostand(env.select,method="standardize", MARGIN=2)
envdis = dist(env.std)


report = c() 
for(i in 3:19){
  envdis = dist(env.std[,i])  #envdis = vegdist(env.std[,i],method="euclidean")需要指定方法为欧氏距离
  envdis2 = dist(env.std[,-i])
  
  pmantel.BC = mantel.partial(BC.beta, envdis, envdis2, na.rm=T)
  pmantel.JC = mantel.partial(JC.beta, envdis, envdis2, na.rm=T)  #环境矩阵与beta多样性做mantel
  
  report = rbind(report,c(colnames(env.select)[i], pmantel.BC$statistic, pmantel.BC$signif, pmantel.JC$statistic, pmantel.JC$signif))
  
  }
report


#calculate partial mantel between community and selected env_factor
env.select = env_dat[,c(3,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
env.std = decostand(env.select,method="standardize", MARGIN=2)
envdis = dist(env.std)


report = c() #方法三
for(i in 1:ncol(env.std))#The first row selected up
  {
  envdis = dist(env.std[,i])  #envdis = vegdist(env.std[,i],method="euclidean")需要指定方法为欧氏距离
  envdis2 = dist(env.std[,-i])
  
  mantel.BC = mantel(envdis, BC.beta, na.rm=T)
  #方法一，print(mantel.BC) 
  mantel.JC = mantel(envdis, JC.beta, na.rm=T) 
  
  
  report = rbind(report,c(colnames(env.select)[i], mantel.BC$statistic, mantel.BC$signif, mantel.JC$statistic, mantel.JC$signif))
  #print(mantel.JC)输出,但是如果结果太多,很不方便,而且又很多不需要的结果,所以一般不用这种方法
  #输出方法二，print(paste(colnames(env.select)[i], mantel.BC$statistic, mantel.BC$signif, mantel.JC$statistic, mantel.JC$signif)) #一种方法，但是时间长
}
report

