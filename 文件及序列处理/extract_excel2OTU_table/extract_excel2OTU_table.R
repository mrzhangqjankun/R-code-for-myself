library(gdata)
library(xlsx)
library(stringr)
setwd("E:/桌面/R script 2017/extract_excel2OTU_table/A大型浮游动物/")
# sub.dir <- dir(".",pattern="监控区")
# 
# plant <- c()
# zoo.sl <- c()
# zoo.big <- c()
# water <- c()
# for (i in sub.dir) {
#   sub.files <- list.files(paste("./",i,sep=""))
#   plant <- c(plant,paste("./",i,"/",sub.files[grep("浮游植物",sub.files)],sep=""))
#   zoo.sl <- c(zoo.sl,paste("./",i,"/",sub.files[grep("小型浮游动物",sub.files)],sep=""))
#   zoo.big <- c(zoo.big,paste("./",i,"/",sub.files[grep("大型浮游动物",sub.files)],sep=""))
#   water <- c(water,paste("./",i,"/",sub.files[grep("水质",sub.files)],sep=""))
# }

sub.dir <- dir(".",pattern="[A-D]")

plant <- c()
zoo.sl <- c()
zoo.big <- c()
water <- c()
#sub.files <- list.files(paste("./",sub.dir[grep("A",sub.dir)],sep=""))
plant <- paste("./",sub.dir[grep("C",sub.dir)],"/",
               list.files(paste("./",sub.dir[grep("C",sub.dir)],sep="")),sep="")
zoo.sl <- paste("./",sub.dir[grep("B",sub.dir)],"/",
                list.files(paste("./",sub.dir[grep("B",sub.dir)],sep="")),sep="")
zoo.big <- paste("./",sub.dir[grep("A",sub.dir)],"/",
                 list.files(paste("./",sub.dir[grep("A",sub.dir)],sep="")),sep="")
water <- paste("./",sub.dir[grep("D",sub.dir)],"/",
               list.files(paste("./",sub.dir[grep("D",sub.dir)],sep=""))[grep("xls",list.files(paste("./",sub.dir[grep("D",sub.dir)],sep="")))],sep="")


# for phytoplankton 
tot.plant <- c()
samp.tot <- c()
sp.tot <- c()
for (f in plant) {
  sub.f <- read.xlsx2(f,sheetIndex = 1,startRow = 5)
  sub.f <- sub.f[1:(grep("填报人",sub.f[,1])-1),]
  if(length(which(sub.f[,1]==""))>0){
    sub.f <- sub.f[1:(which(sub.f[,1]=="")-1),]
  }
  samp.tot <- c(samp.tot,as.character(unique(sub.f[,"X."])))
  sp.tot <- c(sp.tot,as.character(unique(str_replace_all(sub.f[,(which(colnames(sub.f)=="个.m3")-1)]," $|^ ",""))))
  sub.f[,(which(colnames(sub.f)=="个.m3")-1)] <- str_replace_all(sub.f[,(which(colnames(sub.f)=="个.m3")-1)]," $|^ ","")
  sl <- c(which(colnames(sub.f)=="X."),which(colnames(sub.f)=="个.m3"),(which(colnames(sub.f)=="个.m3")-1))
  colnames(sub.f) <- NA
  tot.plant<- rbind(tot.plant,sub.f[,sl])
  print(ncol(sub.f))
}
samp <- sort(unique(samp.tot))
sp <-sort(unique(sp.tot))
otu.plant <- matrix(0,nrow = length(sp),ncol = length(samp))
for (i in 1:ncol(otu.plant)) {
  for (j in 1:nrow(otu.plant)) {
    sub.tab <- tot.plant[which(tot.plant[,1]==samp[i]),]
    if(length(which(sub.tab[,3]==sp[j]))>=1){
      cat(sp[j],sub.tab[which(sub.tab[,3]==sp[j]),3],"\n")
      sl <- which(sub.tab[,3]==sp[j])
      if(length(sl)>1){
        otu.plant[j,i] <- sum(as.numeric(as.vector(sub.tab[sl,2])))
      }else{
        otu.plant[j,i] <- as.numeric(as.vector(sub.tab[sl,2]))
      }
      
    }
  }
}
rownames(otu.plant) <- sp
colnames(otu.plant) <- samp
rowSums(otu.plant)
colSums(otu.plant)
write.csv(otu.plant,"OTU_table_phytoplankton.csv",quote=F)

# for small zooplankton
tot.zoo.sl <- c()
samp.tot <- c()
sp.tot <- c()
for (f in zoo.sl) {
  sub.f <- read.xlsx2(f,sheetIndex = 1,startRow = 5)
  sub.f <- sub.f[1:(grep("填报人",sub.f[,1])-1),]
  if(length(which(sub.f[,1]==""))>0){
    sub.f <- sub.f[1:(which(sub.f[,1]=="")-1),]
  }
  samp.tot <- c(samp.tot,as.character(unique(sub.f[,"X."])))
  sp.tot <- c(sp.tot,as.character(unique(str_replace_all(sub.f[,(which(colnames(sub.f)=="个.m3")-1)]," $|^ ",""))))
  sub.f[,(which(colnames(sub.f)=="个.m3")-1)] <- str_replace_all(sub.f[,(which(colnames(sub.f)=="个.m3")-1)]," $|^ ","")
  sl <- c(which(colnames(sub.f)=="X."),which(colnames(sub.f)=="个.m3"),(which(colnames(sub.f)=="个.m3")-1))
  colnames(sub.f) <- NA
  tot.zoo.sl<- rbind(tot.zoo.sl,sub.f[,sl])
  print(ncol(sub.f))
}
samp <- sort(unique(samp.tot))
sp <-sort(unique(sp.tot))
otu.zoo.sl <- matrix(0,nrow = length(sp),ncol = length(samp))
for (i in 1:ncol(otu.zoo.sl)) {
  for (j in 1:nrow(otu.zoo.sl)) {
    sub.tab <- tot.zoo.sl[which(tot.zoo.sl[,1]==samp[i]),]
    if(length(which(sub.tab[,3]==sp[j]))>=1){
      cat(sp[j],sub.tab[which(sub.tab[,3]==sp[j]),3],"\n")
      sl <- which(sub.tab[,3]==sp[j])
      if(length(sl)>1){
        otu.zoo.sl[j,i] <- sum(as.numeric(as.vector(sub.tab[sl,2])))
      }else{
        otu.zoo.sl[j,i] <- as.numeric(as.vector(sub.tab[sl,2]))
      }
      
    }
  }
}
rownames(otu.zoo.sl) <- sp
colnames(otu.zoo.sl) <- samp
rowSums(otu.zoo.sl)
colSums(otu.zoo.sl)
write.csv(otu.zoo.sl,"OTU_table_small_zooplankton.csv",quote=F)

# for big zooplankton
tot.zoo.big <- c()
samp.tot <- c()
sp.tot <- c()
for (f in zoo.big) {
  sub.f <- read.xlsx2(f,sheetIndex = 1,startRow = 5)
  if(length(grep("填报人",sub.f[,1]))>0){
    sub.f <- sub.f[1:(grep("填报人",sub.f[,1])-1),]
  }
  if(length(which(sub.f[,1]==""))>0){
    sub.f <- sub.f[1:(which(sub.f[,1]=="")-1),]
  }
  samp.tot <- c(samp.tot,as.character(unique(sub.f[,"X."])))
  sp.tot <- c(sp.tot,as.character(unique(str_replace_all(sub.f[,(which(colnames(sub.f)=="个.m3")-1)]," $|^ ",""))))
  sub.f[,(which(colnames(sub.f)=="个.m3")-1)] <- str_replace_all(sub.f[,(which(colnames(sub.f)=="个.m3")-1)]," $|^ ","")
  sl <- c(which(colnames(sub.f)=="X."),which(colnames(sub.f)=="个.m3"),(which(colnames(sub.f)=="个.m3")-1))
  colnames(sub.f) <- NA
  tot.zoo.big<- rbind(tot.zoo.big,sub.f[,sl])
  print(ncol(sub.f))
}
samp <- sort(unique(samp.tot))
sp <-sort(unique(sp.tot))
otu.zoo.big <- matrix(0,nrow = length(sp),ncol = length(samp))
for (i in 1:ncol(otu.zoo.big)) {
  for (j in 1:nrow(otu.zoo.big)) {
    sub.tab <- tot.zoo.big[which(tot.zoo.big[,1]==samp[i]),]
    if(length(which(sub.tab[,3]==sp[j]))>=1){
      cat(sp[j],sub.tab[which(sub.tab[,3]==sp[j]),3],"\n")
      sl <- which(sub.tab[,3]==sp[j])
      if(length(sl)>1){
        otu.zoo.big[j,i] <- sum(as.numeric(as.vector(sub.tab[sl,2])))
      }else{
        otu.zoo.big[j,i] <- as.numeric(as.vector(sub.tab[sl,2]))
      }
      
    }
  }
}
rownames(otu.zoo.big) <- sp
colnames(otu.zoo.big) <- samp
rowSums(otu.zoo.big)
colSums(otu.zoo.big)
write.csv(otu.zoo.big,"OTU_table_big_zooplankton.csv",quote=F)

# for water quality factors
tot.water <- c()
samp.water <- c()
env.water <- c()
i =0
tot = 0
for (f in water) {
  sub.f <- read.xlsx2(f,sheetIndex = 1,startRow = 4)
  sub.f <- sub.f[-1,]
  sub.f <- sub.f[1:(grep("填报人",sub.f[,1])-1),]
  if(length(which(sub.f[,1]==""))>0){
   sub.f <- sub.f[1:(which(sub.f[,1]=="")[1]-1),]
  }
  tot <- tot + nrow(sub.f)
  samp.water <- c(samp.water,as.character(unique(sub.f[,"监测站位"])))
  env.water <- c(env.water,as.character(unique(colnames(sub.f))))
  i = i+1
}
samp <- sort(unique(samp.water))
env.tot <- sort(unique(env.water))

st.line <- 0
water.table <- matrix(NA,nrow=tot,ncol=length(env.tot))
for (f in water) {
  sub.f <- read.xlsx2(f,sheetIndex = 1,startRow = 4)
  sub.f <- sub.f[-1,]
  sub.f <- sub.f[1:(grep("填报人",sub.f[,1])-1),]
  if(length(which(sub.f[,1]==""))>0){
    sub.f <- sub.f[1:(which(sub.f[,1]=="")[1]-1),]
  }
  for (i in 1:nrow(sub.f)) {
    for(j in 1:ncol(sub.f)){
      match.col <- match(colnames(sub.f)[j],env.tot)
      water.table[(st.line+i),match.col] <- as.character(sub.f[i,j])
    }
  }
  st.line = st.line+nrow(sub.f)
}
colnames(water.table) <- env.tot
write.csv(water.table,"water_quality_whole_date.csv",quote=F)


##ecxel中复制，直接就能读进来
read.table("clipboard")

##R语言第一章数据处理基础①读取EXEL表格数据
https://mp.weixin.qq.com/s?__biz=MzI1NDY4ODg2OA==&mid=2247483736&idx=1&sn=bab1de360a8fa257996c6f29a91fecc5&chksm=e9c02a53deb7a34508aa0fb96ecd95ed34da8293257c7852e1c8b406c7eee348640100e4c2a3&scene=0&xtrack=1&pass_ticket=%2FElyDHnrrqcwJePBZ2d4dYGKVzOFjrYAukrNVaBr8yV1Ww3Lx0Kv8OsFMQDbMVcz#rd
#readxl