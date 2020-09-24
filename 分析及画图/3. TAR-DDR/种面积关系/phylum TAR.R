## From Yangying
## 2017.7.6 li learn

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/种面积关系/phylum TAR/')
#setwd('C:/Users/dell/Desktop/article/Beijing-Tibet-Changsha/ITS 236-270/TAR/phylum')
#rm(list=ls(all=TRUE))
rt <- read.table("√Acidobacteria.txt",sep="\t",head=TRUE,row.names=1);

##可以优化为循环+函数

#B1 
b11<- rt[,1:5]#选取OTU table???2???6列赋予b01
b11s<- matrix(rowSums(b11))# 对b01进行行求???
b11s[b11s>0,]<- 1
sb11<- colSums(b11s)

 
b12<- rt[,1:9]
b12s<- matrix(rowSums(b12))
b12s[b12s>0,]<- 1
sb12<- colSums(b12s)

b13<- rt[,1:13]
b13s<- matrix(rowSums(b13))
b13s[b13s>0,]<- 1
sb13<- colSums(b13s)

b14<- rt[,1:17]
b14s<- matrix(rowSums(b14))
b14s[b14s>0,]<- 1
sb14<- colSums(b14s)

b15<- rt[,1:21]
b15s<- matrix(rowSums(b15))
b15s[b15s>0,]<- 1
sb15<- colSums(b15s)

b1<- as.matrix(log10(c(sb11,sb12,sb13,sb14,sb15)))


#B2 
b21<- rt[,22:26]
b21s<- matrix(rowSums(b21))
b21s[b21s>0,]<- 1
sb21<- colSums(b21s)

b22<- rt[,22:30]
b22s<- matrix(rowSums(b22))
b22s[b22s>0,]<- 1
sb22<- colSums(b22s)

b23<- rt[,22:34]
b23s<- matrix(rowSums(b23))
b23s[b23s>0,]<- 1
sb23<- colSums(b23s)

b24<- rt[,22:38]
b24s<- matrix(rowSums(b24))
b24s[b24s>0,]<- 1
sb24<- colSums(b24s)

b25<- rt[,22:42]
b25s<- matrix(rowSums(b25))
b25s[b25s>0,]<- 1
sb25<- colSums(b25s)

b2<- as.matrix(log10(c(sb21,sb22,sb23,sb24,sb25)))

#B3
b31<- rt[,43:47]
b31s<- matrix(rowSums(b31))
b31s[b31s>0,]<- 1
sb31<- colSums(b31s)

b32<- rt[,43:51]
b32s<- matrix(rowSums(b32))
b32s[b32s>0,]<- 1
sb32<- colSums(b32s)

b33<- rt[,43:55]
b33s<- matrix(rowSums(b33))
b33s[b33s>0,]<- 1
sb33<- colSums(b33s)

b34<- rt[,43:59]
b34s<- matrix(rowSums(b34))
b34s[b34s>0,]<- 1
sb34<- colSums(b34s)

b35<- rt[,43:63]
b35s<- matrix(rowSums(b35))
b35s[b35s>0,]<- 1
sb35<- colSums(b35s)

b3<- as.matrix(log10(c(sb31,sb32,sb33,sb34,sb35)))

#t0
t01<- rt[,64:68]
t01s<- matrix(rowSums(t01))
t01s[t01s>0,]<- 1
st01<- colSums(t01s)

t02<- rt[,64:72]
t02s<- matrix(rowSums(t02))
t02s[t02s>0,]<- 1
st02<- colSums(t02s)

t03<- rt[,64:76]
t03s<- matrix(rowSums(t03))
t03s[t03s>0,]<- 1
st03<- colSums(t03s)

t04<- rt[,64:80]
t04s<- matrix(rowSums(t04))
t04s[t04s>0,]<- 1
st04<- colSums(t04s)

t05<- rt[,64:84]
t05s<- matrix(rowSums(t05))
t05s[t05s>0,]<- 1
st05<- colSums(t05s)

t06<- rt[,64:88]
t06s<- matrix(rowSums(t06))
t06s[t06s>0,]<- 1
st06<- colSums(t06s)

t0<- as.matrix(log10(c(st01,st02,st03,st04,st05,st06)))

#T1
t11<- rt[,89:93]
t11s<- matrix(rowSums(t11))
t11s[t11s>0,]<- 1
st11<- colSums(t11s)

t12<- rt[,89:97]
t12s<- matrix(rowSums(t12))
t12s[t12s>0,]<- 1
st12<- colSums(t12s)

t13<- rt[,89:101]
t13s<- matrix(rowSums(t13))
t13s[t13s>0,]<- 1
st13<- colSums(t13s)

t14<- rt[,89:105]
t14s<- matrix(rowSums(t14))
t14s[t14s>0,]<- 1
st14<- colSums(t14s)

t15<- rt[,89:109]
t15s<- matrix(rowSums(t15))
t15s[t15s>0,]<- 1
st15<- colSums(t15s)

t16<- rt[,89:113]
t16s<- matrix(rowSums(t16))
t16s[t16s>0,]<- 1
st16<- colSums(t16s)


t1<- as.matrix(log10(c(st11,st12,st13,st14,st15,st16)))



#CS
cs1<- rt[,114:118]
cs1s<- matrix(rowSums(cs1))
cs1s[cs1s>0,]<- 1
scs1<- colSums(cs1s)

cs2<- rt[,114:122]
cs2s<- matrix(rowSums(cs2))
cs2s[cs2s>0,]<- 1
scs2<- colSums(cs2s)

cs3<- rt[,114:126]
cs3s<- matrix(rowSums(cs3))
cs3s[cs3s>0,]<- 1
scs3<- colSums(cs3s)

cs4<- rt[,114:130]
cs4s<- matrix(rowSums(cs4))
cs4s[cs4s>0,]<- 1
scs4<- colSums(cs4s)

cs5<- rt[,114:134]
cs5s<- matrix(rowSums(cs5))
cs5s[cs5s>0,]<- 1
scs5<- colSums(cs5s)  

CS<-as.matrix(log10(c(scs1,scs2,scs3,scs4,scs5)))


area1<- read.table("area1.txt", header = T, sep="\t",row.names = 1)
area2<- read.table("area2.txt", header = T, sep="\t",row.names = 1)
area3<- read.table("area3.txt", header = T, sep="\t",row.names = 1)

area1<-as.matrix(log10(area1))
area2<-as.matrix(log10(area2))
area3<-as.matrix(log10(area3))


b1.reg <- lm(b1 ~ area1) #?ع?????

slopeb1 = b1.reg$coefficients[2]
slopeb1

corb1 = cor.test(b1, area1)
corb1
corb1$p.value


b2.reg <- lm(b2 ~ area1) #?ع?????

slopeb2 = b1.reg$coefficients[2]
slopeb2

corb2 = cor.test(b2, area1)
corb2
corb2$p.value

b3.reg <- lm(b3 ~ area1) #?ع?????

slopeb3 = b3.reg$coefficients[2]
slopeb3

corb3 = cor.test(b3, area1)
corb3
corb3$p.value

t0.reg <- lm(t0 ~ area2) #?ع?????

slopet0 = t0.reg$coefficients[2]
slopet0
summary(t0.reg)

cort0 = cor.test(t0, area2)
cort0
cort0$p.value

t1.reg <- lm(t1 ~ area2) #?ع?????

slopet1 = t1.reg$coefficients[2]
slopet1

cort1 = cor.test(t1, area2)
cort1
cort1$p.value

cs.reg <- lm(CS ~ area3) #?ع?????

slopecs = cs.reg$coefficients[2]
slopecs

corcs = cor.test(CS, area3)
corcs
corcs$p.value

