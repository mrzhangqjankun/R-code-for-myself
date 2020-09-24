## From Yangying
## 2017.7.6 li learn


rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/种面积关系')
##setwd('C:/Users/dell/Desktop/article/Beijing-Tibet-Changsha/ITS 236-270/TAR')
rt <- read.table("B T C TAR.txt",sep="\t",head=TRUE);
#rt
row.names(rt)<-rt$OTUID

###可以加以改进，写成循环或函数。
##b0 = c
##for (i in 6:22){
    j=paste("B0",1:5,sep="")
    k=paste("sb0",1:5,sep="")
    j[1]
    j[1]<- rt[,2:i]
    j[1]<- data.frame(rowSums(j[1]))
    j[1][j[1]>0,]<- 1
    k[1]<- colSums(j[1])
    k[1]
    
  }
##   i=i+4  


##函数
statistics = function(x) {
  b01s<- data.frame(rowSums(x))
  b01s[b01s>0,]<- 1
  sb01<- colSums(b01s)
  sb01
}
b01<- rt[,2:6]
sb01 = statistics(b01);sb01



#B0        ###统计面积不断扩大的物种数量
b01<- rt[,2:6]#选取OTU table???2???6列赋予b01
b01s<- data.frame(rowSums(b01))# 对b01进行行求???
b01s[b01s$rowSums.b01.>0,]<- 1
sb01<- colSums(b01s)

b02<- rt[,2:10]
b02s<- data.frame(rowSums(b02))
b02s[b02s$rowSums.b02.>0,]<- 1
sb02<- colSums(b02s)

b03<- rt[,2:14]
b03s<- data.frame(rowSums(b03))
b03s[b03s$rowSums.b03.>0,]<- 1
sb03<- colSums(b03s)

b04<- rt[,2:18]
b04s<- data.frame(rowSums(b04))
b04s[b04s$rowSums.b04.>0,]<- 1
sb04<- colSums(b04s)

b05<- rt[,2:22]
b05s<- data.frame(rowSums(b05))
b05s[b05s$rowSums.b05.>0,]<- 1
sb05<- colSums(b05s)

B0<- c(sb01,sb02,sb03,sb04,sb05)

#B1 
b11<- rt[,23:27]
b11s<- data.frame(rowSums(b11))
b11s[b11s$rowSums.b11.>0,]<- 1
sb11<- colSums(b11s)

b12<- rt[,23:31]
b12s<- data.frame(rowSums(b12))
b12s[b12s$rowSums.b12.>0,]<- 1
sb12<- colSums(b12s)

b13<- rt[,23:35]
b13s<- data.frame(rowSums(b13))
b13s[b13s$rowSums.b13.>0,]<- 1
sb13<- colSums(b13s)

b14<- rt[,23:39]
b14s<- data.frame(rowSums(b14))
b14s[b14s$rowSums.b14.>0,]<- 1
sb14<- colSums(b14s)

b15<- rt[,23:43]
b15s<- data.frame(rowSums(b15))
b15s[b15s$rowSums.b15.>0,]<- 1
sb15<- colSums(b15s)

B1<- c(sb11,sb12,sb13,sb14,sb15)

#B2
b21<- rt[,44:48]
b21s<- data.frame(rowSums(b21))
b21s[b21s$rowSums.b21.>0,]<- 1
sb21<- colSums(b21s)

b22<- rt[,44:52]
b22s<- data.frame(rowSums(b22))
b22s[b22s$rowSums.b22.>0,]<- 1
sb22<- colSums(b22s)

b23<- rt[,44:56]
b23s<- data.frame(rowSums(b23))
b23s[b23s$rowSums.b23.>0,]<- 1
sb23<- colSums(b23s)

b24<- rt[,44:60]
b24s<- data.frame(rowSums(b24))
b24s[b24s$rowSums.b24.>0,]<- 1
sb24<- colSums(b24s)

b25<- rt[,44:64]
b25s<- data.frame(rowSums(b25))
b25s[b25s$rowSums.b25.>0,]<- 1
sb25<- colSums(b25s)

B2<- c(sb21,sb22,sb23,sb24,sb25)

#B3
b31<- rt[,65:69]
b31s<- data.frame(rowSums(b31))
b31s[b31s$rowSums.b31.>0,]<- 1
sb31<- colSums(b31s)

b32<- rt[,65:73]
b32s<- data.frame(rowSums(b32))
b32s[b32s$rowSums.b32.>0,]<- 1
sb32<- colSums(b32s)

b33<- rt[,65:77]
b33s<- data.frame(rowSums(b33))
b33s[b33s$rowSums.b33.>0,]<- 1
sb33<- colSums(b33s)

b34<- rt[,65:81]
b34s<- data.frame(rowSums(b34))
b34s[b34s$rowSums.b34.>0,]<- 1
sb34<- colSums(b34s)

b35<- rt[,65:85]
b35s<- data.frame(rowSums(b35))
b35s[b35s$rowSums.b35.>0,]<- 1
sb35<- colSums(b35s)

B3<- c(sb31,sb32,sb33,sb34,sb35)

#T0
t01<- rt[,86:90]
t01s<- data.frame(rowSums(t01))
t01s[t01s$rowSums.t01.>0,]<- 1
st01<- colSums(t01s)

t02<- rt[,86:94]
t02s<- data.frame(rowSums(t02))
t02s[t02s$rowSums.t02.>0,]<- 1
st02<- colSums(t02s)

t03<- rt[,86:98]
t03s<- data.frame(rowSums(t03))
t03s[t03s$rowSums.t03.>0,]<- 1
st03<- colSums(t03s)

t04<- rt[,86:102]
t04s<- data.frame(rowSums(t04))
t04s[t04s$rowSums.t04.>0,]<- 1
st04<- colSums(t04s)

t05<- rt[,86:106]
t05s<- data.frame(rowSums(t05))
t05s[t05s$rowSums.t05.>0,]<- 1
st05<- colSums(t05s)

t06<- rt[,86:110]
t06s<- data.frame(rowSums(t06))
t06s[t06s$rowSums.t06.>0,]<- 1
st06<- colSums(t06s)

T0<- c(st01,st02,st03,st04,st05,st06)

#T1
t11<- rt[,111:115]
t11s<- data.frame(rowSums(t11))
t11s[t11s$rowSums.t11.>0,]<- 1
st11<- colSums(t11s)

t12<- rt[,111:119]
t12s<- data.frame(rowSums(t12))
t12s[t12s$rowSums.t12.>0,]<- 1
st12<- colSums(t12s)

t13<- rt[,111:123]
t13s<- data.frame(rowSums(t13))
t13s[t13s$rowSums.t13.>0,]<- 1
st13<- colSums(t13s)

t14<- rt[,111:127]
t14s<- data.frame(rowSums(t14))
t14s[t14s$rowSums.t14.>0,]<- 1
st14<- colSums(t14s)

t15<- rt[,111:131]
t15s<- data.frame(rowSums(t15))
t15s[t15s$rowSums.t15.>0,]<- 1
st15<- colSums(t15s)

t16<- rt[,111:135]
t16s<- data.frame(rowSums(t16))
t16s[t16s$rowSums.t16.>0,]<- 1
st16<- colSums(t16s)

T1<- c(st11,st12,st13,st14,st15,st16)

#CS
cs1<- rt[,136:140]
cs1s<- data.frame(rowSums(cs1))
cs1s[cs1s$rowSums.cs1.>0,]<- 1
scs1<- colSums(cs1s)

cs2<- rt[,136:144]
cs2s<- data.frame(rowSums(cs2))
cs2s[cs2s$rowSums.cs2.>0,]<- 1
scs2<- colSums(cs2s)

cs3<- rt[,136:148]
cs3s<- data.frame(rowSums(cs3))
cs3s[cs3s$rowSums.cs3.>0,]<- 1
scs3<- colSums(cs3s)

cs4<- rt[,136:152]
cs4s<- data.frame(rowSums(cs4))
cs4s[cs4s$rowSums.cs4.>0,]<- 1
scs4<- colSums(cs4s)

cs5<- rt[,136:156]
cs5s<- data.frame(rowSums(cs5))
cs5s[cs5s$rowSums.cs5.>0,]<- 1
scs5<- colSums(cs5s)  

CS<-c(scs1,scs2,scs3,scs4,scs5)

X<-rbind(B0,B1,B2,B3,T0,T1,CS)
X

##如何画出来，li added
area1 <- read.table("phylum TAR/area1.txt",sep="\t",head=TRUE);
area1

B0<- c(sb01,sb02,sb03,sb04,sb05)
B1<- c(sb11,sb12,sb13,sb14,sb15)
B2<- c(sb21,sb22,sb23,sb24,sb25)
B3<- c(sb31,sb32,sb33,sb34,sb35)
T0<- c(st01,st02,st03,st04,st05,st06)
T1<- c(st11,st12,st13,st14,st15,st16)
CS<-c(scs1,scs2,scs3,scs4,scs5)


x=log(area1$area)
yB0=log(B0);yB1=log(B1);yB2=log(B2);yB3=log(B3)
##par(new=TURE)  ##两次plot图形叠加在一起。两次的图形应该设置横纵坐标一致。
##par(mfrow = c(2,2), font.lab = 6, font.main = 6,font.axis = 6, font = 6) ## 分割成2行2列，共四个图
plot(x,yB0,type="p")
abline(lm(yB0~x),col=2)   ##abline添加给定斜率的线

yB0.reg <- lm(yB0 ~ x)   ##lm线性模型，yB0 ~ x 前为纵轴，后为横轴

slopeb0 = yB0.reg$coefficients[2]
slopeb0

corb0 = cor.test(yB0, x)
corb0
p=corb0$p.value ;p ##p
r2=corb0$est ;r2     ##cor
legend.label = c(p,r2)

legend(yB0~x,legend = legend.label)






##曲线拟合（局部回归）
##lowess(x,y,f=2/3,iter = 3)
##f为窗宽参数，越大越平滑
##iter为迭代次数，越大计算越慢
##loess(y~x, data,span=0.75,degree=2)
##data为包含x,y的数据集；span为窗宽参数，degree默认为二次回归。
##sample
lines(lowess(x,yB0))  ####lowess做回归曲线
lines(x,predict(loess(yB0~x)))  ####lowess做回归曲线,predict是取回归预测值
lowess(x,yB0,f=50,iter = 10);plot(x,y)
loess(yB0~x,span=0.75,degree=2);plot(yB0~x)




##通过lines一条一条加。
plot(x,yB0,type="p")
lines(x,yB1,type="p")
lines(x,yB2,type="p")
...
