##2017.7.7 li learn from OS Forum
##http://www.omicshare.com/forum/forum.php?mod=viewthread&tid=918&extra=page%3D1%26filter%3Dtypeid%26typeid%3D18
##resample question:
#16S测序结果返回一个OTU表格，这个OTU表格里有M个样品（且不论分组），
#总的OTU数目为N，我们随机地从M个样品里抽取m个样品所获得的OTU总数为n，
#那么请计算，m需要满足什么一个什么样的条件，就能使得n>=95%N?

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/resample/')
#setwd('C:/Users/dell/Desktop/article/Beijing-Tibet-Changsha/ITS 236-270/TAR/phylum')
#rm(list=ls(all=TRUE))
x <- read.table("resample_OTU.txt",sep="\t",head=TRUE,row.names=1);
#head(x)
#tail(x,10)
M=length(x[1,])
M   ## 24

##nrow(x)  行数统计
##length(x[,1]) 这个也可以。直接length(x)不行
##统计N中OTU数
rowsum = rowSums(x[1:nrow(x),]); ##rowsum
rowsum[rowsum > 0] = 1 ; ##rowsum
N=length(rowsum[rowsum > 0]) ;N  ## 401

########################################### m=C(1:24)
####1.抽样函数sample。抽m个样本,即按照列来抽。每个样本抽一次。（这样的话只有一个样本不能画箱式图。）
m=c(floor(M/10),floor(M/5),floor(M/3),floor(M/2),M,2*M,3*M,5*M,10*M,50*M,100*M);m  ##ceiling()是上取整 ；floor() 是下取整
n=c(rep(0,length(m))) ;n
for (i in 1:length(m)){
  sample = sample(x,m[i],replace=T); ##head(m[i])
  ##统计sample中OTU数
  rowsum = rowSums(sample[1:nrow(sample),]); ##rowsum
  rowsum[rowsum > 0] = 1 ; ##rowsum
  n[i]=length(rowsum[rowsum > 0]);  ##n[i]
  i = i+1
}
n
y=n/N;y   ## sample的OTU占总OTU的比例
plot(m,y)
plot(m,n)

#######2.1  改进。抽m个样本，每个样本抽k次。
k=10
m=rep(c(floor(M/10),floor(M/5),floor(M/3),floor(M/2),M,2*M,3*M,5*M,10*M,50*M,100*M),k);m
p=length(c(floor(M/10),floor(M/5),floor(M/3),floor(M/2),M,2*M,3*M,5*M,10*M,50*M,100*M));p

n=c(rep(0,length(m))) ;n
y=c(rep(0,length(m))) ;y

for (i in 1:length(m)){
  sample = sample(x,m[i],replace=T); ##head(m[i])
  ##统计sample中OTU数
  rowsum = rowSums(sample[1:nrow(sample),]); ##rowsum
  rowsum[rowsum > 0] = 1 ; ##rowsum
  n[i]=length(rowsum[rowsum > 0]);  ##n[i]
  y[i]=n[i]/N    
  i = i+1
}
n;
y;

plot(m,n)

##2.2 改进。将每次循环得到的m和y放在一起。m为抽样数，y为OTU数占总体的比例。
#m=rep(c(1:24),k);m
#p=24

k=10
p   ##11
r=matrix(0,nr=p,nc=k) ;r  ##建立一个p行K列的空矩阵，往里面填结果。

 for (i in 1:p){           ##p为一次抽样的长度，11                  
    for (j in 0:k-1) {        ##k为抽样次数，10。每隔这么多次m出现相同数据。
       r[i,j+1] = y[i+p*j];  ##行为样本，列为重复次数。
       j=j+1;  }
      i=i+1
}
  r   
  rowname = m[1:p];rowname; colname = c(1:10);colname
  rownames(r)=rowname;colnames(r)=colname;r
##画箱式图
##http://www.omicshare.com/forum/forum.php?mod=viewthread&tid=2456&extra=page%3D1%26filter%3Dtypeid%26typeid%3D18
##http://www.omicshare.com/forum/forum.php?mod=viewthread&tid=2514&extra=page%3D1%26filter%3Dtypeid%26typeid%3D18
##


##boxplot(r)
boxplot(t(r))  ##横坐标为resample数
boxplot(t(r),main="Rarefraction curve",xlab="resample number",ylab="%",border=2,col="blue") 

###加图例legend(0.5,33,#前一个是距离左侧距离，后一个是距离底端距离,现在的位置是左上角
       ##c("a","b","c"),#图例名称
       ##fill = c("yellow","orange","brown")#图例填充颜色  )

#添加平均数点
#points(
#c(1:11),#“(1:11)”要添加到1~12个盒子
#       c(read.table("mean.txt")),#读取平均值数据，我不会写直接运算的代码，用了最笨的办法，需要其他同事完善。
#       pch=15,#点的样式，0~25，图例可以在R中输入“？points”查看
#      cex=0.5)#点的大小


##2.3 改进。y为OTU数占总体的比例,n为得到的OTU数。y变为n,其他不动。sample-OTU图。
#m=rep(c(1:24),k);m
#p=24

k=10
p   ##11
r=matrix(0,nr=p,nc=k) ;r  

for (i in 1:p){                     
  for (j in 0:k-1) {        
    r[i,j+1] = n[i+p*j];   ## y变为n
    j=j+1;  }
  i=i+1
}
r   
rowname = m[1:p];rowname; colname = c(1:10);colname
rownames(r)=rowname;colnames(r)=colname;r

boxplot(t(r),main="Rarefraction curve-OTU",xlab="resample number",ylab="%",col="red") 

##3 改进。以上为按照列进行resample。下面按照行进行resampel。
##随机取一些行得到多少OTU——行求和；得到多少序列——列求和。

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/resample/')
x <- read.table("resample_OTU.txt",sep="\t",head=TRUE,row.names=1);
M=length(x[1,])
M   ## 样本数24

##统计N中OTU数，序列数。行数

rowsum = rowSums(x[1:nrow(x),]); 
rowsum[rowsum > 0] = 1 ; 
N=length(rowsum[rowsum > 0]) ;N  ## 401种OTU
seq_total=apply(x,2,sum);seq_total  ##OTU数。 apply,1为行，2为列
H=nrow(x);H  ##行数统计,558
##length(x[,1]) 这个也可以。直接length(x)不行


####行抽样
a=5
random=floor(runif(a,min=0,max=H)) ;random ##产生a个最小值为0，最大值为H的随机数。floor向下取整
##m=c(floor(H/10),floor(H/5),floor(H/3),floor(H/2),H,2*H);m    ##random即之前的m

x=t(x);head(x)  ###默认按照列抽样。注意要转置。转置后列为样本，行为OTU。
row=c(rep(0,M)) ;row
col=c(rep(0,length(random))) ;col

for (i in 1:length(random)){   ##统计sample中OTU数和序列数
  sample = sample(x,random[i],replace=T); sample   ##因为转置，行求和为序列数，列进行（0,1）求和为OTU数。
  #列，OTU
  colsum = colSums(sample[,1:ncol(sample)]); ##colsum
  colsum[colsum > 0] = 1 ; ##colsum
  col[i]=length(colsum[colsum > 0]);  ##n[i]
  i = i+1
  #行
  rowsum = rowSums
}
col
row
plot(random,col)
plot(random,row)




#三个函数

f1<-function(x) {   ##编写用于对向量进行判定的函数
n<-length(x)   
a<-vector(length=n)  
for (i in 1:n) {
a[i]<-ifelse(x[i]==0,0,1)}
return(a)
}

f2<-function(x) data.frame(apply(x,2,f1)  ##将判定函数应用到数据框并且返回数据框，注意数据框只能是数值型的

##对普通的含有因子数据（OTU1，OTU2..)的数据框，用索引选择就行了

f3<-function(x,m){   ## x是数据框，m是打算取多少个样本
e<-ncol(x)
a<-combn(1:e,m)
b<-ncol(a)
c<-vector(length=b)
for(i in 1:b){
c[i]<-sum(f1(apply(x[,a[,i]],1,sum)))}
return(c)
}
