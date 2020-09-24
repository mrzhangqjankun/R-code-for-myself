

install.packages("NST")

library(NST)

#NST可以根据不同的相似性矩阵和不同的零模型算法，以及以前的一些指标，如以前的Stochasticity Ratio (ST), Standard Effect Size (SES), modified Raup-Crick metrics (RC)来计算


ab.assign
在考虑丰度的零模型基础上随机化群落时，将丰度分配给物种。根据指定的概率，个体被随机分为不同的种类。
samp.ab: 样本的总丰度
prob.ab:在一个特定的样本中，每个物种的个体被抽取的概率。

data(tda)
comm=tda$comm
comm.b=comm
comm.b[comm.b>0]=1
samp.ab=rowSums(comm)
prob.ab=matrix(colSums(comm),nrow=nrow(comm),ncol=ncol(comm),byrow=TRUE)
comm.rand=ab.assign(comm.b,samp.ab,prob.ab)

beta.g
可计算21种beta多样性参数

beta.limit
对多样性值设置一个上限


dist.3col
这个很有用，可将beta多样性的矩阵转化为3列的形式。前两列为样本，第三列为相似性
data(tda)
comm=tda$comm
bray=beta.g(comm,dist.method="bray")
bray.3col=dist.3col(bray)

tNST
最重要的函数，计算NST
data(tda)
comm=tda$comm
group=tda$group
tnst=tNST(comm=comm, group=group, dist.method="jaccard",
          abundance.weighted=TRUE, rand=20,
          nworker=1, null.model="PF", between.group=TRUE,
          SES=TRUE, RC=TRUE)


nst.boot
检验各组ST、NST的分布情况及各组ST、NST差异的显著性。
nst.bt=nst.boot(nst.result=tnst, group=NULL, rand=99,
                trace=TRUE, two.tail=FALSE, out.detail=FALSE,
                between.group=FALSE, nworker=1)

nst.panova
ST和NST组间进行Permutational multivariate ANOVA
nst.pova=nst.panova(nst.result=tnst, rand=99)
