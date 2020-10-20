##2020.10.18

##https://github.com/DaliangNing/iCAMP1

#install.packages("iCAMP")
library(iCAMP)
?iCAMP


##主函数icamp.big,基于系统发育零模型分析和量化不同过程的相对重要性。
icamp.big(comm, tree, pd.desc = NULL, pd.spname = NULL, pd.wd = getwd(),
          rand = 1000, prefix = "iCAMP", ds = 0.2, pd.cut = NA, sp.check = TRUE,
          phylo.rand.scale = c("within.bin", "across.all", "both"),
          taxa.rand.scale = c("across.all", "within.bin", "both"),
          phylo.metric = c("bMPD", "bMNTD", "both", "bNRI", "bNTI"), 
          sig.index=c("Confidence","SES.RC","SES","RC"), bin.size.limit = 24,
          nworker = 4, memory.G = 50, rtree.save = FALSE, detail.save = TRUE,
          qp.save = TRUE, detail.null=FALSE, ignore.zero = TRUE,
          output.wd = getwd(), correct.special = TRUE, unit.sum = rowSums(comm),
          special.method = c("depend","MPD","MNTD","both"),
          ses.cut = 1.96, rc.cut = 0.95, conf.cut=0.975,
          omit.option = c("no", "test", "omit"), meta.ab = NULL,
          treepath.file="path.rda", pd.spname.file="pd.taxon.name.csv",
          pd.backingfile="pd.bin", pd.desc.file="pd.desc")
#主要参数
#comm:数据，行为样本，列为物种（OTU或ASV）
#tree: 系统发育树
#pd.desc:系统发育距离矩阵文件
#pd.spname:系统发育距离矩阵taxa id
#pd.wd:文件保存路径
#rand:随机化次数，默认1000
#ds:系统发育信号阈值，默认0.2
#pd.cut:系统发育树截断的位置，得到严格的Bins。默认NA
#phylo.rand.scale：系统发育零模型随机化方法，默认within.bin
#taxa.rand.scale：分类学零模型随机化方法，默认across.all
#phylo.metric：零模型计算方法，默认MPD。
#sig.index：零模型显著性检验方法
#bin.size.limit：最小bin的大小。默认24
#nworker：并行运算，默认4线程
#其他参数不太重要，也基本不需要动。

##实例
data("example.data")
comm=example.data$comm  ##OTU
tree=example.data$tree  ##tree

save.wd=tempdir()
pd.wd=paste0(save.wd,"/pdbig")

setwd(save.wd)
icamp.out=icamp.big(comm=comm,tree=tree,pd.wd=pd.wd,
                    rand=20, nworker=4,
                    bin.size.limit=5)
options(max.print = 999999)
sink("icamp.txt")
icamp.out
sink()

##结果包含两个样本之间生态过程相对重要性，这是我们主要需要的结果。
##另外还包含每个OTU（ASV）所属的bin及每两个bin之间的系统发育距离;bin在不同零模型中的显著性等等。

###其他功能
#1.进一步分析
#icamp.bins对结果进行整理后输出，
#icamp.boot对结果进行bootstrapping分析
#qp.bin.js计算每个bin的群落构建(即个样本/群落之间的两两比较)，然后计算群落构建相对重要性
#taxa.binphy.big：系统发育树的binning
#计算MNTD（mntdn）,MPD(mpdn),NRI(NRI.p),NTI(NTI.p)，RC(RC.bin.bigc, RC.pc),betaNTI(bNTIn.p,bNTI.bin.big),betaMNTD(bmntd),betaMPD(bmpd),betaNRI(bNRI.bin.big,bNRIn.p)
#null.norm计算零模型正态性
#pdist.big:系统发育树种间系统发育距离矩阵，使用bigmemory处理过大的数据集。
#ps.bin:bin内部的系统发育信号
#change.sigindex:快速切换不同指标进行零模型显著性检验。
#2.其他随机性方法
#qpen: 之前stegen的QPEN方法
#snm:之前Sloan提出的基于丰度加权和不加权的分类单元百分比的中性理论模型。

#3.实用工具
#cohend：计算Cohen's效应量，这个之前介绍过：
#效应量的计算——Cohen's d statistic

#dist.3col: 将距离矩阵转换为3列数据框，这个超级实用！之前也介绍过方法，利用as.vector，或用simba包的liste
#见：一些R代码学习笔记

#dniche:根据物种的环境变量计算物种间的生态位差，直接输出矩阵或保存为big.matrix
#生态位计算方法为各环境因子丰度加权平均值.
#关于生态位的计算，之前也介绍很多了。但是这里的方法是第一次介绍，源于
#Stegen, J.C., Lin, X., Konopka, A.E. & Fredrickson, J.K. (2012). Stochastic and deterministic assembly processes in subsurface microbial communities. ISME J, 6, 1653-1664.
#之前的文章见：
#spaa: 计算生态位宽度
#indicspecies:计算物种与样本之间关系的强度与生态位宽度
#MicroNiche: 生态位概念及计算

#match.2col：比较两个矩阵的名字是否相同
#match.name：比较两个数据的行名或列名是否相同
#maxbigm：找到大矩阵中最大的值及其位置
#midpoint.root.big:计算大树的系统发育距离