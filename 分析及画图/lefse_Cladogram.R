##2019.6.18
#lmicrobiomeViz：绘制lefse结果中Cladogram

#https://blog.csdn.net/woodcorpse/article/details/80631232

download.file("https://bitbucket.org/biobakery/biobakery/raw/tip/demos/biobakery_demos/data/metaphlan2/output/SRS014459-Stool_profile.txt", 'SRS014459-Stool_profile.txt')
knitr::kable(head(read.table('SRS014459-Stool_profile.txt')))

#第一列是分类信息注释，第二列是相对丰度（百分比）。
#在做这种图可视化方面，目前个人见过最强大的是GraPhlAn

#让我们产生lefse调用graphlan绘制的物种树标记差异物种的Cladogram
# microbiomeViz需要 R 3.5 以上，依赖包安装
#devtools::install_github("lch14forever/microbiomeViz")
library(pacman);
p_install_gh("lch14forever/microbiomeViz")
library(microbiomeViz)   ###总会报错，版本的问题。
library(ggtree)

#输入数据为metaphlan2结果合并的矩阵。如何生成详见：MetaPhlAn2一条命令获得宏基因组物种组成

# 加载测试数据
df <- read.table("http://bailab.genetics.ac.cn/markdown/R/microbiomeViz/merged_abundance_table.txt", head=TRUE, stringsAsFactors = FALSE)

## 计算均值用于呈现结点大小
dat <- data.frame(V1=df[,1], V2=rowMeans(df[,-1]), stringsAsFactors = FALSE)

# 用物种和丰度生成树骨架
tr <- parseMetaphlanTSV(dat, node.size.offset=2, node.size.scale=0.8)
p <- tree.backbone(tr, size=0.5)
p

#差异物种注释

# 读取需要颜色标注的差异物种列表，本质上是两列和颜色对应表
lefse_lists = data.frame(node=c('s__Haemophilus_parainfluenzae','p__Proteobacteria',
                                'f__Veillonellaceae','o__Selenomonadales',
                                'c__Negativicutes', 's__Streptococcus_parasanguinis',
                                
                                'p__Firmicutes','f__Streptococcaceae',
                                'g__Streptococcus','o__Lactobacillales',
                                'c__Bacilli','s__Streptococcus_mitis'),
                         color=c(rep('darkgreen',6), rep('red','6')),
                         stringsAsFactors = FALSE
)


# 注释树

p <- clade.anno(p, lefse_lists, alpha=0.3)
p
