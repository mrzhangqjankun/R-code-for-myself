##li 2017.7.5 
##http://www.omicshare.com/forum/thread-146-1-1.html
##[R语言] R语言ggplot2画图教程之——Pathway富集分析气泡图

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/Pathway富集分析气泡图')

library(ggplot2)

pathway = read.table("R0-vs-R3.path.richFactor.head20.tsv",header=T,sep="\t")

# 画图
pp = ggplot(pathway,aes(richFactor,Pathway))
pp + geom_point()

# 改变点的大小
pp + geom_point(aes(size=R0vsR3))

# 四维数据的展示
pbubble = pp + geom_point(aes(size=R0vsR3,color=-1*log10(Qvalue)))
# 自定义渐变颜色
pbubble + scale_colour_gradient(low="green",high="red")

# 绘制pathway富集散点图
pr = pbubble + scale_colour_gradient(low="green",high="red") + labs(color=expression(-log[10](Qvalue)),size="Gene number",x="Rich factor",y="Pathway name",title="Top20 of pathway enrichment")
# 改变图片的样式（主题）
pr + theme_bw()
## 保存图片
ggsave("out.pdf")   # 保存为pdf格式，支持 pdf，png，svg多重格式
ggsave("out.png")  # 保存为png格式
ggsave("out2.png",width=4,height=4)   # 设定图片大小

备注，绘图数据的说明：
（1）绘图数据来自我们公司KEGG富集分析的结果，相应文件是结题报告中存在的，略作调整即可；
（2）绘图数据每一列的意思：
1)Pathway      : 通路的名称        
2)R0vsR3            ：差异表达基因中，属于这个通路的基因的数量
3)All_Unigene        ：所有基因中属于这个通路的基因的数量  
4)Pvalue            ：富集分析p值
5)Qvalue                ：富集分析的Q值
6)richFactor        ：在我们分析报告中，没有提供这一列，但很容易计算。是 第二列 除以 第三列得到；
7)Pathway ID        ：通路ID  
8)Genes                ：通路中基因的ID
9)KOs                  ：通路中基因的KO号
补充一点：绘图仅仅用到4类，分布是第1,2,5,6列
再补充一点（来源25,26,28楼的讨论）
（1）在pathway名称中如有重名，这会导致错误。在表中每个pathway只能出现一次；
（2）文本中，出现了引号会导致错误。例如，  Alzheimer's disease， Huntington's disease这样的名称。
这两个pathway 名称中的引号需要删除。引号的出现，会导致R无法识别两个引号间的其他符号（退格符，换行符等），导致文件读取错误。
如果一定要保留引号，则引号的内容再用引号囊括起来，例如：
"Alzheimer's disease"，"Huntington's disease"，从而避免单个出现的引号对其他字符的影响 。