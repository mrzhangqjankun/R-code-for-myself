##R专题：把fa/fq读到R里面去~ 
##https://mp.weixin.qq.com/s?__biz=MzUzMTEwODk0Ng==&mid=2247485133&idx=1&sn=4499a3090c69f49d574b190a608ccaa5&chksm=fa46c3f0cd314ae66c2774be961d2a4bb5ac5ba46733b09cdea33dccd1b6700c757b52579884&scene=0#rd
##Biostrings

rm(list=ls(all=TRUE))

source("http://bioconductor.org/biocLite.R")
biocLite("Biostrings")
library(Biostrings)

############表示序列
# 直接输入序列
dna1 <- DNAString("ACGT-N")
dna1
dna2 <- DNAStringSet(c("ACGT", "GTCA", "GCTA"))
dna2
# 从fasta读入
dna3 = readDNAStringSet("./test/sequence.fasta",format="fasta")
dna3
# 详细参数
readBStringSet(filepath, format="fasta",
               nrec=-1L, skip=0L, seek.first.rec=FALSE, use.names=TRUE)
readDNAStringSet(filepath, format="fasta",
                 nrec=-1L, skip=0L, seek.first.rec=FALSE, use.names=TRUE)
readRNAStringSet(filepath, format="fasta",
                 nrec=-1L, skip=0L, seek.first.rec=FALSE, use.names=TRUE)
readAAStringSet(filepath, format="fasta",
                nrec=-1L, skip=0L, seek.first.rec=FALSE, use.names=TRUE)
# nrec:最多从文件读入多少记录，负数则表示该项被忽略；
# skip:一个非负整数，表示在开始读入记录之前要跳过的记录数；
# seek.first.rec:TRUE或者FALSE（默认）。 如果为TRUE，则一定是以“>”（对于FASTA）或“@”（对于FASTQ）作为第一行。 如果找不到这样的行，就会出现错误。值得一提的是，TRUE还可以用于解析带有FASTA数据的GFF3文件。
# use.names:返回的向量是否应该被命名，对于FASTA，名称取自描述行。对于FASTQ，它们取自记录序列ID。 删除名称可以帮助减少内存占用，例如：对于包含数百万个记录的FASTQ文件。

#################基本信息
# rev运用在DNAStringSet只是反向元素的顺序，而在 DNAString是整个string的反转。reverse是反转序列dna1
rev(dna1)
reverse(dna1)
dna2
rev(dna2)   ##互补
reverse(dna2) ##反过来
# width查看序列长度
width(dna1)
width(dna2)
# letterFrequency用于统计指定字符的频率或比例
letterFrequency(dna1, DNA_BASES)
letterFrequency(dna1, DNA_ALPHABET)
letterFrequency(dna1, DNA_BASES, as.prob = TRUE)
# 计算GC含量
letterFrequency(dna1, "GC", as.prob = TRUE)
# 分别统计2核苷酸组合和3核苷酸组合dinucleotideFrequency(dna1)
trinucleotideFrequency(dna1)

##treeio包里的read.fasta，也可以读入序列，它有个好处是你直接as.character(aa[[i]])就是个字母的向量，方便比较。
require(treeio)
fasta <- system.file("./test/sequence.fasta", package="ggtree")
aa <- read.fasta(fasta)


##Shortread
source("http://bioconductor.org/biocLite.R")
biocLite("ShortRead")
library(ShortRead)
fq <- readFastq("./test/sequence.fastq")

fa <- readFasta("./test/sequence.fasta")
