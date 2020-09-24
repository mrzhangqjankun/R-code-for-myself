##2019.9.24

##rBLAST

##https://github.com/mhahsler/rBLAST

library(devtools)
devtools::install_github("mhahsler/rBLAST")

library(rBLAST)
?blast

##需要自己下载blast
#寻找本地可用的blast
Sys.which("blastn") 

#如果找不到需要自己设置环境变量
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "path_to_BLAST", sep= .Platform$path.sep))

#版本检查
system("blastn -version")

## 下载数据
download.file("ftp://ftp.ncbi.nlm.nih.gov/blast/db/16SMicrobial.tar.gz",
              "16SMicrobial.tar.gz", mode='wb')

##解压
untar("16SMicrobial.tar.gz", exdir="16SMicrobialDB")

## 导入测试数据
seq <- readRNAStringSet(system.file("examples/RNA_example.fasta",
                                    package="rBLAST"))
seq

##建数据库
dir <- tempdir()
db = makeblastdb(file.path(dir, "seqs.fasta"), dbtype = "nucl", args="")
#dbtype:"nucl" or "prot"
#args:其他参数

## 加载数据库
bl <- blast(file.path(dir, "seqs.fasta"))
bl

print(bl, info=TRUE)

## 比对
cl <- predict(bl, seq[1,])
cl[1:5,]

## 比对，并设置输出格式
cl <- predict(bl, seq[1,], custom_format = "qseqid bitscore length")
cl[1:5,]

bl <- blast(db="./16SMicrobialDB/16SMicrobial")
bl

print(bl, info=TRUE)

## query a sequence using BLAST
cl <- predict(bl, seq[1,])
cl[1:5,]

## custom format (see BLAST documentation)
cl <- predict(bl, seq[1,], custom_format = "qseqid bitscore length")
cl[1:5,]