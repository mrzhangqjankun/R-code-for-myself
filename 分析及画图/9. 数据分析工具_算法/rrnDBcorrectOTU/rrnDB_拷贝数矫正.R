
rm(list=ls())
setwd("E:/桌面/")

rrnDB = read.csv(file="rrnDB-5.6_pantaxa_stats_RDP.tsv",header=T,sep="\t")
otu = read.table(file="otu.tabular",header=T,row.names=1,sep="\t")
classifer = read.table(file="Classifier.txt",header=T,row.names=1,sep="\t")


#genus  classifer中第6列不是Unclassified,第7列是Unclassified
genus = classifer[(classifer[,6] != "Unclassified") &  (classifer[,7] == "Unclassified"),]
rgenus = rrnDB[ rrnDB[,2]=="genus",c(3,9)]
#rownames(rgenus) = rgenus[,1]



#classifer 第6列为属，与OTU match
name.genus = rownames(genus)
name.otu = rownames(otu)
name.match = match(name.genus,name.otu)
match.otu = otu[name.match,]
#属水平注释信息加到otu中，得到otu.genus
otu.genus = cbind(genus[,6],match.otu)

#或者用intersect。注意这个函数使用的时候行名必须是交集的结果才能直接用。
###############
inta = intersect(name.genus,name.otu)
oo = otu[inta,]
tt = genus[inta,]
###############

############################
#注意这种方法不对。match后出现大量的NA，且同一属只保留了第一个。
#otu.genus 和rrnDB match
m1 = match(as.character(rgenus[,1]),as.character(otu.genus[,1]))
otu.genus1 = otu.genus[m1,];otu.genus1 = otu.genus1[complete.cases(otu.genus1),]
rgenus1 = rgenus[m1,];rgenus1 = rgenus1[complete.cases(rgenus1),]

#rrnDB和otu.genus1 match
m2 = match(as.character(otu.genus1[,1]),as.character(rgenus1[,1]))
otu.genus2 = otu.genus[m2,];otu.genus2 = otu.genus2[complete.cases(otu.genus2),]
rgenus2 = rgenus[m2,];rgenus2 = rgenus2[complete.cases(rgenus2),]

rgenus3 = rgenus2[order(rgenus2[,1]),]
otu.genus3 = otu.genus2[order(otu.genus2[,1]),]
################################


#注意intersect这种方法存在问题，同一个属只会保留第一个。后面的都丢掉了。
###################
qq = intersect(as.character(rgenus[,1]),as.character(otu.genus[,1]))
rownames(rgenus)=rgenus[,1]
g1 = rgenus[qq,]
ss = as.character(otu.genus[,1])
o1 = otu.genus[match(qq,ss),]
##################

##############################
#用for 循环遍历
#规定OTU中有而数据库中没有的为-1
whole = data.frame(matrix(data=NA,
               nrow=length(rownames(otu.genus)),
               ncol=length(colnames(otu.genus))+1),stringsAsFactors=FALSE)

for (i in 1:length(rownames(otu.genus))){  #i=1
  mat = match(as.character(otu.genus[i,1]),as.character(rgenus[,1]))
  #mat = match("aaa",as.character(rgenus[,1]))  #NA
  if(!is.na(mat)){
    each = cbind(rgenus[mat,2],otu.genus[i,-1])
  } else{
    each = cbind(-1,otu.genus[i,-1])
  }
  whole[i,] = each
}
head(whole)

#加上物种信息
genus.res = cbind(taxa=otu.genus[,1],level=rep("genus",length(rownames(otu.genus))),whole)
rownames(genus.res) = rownames(otu.genus)


##############每一个level做完之后都需要将已做过的OTU去掉。加快后面的速度。
##############最重要的是避免“有物种信息；unclassifiered；有物种信息”这种情况出现。保证有信息的是最小的level，结果最准确。
otu = otu.genus
diffotu = setdiff(rownames(otu),rownames(otu.genus))
otu = otu[diffotu,]

###########################################科水平
#family  classifer中第5列不是Unclassified,第6列是Unclassified
family = classifer[(classifer[,5] != "Unclassified") &  (classifer[,6] == "Unclassified"),]
rfamily = rrnDB[ rrnDB[,2]=="family",c(3,9)]
#rownames(rgenus) = rgenus[,1]


#classifer 第5列为科，与OTU match
name.family = rownames(family)
name.otu = rownames(otu)
name.match = match(name.family,name.otu)
match.otu = otu[name.match,]
#科水平注释信息加到otu中，得到otu.family
otu.family = cbind(family[,5],match.otu)


##############################
#用for 循环遍历
#规定OTU中有而数据库中没有的为-1
whole = data.frame(matrix(data=NA,
                          nrow=length(rownames(otu.family)),
                          ncol=length(colnames(otu.family))+1),stringsAsFactors=FALSE)

for (i in 1:length(rownames(otu.family))){  #i=1
  mat = match(as.character(otu.family[i,1]),as.character(rfamily[,1]))
  #mat = match("aaa",as.character(rfamily[,1]))  #NA
  if(!is.na(mat)){
    each = cbind(rfamily[mat,2],otu.family[i,-1])
  } else{
    each = cbind(-1,otu.family[i,-1])
  }
  whole[i,] = each
}
head(whole)

family.res = cbind(family=otu.family[,1],whole)
rownames(family.res) = rownames(otu.family)






###############################综上，可将所有水平都写到一个函数中。
rm(list=ls())
setwd("E:/桌面/")

rrnDB = read.csv(file="rrnDB-5.6_pantaxa_stats_RDP.tsv",header=T,sep="\t")
otu = read.table(file="otu.tabular",header=T,row.names=1,sep="\t")
classifer = read.table(file="Classifier.txt",header=T,row.names=1,sep="\t")

hang1 = length(rownames(otu))

#rrnDB只到class;
#a为genus,family,order,class
#b比a低一级
#从genus到class,即从低到高遍历levels
a= c(6,5,4,3)
b= c(7,6,5,4)
levels= c("genus","family","order","class")
whole.res = c()

for (q in 1:4){  #q=1
  spe = classifer[(classifer[,a[q]] != "Unclassified") &  (classifer[,b[q]] == "Unclassified"),]
  rspe = rrnDB[ rrnDB[,2]==levels[q],c(3,9)]

  name.spe = rownames(spe)
  name.otu = rownames(otu)
  name.match = match(name.spe,name.otu)
  match.otu = otu[name.match,]
  #注释信息加到otu中
  otu.spe = cbind(spe[,a[q]],match.otu)

  ##############################
  #用for 循环遍历
  #规定OTU中有而数据库中没有的为-1
  whole = data.frame(matrix(data=NA,
                            nrow=length(rownames(otu.spe)),
                            ncol=length(colnames(otu.spe))+1),stringsAsFactors=FALSE)

  for (i in 1:length(rownames(otu.spe))){  #i=1
    mat = match(as.character(otu.spe[i,1]),as.character(rspe[,1]))
    if(!is.na(mat)){
      each = cbind(rspe[mat,2],otu.spe[i,-1])
    } else{
      each = cbind(-1,otu.spe[i,-1])
    }
    whole[i,] = each
  }
  #加上物种信息
  spe.res = cbind(taxa=otu.spe[,1],level=rep(levels[q],length(rownames(otu.spe))),whole)
  rownames(spe.res) = rownames(otu.spe)

  #输出到whole.res
  whole.res = rbind(whole.res,spe.res)
  ##############每一个level做完之后都需要将已做过的OTU去掉。加快后面的速度。
  ##############最重要的是避免“有物种信息；unclassifiered；有物种信息”这种情况出现。保证有信息的是最小的level，结果最准确。
  diffotu = setdiff(rownames(otu),rownames(otu.spe))
  otu = otu[diffotu,]
}
whole.res = whole.res[complete.cases(whole.res),]
colnames(whole.res) = c("taxa","level",colnames(otu))

#检验一下whole.res是否和OTU表的行数一样
#结果的行数+属水平数据库空缺的行数+class做完之后还剩下来的OTU
hang2 = length(rownames(whole.res))+length(rownames(otu))

if (hang1==hang2){
  print("Well done!")}

getwd()
write.table(whole.res, file="rrnDB_combined_otu.txt",sep='\t',col.names = NA)

#######################OTU丰度除以拷贝数，得到校正的表格
#先去掉第三列为-1的值
simply.res = whole.res[whole.res[,3]!= -1,]
correct = simply.res[,-c(1:3)] / simply.res[,3]
correct.table = as.data.frame(cbind(simply.res[,1:2],correct))

write.table(correct.table, file="rrnDB_corrected_otu.txt",sep='\t',col.names = NA)




#############编写函数
#rrnDBcorrectOTU

rco = function(otu,classifer,rrnDB){

  hang1 = length(rownames(otu))
  #rrnDB只到class;
  #a为genus,family,order,class
  #b比a低一级
  #从genus到class,即从低到高遍历levels
  a= c(6,5,4,3)
  b= c(7,6,5,4)
  levels= c("genus","family","order","class")
  whole.res = c()

  for (q in 1:4){  #q=1
    spe = classifer[(classifer[,a[q]] != "Unclassified") &  (classifer[,b[q]] == "Unclassified"),]
    rspe = rrnDB[ rrnDB[,2]==levels[q],c(3,9)]

    name.spe = rownames(spe)
    name.otu = rownames(otu)
    name.match = match(name.spe,name.otu)
    match.otu = otu[name.match,]
    #注释信息加到otu中
    otu.spe = cbind(spe[,a[q]],match.otu)

    ##############################
    #用for 循环遍历
    #规定OTU中有而数据库中没有的为-1
    whole = data.frame(matrix(data=NA,
                              nrow=length(rownames(otu.spe)),
                              ncol=length(colnames(otu.spe))+1),stringsAsFactors=FALSE)

    for (i in 1:length(rownames(otu.spe))){  #i=1
      mat = match(as.character(otu.spe[i,1]),as.character(rspe[,1]))
      if(!is.na(mat)){
        each = cbind(rspe[mat,2],otu.spe[i,-1])
      } else{
        each = cbind(-1,otu.spe[i,-1])
      }
      whole[i,] = each
    }
    #加上物种信息
    spe.res = cbind(taxa=otu.spe[,1],level=rep(levels[q],length(rownames(otu.spe))),whole)
    rownames(spe.res) = rownames(otu.spe)

    #输出到whole.res
    whole.res = rbind(whole.res,spe.res)
    ##############每一个level做完之后都需要将已做过的OTU去掉。加快后面的速度。
    ##############最重要的是避免“有物种信息；unclassifiered；有物种信息”这种情况出现。保证有信息的是最小的level，结果最准确。
    diffotu = setdiff(rownames(otu),rownames(otu.spe))
    otu = otu[diffotu,]
  }
  whole.res = whole.res[complete.cases(whole.res),]
  colnames(whole.res) = c("taxa","level",colnames(otu))

  #检验一下whole.res是否和OTU表的行数一样
  #结果的行数+属水平数据库空缺的行数+class做完之后还剩下来的OTU
  hang2 = length(rownames(whole.res))+length(rownames(otu))

  if (hang1==hang2){
    print("Well done!")}

  #######################OTU丰度除以拷贝数，得到校正的表格
  #先去掉第三列为-1的值
  simply.res = whole.res[whole.res[,3]!= -1,]
  correct = simply.res[,-c(1:3)] / simply.res[,3]
  correct.table = as.data.frame(cbind(simply.res[,1:2],correct))

  list(whole.res=whole.res,correct.table=correct.table)
}


res = rco(otu,classifer,rrnDB)
res$whole.res
res$correct.table


####安装Rtools

library(stringr)
library(installr)

install.Rtools()

##测试一下
library(rrnDBcorrectOTU)
aaa = rco(otu,classifer,rrnDB)
aaa$whole.res


##从github上下载测试
devtools::install_github("Listen-Lii/rrnDBcorrectOTU", subdir = "pkg")
