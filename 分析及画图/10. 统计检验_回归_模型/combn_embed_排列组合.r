##2019.3.11
#https://mp.weixin.qq.com/s/tyZsciAROxzOpkIeFrgK2Q
#R语言 | 多组样本的N种组合
#combn;combinations;embed


#combn() 函数从一个向量中获得所有可能的元素组合：
?combn
#combn(x, m, FUN = NULL, simplify = TRUE, ...)
#x 需要组合的元素；m几个元素进行组合；FUN组合所用的公式；
#simplify，true返回数组或矩阵；false返回列表

#一下两者相同
n=5;m=2
dim(combn(n, m)) == c(m, choose(n, m))
?choose


combn(letters[1:4], 2)
(m <- combn(10, 5, min))   # minimum value in each combination
mm <- combn(15, 6, function(x) matrix(x, 2, 3))


labels = c("I","II","III","IV")
res = combn(labels,2);res

library(gtools)
index = combinations(length(labels),2);index;
?combinations  ##返回元素的排列或组合数

##只取某位与其相邻下一位的组合：
res[,index[,2] == index[1,]+1 ]

##embed函数也可以实现：
?embed  #低纬度的欧式空间中嵌入时间序列
#embed (x, dimension = 1)
#Each row of the resulting matrix consists of sequences 
#x[t], x[t-1], ..., x[t-dimension+1], where t is the original index of x. 
#If x is a matrix, i.e., x contains more than one variable, 
#then x[t] consists of the tth observation on each variable.
comps = embed(labels,2) ;comps
comps = embed(labels,2)[,2:1] ;comps ##两列顺序调换

