##'2019.9.25
##'于排序图标识的虚化代码 
##'http://wap.sciencenet.cn/blog-267448-1140683.html



#有时候排序图的中物种或样方过多，造成标识重叠的情况，可以用vegan包中的orditorp将标识虚化

## A cluttered ordination plot :
library(vegan)
data(BCI)
mod <- cca(BCI)
plot(mod, dis="sp", type="t")
# Now with orditorp and abbreviated species names
cnam <- make.cepnames(names(BCI))
plot(mod, dis="sp", type="n")
stems <- colSums(BCI)
orditorp(mod, "sp", label = cnam, priority=stems, pch="+", pcol="grey")

?orditorp
