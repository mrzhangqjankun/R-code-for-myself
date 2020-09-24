##2019.4.23
#Renyi index

library(vegan);?vegan
#Renyi and Hill Diversities and Corresponding Accumulation Curves

#renyi函数计算不同alpha时的Renyi diversity或者相应的Hill number
#Usage
#renyi(x, scales = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, Inf),hill = FALSE)
#scales: Renyi 公式中的alpha参数
#hill:是否计算hill number
data(BCI)
i <- sample(nrow(BCI), 12);i
mod <- renyi(BCI[i,]);mod
mod2 <- renyi(BCI[i,],hill=TRUE);mod2
plot(mod)

#按照行累积多样性。(与specaccum函数类似)
#renyiaccum(x, scales = c(0, 0.5, 1, 2, 4, Inf), permutations = 100,
#           raw = FALSE, collector = FALSE, subset, ...)
#permutations:置换次数
#raw:FALSE返回整体置换之后的结果；TRUE返回每次置换的结果
#collector：根据行累积得到的diversity
#subset:保留的行子集
mod <- renyiaccum(BCI[i,]);mod
plot(mod, as.table=TRUE, col = c(1, 2, 2))
persp(mod)

#persp(x, theta = 220, col = heat.colors(100), zlim, ...)
#plot(x, what = c("Collector", "mean", "Qnt 0.025", "Qnt 0.975"),type = "l")

#theta：角度
#zlim:纵轴极限值

##Hill和Renyi的关系为：Hill=exp(Renyi)
#当Renyi中的alpha=0，Hill中的阶数q=0，计算的是物种数量；
#alpha=q=1,计算shannon;alpha=hill=2,计算simpson

#群落的Renyi diversity越高，群落的多样性越高。
##越来越感觉到Hill的强大了。。。
##还是得找个时间把vegan所有的功能好好看一下。