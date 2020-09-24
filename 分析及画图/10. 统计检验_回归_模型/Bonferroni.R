###Bonferroni and FDR校正
##R p.adjust文档https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
?p.adjust

##原理
##http://blog.sina.com.cn/s/blog_4af3f0d20100bzx9.html
##https://wenku.baidu.com/view/8769ef556bd97f192279e9aa.html

##Example
require(graphics)

set.seed(123)
x <- rnorm(50, mean = c(rep(0, 25), rep(3, 25)));x
p <- 2*pnorm(sort(-abs(x)));p

round(p, 3)
round(p.adjust(p), 3)
round(p.adjust(p, "BH"), 3)

## or all of them at once (dropping the "fdr" alias):
p.adjust.M <- p.adjust.methods[p.adjust.methods != "holm"]
p.adj    <- sapply(p.adjust.M, function(meth) p.adjust(p, meth))
p.adj.60 <- sapply(p.adjust.M, function(meth) p.adjust(p, meth, n = 60))
stopifnot(identical(p.adj[,"none"], p), p.adj <= p.adj.60)
round(p.adj, 3)
## or a bit nicer:
noquote(apply(p.adj, 2, format.pval, digits = 3))


## and a graphic:
matplot(p, p.adj, ylab="p.adjust(p, meth)", type = "l", asp = 1, lty = 1:6,
        main = "P-value adjustments")
legend(0.7, 0.6, p.adjust.M, col = 1:6, lty = 1:6)
?matplot
      


?fisher.test()  ##女士品茶例子
