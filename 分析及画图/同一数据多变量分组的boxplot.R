##2018.8.16

##同一数据多变量分组的boxplot

##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247485019&idx=1&sn=7ec417dc7a35b259526f7afb622213e2&chksm=ec43b71cdb343e0a9bfff7d9330c3ec58754eb6f25309389327e8d774105ef0a4b16ba66c85d&mpshare=1&scene=1&srcid=0809ujH8Mjjj9QuDwZFeUWOY&pass_ticket=ThN4hhi7E1DQ3YJfJqqhU8fxbj7J7Yl7h9NOijKuR3Y2pUTHP%2BgpXRLvTEVt0ZTc#rd

set.seed(2017-10-30)
d <- data.frame(riskScore = abs(rnorm(100)),
                BMI = sample(1:2, 100, replace=T),
                stage = sample(1:2, 100, replace=T),
                age = sample(1:2, 100, replace=T),
                gender = sample(1:2, 100, replace=T))

head(d)

#下面我将定义一个myboxplot，它画boxplot，不带x和y轴，然后加x轴不带labels，再额外打labels，
#因为提问者的图中还有pvalue，我顺道把pvalue也整合进这个myboxplot里去，可以用pvalue=NULL来关掉这个功能。

myboxplot <- function(x, data, col = NULL, xlab, pvalue="auto") {
  boxplot(x, data, axes = FALSE, col = col)
  axis(1, at = 1:2, labels =FALSE)
  text(1:2, y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
       srt=60, xpd=T, adj=1, labels = xlab)
  if (pvalue == "auto") {
    pvalue <- round(t.test(x, data=data)$p.value, 3)
  }
  
  if (!is.null(pvalue)) {
    plab <- paste("p =", pvalue)
    text(1.5, y = par()$usr[4]*1.05, xpd=T, label=plab, col=col)
  }
}

#万事具备，有函数，有数据，我们先初始化画4个column，然后你只要调用myboxplot，
#分4次画4个图，就大功告成了，第一个图的时候，把y轴给加上。
layout(t(1:4))
par(oma=c(2, 4, 4, 0), mar=c(5,2,1,1), cex=1)

myboxplot(riskScore~age, data=d, col='red', xlab=c("age < 60", "age > 60"))
axis(2, las=1)
myboxplot(riskScore~gender, data=d, col='green', xlab=c("Male", "Female"))
myboxplot(riskScore~stage, data=d, col='blue', xlab=c("pStage 1-2", "pStage 1-2"))
myboxplot(riskScore~BMI, data=d, col='cyan', xlab=c("BMI < 24", "BMI > 24"))

#假如我们想要用ggplot2来画，该怎么搞？首先毫无意外，要把数据整理成ggplot2喜欢的样子，
#我定义一个convert函数专门来搞这个数据：
library(magrittr)
convert <- function(d) {
  lapply(2:ncol(d), function(i) {
    d2 <- d[, c(1,i)]
    d2$type = colnames(d2)[2]
    colnames(d2) = c("riskScore", "category", "type")
    return(d2)
  }) %>% do.call('rbind', .)
}

dd <- convert(d)

head(dd)

library(ggplot2)
ggplot(dd, aes(type, riskScore, fill=factor(category))) + geom_boxplot()

#当然可以通过分面来补救：
ggplot(dd, aes(type, riskScore, group=factor(category), fill=type)) +
  geom_boxplot() + facet_grid(.~type, scales = "free_x")

ggplot(dd, aes(type, riskScore, color=factor(category), fill=type)) + geom_boxplot() +
  scale_color_manual(values=rep('black',2), guide=FALSE)

bmi = c("BMI < 24", "BMI > 24")
stage = c("pStage 1-2", "pStage 3-4")    
age = c("age < 60", "age > 60")
gender = c("Male", "Female")
d$BMI = bmi[d$BMI]
d$stage = stage[d$stage]
d$age = age[d$age]
d$gender = gender[d$gender]
dd = convert(d)
dd$category = factor(dd$category, levels=c(age, gender, stage, bmi))    
p1 = ggplot(dd, aes(category, riskScore, fill=type)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle=60, vjust=1, hjust=1))
p2 = p1 + facet_grid(.~type, scales="free_x")
cowplot::plot_grid(p1, p2, ncol=2)    
