##2019.12.12

##base plot出的图，富有时代感！这个包，让你穿越回9102！

##https://mp.weixin.qq.com/s/qRX3q4krMu6l_UCT8O8ldQ

#这里要推荐的是base plot的主题包，prettyB (https://github.com/jumpingrivers/prettyB)，这是一个非常好的尝试，可以让大量现有的画图函数出图效果更加现代化，减少illustrator修图的麻烦。

remotes::install_github("jumpingrivers/prettyB")

library("prettyB")
?prettyB

#差别只在于plot换成plot_p，参数是一模一样的。
op = par(mfrow = c(1, 2))
plot(iris$Sepal.Length, iris$Sepal.Width)
plot_p(iris$Sepal.Length, iris$Sepal.Width)

#柱状图
z = rt(100, 4)
hist(z, main = "The t-distribution")
hist_p(z, main = "The t-distribution")

barplot(VADeaths, main = "Death Rates in Virginia")
barplot_p(VADeaths, main = "Death Rates in Virginia")
