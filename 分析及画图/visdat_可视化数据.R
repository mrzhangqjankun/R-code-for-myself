##2019.5.30
#https://www.jianshu.com/p/81fb120218b4?utm_campaign=haruki&utm_content=note&utm_medium=reader_share&utm_source=qq
#R语言新神器visdat包

library(pacman)
p_load(visdat)

?visdat
# vis_dat（）可视化一个数据框，显示列的类别，并显示缺少的数据。
# vis_miss（）只显示缺失的数据，并允许对缺失进行聚类并重新排列。vis_miss（）类似于mi包中的missing.pattern.plot。
# vis_compare（）可视化相同维度的两个数据帧之间的差异
# vis_expect（）可视化数据中满足某些条件成立的数据
# vis_cor（）在一个漂亮的热图中可视化变量的相关性
# vis_guess（）可视化数据中各个数据的类别

head(airquality)
vis_dat(airquality)
vis_miss(airquality) #缺失值的百分比
vis_miss(airquality,cluster = TRUE)#cluster = TRUE来对缺失进行聚类：
vis_miss(airquality,
         sort_miss = TRUE)#列也可以按缺失最多的列排列

#当缺失率<0.1％时，或者缺少数据的数量非常少时
test_miss_df <- data.frame(x1 = 1:10000,
                           x2 = rep("A", 10000),
                           x3 = c(rep(1L, 9999), NA))
vis_miss(test_miss_df)

#如果数据不含有任何缺失数据：
vis_miss(mtcars)

#vis_compare（）可以显示两个相同大小的数据帧的差异。 我们来看一个例子。
set.seed(2019-04-03-1105)
chickwts_diff <- chickwts
chickwts_diff[sample(1:nrow(chickwts), 30),sample(1:ncol(chickwts), 2)] <- NA
vis_compare(chickwts_diff, chickwts) #两个数据框的差异被蓝色标出。

#vis_expect可视化数据中满足条件的值。 例如显示数据中大于25的值可以通过：
vis_expect(airquality, ~.x >= 25)

#探索一组字符串或可能的NA值，并可视化它们的位置，
bad_data <- data.frame(x = c(rnorm(100), rep("N/A", 10)),
                       y = c(rep("N A ", 30), rnorm(80)))
bad_data
vis_expect(bad_data, ~.x %in% c("N/A", "N A "))

#vis_cor是基于基础R中的cor函数，并且可以指示要计算哪个相关系数： “pearson”（默认），“kendall”或“spearman”之一。
vis_cor(airquality, cor_method = "spearman")

#可以使用na_action函数指定要对缺失数据执行的操作，该函数再次借用cor方法。例如：
vis_cor(airquality,
        na_action = "complete.obs")

#vis_guess（）函数
#用来猜测数据框中每个单元格是什么类型的数据。因此最好使用一些杂乱的数据进行说明：
messy_vector <- c(TRUE,
                  T,
                  "TRUE",
                  "T",
                  "01/01/01",
                  "01/01/2001",
                  NA,
                  NaN,
                  "NA",
                  "Na",
                  "na",
                  "10",
                  10,
                  "10.1",
                  10.1,
                  "abc",
                  "$%TG")

set.seed(2019-04-03-1106)
messy_df <- data.frame(var1 = messy_vector,
                       var2 = sample(messy_vector),
                       var3 = sample(messy_vector))
messy_df
vis_guess(messy_df)

#目前vis_guess非常缓慢。 当在超过1000行的数据上使用它时，请考虑这一点。

#绘制交互性图片
#您可以通过将它们包装在plotly :: ggplotly中来制作visdat中的图：
library(plotly);?plotly
ggplotly(vis_dat(airquality))
ggplotly(vis_miss(airquality))
ggplotly(vis_guess(airquality))

