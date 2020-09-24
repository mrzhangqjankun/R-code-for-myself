##2019.11.25

##https://mp.weixin.qq.com/s/A1phOKPQl3T0mV1_u2GlrQ

##

library(knitr);?knitr


kable(head(iris), format = "latex")
kable(head(iris), format = "html")
kable(head(iris), format = "latex", caption = "Title of the table")
kable(head(iris), format = "html", caption = "Title of the table")
# use the booktabs package
kable(mtcars, format = "latex", booktabs = TRUE)
# use the longtable package
kable(matrix(1000, ncol = 5), format = "latex", digits = 2, longtable = TRUE)
# change LaTeX default table environment
kable(head(iris), format = "latex", caption = "My table", table.envir = "table*")
# add some table attributes
kable(head(iris), format = "html", table.attr = "id=\"mytable\"")
# reST output
kable(head(mtcars), format = "rst")
# no row names
kable(head(mtcars), format = "rst", row.names = FALSE)
# R Markdown/Github Markdown tables
kable(head(mtcars[, 1:5]), format = "markdown")
# no inner padding
kable(head(mtcars), format = "markdown", padding = 0)
# more padding
kable(head(mtcars), format = "markdown", padding = 2)
# Pandoc tables
kable(head(mtcars), format = "pandoc", caption = "Title of the table")
# format numbers using , as decimal point, and ' as thousands separator
x = as.data.frame(matrix(rnorm(60, 1e+06, 10000), 10))
kable(x, format.args = list(decimal.mark = ",", big.mark = "'"))
# save the value
x = kable(mtcars, format = "html")
cat(x, sep = "\n")
# can also set options(knitr.table.format = 'html') so that the output is HTML


############修改字体
# 首次需要安装win字体并导入
#install.packages("extrafont")
# library(extrafont)

loadfonts(device="win")
fonts()

####这种方式将所有字体调整为新罗马字体####
windowsFonts(myFont = windowsFont("Times New Roman"))
p1 + theme_gray(base_size = 20, base_family = "myFont")


#散点连接起来
p = p +  geom_polygon()


#ggsave保存中文正确用法
#install.packages('Cairo')
library("Cairo")
#ggsave("geo_Fus_wilt1.pdf", p1, width = 12, height =8 )

ggsave("geo_Fus_wilt.pdf", p1, width = 12, height =8 , device = cairo_pdf, family = "Song")


#查看目录下文件
list.files(path, full.names = TRUE)


#新建文件夹
filtpath <- file.path(path, "a2_filtered")
#方便我们建立文件夹
dir.create(dirName)


### 查看包的安装地址
.libPaths()#查看包的加载地址
.libPaths("C:/Program Files/R/R-3.5.1/library")#修改到你包的安装地址

#加载多个包
pkgs <- c("phyloseq", "structSSI", "dplyr", "reshape2",
          "ggplot2", "DESeq2")
sapply(pkgs, require, character = TRUE)

####查看默认载入的包########
getOption("defaultPackages")#：查看启动R时自动载入的包。


#按照特定字符拆分字符串
#basename提取路径下的文件名， strsplit使用制定分隔符拆分字符串，sapply提取制定字符串
sample.names <- sapply(strsplit(basename(fnFs), ".R1."), `[`, 1)
sample.names