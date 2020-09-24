##2018.12.11
##安装包的方法
https://mp.weixin.qq.com/s?__biz=MzIyNzk1NjUxOA==&mid=2247484397&idx=1&sn=2785e8538162b8036d2b3d3e8ce8923c&chksm=e85801a4df2f88b234702b3b543cfe9d5424211c711ec81f3815814d1618f757550f7471314e&scene=21#wechat_redirect

##########1. CRAN
install.packages("xlsx", repos = 'https://mirror.lzu.edu.cn/CRAN/')
install.packages("xlsx", repos = 'http://cran.us.r-project.org')  # 或者换个镜像

#########2.Bioconductor
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("DESeq2")


#########3. 如何一次安装多个包
#### 安装未安装的包 
need.packs = c("survival", "pROC", "xlsx")  # 想要安装的R包
has = need.packs %in% rownames(installed.packages())  
has
# 判断是否已经安装过了
if(any(!has)) install.packages(need.packs[!has], repos = 'https://mirror.lzu.edu.cn/CRAN/')


#########4. 可以自行 判断是否已安装 并 安装还未安装的包 的包：pacman
#这个包要求R>3.5.0
# install.packages("installr")
# require(installr)
# updateR()

# install.packages("devtools")
# library(devtools)
# install_github("trinker/pacman")

if (!require("pacman")) install.packages("pacman", repos = 'https://CRAN.R-project.org/package=pacman')

library(pacman) 
options(warn = -1)    # 这里
p_load(ggplot2)  ##p_load = install.packages + library()


##安装github包
p_load(devtools) # 你需要先有个devtools
install_github(repo = "BioinformaticsFMRP/TCGAbiolinks")
#or:
p_load_gh("BioinformaticsFMRP/TCGAbiolinks")

##############直接通过包名就能从Github上安装相应的R包:githubinstall
#library(githubinstall)
p_load(githubinstall)
githubinstall("TCGAbiolinks")

# 更炫酷的功能
# 1、模糊匹配。例如，你不记得包名哪些字母要大小写：
githubinstall("tcgabiolinks")

#2、全局搜索。例如有很多叫cats的包：
githubinstall("cats")

#3、列举R包。例如hadley开发了哪些包，或者作者是不是叫hadly？
gh_suggest_username('hadly')
repos <- with(hadleyverse, paste(username, package_name, sep="/"))  
# 一次安装hadley所有的包
githubinstall(repos)

#4、查看Github上函数的源代码：
gh_show_source("gh_search_packages", repo = "githubinstall")

#############加载多个包
need.packages=c("vegan","ggplot2","ggtree")
lapply(need.packages,library,character.only=TRUE)


##修改默认加载的包
file.path(R.home(), "etc", "Rprofile.site")
rm(list=ls())
?p_load

##R语言 | R包安装大法安装特定版本的R包 
https://mp.weixin.qq.com/s?__biz=MzIyNzk1NjUxOA==&mid=2247484415&idx=1&sn=344d92a1d9159fb47343b84f640f6042&chksm=e85801b6df2f88a0676f4fe908d4d0fc7376cd3ae8e32787b361c0a44d5165b324ca5a74d089&scene=21#wechat_redirect

#1.最简便的方法是使用devtools包中的install_version函数：
require(devtools)
install_version("survminer", version = "0.4.2")


#2.虽然 install.packages 函数并不支持版本的选择，但是如果你知道R包的源文件的位置：https://cran.r-project.org/src/contrib/Archive 或者其他镜像，则可以直接通过 source 的方式安装：
packageurl = "https://cran.r-project.org/src/contrib/Archive/survminer/survminer_0.4.2.tar.gz"
install.packages(packageurl, repos = NULL, type = "source")