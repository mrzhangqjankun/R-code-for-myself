##2018.6.22

##ggalluvial：冲击图展示组间变化、时间序列和复杂多属性alluvial diagram 

##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247485074&idx=1&sn=4d4456edf0be14d2b08e335f9641ca39&scene=21#wechat_redirect

# 国内用户推荐清华镜像站
site="https://mirrors.tuna.tsinghua.edu.cn/CRAN"
# 安装稳定版(推荐)
install.packages("ggalluvial", repo=site)

# 安装开发版(连github不稳定有时间下载失败，多试几次可以成功)
devtools::install_github("corybrunson/ggalluvial", build_vignettes = TRUE)

# 安装新功能最优版
devtools::install_github("corybrunson/ggalluvial", ref = "optimization")

# 查看教程
vignette(topic = "ggalluvial", package = "ggalluvial")

###1.实例
#基于泰坦尼克事件人员统计绘制性别与舱位和年龄的关系。
# 加载包
library(ggalluvial)

# 转换内部数据为数据框，宽表格模式
titanic_wide <- data.frame(Titanic)

# 显示数据格式
head(titanic_wide)
#>   Class    Sex   Age Survived Freq
#> 1   1st   Male Child       No    0
#> 2   2nd   Male Child       No    0
#> 3   3rd   Male Child       No   35
#> 4  Crew   Male Child       No    0
#> 5   1st Female Child       No    0
#> 6   2nd Female Child       No    0

# 绘制性别与舱位和年龄的关系
ggplot(data = titanic_wide,
       aes(axis1 = Class, axis2 = Sex, axis3 = Age,
           weight = Freq)) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.1, .05)) +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")
#具体参考说明：data设置数据源，axis设置显示的柱，weight为数值，
# geom_alluvium为冲击图组间面积连接并按生存率比填充分组，
# geom_stratum()每种有柱状图，geom_text()显示柱状图中标签，
# theme_minimal()主题样式的一种，ggtitle()设置图标题

#我们发现上图居然画的是宽表格模式下的数据，而通常ggplot2处理都是长表格模式，如何转换呢？
#to_loades转换为长表格
# 长表格模式，to_loades多组组合，会生成alluvium和stratum列。主分组位于命名的key列中
titanic_long <- to_lodes(data.frame(Titanic),
                         key = "Demographic",
                         axes = 1:3)
head(titanic_long)
ggplot(data = titanic_long,
       aes(x = Demographic, stratum = stratum, alluvium = alluvium,
           weight = Freq, label = stratum)) +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() + geom_text(stat = "stratum") +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")


###2. 输入数据格式
#定义一种Alluvial宽表格
# 显示数据格式
head(as.data.frame(UCBAdmissions), n = 12)
##       Admit Gender Dept Freq
## 1  Admitted   Male    A  512
## 2  Rejected   Male    A  313
## 3  Admitted Female    A   89
## 4  Rejected Female    A   19
## 5  Admitted   Male    B  353
## 6  Rejected   Male    B  207
## 7  Admitted Female    B   17
## 8  Rejected Female    B    8
## 9  Admitted   Male    C  120
## 10 Rejected   Male    C  205
## 11 Admitted Female    C  202
## 12 Rejected Female    C  391

# 判断数据格式
is_alluvial(as.data.frame(UCBAdmissions), logical = FALSE, silent = TRUE)
## [1] "alluvia"
#查看性别与专业间关系，并按录取情况分组
ggplot(as.data.frame(UCBAdmissions),
       aes(weight = Freq, axis1 = Gender, axis2 = Dept)) +
  geom_alluvium(aes(fill = Admit), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c("Gender", "Dept")) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")

####3. 三类型间关系，按重点着色
#Titanic按生存，性别，舱位分类查看关系，并按舱位填充色
ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Survived, axis2 = Sex, axis3 = Class)) +
  geom_alluvium(aes(fill = Class),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
  coord_flip() +
  ggtitle("Titanic survival by class and sex")

###5. 绘制非等高冲击图
#以各国难民数据为例，观察多国难民数量随时间变化
data(Refugees, package = "alluvial") ##install.packages("alluvial")
country_regions <- c(
  Afghanistan = "Middle East",
  Burundi = "Central Africa",
  `Congo DRC` = "Central Africa",
  Iraq = "Middle East",
  Myanmar = "Southeast Asia",
  Palestine = "Middle East",
  Somalia = "Horn of Africa",
  Sudan = "Central Africa",
  Syria = "Middle East",
  Vietnam = "Southeast Asia"
)
Refugees$region <- country_regions[Refugees$country]
ggplot(data = Refugees,
       aes(x = year, weight = refugees, alluvium = country)) +
  geom_alluvium(aes(fill = country, colour = country),
                alpha = .75, decreasing = FALSE) +
  scale_x_continuous(breaks = seq(2003, 2013, 2)) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0)) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  scale_color_brewer(type = "qual", palette = "Set3") +
  facet_wrap(~ region, scales = "fixed") +
  ggtitle("refugee volume by country and region of origin")

###6. 等高非等量关系
#不同学期学生学习科目的变化
data(majors)
majors$curriculum <- as.factor(majors$curriculum)
ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft",
            color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("student curricula across several semesters")

####8. 分类学门水平相对丰度实战
# 实战1. 组间丰度变化 

# 编写测试数据
df=data.frame(
  Phylum=c("Ruminococcaceae","Bacteroidaceae","Eubacteriaceae","Lachnospiraceae","Porphyromonadaceae"),
  GroupA=c(37.7397,31.34317,222.08827,5.08956,3.7393),
  GroupB=c(113.2191,94.02951,66.26481,15.26868,11.2179),
  GroupC=c(123.2191,94.02951,46.26481,35.26868,1.2179),
  GroupD=c(37.7397,31.34317,222.08827,5.08956,3.7393)
)

# 数据转换长表格

library(reshape2)

melt_df = melt(df)

# 绘制分组对应的分类学，有点像circos
ggplot(data = melt_df,
       aes(axis1 = Phylum, axis2 = variable,
           weight = value)) +
  scale_x_discrete(limits = c("Phylum", "variable"), expand = c(.2, .05)) +
  geom_alluvium(aes(fill = Phylum)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("Phlyum abundance in each group")


##绘制分组对应的分类学，有点像circos
# 组间各丰度变化 
ggplot(data = melt_df,
       aes(x = variable, weight = value, alluvium = Phylum)) +
  geom_alluvium(aes(fill = Phylum, colour = Phylum, colour = Phylum),
                alpha = .75, decreasing = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -30, hjust = 0)) +
  ggtitle("Phylum change among groups")
