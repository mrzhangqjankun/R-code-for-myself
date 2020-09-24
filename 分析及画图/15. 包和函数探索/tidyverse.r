##2019.3.12
#tidyverse学习
#https://www.jianshu.com/p/f3c21a5ad10a
#https://blog.csdn.net/c1z2w3456789/article/details/79953079

library(tidyverse)


# tidyverse与其他包的冲突
tidyverse_conflicts()
# 列出所有tidyverse的依赖包
tidyverse_deps()
#获取tidyverse的logo
tidyverse_logo()
# 列出所有tidyverse包
tidyverse_packages()
# 更新tidyverse包
tidyverse_update()

####################载入数据
library(datasets)
#install.packages("gapminder")
library(gapminder)
attach(iris)

######################dplyr
?dplyr
#filter()过滤
iris %>% 
  filter(Species == "virginica") # 指定满足的行
iris %>% 
  filter(Species == "virginica", Sepal.Length > 6) # 多个条件用,分隔

#arrange()排序。默认升序
iris %>% 
  arrange(Sepal.Length)
iris %>% 
  arrange(desc(Sepal.Length)) # 降序

by_cyl <- mtcars %>% group_by(cyl)     #按照cyl分组
by_cyl %>% arrange(desc(wt))     #无视之前的分组
by_cyl %>% arrange(desc(wt), .by_group = TRUE)     #在之前的分组内再排序

#mutate()可以更新或者新增数据框一列。
#transmute() 会只保留新的列，原始列都删除
iris %>% 
  mutate(Sepal.Length = Sepal.Length * 10) # 将该列数值乘十。由于名字和原来的一样，原来的这一列没了。

iris %>% 
  mutate(SLMn = Sepal.Length * 10) # 创建新的一列

#mutate_all()，mutate_if() 和mutate_at()三个函数可以划定函数和变量范围来生成新变量。
iris %>% 
  as_tibble() %>% 
    mutate_if(is.factor, as.character)  #如果是因子，则转为字符 

#summarize()函数可以让我们将很多变量汇总为单个的数据点。
iris %>% 
  summarize(medianSL = median(Sepal.Length))
##   medianSL
## 1      5.8
iris %>% 
  filter(Species == "virginica") %>% 
  summarize(medianSL=median(Sepal.Length))
##   medianSL
## 1      6.5

#还可以一次性汇总多个变量
iris %>% 
  filter(Species == "virginica") %>% 
  summarize(medianSL = median(Sepal.Length),
            maxSL = max(Sepal.Length))
##   medianSL maxSL
## 1      6.5   7.9

mtcars %>%
  group_by(cyl) %>%
  summarise(sd = sd(disp))

#group_by()可以让我们安装指定的组别进行汇总数据，而不是针对整个数据框
iris %>% 
  group_by(Species) %>% 
  summarize(medianSL = median(Sepal.Length),
            maxSL = max(Sepal.Length))

iris %>% 
  filter(Sepal.Length>6) %>% 
  group_by(Species) %>% 
  summarize(medianPL = median(Petal.Length), 
            maxPL = max(Petal.Length))

#summarise_at函数允许我们通过名称选择多个变量。
summarize_at(mtcars,vars(mpg,disp),funs(n(),mean,median)) 

#summarise_if函数允许您有条件地总结。
summarize_if(mtcars,is.numeric,funs(n(),mean,median)) 

#select() 选择特定的列
iris %>% 
  select(Sepal.Length)
iris %>% 
  select(-Sepal.Length) #不含这一列

iris %>% 
  select(iris, contains("etal", ignore.case = TRUE))
iris %>% 
  select(iris, matches(".t."))
#matches()函数可以用正则表达式匹配变量名，而contains()只能匹配字符串
#starts_with（）函数用于选择以字母开头的变量。
select(mtcars, starts_with('Y'))
#还有ends_with,num_range(),one_of(),everything()

#slice()函数通过行数来选取子集，前加“-”表示除去该行不选。支持组内操作。
slice(mtcars, 1L)
slice(mtcars, n())
slice(mtcars, 5:n())

by_cyl <- group_by(mtcars, cyl)
slice(by_cyl, 1:2)

#sample_n函数从数据框（或表）中随机选择行。 函数的第二个参数告诉R要选择的行数。
?sample
##注意，我平时用的sample来自base包，默认选取的是列。
sample_n(mtcars, 3)
#sample_frac函数随机返回N％的行
sample_frac(mtcars, 0.1)

#distinct函数用于消除重复行
distinct(mtcars)
distinct(mtcars,cyl,.keep_all = TRUE) #单变量
distinct(mtcars,cyl,am,.keep_all = TRUE) #多变量
#keep_all函数用于保留输出数据框中的所有其他变量。

#rename函数可用于重命名变量。
rename(mtcars,mpggg=mpg)  #新明在前，旧名在后

#%in%运算符可用于选择多个项目。
filter(mtcars,cyl %in% c("4","6"))

##unite ; separate
#https://mp.weixin.qq.com/s/LH79XrJr8B0ffTUC5A8Nlg


#######################ggplot2
by_species <- iris %>%  
  filter(Sepal.Length > 6) %>% 
  group_by(Species) %>% 
  summarize(medianPL=median(Petal.Length))

ggplot(by_species, aes(x = Species, y=medianPL)) + 
  geom_col()

#####################magrittr 
?magrittr #这个包提供管道操作 %>%










