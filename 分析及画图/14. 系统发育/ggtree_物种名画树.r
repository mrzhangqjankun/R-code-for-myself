##2020.1.16

##https://mp.weixin.qq.com/s/BJjy28aru_l40v-jdPnSPA

##当生物女博士遇到小学二年级语文作业

##输入物种学名，画进化树。

remotes::install_github("ropensci/rotl")
devtools::install_github("GuangchuangYu/ggimage")
devtools::install_github("GuangchuangYu/ggtree")


Sys.setenv(LANGUAGE = "en") #显示英文报错信息

library(rotl)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggtree)
library(ggimage)

options(stringsAsFactors = FALSE) #禁止chr转成fac

# 输入文件
# easy_input.csv，物种名。至少包含学名一列。
# 
# 第一列，根据原题中文名，修改到种，跟学名对应
# 第二列，英文名，用google翻译获得
# 第三列，学名，手动查询获得
# 第四列，手动找出相似小动物的emoji，把emoji code写到这里。没找到蜻蜓和大雁的emoji，用bee和eagle代替


oriname <- c("老虎", "狮子", "鲫鱼", "鸽子", "鸭子", "鸡", "蜻蜓", "鲨鱼", "菜粉蝶", "大雁")
englishname <- c("tiger", "lion", "fish", "dove", "duck", "rooster", "dragonfly", "shark", "butterfly", "wild goose")
sci <- c("Panthera tigris", "Panthera leo", "Carassius auratus auratus", "Columba livia domestica", "Anas platyrhynchos", "Gallus gallus domesticus", "Anax parthenope", "Chondrichthyes", "Pieris rapae", "Anser anser")
emojiname <- c("tiger", "lion", "fish", "dove", "duck", "rooster", "bee", "shark", "butterfly", "eagle")
apes <- data.frame(oriname = oriname,
                   englishname = englishname,
                   sci = sci,
                   emojiname = emojiname)
write.csv(apes, "easy_input.csv", quote = F, row.names = F)

## 读入物种名文件
apes <- read.csv("easy_input.csv")

# 用学名查询ott_id，用于建树
resolved_names <- tnrs_match_names(apes$sci);?tnrs_match_names

#构建进化树
#用Open Tree of Life，参考https://yulab-smu.github.io/treedata-book/related-tools.html#rtol

tr <- tol_induced_subtree(ott_ids = ott_id(resolved_names))
#开始画图
#用ggtree画图，参考https://yulab-smu.github.io/treedata-book/chapter8.html

#基本款
ggtree(tr) +
  geom_tiplab(x = 7.3) + # 可左右移动位置
  xlim(NA, 12) # 树不要太长


#只保留学名
ottsci <- resolved_names$unique_name
names(ottsci) <- resolved_names$ott_id

ott <- str_split_fixed(tr$tip.label, "_ott",2)[,2]
# 检查一下，tree里的ott_id跟输入文件是否一致
# 如果输入的不是种的学名，会不一致
setdiff(resolved_names$ott_id, ott)
## integer(0)
d <- data.frame(label = tr$tip.label, # 树里的名字
                sci = ottsci[ott]) # 要替换的名字，要跟树里的名字一一对应

ggtree(tr) %<+% d + # 添加注释
  geom_tiplab(aes(label = sci),
              x = 7.3) + # 可左右移动位置
  xlim(NA, 12) # 树不要太长


#用英文名做label
otteng <- apes$englishname
names(otteng) <- resolved_names$ott_id

ott <- str_split_fixed(tr$tip.label, "_ott",2)[,2]
setdiff(resolved_names$ott_id, ott)
## integer(0)
d <- data.frame(label = tr$tip.label,
                eng = otteng[ott])

ggtree(tr) %<+% d + # 添加注释
  geom_tiplab(aes(label = eng),
              x = 7.3) + # 可左右移动位置
  xlim(NA, 12) # 树不要太长


# 剪影 - phylopic
# 加入PhyloPic剪影，跟taxon对应，让paper更美～
# 
# http://phylopic.org/

# 用学名获取PhyloPic的id
#phylopic_id <- phylopic_uid(resolved_names$unique_name)
#write.csv(phylopic_id, "phylopic_id.csv", quote = F)

# 把PhyloPic的id、tree里的id和学名对应起来
phylopic_id <- read.csv("phylopic_id.csv", row.names = 1)
phylopic_resolved_names <- merge(phylopic_id, resolved_names, by.x = "name", by.y = "unique_name")

ottname <- tr$tip.label
names(ottname) <- str_split_fixed(tr$tip.label, "_ott",2)[,2]

phylopic_resolved_names$label <- ottname[as.character(phylopic_resolved_names$ott_id)]

d <- select(phylopic_resolved_names, c("label", "name", "uid"))

#下面这步取决于网络

ggtree(tr) %<+% d +
  geom_tiplab(aes(image=uid), geom="phylopic") +
  geom_tiplab(aes(label=name),
              x = 7.8) +
  xlim(NA, 12)

##萌版 - emoji
ottemoji <- apes$emojiname
names(ottemoji) <- resolved_names$ott_id

ott <- str_split_fixed(tr$tip.label, "_ott",2)[,2]
setdiff(resolved_names$ott_id, ott)
## integer(0)
# 把树里的ID替换成emoji
trr <- tr
trr$tip.label <- ottemoji[ott]

d <- data.frame(label = ottemoji[ott],
                sci = ottsci[ott])

ggtree(trr) %<+% d + # 添加注释
  geom_tiplab(parse="emoji", # 解析emoji code，画小动物图案
              size=10, # 把小动物调整到合适大小
              vjust=.25, # 可上下移动位置
              hjust=-.1) + # 可左右移动位置
  geom_tiplab(aes(label = sci),
              x = 8) + # 可左右移动位置
  xlim(NA, 12) # 树不要太长


#掰弯
ggtree(trr, layout = "circular") %<+% d + # 添加注释
  geom_tiplab(parse="emoji", # 解析emoji code，画小动物图案
              size=15, # 把小动物调整到合适大小
              vjust=.25, # 可上下移动位置
              hjust=-.1) + # 可左右移动位置
  xlim(NA, 10)


#拉直
ggtree(trr, layout = "slanted") %<+% d + # 添加注释
  geom_tiplab(parse="emoji", # 解析emoji code，画小动物图案
              size=10, # 把小动物调整到合适大小
              vjust=.25, # 可上下移动位置
              hjust=-.1) + # 可左右移动位置
  geom_tiplab(aes(label = sci), # 写学名
              x = 8) + # 可左右移动位置
  xlim(NA, 12) # 树不要太长


#转个圈儿
ggtree(trr, layout = "radial") %<+% d + # 添加注释
  geom_tiplab(parse="emoji", # 解析emoji code，画小动物图案
              size=15, # 把小动物调整到合适大小
              vjust=.25, # 可上下移动位置
              hjust=-.1) + # 可左右移动位置
  xlim(NA, 10)

# 文中说到英文名由google translate获得，我觉得可以用youdao的api，写点小代码自动翻译。然后学名也不用一个个去查，用NCBI的api，去taxonomy数据库爬一下就有了。
# 
# 也就是说，其实你输入几个中文的物种名，然后程序可以翻英文，再转学名，然后就可以用学名去拿到进化树，再衔接ggtree可视化，包括phylopic这些，都是全自动的。你唯一的输入可以做到只需要几个中文名。
# 
# 试想一下，你再做成一个shiny app，输入几个中文名，点一下鼠标图就出来，再按不同的选项，出不同风格的图，和小朋友交流起来毫无压力。