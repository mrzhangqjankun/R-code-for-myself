library(tidyverse)
library(magrittr)
library(reshape2)
library(ggplot2)
df=data.frame(
  Phylum=c("Proteobacteria"
           ,"Chloroflexi"
           ,"Unclassified"
           ,"Actinobacteria"
           ,"Acidobacteria"
           ,"Planctomycetes"
           ,"Bacteroidetes"
           ,"Thaumarchaeota"
           ,"Firmicutes"
           ,"Gemmatimonadetes"
           ,"Verrucomicrobia"
           ,"Spirochaetes"
           ,"Nitrospirae","Latescibacteria","candidate division WPS-1","Armatimonadetes"),
 CK=c(74,50,47,29,22,10,9,7,6,4,2,1,1,1,1,1),
 Warming=c(50,17,35,11,7,1,12,5,4,3,1,0,1,0,0,0)
)


# 计算连线起始点Y轴坐标，即累计丰度的值
link_dat = df %>% 
  arrange(by=desc(Phylum)) %>% 
  mutate(CK=cumsum(CK), Warming=cumsum(Warming)) 

# 数据格式转换，宽表格转换为ggplot2使用的长表格
df.long <- df %>% gather(group, node_number, -Phylum)
## 或者使用reshape2的melt函数
## df.long <- reshape2::melt(df, value.name='node number', variable.name='group') 

col=c("#ffb6b9", "#fae3d9", "#bbded6","#8ac6d1", "#f4f0e6", "#d9d9f3", "#ceefe4", "#9dd3a8", "#FF82A9", "#7F95D1","#F38181","#eec60a","#f07810","#3dd9d6","#add9d8","#C08497")

df.long = as.data.frame(df.long)
# 绘图，堆叠柱状图+组间连线  #geom_bar 可以选择“position=dodge/fill/stack”表示不同柱形图

p =   ggplot(df.long, aes(x=group, y=node_number, fill=Phylum)) + 
  geom_bar(stat = "identity", width=0.5, col='black')  + 
  geom_segment(data=link_dat, aes(x=1.25, xend=1.75, y=CK, yend=Warming))
p

p = p + scale_fill_manual(values = col, limits = c("Proteobacteria" ,"Chloroflexi" ,"Unclassified","Actinobacteria","Acidobacteria"
                                                                     ,"Planctomycetes","Bacteroidetes" ,"Thaumarchaeota" ,"Firmicutes","Gemmatimonadetes"
                                                                     ,"Verrucomicrobia" ,"Spirochaetes","Nitrospirae","Latescbacteria","candidate division WPS-1","Armatimonadetes"))
               
p
