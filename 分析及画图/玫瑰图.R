##2020.10.11

##https://mp.weixin.qq.com/s/fDVL_YGm0ciYimGfRO6M6A

##R-南丁格尔玫瑰图

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages(tidyverse)
}
library(tidyverse)

dat <-
  data.frame(id = 1:26,
             height = c(seq(100, 550, 20),
                        seq(650, 700, 20))) %>%
  mutate(
    label = case_when(
      id <= 13 ~ paste0(height, "例 ", LETTERS[id], "国"),
      id <= 21 ~ paste0(height, "例\n", LETTERS[id], "国"),
      T ~ paste0(LETTERS[id], "国\n", height, "例")
    )
  )

#开始画底图 + 同心圆的bar plot
p1 <-
  ggplot(data = dat, aes(x = id, y = height, label = label)) +
  geom_col(aes(fill = id), width = 1, size = 0) +
  geom_col(   #外部的同心圆bar plot，白色填充，同时设置alpha属性。
    aes(y = 40),
    fill = "white",
    width = 1,
    alpha = 0.2,
    size = 0
  ) +
  geom_col(  #内部的同心圆bar plot，同理操作，外部和内部的alpha设置一样大，这样一重叠，内部的实际显示就会变成0.4。
    aes(y = 20),
    fill = "white",
    width = 1,
    alpha = 0.2,
    size = 0
  )
p1

p2 <-
  p1 +
  coord_polar() +
  theme_void() +
  scale_y_continuous(limits = c(-60, 701))
p2

#为每一条bar添加label。
p3 <-
  p2 +
  geom_text(
    data = . %>% filter(id <= 13),
    nudge_y = 80,   #nudge_*属性来调整3种label的相对位置，实现label分别位于外面和里面。
    angle = 95 - 180 * c(1:13) / 13,  #设置angle来根据bar的位置来动态调整label的角度，以实现旋转
    fontface = "bold"
  ) +
  geom_text(
    data = . %>% filter(between(id, 14, 21)),
    nudge_y = -55,
    nudge_x = -0.1,
    color = "white",  #将内部的label设置为白色勾勒，增加文字的可读性，同时对所有的label，我们都对其加粗以达到图片的效果。
    fontface = "bold"
  ) +
  geom_text(
    data = . %>% filter(id >= 22),
    nudge_y = -50,
    color = "white",
    angle = 80 - 75 * c(1:5)/5,
    fontface = "bold"
  )
p3

p4 <-
  p3 +
  scale_fill_gradientn(   #载入渐变色板。
    colors = c("#54778f", "#4EB043", "#E69D2A", "#DD4714", "#A61650"),
    guide = F  #删除fill图例
  )
p4
