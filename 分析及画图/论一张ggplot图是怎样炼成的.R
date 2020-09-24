##2019.8.9

##https://mp.weixin.qq.com/s/nXaoaN7u43gOLAvYPsiVQw

##论一张ggplot图是怎样炼成的 - 从小白都会画到逼格十足是如何一步步产生的？ 

library(tidyverse)
data <- read_csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv",
                 col_names=TRUE)
data

ggplot(data, aes(x, y)) + geom_tile(aes(fill=region),color="black")

#优化
library("ggsci")

(map_regions <- df_sorted %>%
    ggplot(aes(x = x, y = y, fill = region, color = region)) +
    geom_tile(color = "white") +
    scale_y_reverse() +
    scale_fill_uchicago(guide = F) +
    coord_equal() +
    theme(line = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent",
                                         color = "transparent"),
          panel.border = element_rect(color = "transparent"),
          strip.background = element_rect(color = "gray20"),
          axis.text = element_blank(),
          plot.margin = margin(0, 0, 0, 0)) +
    labs(x = NULL, y = NULL))


#构造一个比例
df_ratios = data %>%
            mutate(student_ratio=x*100/y)

#boxplot
ggplot(df_ratios, aes(region, student_ratio)) +
  geom_boxplot()

#排序
?fct_reorder #通过对另一个变量排序来重新排序因子级别
df_sorted <- df_ratios %>%
  mutate(region = fct_reorder(region, -student_ratio))

ggplot(df_sorted, aes(region, student_ratio)) +
  geom_boxplot()

#翻转一下坐标轴，再一点，数据起点从0开始是比较好的，这个可以通过设置y轴来实现。
ggplot(df_sorted, aes(region, student_ratio)) +
  geom_boxplot() +
  coord_flip() 
  #scale_y_continuous(limits = c(0, 90))

#theme_light，然后再改一点细节，包括改一下字体和大小，去掉网格线。另一点是改字体，你可以使用extrafont和showtext来加载字体
extrafont::loadfonts(device = "win")

#theme_set(theme_light(base_size = 15, base_family = "Poppins"))
theme_set(theme_light(base_size = 15))
                      
g <- ggplot(df_sorted, aes(region, student_ratio, color = region)) +
  coord_flip() +
  #scale_y_continuous(limits = c(0, 90), expand = c(0.005, 0.005)) +
  #scale_color_uchicago() +
  labs(x = NULL, y = "Student to teacher ratio") +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        #axis.text.x = element_text(family = "Roboto Mono", size = 10),
        axis.text.x = element_text(size = 10),
        panel.grid = element_blank())
g
?scale_color_uchicago

g + geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.15)

#抖动的大小可以通过width设置，而抖动是随机的，也就是说你每次跑出的图是不太一样的，为了让出图具有可重复性，我们可以通过set.seed来解决。
set.seed(123)
g + geom_jitter(size = 2, alpha = 0.25, width = 0.2)

#加统计量，比如均值、平均值等。
g +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", size = 5)

#统计一下，世界的平均水平，在图上用直线标注出来，这样谁引领世界，谁又在拖后腿，也就比较清楚了。
world_avg <- df_ratios %>%
  summarize(avg = mean(student_ratio, na.rm = T)) %>%
  pull(avg)

g +
  geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2)

#然后我们可以把每个大洲的平均值和世界的平均值用线条连接起来，这样差距更容易看。
student_ratio_region = df_sorted %>% 
                      group_by(region) %>% summarise(mean(student_ratio))

g +
  geom_segment(aes(x = region, xend = region,
                   y = world_avg, yend = student_ratio_region),
               size = 0.8) +
  geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", size = 5)


#加一些注释的信息：
(g_text <- g +
   geom_segment(aes(x = region, xend = region,
                    y = world_avg, yend = student_ratio_region),
                size = 0.8) +
   geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
   stat_summary(fun.y = mean, geom = "point", size = 5) +
   geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
   annotate("text", x = 6.3, y = 35, family = "Poppins", size = 2.7, color = "gray20",
            label = glue::glue("Worldwide average:\n{round(world_avg, 1)} students per teacher")) +
   annotate("text", x = 3.5, y = 10, family = "Poppins", size = 2.7, color = "gray20",
            label = "Continental average") +
   annotate("text", x = 1.7, y = 11, family = "Poppins", size = 2.7, color = "gray20",
            label = "Countries per continent") +
   annotate("text", x = 1.9, y = 64, family = "Poppins", size = 2.7, color = "gray20",
            label = "The Central African Republic has by far\nthe most students per teacher"))

#然后我们可以用geom_curve把一些弯的箭头来把文本指向图中的一些元素，让文本解说有针对性。
arrows <- tibble(
  x1 = c(6.2, 3.5, 1.7, 1.7, 1.9),
  x2 = c(5.6, 4, 1.9, 2.9, 1.1),
  y1 = c(35, 10, 11, 11, 73),
  y2 = c(world_avg, 19.4, 14.1, 12, 83.4)
)

g_text + geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
                    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
                    color = "gray20", curvature = -0.3)


(g_final <- g_arrows +
    scale_y_continuous(limits = c(0, 90), expand = c(0.005, 0.005),
                       breaks = c(1, seq(20, 80, by = 20))) +
    labs(caption = "Data: UNESCO Institute for Statistics") +
    theme(plot.caption = element_text(size = 9, color = "gray50")))


g_final +
  annotation_custom(ggplotGrob(map_regions), xmin = 2.5, xmax = 7.5, ymin = 55, ymax = 85)


####sinaplot  抖动图。
#点的抖动不再是瞎抖，而是按照密度分布来抖，这样给出更多的信息，散点组成的形状，直接就像小提琴图。