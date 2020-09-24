##2019.3.12
#ggupset -- ggplot2版本的upset plot
#https://mp.weixin.qq.com/s/N2Qwd8NAkmXEqBSCyNLaOw


install.packages("ggupset")
library(ggupset)
??ggupset

library(tidyverse)

tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>% 
  ggplot(aes(x=Genres)) +
  geom_bar() +
  scale_x_upset(n_intersections = 20)
?distinct  #挑出不同的行，类似于unique.data.frame()
?scale_x_upset
#它的做法是把x-axis给改了，不过我发现还有一个不太兼容的地方，
#你不能对输出使用theme，像上面的图，你如果+theme_bw()就会报错。
#但好在你可以在scale_x_upset前面加theme

tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
  geom_bar() +
  theme_bw() + #加在最后则不行
  scale_x_upset(n_intersections = 20)

##为了控制x轴的散点图的格式，作者又提供了theme_combmatrix来控制下面那部分。

tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
  geom_bar() +
  scale_x_upset(order_by = "degree") +
  theme_combmatrix(combmatrix.panel.point.color.fill = "green",
                   combmatrix.panel.line.size = 0,
                   combmatrix.label.make_space = FALSE)

###进一步拓展。和ggplot其他图形相结合。
tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres, y=year)) +
  geom_violin() +
  scale_x_upset(order_by = "freq", n_intersections = 12)

#df_complex_conditions 一个虚构的生物数据集
df_complex_conditions %>%
  mutate(Label = pmap(list(KO, DrugA, Timepoint), function(KO, DrugA, Timepoint){
    c(if(KO) "KO" else "WT", if(DrugA == "Yes") "Drug", paste0(Timepoint, "h"))
  })) %>%
  ggplot(aes(x=Label, y=response)) +
  geom_boxplot() +
  geom_jitter(aes(color=KO), width=0.1) +
  geom_smooth(method = "lm", aes(group = paste0(KO, "-", DrugA))) +
  scale_x_upset(order_by = "degree",
                sets = c("KO", "WT", "Drug", "8h", "24h", "48h"),
                position="top", name = "") +
  theme_combmatrix(combmatrix.label.text = element_text(size=12),
                   combmatrix.label.extra_spacing = 5)


avg_rating <- tidy_movies %>%
  mutate(Genres_collapsed = sapply(Genres, function(x) paste0(sort(x), collapse="-"))) %>%
  mutate(Genres_collapsed = fct_lump(fct_infreq(as.factor(Genres_collapsed)), n=12)) %>%
  group_by(stars, Genres_collapsed) %>%
  summarize(percent_rating = sum(votes * percent_rating)) %>%
  group_by(Genres_collapsed) %>%
  mutate(percent_rating = percent_rating / sum(percent_rating)) %>%
  arrange(Genres_collapsed) 

  ggplot(avg_rating, aes(x=Genres_collapsed, y=stars, fill=percent_rating)) +
  geom_tile() +
  stat_summary_bin(aes(y=percent_rating * stars), fun.y = sum,  geom="point", 
                   shape="—", color="red", size=6) +
  axis_combmatrix(sep = "-", levels = c("Drama", "Comedy", "Short", 
                                        "Documentary", "Action", "Romance", "Animation", "Other")) +
  scale_fill_viridis_c()




