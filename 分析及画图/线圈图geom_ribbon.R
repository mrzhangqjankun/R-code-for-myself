##2019.12.10

##https://mp.weixin.qq.com/s/pnwV3sxQoHxp_28RIf9MtQ

##线圈图geom_ribbon

#这是线圈图，原文来自：https://thoughtfulbloke.wordpress.com/2017/06/26/circular-banded-graphs-for-ggplot/
  
#看来ggplot能做的事情越来越多了，但是不希望这种方便阻止了一些人的前进。
# install.packages("readxl")
library(readxl);?readxl #Read Excel Files
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)

if(!file.exists("births.xlsx")){
  download.file("https://www.stats.govt.nz/assets/Tools/Most-common-birthday-in-New-Zealand/most-common-birthdays-1980-2017.xlsx", destfile="births.xlsx", mode="wb")
}
birthnums <- read_excel("births.xlsx", sheet = 2, skip=2);?read_excel
## New names:
## * `` -> ...1
names(birthnums)[1] <- "day"
long1 <- birthnums %>% gather(month,births, January:December) %>%
  filter(!is.na(births) & !(month=="February" & day==29)) %>%
  mutate(day_yr = 1:n(), perc=births/mean(births)) %>%
  select(day_yr, perc)

long1$baseline <- 1
long1$trueday <- long1$day_yr
long2 <- long1
long1$part <- 1
long2$part <- 2
long2$day_yr <- long2$day_yr + 1
long3 <- rbind(long1, long2) %>% arrange(trueday,part)

#之前我采用这种方式做圆圈图，但是如果柱子往里面延伸就做不到了
# ?geom_bar
ggplot(long3) +  coord_polar()+
  geom_bar(aes(y=perc,
               x=day_yr), fill="red",stat = "identity") +
  ylim(c(-3,3))+
  theme_classic()

#这片文章作者提到的方法让我能够换一种方式可视化圈图
ggplot(long3) +
  geom_ribbon(aes(ymin=baseline,
                  ymax=perc,
                  x=day_yr), fill="red") + theme_classic()
?geom_ribbon
ggplot(long3) + coord_polar() +
  geom_ribbon(aes(ymin=baseline,
                  ymax=perc,
                  x=day_yr), fill="red") + theme_classic()

range(long3$perc)
## [1] 0.7528693 1.0957603
long_postive1.15 <- long3
long_postive1.15$perc[long_postive1.15$perc < 1] <- 1
long_postive1.15$ranges <- "+10% to +15%"

long_postive1.05 <- long3
long_postive1.05$perc[long_postive1.05$perc < 1] <- 1
long_postive1.05$perc[long_postive1.05$perc > 1.05] <- 1.05
long_postive1.05$ranges <- "0 to +5%"

grf <- rbind(long_postive1.15, long_postive1.05)

ggplot() + coord_polar() +
  geom_blank(data=long3, aes(x=day_yr,
                             y=perc)) +
  geom_ribbon(data=grf,aes(ymin=baseline,
                           ymax=perc,
                           x=day_yr,
                           group=ranges,
                           fill=ranges))

long_postive1.15 <- long3
long_postive1.15$perc[long_postive1.15$perc < 1] <- 1
long_postive1.15$ranges <- "+10% to +15%"

long_postive1.10 <- long3
long_postive1.10$perc[long_postive1.10$perc < 1] <- 1
long_postive1.10$perc[long_postive1.10$perc > 1.10] <- 1.10
long_postive1.10$ranges <- "+5% to +10%"

long_postive1.05 <- long3
long_postive1.05$perc[long_postive1.05$perc < 1] <- 1
long_postive1.05$perc[long_postive1.05$perc > 1.05] <- 1.05
long_postive1.05$ranges <- "0% to +5%"

long_postive0.75 <- long3
long_postive0.75$perc[long_postive0.75$perc > 1] <- 1
long_postive0.75$ranges <- "-25% to -20%"

long_postive0.80 <- long3
long_postive0.80$perc[long_postive0.80$perc > 1] <- 1
long_postive0.80$perc[long_postive0.80$perc < 0.8] <- 0.8
long_postive0.80$ranges <- "-20% to -15%"

long_postive0.85 <- long3
long_postive0.85$perc[long_postive0.85$perc > 1] <- 1
long_postive0.85$perc[long_postive0.85$perc < 0.85] <- 0.85
long_postive0.85$ranges <- "-15% to -10%"

long_postive0.90 <- long3
long_postive0.90$perc[long_postive0.90$perc > 1] <- 1
long_postive0.90$perc[long_postive0.90$perc < 0.90] <- 0.90
long_postive0.90$ranges <- "-10% to -5%"

long_postive0.95 <- long3
long_postive0.95$perc[long_postive0.95$perc > 1] <- 1
long_postive0.95$perc[long_postive0.95$perc < 0.95] <- 0.95
long_postive0.95$ranges <- "-5% to 0%"

grf <- rbind(long_postive1.15,
             long_postive1.10,
             long_postive1.05,
             long_postive0.75,
             long_postive0.80,
             long_postive0.85,
             long_postive0.90,
             long_postive0.95)

ggplot() + coord_polar() +
  geom_blank(data=long3, aes(x=day_yr,
                             y=perc)) +
  geom_ribbon(data=grf,aes(ymin=baseline,
                           ymax=perc,
                           x=day_yr,
                           group=ranges,
                           fill=ranges))

grf$`Difference to mean` <- factor(grf$ranges, levels=c("-25% to -20%",
                                                        "-20% to -15%",
                                                        "-15% to -10%",
                                                        "-10% to -5%",
                                                        "-5% to 0%",
                                                        "+10% to +15%",
                                                        "+5% to +10%",
                                                        "0% to +5%"))
ggplot() + coord_polar() +
  geom_blank(data=long3, aes(x=day_yr,
                             y=perc)) +
  geom_ribbon(data=grf,aes(ymin=baseline,
                           ymax=perc,
                           x=day_yr,
                           group=`Difference to mean`,
                           fill=`Difference to mean`))

clrs <- c("#1D5C8F",
          "#266f9B",
          "#2F84A3",
          "#3498A1",
          "#16AE99",
          "#E76525",
          "#F38320",
          "#F7A21C")
legendorder=c("-25% to -20%",
              "-20% to -15%",
              "-15% to -10%",
              "-10% to -5%",
              "-5% to 0%",
              "0% to +5%",
              "+5% to +10%",
              "+10% to +15%")
linedf <- data.frame(ax = c(1,1), ay=c(1.05,1.15))
ggplot() + coord_polar() +
  ylim(.4,1.15) + ggtitle("New Zealand births by day of year, over/under average") + theme_classic() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line=element_blank(), axis.ticks =element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank()) +
  geom_blank(data=long3, aes(x=day_yr, y=perc)) +
  geom_ribbon(data=grf,aes(ymin=baseline,
                           ymax=perc,
                           x=day_yr,
                           group=`Difference to mean`,
                           fill=`Difference to mean`)) +
  geom_line(data=grf,aes(x=day_yr, y=1), colour="#444444", lwd=0.5) +
  geom_line(data=linedf,aes(x=ax, y=ay), colour="#444444") +
  annotate(geom="text", x=15, y=1.1, label="January", size=3) +
  annotate(geom="text", x=350, y=1.1, label="December", size=3) +
  scale_fill_manual(values=clrs, breaks=legendorder)
