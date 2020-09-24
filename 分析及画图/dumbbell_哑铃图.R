library(ggplot2)
library(ggalt)
theme_set(theme_classic())

health <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/health.csv")
health$Area <- factor(health$Area, levels=as.character(health$Area))  # for right ordering of the dumbells

# health$Area <- factor(health$Area)
gg <- ggplot(health, aes(x=pct_2013, xend=pct_2014, y=Area, group=Area)) +
  geom_dumbbell(color="red",
                size=0.75)+
  scale_x_continuous(label=percent) +
  labs(x=NULL,
       y=NULL,
       title="Dumbbell Chart",
       subtitle="Pct Change: 2013 vs 2014",
       caption="Source: https://github.com/hrbrmstr/ggalt") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)


?geom_dumbbell
library(ggplot2)

df <- data.frame(trt=LETTERS[1:5], l=c(20, 40, 10, 30, 50), r=c(70, 50, 30, 60, 80))

ggplot(df, aes(y=trt, x=l, xend=r)) +
  geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25) +
  labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))
