##2019.8.15

##听说你也有 emojifont 的字体使用困惑 

##https://mp.weixin.qq.com/s/RWw9xdJb9WsbeZu3ohUI7A

library(tidyverse)
library(emojifont)
?load.emojifont
load.emojifont()
ggplot() +
  geom_emoji("rose", color='steelblue') +
  theme_void()

ggplot() + geom_fontawesome("fa-github", color='black') + theme_void()


##更改图形设备
png(file = "test_emoji.png")
ggplot() + 
  geom_emoji("rose", color='steelblue') + 
  theme_void() 
dev.off()

#当然，如果使用ggsave去保存也没有任何问题。

#问题就基本定位应该是 RStudio graphics device 和这个包并不兼容。

#emojifont 依赖于 showtext ，而 showtext 本身和 RStudio GD 存在兼容性问题，
#所以在 Rstudio 中会出现各种问题，这也是为什么直接用png()保存则正常。
#因此给出的解决建议是即便在使用 Rstudio，也还是用系统本身的 GD 去打印，
#在Windows中就用 windows() 在 Mac 中就用 quartz()，然后万事大吉功成身退。
windows() #windows
x11()  #linux
quartz() #Mac


emoji("smiley_cat")
emoji("smiley")
fontawesome("fa-github")
fontawesome("fa-weixin")
