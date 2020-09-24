##2019.10.10

##https://mp.weixin.qq.com/s/fZqi6wuI7G1kDeUSa_HzPA

##把你用R画的图（base或ggplot2）变成ASCII纯文本！

#devout

##https://github.com/coolbutuseless/devout

# remotes::install_github("coolbutuseless/devout")
# pkgbuild::check_build_tools(debug = TRUE)
devtools::install_github("coolbutuseless/devout")

library(devout)
library(ggplot2)

p <- ggplot(mtcars) + 
  geom_point(aes(mpg, wt)) +
  labs(
    y     = "Car Weight",
    title    = "Basic scatter plot",
    subtitle = "Rendered with devout::ascii()"
  )  + 
  theme_bw()
p
ascii(width = 100)   ##这一步R会崩溃。应该只能在服务器上做
p
invisible(dev.off())


###pie plot in base R
ascii(width = 100) 
pie(c(cool = 4, but = 2, use = 1, less = 8))
invisible(dev.off())

####################3
#10.11

#https://mp.weixin.qq.com/s/PuPXR4L-DyAs3fOB1n64Vg

##不想画彩图了，用纹理填充吧，省掉好多版面费！ 

devtools::install_github("coolbutuseless/lofi")      # Colour encoding
devtools::install_github("coolbutuseless/minisvg")   # SVG creation
devtools::install_github("coolbutuseless/devout")    # Device interface
devtools::install_github("coolbutuseless/devoutsvg") # This package

