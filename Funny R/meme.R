##2018.6.23
##meme

###########################################1. 用R画meme ;)
https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247485006&idx=1&sn=6180f5bea2461a9059ece8f62a3c35ab&chksm=ec43b709db343e1fe73dcf39a6e89240157244419d164bc46458365ad5d9b806206c5d7e5c72&mpshare=1&scene=1&srcid=06237XqiLLauDHQ8DRgxOpB4&pass_ticket=dsh7qIb5mj6erisMOzyYS4xK%2BhQmceJy6mGXI9YplY9ghTMfNUqgZDDxs5GNrAOU#rd


library(meme)
u <- "http://i0.kym-cdn.com/entries/icons/mobile/000/000/745/success.jpg"

#use meme() function to add meme captions
meme(u, "code", "all the things!")

#mmplot() to read and plot the input image and then using + mm_caption() to add meme captions.
mmplot(u) + mm_caption("calm down", "and RTFM", color="purple")

meme_save(x, file="Figs/meme.png")  ##保存为本地文件

#Users can plot the meme() output and change the caption or other parameters in real time.
x <- meme(u, "please", "tell me more")
plot(x, size = 2, "happy friday!", "wait, sorry, it's monday", 
     color = "firebrick", font = "Courier")

##### the + method
#Instead of using parameters in plot() explictely, Users can use + aes() to set the plot parameters:
  x + aes(upper = "#barbarplots",
          lower = "friends don't let friends make bar plots",
          color = firebrick, font = Courier, size=1.5)
#or using + list(). The following command will also generate the figure displayed above.
  x + list(upper = "#barbarplots",
           lower = "friends don't let friends make bar plots",
           color = "firebrick", font = "Courier", size=1.5)
  
#####支持多语言
  y <- meme(u, "卧槽", "听说你想用中文", font="STHeiti")
  y
  
#######grid support
  library(grid)
  mm <- meme(u, "code", "all the things!", size=.3, color='firebrick')
  mm
  grid.newpage()
  pushViewport(viewport(width=.9, height=.9))
  grid.rect(gp = gpar(lty="dashed"))
  
  xx <- seq(0, 2*pi , length.out=10)
  yy <- sin(xx)
  for (i in seq_along(xx)) {
    vp <- viewport(x = xx[i]/(2*pi), y = (yy[i]-min(yy))/2, width=.05, height=.05)
    print(mm, vp = vp)
  }

#####ggplot2 support
  library(ggplot2)
  library(ggimage)
  
  d <- data.frame(x = xx, y = yy)
  ggplot(d, aes(x, y)) + geom_line() +
    geom_subview(mm, x = xx, y = yy, width=.3, height=.15)
  
  ggplot(d, aes(x, y)) +
    geom_subview(mm+aes(size=3), x=0, y=0, width=Inf, height=Inf) +
    geom_point() + geom_line()
  
#####  cowplot support
  cowplot::plot_grid(x, y, ncol=1, labels = c("A", "B"))


###########################################2. 制作meme的通用方式，来了解一下 
  https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg%3D%3D&mid=2247486060&idx=1&sn=f2bfc3b3a5f6920df3fb0e40a37b9705&scene=45#wechat_redirect
  
  library(magick)
  library(ggplot2)
  library(shadowtext)
  library(ggplotify)
  
  setwd("E:/桌面/R script 2017")
  x = image_read("Rplot01.png")  ##magick读图
  p = as.ggplot(x)               ##ggplotify包把图转化成ggplot对象
  #然后我们就可以用ggplot2各种加图层了.一般画meme用的是Impact字体
  #shadowtext包可实现文字的背景有阴影效果
  p + geom_shadowtext(x=.25, y=.1, size=10, label="GITLAB", family="Impact") +
    geom_shadowtext(x=.62, y=.5, size=6, label="EVERY GITHUB\nREPO RIGHT NOW", family="Impact") +
    geom_shadowtext(x=.89, y=.25, size=7, label="MICROSOFT", family="Impact")
  p
  
####meme包生成的meme对象，你也可以用ggplotify转成ggplot对象，
###然后参照上面示例的代码，用shadowtext包，自己随意加图层，
###可以说这个功能本身也是有的，只不过不单独存在于meme中而已，
##meme、ggplotify、shadowtext三个包相互衔接。

##假设你有实验的图片，发表文章要拼图，你也能够用cowplot拼哦，
##上面的例子就是了，你用magick读图，用ggplotify转成ggplot对象，
##这不就可以拼了么！你还能做注释呢（meme台词就是注释）

  library(meme)
  u <- "http://i0.kym-cdn.com/entries/icons/mobile/000/000/745/success.jpg"
  
  #use meme() function to add meme captions
  a = meme(u, "Hi", "I am big!")
  meme_save(a, file="a.png")  ##保存为本地文件
  b = image_read("a.png")  ##magick读图
  p = as.ggplot(b)
  bp = p + geom_shadowtext(x=.25, y=.1, size=10, label="你好呀", family="Impact")
  
  c = image_read("ggplot_theme.png")
  cp = as.ggplot(c)
  cowplot::plot_grid(bp, cp, ncol=1, labels = c("A", "B"))

  
###########################################3. 使用外部字体画meme 
  library(meme)
  ## import pokemon fonts
  ## downloaded from <https://fontmeme.com/fonts/pokmon-font/>font_pokemon()
  
  u <- 'https://ravishly.com/sites/default/files/landscape-1456483171-pokemon2.jpg'
  meme(u, "Pokemon", "pikachu i choose you!", font='Pokemon_Hollow')
  
  meme(u, "Pokemon", "pikachu i choose you!", font='Pokemon_Solid', color='#FCCF00')
  
  ## folder that contains bubble1 font
  ## downloaded from https://fontmeme.com/fonts/bubble-1-font/
  
  #下面的代码给font_import传入dir，它可以是你自己下载字体的目录，这个函数就会干所有事情，然后你直接就可以用了。
  dir <- system.file('fonts/bubble', package='meme')
  font_import(dir)
  
  meme(u, "the meme package", "is awesome!", font="bubble1")
  
  #当然这个函数虽然在meme包里，为了大家创作meme更尽性，但它不限于做meme，加载了字体之后，它可以用于R的其它画图系统中，比如base, grid等等。
  library(ggplot2)
  qplot(1:10, 1:10) + labs(title="meme is awesome") +
    theme(plot.title=element_text(family='bubble1', size=30, color='firebrick'))
 
  ###########################################4. 扪心自问，meme几何？ 
  https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247485721&idx=1&sn=8a3ec024c2cdd455d815ef3ebd15bec2&scene=21#wechat_redirect
  
  #meme的输出就是一个叫meme的对象，只是个简单的list，而内容基本上就是参数而已。
  #ggplot的输出是一个叫gg的对象，实质上也是一个简单的list，同理你就能理解为什么它能出图？
  meme <- function(img, upper="", lower="", size="auto", color="white", font="Impact",
                   vjust = .1, bgcolor="black", r = 0.2) {
    x <- image_read(img)
    info <- image_info(x)
    imageGrob <- rasterGrob(x)
    p <- structure(
      list(img = img, imageGrob = imageGrob,
           width = info$width, height = info$height,
           upper=upper, lower=lower,
           size = size, color = color,
           font = font, vjust = vjust,
           bgcolor = bgcolor, r = r),
      class = c("meme", "recordedplot"))
    p
  }