##2019.7.15
##中文字体
#https://mp.weixin.qq.com/s/2DaIDHr0ptDRab-Vyx7g_g

#install.packages('Cairo')
library("Cairo")

?Cairo
#添加参数device，设置字体，这里设置为宋体
ggsave("geo_Fus_wilt.pdf", p1, width = 12, height =8 , device = cairo_pdf, family = "Song")


CairoX11(display=Sys.getenv("DISPLAY"), width = 7, height = 7,
         pointsize = 12, gamma = getOption("gamma"), bg = "transparent",
         canvas = "white", xpos = NA, ypos = NA, ...)
CairoPNG(filename = "Rplot%03d.png", width = 480, height = 480,
         pointsize = 12, bg = "white",  res = NA, ...)
CairoJPEG(filename = "Rplot%03d.jpeg", width = 480, height = 480,
          pointsize = 12, quality = 75, bg = "white", res = NA, ...)
CairoTIFF(filename = "Rplot%03d.tiff", width = 480, height = 480,
          pointsize = 12, bg = "white", res = NA, ...)
CairoPDF(file = ifelse(onefile, "Rplots.pdf","Rplot%03d.pdf"),
         width = 6, height = 6, onefile = TRUE, family = "Helvetica",
         title = "R Graphics Output", fonts = NULL, version = "1.1",
         paper = "special", encoding, bg, fg, pointsize, pagecentre)

# 一下是字体选择链接：
# 新细明体,    PMingLiU ,            
# 细明体,      MingLiU,            
# 标楷体,      DFKai-SB,           
# 黑体,        SimHei,             
# 宋体,        SimSun ,            
# 新宋体,      NSimSun,            
# 仿宋,        FangSong,           
# 楷体,        KaiTi,              
# 仿宋_GB2312, FangSong_GB2312,    
# 楷体_GB2312, KaiTi_GB2312,       
# 微软正黑体,  Microsoft JhengHei, 
# 微软雅黑,    Microsoft YaHei,    
# 隶书,        LiSu,               
# 幼圆,        YouYuan,            
# 华文细黑,    STXihei,            
# 华文楷体,    STKaiti,            
# 华文宋体,    STSong,             
# 华文中宋,    STZhongsong,        
# 华文仿宋,   STFangsong,         
# 方正舒体 ,   FZShuTi,            
# 方正姚体,    FZYaoti,            
# 华文彩云,    STCaiyun,           
# 华文琥珀,    STHupo,             
# 华文隶书,    STLiti,             
# 华文行楷,    STXingkai,          
# 华文新魏,    STXinwei  
# 
# --------------------- 
#   原文：https://blog.csdn.net/hongweigg/article/details/47907555 