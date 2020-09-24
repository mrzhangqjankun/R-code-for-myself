##2019.3.14
#ggplot出图显示中文
#https://mp.weixin.qq.com/s/ITKP6zlkbXGKeCWnNMP3Bw
install.packages('Cairo')
library("Cairo")
?Cairo
#example
ggsave("geo_Fus_wilt.pdf", p1, width = 12, height =8 , device = cairo_pdf, family = "Song")

# 字体选择链接：
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

#https://blog.csdn.net/hongweigg/article/details/47907555 

#在使用pdf()函数时，要输出中文，只有一种字体可选。例子：
pdf("chinese.pdf",family="GB1")
plot(m2,xlab="高度",ylab="体重",main="统计")
dev.off()
#这里字体参数family只能设置成"GB1"(不知是否还有其他字体可选)，默认宋体。

getwd()
#在使用Cairo包时，进行中文输出时，可以选择多种字体
CairoPDF("chinese.pdf",family="SimSun")
plot(1:10,1:10,type="n");
text(2,10,"宋体",family="SimSun");
text(2,8,"黑体",family="SimHei");
text(2,6,"楷体",family="KaiTi_GB2312");
text(2,4,"隶书",family="LiSu");
text(2,2,"幼圆",family="YouYuan");
text(6,10,"Arial",family="Arial");
text(6,8,"Times New Roman",family="Times New Roman");
text(6,6,"Courier New",family="Courier New");
text(6,4,"Consolas",family="Consolas");
text(6,2,"Symbol",family="Symbol");
dev.off();
#family参数为字体名称，如宋体：SimSun，黑体：SimHei。




