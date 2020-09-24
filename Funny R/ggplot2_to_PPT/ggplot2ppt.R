##2019.4.2

#https://mp.weixin.qq.com/s/nHT3vkwNfQknr6CLF0cUDQ

install.packages("export")
library(export);??export

library(effects)
fit=lm(prestige ~ type + income*education, data=Prestige)
plot(Effect(c("income", "education"), fit),multiline=T, ci.style="bands")

#导出到PPT
getwd()
graph2ppt(gg,file="E:/gg.pptx", width=7, height=5);?graph2ppt
##输出路径好像不能有中文。否则会报错无法打开压缩文件。



#在PPT中取消组合，那么所有的元素都是分离的，而且是向量格式，你可以修改字体，改大小，改颜色，等等。

# graph2bitmap
# graph2doc    
# graph2eps  
# graph2jpg  
# graph2office
# graph2pdf
# graph2png  
# graph2ppt  
# graph2svg  
# graph2tif  
# graph2vector
# 
# 而且更为良心的是，连表格也是支持的。
# 
# table2csv
# table2csv2        
# table2doc        
# table2excel
# table2html        
# table2office      
# table2ppt        
# table2spreadsheet
# table2tex

#myplot：像ggsave一样优雅地保存R基础图形
##https://mp.weixin.qq.com/s/NTybtxSWNrY0aLEXPOtUfg


###语言设置为英文
Sys.setenv(LANG ="EN")


#万能转换：R图和统计表转成发表级的Word、PPT、Excel、HTML、Latex、矢量图等 

#https://mp.weixin.qq.com/s/OtOpfob4fnqWJ3Ah-y36mA

# graph2bitmap: 将当前R图保存到bmp文件中
# graph2png: 将当前R图保存到png文件中
# graph2tif: 将当前R图保存到TIF文件中
# graph2jpg: 将当前R图保存为JPEG文件

# graph2bitmap(x = NULL, file = "Rplot", fun = NULL, type = c("PNG","JPG", "TIF"),
#              aspectr = NULL, width = NULL, height = NULL, dpi = 300,scaling = 100,
#              font =ifelse(Sys.info()["sysname"] == "Windows", "Arial",
#                           "Helvetica")[[1]], bg = "white", cairo = TRUE,
#              tiffcompression = c("lzw", "rle", "jpeg", "zip", "lzw+p", "zip+p"),
#              jpegquality = 99, ...)
# aspectr: 期望纵横比。如果设置为空，则使用图形设备的纵横比。
# width: 所需宽度(英寸);可以与期望的纵横比aspectr组合。
# height: 所需高度(英寸);可以与期望的纵横比aspectr组合。
# scaling: 按一定比例缩放宽度和高度。
# font: PNG和TIFF输出中标签所需的字体; Windows系统默认为Arial，其他系统默认为Helvetica。
# bg: 所需的背景颜色，例如“白色”或“透明”。
# cairo: 逻辑，指定是否使用Cairographics导出。
# tiffcompression: 用于TIF文件的压缩。
# jpegquality: JPEG压缩的质量。

library(export)
library(ggplot2)
library(datasets)
x=qplot(Sepal.Length, Petal.Length, data = iris,
        color = Species, size = Petal.Width, alpha = I(0.7))
x

#导出图形对象
# 需运行上面的ggplot2绘图
# Create a file name
# 程序会自动加后缀
filen <- "output_filename" # or
# filen <- paste("YOUR_DIR/ggplot")

# There are 3 ways to use graph2bitmap():

### 1. Pass the plot as an object
graph2png(x=x, file=filen, dpi=400, height = 5, aspectr=4)
graph2tif(x=x, file=filen, dpi=400, height = 5, aspectr=4)
graph2jpg(x=x, file=filen, dpi=400, height = 5, aspectr=4)

#导出当前绘图窗口展示的图
### 2. Get the plot from current screen device

# 注意这个x，是运行命令，展示图像
x
graph2png(file=filen, dpi=400, height = 5, aspectr=4)
graph2tif(file=filen, dpi=400, height = 5, aspectr=4)
graph2jpg(file=filen, dpi=400, height = 5, aspectr=4)

#导出自定义函数输出的一组图
### 3. Pass the plot as a functio
plot.fun <- function(){
  print(qplot(Sepal.Length, Petal.Length, data = iris,
              color = Species, size = Petal.Width, alpha = 0.7))
}
graph2png(file=filen, fun=plot.fun, dpi=400, height = 5, aspectr=4)
graph2tif(file=filen, fun=plot.fun, dpi=400, height = 5, aspectr=4)
graph2jpg(file=filen, fun=plot.fun, dpi=400, height = 5, aspectr=4)

###############与Office系列的交互
# graph2ppt: 将当前R图保存到Microsoft Office PowerPoint/LibreOffice Impress演示文稿中。
# graph2doc:将当前的R图保存到Microsoft Office Word/LibreOffice Writer文档中。
# 函数参数展示和解释
# graph2office(x = NULL, file = "Rplot", fun = NULL, type = c("PPT", "DOC"),
#              append = FALSE, aspectr = NULL, width = NULL, height = NULL,scaling = 100,
#              paper = "auto", orient = ifelse(type[1] == "PPT","landscape", "auto"),
#              margins = c(top = 0.5, right = 0.5, bottom = 0.5, left= 0.5),
#              center = TRUE, offx = 1, offy = 1, upscale = FALSE, vector.graphic = TRUE, ...)
# margins: 预设留白边距向量。
# paper: 纸张尺寸——“A5”至“A1”用于Powerpoint导出，或“A5”至“A3”用于Word输出;默认“auto”自动选择适合您的图形的纸张大小。如果图太大，无法在给定的纸张大小上显示，则按比例缩小。
# orient: 所需的纸张方向-“自动”，“纵向”或“横向”; Word输出默认为“自动”，Powerpoint默认为“横向”。
# vector.graphic: 指定是否以可编辑的向量DrawingML格式输出。默认值为TRUE，在这种情况下，编辑Powerpoint或Word中的图形时，可以先对图形元素进行分组。如果设置为FALSE，则将该图以300 dpi的分辨率栅格化为PNG位图格式。(栅(shān)格化,是PS中的一个专业术语,栅格即像素,栅格化即将矢量图形转化为位图。)

同样有3种导出方式
# 需运行上面的ggplot2绘图
# Create a file name
filen <- "output_filename" # or
# filen <- paste("YOUR_DIR/ggplot")

# There are 3 ways to use graph2office():

### 1. Pass the plot as an object
# 导出图形对象

graph2ppt(x=x, file=filen)
graph2doc(x=x, file=filen, aspectr=0.5)
### 2. Get the plot from current screen device

# 导出当前预览窗口呈现的图
x
graph2ppt(file=filen, width=9, aspectr=2, append = TRUE)
graph2doc(file=filen, aspectr=1.7, append =TRUE)

### 3. Pass the plot as a function
# 导出自定义函数输出的一系列图
graph2ppt(fun=plot.fun, file=filen, aspectr=0.5, append = TRUE)
graph2doc(fun=plot.fun, file=filen, aspectr=0.5, append = TRUE)

#
#禁用矢量化图像导出
graph2ppt(x=x, file=filen, vector.graphic=FALSE, width=9, aspectr=sqrt(2), append = TRUE)
#用图填满幻灯片
graph2ppt(x=x, file=filen, margins=0, upscale=TRUE, append=TRUE)

#输出矢量图
graph2svg: 将当前的R图保存为SVG格式
graph2pdf: 将当前的R图保存为PDF格式
graph2eps: 将当前的R图保存为EPS格式
#函数参数解释
graph2vector(x = NULL, file = "Rplot", fun = NULL, type = "SVG",aspectr = NULL,
             width = NULL, height = NULL, scaling = 100,
             font = ifelse(Sys.info()["sysname"] == "Windows",
                           "Arial","Helvetica")[[1]], bg = "white", colormodel = "rgb",
             cairo = TRUE,fallback_resolution = 600, ...)
#fallback_resolution: dpi中的分辨率用于栅格化不支持的矢量图形。
#需运行上面的ggplot2绘图
# Create a file name
filen <- "output_filename" # or
# filen <- paste("YOUR_DIR/ggplot")
# There are 3 ways to use graph2vector():
### 1. Pass the plot as an object
# 导出图形对象
graph2svg(x=x, file=filen, aspectr=2, font = "Times New Roman",
          height = 5, bg = "white")
graph2pdf(x=x, file=filen, aspectr=2, font = "Arial",
          height = 5,  bg = "transparent")
graph2eps(x=x, file=filen, aspectr=2, font = "Arial",
          height = 5, bg = "transparent")

# 导出当前预览窗口呈现的图
### 2. Get the plot from current screen device
x
graph2svg(file=filen, aspectr=2, font = "Arial",
          height = 5, bg = "transparent")
graph2pdf(file=filen, aspectr=2, font = "Times New Roman",
          height = 5, bg = "white")
graph2eps(file=filen, aspectr=2, font = "Times New Roman",
          height = 5, bg = "white")

# 导出自定义函数输出的一系列图
### 3. Pass the plot as a function

graph2svg(file=filen, fun = plot.fun, aspectr=2, font = "Arial",
          height = 5, bg = "transparent")
graph2pdf(file=filen, fun=plot.fun, aspectr=2, font = "Arial",
          height = 5, bg = "transparent")
graph2eps(file=filen, fun=plot.fun, aspectr=2, font = "Arial",
          height = 5, bg = "transparent")


#输出统计结果到表格 table2spreadsheet
#table2excel: 导出统计输出到Microsoft Office Excel/ LibreOffice Calc电子表格中的一个表.
#table2csv:将统计输出以CSV格式导出到表中(“，”表示值分隔，“。”表示小数)
#table2csv2: 将统计输出以CSV格式导出到表中(“;”表示值分隔，”，”表示小数)
table2spreadsheet(x = NULL, file = "Rtable", type = c("XLS", "CSV",
                                                      "CSV2"), append = FALSE, sheetName = "new sheet", digits = 2,
                  digitspvals = 2, trim.pval = TRUE, add.rownames = FALSE, ...)
#sheetName: 一个字符串，给出创建的新工作表的名称(仅针对type==”XLS”)。它必须是惟一的(不区分大小写)，不受文件中任何现有工作表名称的影响。
#digits:除具有p值的列外，要显示所有列的有效位数的数目。
#digitspvals:具有p值的列要显示的有效位数的数目。
# Create a file name
filen <- "table_aov" # or
# filen <- paste("YOUR_DIR/table_aov")

# Generate ANOVA output
fit=aov(yield ~ block + N * P + K, data = npk) # 'npk' dataset from base 'datasets'
x=summary(fit)

# Save ANOVA table as a CSV
### Option 1: pass output as object
# 输出对象
table2csv(x=x,file=filen, digits = 1, digitspvals = 3, add.rownames=TRUE)

# 屏幕输出导出到文件
### Option 2: get output from console
summary(fit)
table2csv(file=filen, digits = 2, digitspvals = 4, add.rownames=TRUE)

# Save ANOVA table as an Excel
# Without formatting of the worksheet
x
table2excel(file=filen, sheetName="aov_noformatting", digits = 1, digitspvals = 3, add.rownames=TRUE)
# 更多参数
# With formatting of the worksheet
table2excel(x=x,file=filen, sheetName="aov_formated", append = TRUE, add.rownames=TRUE, fontName="Arial", fontSize = 14, fontColour = rgb(0.15,0.3,0.75),  border=c("top", "bottom"), fgFill = rgb(0.9,0.9,0.9), halign = "center", valign = "center", textDecoration="italic")

#导出为Word中的表，再也不用复制粘贴调格式了 table2office
#table2ppt: 导出统计输出到Microsoft Office PowerPoint/ LibreOffice Impress演示文稿中的表
#table2doc: 将统计输出导出到Microsoft Office Word/ LibreOffice Writer文档中的表
table2office(x = NULL, file = "Rtable", type = c("PPT", "DOC"),
             append = FALSE, digits = 2, digitspvals = 2, trim.pval = TRUE,
             width = NULL, height = NULL, offx = 1, offy = 1,
             font = ifelse(Sys.info()["sysname"] == "Windows", "Arial",
                           "Helvetica")[[1]], pointsize = 12, add.rownames = FALSE)
# Create a file name
filen <- "table_aov"
# filen <- paste("YOUR_DIR/table_aov")

# Generate ANOVA output
fit=aov(yield ~ block + N * P + K, data = npk) # 'npk' dataset from base 'datasets'
# Save ANOVA table as a PPT
### Option 1: pass output as object
x=summary(fit)
table2ppt(x=x,file=filen, digits = 1, digitspvals = 3, add.rownames =TRUE)

### Option 2: get output from console
summary(fit)
table2ppt(x=x,file=filen, width=5, font="Times New Roman", pointsize=14, digits=4, digitspvals=1, append=TRUE, add.rownames =TRUE) # append table to previous slide

# Save ANOVA table as a DOC file
table2doc(x=x,file=filen, digits = 1, digitspvals = 3, add.rownames =TRUE)
summary(fit)
table2doc(file=filen, width=3.5, font="Times New Roman", pointsize=14,  digits=4, digitspvals=1, append=TRUE, add.rownames =TRUE) # append table at end of document