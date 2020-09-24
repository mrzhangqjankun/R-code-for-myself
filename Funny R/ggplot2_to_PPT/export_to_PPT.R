##2019.8.8

##https://mp.weixin.qq.com/s/qCB-3t3fuNzFFJoDv_jb8g

##R画的图，能够在PowerPoint里面编辑

#install.packages("export")
library(export)

install.packages("effect")
library(effects)
fit=lm(prestige ~ type + income*education, data=Prestige)
plot(Effect(c("income", "education"), fit),multiline=T, ci.style="bands")
getwd()
graph2ppt(file="effect plot.pptx", width=7, height=5)

?graph2ppt

#这个包提供了各种函数，你可以通过graph2doc导出到Word，还支持通过graph2office导出到LibreOffice。
graph2bitmap
graph2doc    
graph2eps  
graph2jpg  
graph2office
graph2pdf
graph2png  
graph2ppt  
graph2svg  
graph2tif  
graph2vector
#而且更为良心的是，连表格也是支持的。
table2csv
table2csv2        
table2doc        
table2excel
table2html        
table2office      
table2ppt        
table2spreadsheet
table2tex