#2019.8.4
#https://mp.weixin.qq.com/s/3u9uFNlD3p9jFIYktlQCJQ


getwd()
setwd('E:/桌面/R script 2017/cor_mantel_science/')
env.st = read.csv("./env.st.csv",row.names = 1)
env.st 

report = read.csv("./report.csv",row.names = 1)
report

p = plot_mantel_cor(env = env.st,report = report,title = "好久不见-我的朋友" )
p



######环境变量之间的关系··································································

plot_mantel_cor = function(env = env.st,report = report,title = "16S_microbiology"){
  library(ggcorrplot)
  library(igraph)
  library(psych)
  
  occor = corr.test(env.st,use="pairwise",method="spearman",adjust="fdr",alpha=.05)
  occor.r = occor$r # 取相关性矩阵R值
  
  occor.p = occor$p # 取相关性矩阵p值
  occor.r[occor.p>0.05] = 0
  
  # p = ggcorrplot(occor.r,p.mat = occor.p, method = "circle",lab = TRUE,insig = "blank",outline.color	= "white",
  #                ggtheme = ggplot2::theme_bw())
  # 
  # p
  # 
  ###############
  
  
  
  # write.csv(occor.r,"./occor.r.csv",quote = F)
  library(reshape2)
  
  occor.r2 = occor.r[lower.tri(occor.r, diag = TRUE)]
  # occor.r2 = occor.r[lower.tri(occor.r, diag = TRUE)]
  length(colnames(occor.r))
  # a = rep(0,length(occor.r2))
  # i = 1
  ### 构造环境变量三角矩阵图
  
  ## dd：构建三交矩阵横坐标
  cc = rep(length(colnames(occor.r)),(length(colnames(occor.r))))
  for (i in 1:(length(colnames(occor.r))-1)) {
    ee = rep(length(colnames(occor.r))-i,(length(colnames(occor.r))-i))
    dd = c(cc,ee)
    
    cc = dd
  }
  dd = (length(colnames(occor.r))+1) - dd
  
  
  # cc = rep(1:length(colnames(occor.r)))
  # for (i in 1:(length(colnames(occor.r))-1)) {
  #   ee = c(1:(length(colnames(occor.r))-i))
  #   rr = c(cc,ee)
  #   
  #   cc = rr
  # }
  # 
  # rr
  ##ww:构建三角矩阵纵坐标
  gg =  rep(length(colnames(occor.r)):1)
  for (i in 1:(length(colnames(occor.r))-1)) {
    ee = c((length(colnames(occor.r))-i):1)
    ww = c(gg,ee)
    
    gg = ww
  }
  
  ww
  ##构造变量对应关系
  wwt =data.frame(x = dd,y = ww)
  ##提取相关值大小，这是下三角矩阵转化
  # xa = melt(occor.r)
  wwt$mantelR = occor.r2
  wwt
  
  
  ### 下面添加群落和环境因子的相关结果
  
  ##构造每个环境因子的坐标
  
  x = c(1:(length(colnames(occor.r))) )
  y = c((length(colnames(occor.r))+1) :2)
  
  data2 = data.frame(x = x,y = y)
  
  data2
  # ggplot() + 
  #   geom_tile(aes(x = x, y = y),wwt,fill=NA,color='gray',size=1)+
  #   geom_point(aes(x = x, y = y,size=mantelR,fill=mantelR),wwt, shape=22,color='white')+
  #   scale_size(range = c(1, 8))+
  #   scale_fill_distiller(palette="RdYlBu")+
  #   geom_curve(aes(x = 10, y = 10, xend = x, yend = y),curvature = 0.2,data2) +
  #   geom_point(aes(x = x, y = y),pch = 21) +
  #   geom_point(aes(x = 10, y = 10),pch = 21,size = 4)
  
  ###添加线的粗细和颜色映射
  
  # write.csv(report,"./report.csv",quote = F)
  
  data2$count = report$R
  data2$count = as.character(data2$count)
  data2$count = as.numeric(data2$count)
  data2
  
  
  as = rep("a",length(data2$count))
  
  
  for (i in 1:length(data2$count)) {
    if (data2$count[i] > 0) {
      as[i] = "+"
    }
    
    if (data2$count[i] < 0) {
      as[i] = "-"
    }
    if (data2$count[i] == 0) {
      as[i] = "-"
    }
  }
  as
  
  data2$group = as
  data2$label = colnames(env.st)
  wwt
  
  
  data2$count1 = data2$count^2*500
  # data2$count1 = c(1:length(data2$count))
  ##这和图形整体绘制
  
  ###geom_curve添加size参数如果在aes内部的话默认线非常宽，但是size放到sea外就不会这样，这里我选择放到外面
  p = ggplot() + 
    geom_tile(aes(x = x, y = y),wwt,fill=NA,color='gray',size=0.5)+
    geom_point(aes(x = x, y = y,size=mantelR,fill=mantelR),wwt, shape=22,color='white')+
    scale_size(range = c(1, 8))+
    scale_fill_distiller(palette="RdYlBu")+
    geom_curve(aes(x = max(wwt$x)*3/4, y = max(wwt$y)*3/4, xend = x, yend = y,group = group,color = group),curvature = 0.2,data2,size = data2$count1) +
    geom_point(aes(x = x, y = y),pch = 21,size =4,data2,color = "black",fill = "#FFF5EB")+
    geom_point(aes(x = max(wwt$x)*3/4, y = max(wwt$y)*3/4),pch = 21,size = 6,color = "black",fill = "#FEE6CE")
  
  p
  
  ### 添加环境因子添加标签
  p = p + geom_text(aes(x = x, y = 0,label= colnames(env.st)),size=4,data2) +
    geom_text(aes(x = 0, y = y-1,label= colnames(env.st)),size=4,data2) 
  p = p + geom_text(aes(x = x+0.5, y = y+0.5,label=data2$label),size=4,data2)
  ##添加群落矩阵标签
  wwt
  
  # max(wwt$x)*3/4
  # max(wwt$y)*3/4
  
  p = p +
    geom_text(aes(x = max(wwt$x)*3/4, y = max(wwt$y)*3/4,label= title),size = 6) 
  p
  
  ### 修改主题
  p = p +theme_void()#横纵坐标群去掉
  
  p
  
  
  
}




plotnamea = paste(path,"New_matel_cor_plot.pdf",sep = "")
ggsave(plotnamea, p, width = 8, height = 6)


