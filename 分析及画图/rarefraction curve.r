##2018.12.28
#Alpha多样性稀释曲线rarefraction curve还不会画吗？快看此文 
https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247487361&idx=5&sn=d16b062d3688cd10e1b94b72379a6dd8&chksm=fab9e530cdce6c26a5b1c089d24d64efbe7c00b38a5e586a33edf2977d6ac8fa79bb11e22af6&scene=0&xtrack=1#rd

setwd("E:/桌面/长兴岛水样及底泥2018.6/数据分析/2018.8.4-16S")
suppressMessages(library(vegan))

x<-read.table(file="Galaxy70-[rarefaction.txt].txt",sep="\t",header=T,row.names=1)
group = read.table("group0.txt", sep="\t", row.names=1 )


#2. 按组求均值和标准误展示组间差异
# 求各组均值
# 读取usearch rarefraction文件，上面己经修改，必须重新读入
rare = read.table("alpha_rare.txt", header=T, row.names= 1, sep="\t") 
mat_t = merge(sampFile, t(rare), by="row.names")[,-1]
# 按第一列合并求均值
mat_mean = aggregate(mat_t[,-1], by=mat_t[1], FUN=mean)
# 修正行名
mat_mean_final = do.call(rbind, mat_mean)[-1,]
geno = mat_mean$group
colnames(mat_mean_final) = geno

rare=as.data.frame(round(mat_mean_final))
rare$x = rownames(rare)
rare_melt = melt(rare, id.vars=c("x"))

# 求各组标准误
# 转置rare表格与实验设计合并，并去除第一列样品名
se = function(x) sd(x)/sqrt(length(x)) # function for Standard Error
mat_se = aggregate(mat_t[,-1], by=mat_t[1], FUN=se) # se 为什么全是NA
mat_se_final = do.call(rbind, mat_se)[-1,]
colnames(mat_se_final) = geno

rare_se=as.data.frame(round(mat_se_final))
rare_se$x = rownames(rare_se)
rare_se_melt = melt(rare_se, id.vars=c("x"))

# 添加标准误到均值中se列
rare_melt$se=rare_se_melt$value

rare_melt$x = factor(rare_melt$x, levels=c(1:100))

p = ggplot(rare_melt, aes(x = x, y = value, group = variable, color = variable )) + 
  geom_line()+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.5) +
  xlab("Percentage")+ylab("Richness (Observed OTUs)")+
  theme(axis.text.x=element_text(angle=90,vjust=1, hjust=1))+
  scale_x_discrete(breaks = c(1:10)*10, labels = c(1:10)*10)+
  theme_classic()
p
ggsave(paste("alpha_rare_groups.pdf", sep=""), p, width = 8, height = 5)


https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651058567&idx=1&sn=7a8ca6f3d974e850f85e04b7feaa7cb6&chksm=84d9d010b3ae590600cbb821444692de8d6b6d077546e585470e2e7da2f07ca0250f634cb7e4&scene=21#wechat_redirect

