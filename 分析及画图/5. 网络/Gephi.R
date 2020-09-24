##2019.5.31
#利用Gephi软件绘制网络图 
#https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247486630&idx=1&sn=a90a66272ea2375bed8b984dcd73e18f&chksm=fab9e617cdce6f011e22ca8bd8655314f480bfc891c1523c5717cb4efa92d2512201d2fe3696&scene=0#rd

library(psych)
# 读取otu-sample矩阵，行为sample，列为otu
otu1=read.table("E:/桌面/2018.5培训班/U盘/实际操作与测试数据/MENA分析及画图/Resampled OTU table.txt", head=T, row.names=1,sep="\t")
otu = t(otu1)

#几个小时都跑不出来。取前50个OTU试一下
otu = otu[,1:50]

# 计算OTU间两两相关系数矩阵
# 数据量小时可以用psych包corr.test求相关性矩阵，数据量大时，可应用WGCNA中corAndPvalue, 但p值需要借助其他函数矫正
occor = corr.test(otu,use="pairwise",method="spearman",adjust="fdr",alpha=0.05)
occor.r = occor$r # 取相关性矩阵R值
occor.p = occor$p # 取相关性矩阵p值

# 确定物种间存在相互作用关系的阈值，将相关性R矩阵内不符合的数据转换为0
occor.r[occor.p>0.05|abs(occor.r)<0.6] = 0

# 将occor.r保存为csv文件
write.csv(occor.r,file="E:/桌面/test_gephi.csv")

##在gehpi中打开继续。