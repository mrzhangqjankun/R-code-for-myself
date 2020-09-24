##2019.6.24
#Forest plot
#https://mp.weixin.qq.com/s/Ic1NwsgmOSMpDQJHQZY6lA

library(pacman)
p_load(survival)
data(colon)


options(width =200)
# 增加显示宽度
head(colon)

#使用 coxph() 函数构建预后模型：
cox_model = coxph(Surv(time,status) ~ sex + rx + adhere,data=colon)
summary(cox_model)

#使用survminer包中的 ggforest() 函数绘制森林图：
#install.packages('BiocManager')
p_load(sruvminer)  #这个包不适合R3.6。。。
ggforest(cox_model)

#后面略。比mata自带的画出来好看点
#


#2019.6.30
#https://mp.weixin.qq.com/s/kO9W-ChcxyBOmcpv88Uo6A


###2020.4.17
##R包metafor的Meta分析及森林图示例
##https://mp.weixin.qq.com/s/7unOm-uiwOV0Xlw5ipUSYg
##小白鱼。资源众多！看这个