##2019.4.25
##sars

install.packages("sars")
library(sars);??sars

#模型的拟合
##fit the logarithmic SAR model:
fit <- sar_loga(data = galap) 
summary(fit) 
plot(fit)

#Create a fit_collection object containing multiple SAR model fits, and plot all fits. 
fitC <- sar_multi(data = galap, obj = c("power", "loga", "monod"))
plot(fitC)

#模型的一般性和同质性检验
#load an example dataset, fit the linear SAR model whilst running residual
#normality and homogeneity tests, and return the results of the residual
#normality test 
data(galap) 
fit <- sar_linear(data = galap, normaTest ="lillie", homoTest = "cor.fitted") 
summary(fit)

fit$normaTest

#模型形状及渐近线
#计算二阶导数确定形状；根据参数确定是否有渐近线。

##多模型曲线
#将每一个成功拟合模型的预测丰度值乘以模型的权重(AIC,AICC,BIC等)，
#然后对所有模型的结果值求和，单个模型的线性组合构建多模型平均曲线
data(niering) 
fit <- sar_average(data= niering, obj =c("power","loga","koba","mmf","monod",
                                         "negexpo","chapman","weibull3","asymp"),
                   normaTest = "none", homoTest = "none", neg_check = FALSE, 
                   confInt = TRUE, ciN= 50) #a message is provided indicating that one model (asymp) could not be
par(mfrow = c(3,1)) #plot all model fits with the multimodel SAR curve
plot(fit, ModTitle = "a) Multimodel SAR")

#plot the multimodel SAR curve (with confidence intervals; see explanation in the main text, above) on its own 
plot(fit, allCurves = FALSE, ModTitle =
       "c) Multimodel SAR with confidence intervals", confInt = TRUE)

#Barplot of the information criterion weights of each model 
plot(fit, type = "bar", ModTitle = "b) Model weights", cex.lab = 1.3)

dev.off()

##其他功能
#直接拟合log-log的幂律分布
#load an example dataset, fit the log-log power model, return a model fit
#summary and plot the model fit. When ‘compare’ == TRUE, the non-linear
#power model is also fitted and the resultant parameter values compared. 
#If any islands have zero species, a constant (‘con’) is added to all 
#species richness values. 
data(galap) 
fit <- lin_pow(dat = galap, compare = TRUE, con = 1) 
summary(fit) 
plot(fit)

#随机放置模型
#load an example dataset, fit the random placement model and plot the 
#model fit and standard deviation. The ‘data’ argument requires a species-
#site abundance matrix: rows are species and columns are sites. The area 
#argument requires a vector of site (island) area values. 
data(cole_sim) 
fit <- coleman(data = cole_sim[[1]], area = cole_sim[[2]]) 
plot(fit, ModTitle = "Hetfield")

#一般动态模型（GDM）
#load an example dataset, fit the GDM using the logarithmic SAR model, and
#compare the GDM with three alternative (nested) models: area and time 
#(age of each island), area only, and intercept only. 
data(galap) 
galap$t <- rgamma(16, 5, scale = 2)#add a random time variable 
gdm(data = galap, model = "loga", mod_sel = TRUE)
