##2019.11.4

##https://mp.weixin.qq.com/s/jGLEQ1r7a5X6MCJF1LF0yA

##一键更新所有R包

#update.packages()

require("installr")
updateR()

require(rvcheck)
library(rvcheck)

rvcheck::update_all()
