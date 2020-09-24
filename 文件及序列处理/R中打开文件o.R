##2020.41.17

# 直接在R里o(文件名),然后它就会根据你的系统去调用open, start或xdg-open，
# 然后这三个神奇的指令中的一个，就会把该干的活都干了，
# 要打开当前的工作目录的话就是o('.').
# 
# 至于这个o函数在那里，就在rvcheck这个包里，因为这个包没有依赖
# ，代码很简单，所以我写到~/.Rprofile中，打开R的时候，自动加载，
# 方便我平时想在R中打开文件。

##https://mp.weixin.qq.com/s/0LNCDGqT_-OgHiR583TgAw

library(rvcheck);?o
#o(file = ".")

check_r()
check_github('guangchuangyu/ggtree')
update_all() ##更新所有包
