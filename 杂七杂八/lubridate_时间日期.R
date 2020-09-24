##2019.3.11
#https://mp.weixin.qq.com/s/JONCYMjUUhka5iYQ8HgpCA
#时间相关

Sys.Date() #当天的日期（年-月-日）
date() #当前的具体时间

install.packages("lubridate")
library(lubridate) 

today()
now()
as_date(now()) #将日期时间型数据转换为日期型，即只保留年月日信息！

#读取日期
mdy("02-15-2019")    # mdy表示日期的顺序为月-日-年
ymd(20190215)
dmy("15/02/2019")
dmy("15-Feb-2019")

#如果输入的是日期时间型数据，即包含小时(h)分(m)秒(s)信息，则在其后加一个下划线，以及 h、m 和 s 之中的一个或多个字母，如下
ymd_hms("2019-02-15 11:47:58")


#对日期数据的数学运算包括减法、加法和除法。其中最常见的可能就是计算两个时间点之间的时间间隔(Time Intervals)了：
interval(ymd( "19930217"), today())    # 新的日期在后

#计算两者之间相差的天数，最直接的：
today() - ymd( "19930217")

#也可以使用基础函数 difftime，其units参数默认单位为天，但能设置为"secs", "mins", "hours", "days", "weeks"：
difftime(today(), ymd(19930217), units = "weeks")
#但是不能设置为"month" 和 "year"!

#如果想要获取间隔的年数：
year(today()) - year(ymd("19930217"))    

#https://mp.weixin.qq.com/s/koUQzE84ga1I5Njpb94AQA

####microbenchmark”这个包很简单，只要输入你的代码，并且指定“times=N”，
#程序就会重复运行你的代码N次，然后返回运行时间的平均值。默认的话times=100
library(microbenchmark)
?microbenchmark

#生成100万行日期数据。
library(data.table)
library(dplyr)
n <- 1e6
dt <- data.table(id = seq_len(n), date = seq(as.Date("0001-01-01"), len = n, by = "day") %>% as.character)

microbenchmark(dt[, .(id, date = as.Date(date))], times = 5) # 0.93 s

microbenchmark(dt[, .(id, date = as.IDate(date))], times = 5) # 0.93 s

microbenchmark(dt[, .(id, date = ymd(date))], times = 5) # 0.018 s

microbenchmark(dt[, .(id, date = fast_strptime(date, "%Y-%m-%d", lt = F) %>% as.IDate())], times = 5) # 0.006 s

# 结论：fast_strptime最快，但如果for less key stoke，推荐用ymd
# C语言的包速度比较快

mic = microbenchmark(mean(c(1:100)),times=10)
boxplot(mic)
autoplot(mic)
