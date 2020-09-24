##用R玩一个小游戏吧！拖延症的老毛病又犯了！ 
##2018.6.21
##Rcade
##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247486077&idx=1&sn=aba1db6ecc708ebb760282b60750ae4e&chksm=ec43bb3adb34322c592ca22c268044a88fce65d2730dc51b0e20b0e20e35dc0aee7be8877298&mpshare=1&scene=1&srcid=0620O2g7DmPN3ImzqaMcE6OZ&pass_ticket=7Wr8HH817FPWW%2F%2BpeXEXkT%2BkfHyuaSvGUXXfUqj3yToWgE4iq3%2FbboLmjZJ2njGO#rd


rm(list=ls())
library(devtools)
devtools::install_github("RLesur/Rcade")

Rcade::games
Rcade::games$`2048`
Rcade::games$SpiderSolitaire  ##蜘蛛纸牌
Rcade::games$SURVIVOR
Rcade::games$CathTheEgg  ##接鸡蛋
Rcade::games$BoulderDash
