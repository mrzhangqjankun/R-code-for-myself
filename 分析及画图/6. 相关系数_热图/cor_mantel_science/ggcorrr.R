#2019.9.24
gc()
##ggcorrr   html

if(!require(devtools))
  install.packages("devtools")
if(!require(gt))
  devtools::install_github("rstudio/gt")
# install `ggcorrr`
devtools::install_github("houyunhuang/ggcorrr")

library(ggcorrr)
