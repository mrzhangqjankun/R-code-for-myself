#2019.6.10
#ANCOM

#install.packages("remotes")
#remotes::install_github("ZRChao/LRTT")

#可用于比较两个或多个种群的微生物群落组成。
#ANCOM不做分布假设，可以在线性模型框架中实现

library(LRTT)
?ANCOM.Sim

data <- ANCOM.Sim(p = 50, N = 50, dif = c(1:10))
dim(data)

ANCOM.Sim()

setwd("E:/桌面/test_data/")
suppressMessages(library(vegan))

x<-read.table(file="otu.tabular",sep="\t",header=T,row.names=1)

da <- ANCOM.Sim(x, N = 10, dif = c(1:5))

#源代码
#############################################################################
###            ANCOM: Poission distribution simulation                  #####
###   Each OTU on the leaf is sampled from poission distribution.       #####
###   Correspond to the tree structure, the differential OTU on the     #####
###   tree leafs have different mu for poission distribution otherwise  #####
###   follow the same mu. The results is the count data on leafs and    #####
###   colname is correspond to the tree structure. All the parameters   #####
###   set accroding to ANCOM simulation.                                #####
#############################################################################

#-----------------------------------------------------------------------#####

ANCOM.Sim = function(p, seed = 1, N, dif = diff_leaf){
  control <- case <- matrix(NA, N, p)
  
  for(i in 1:N){
    a <- 200
    for(o in 1:p){
      A <- c()
      B <- c()
      for(n in 1:N){
        set.seed((o + n)*i*seed)
        mu <- rgamma(1, a, 1)
        set.seed((o*n)*i*seed)
        A[n] <- rpois(1, mu)
        
        if(o %in% dif){
          ul <- 200
          set.seed((o + n)*i*seed + 2)
          u <- runif(1, ul, 3*ul/2)
          set.seed((o + n)*i*seed + 3)
          B[n] <- rpois(1, mu + u)
        }
        else{
          set.seed((o + n)*i*seed + 4)
          B[n] <- rpois(1, mu)
        }
      }
      case[, o] <- B
      control[, o] <- A
    }
  }  
  ancom_otutab <- rbind(case, control)
  colnames(ancom_otutab) <- as.character(1:p)
  return(ancom_otutab)
}

#############################################################################

#reference
#https://rdrr.io/github/ZRChao/LRTT/src/R/ANCOM.Sim.R



library(MiSPU)
data(throat.otu.tab)
data(throat.tree)
data(throat.meta)


p <- ncol(throat.otu.tab)
# throat.taxa.index <- Taxa.index(p, throat.tree)
data(throat.taxa.index )
colnames(throat.otu.tab) <- as.character(1:p)
throat.taxa.tab <- as.matrix(throat.otu.tab) %*% throat.taxa.index
throat.alltab <- cbind(throat.taxa.tab,  throat.otu.tab)
group <- throat.meta$SmokingStatus
                             
result <- Tree.ratio(p, throat.tree, throat.taxa.index, throat.alltab, group)
                             
throat.detected <- Tree.ratio.back(p, result)
                             