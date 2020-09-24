###2019.3.24
#compute.es for cohen's d-statistic
#calculates Cohen's d, Hedges' g, and the effect size (ES) correlation r corrected for uneven cell sizes.
install.packages("compute.es")
library(compute.es);??compute.es

####Example
## 1. Computations to Calculate Effect Sizes:

# For example, suppose the primary study reported a t-test 
# value for differences between 2 groups. Then, running:

tes(t=1.74, n.1=30, n.2=31)

# Or, more simply:

tes(1.74, 30, 31)  

# where the reported t-value = 1.74, treatment sample 
# size = 30, and the control/comparison sample size = 31 will
# output effect sizes of d, g, r, z, OR, and log odds ratio. 
# The variances, confidence intervals, p-values and other 
# statistics will also be computed.
# Note: If only the total sample size is reported simply split 
# the number in half for entry into the function. 

# Now suppose one has a dataset (i.e., data.frame in R-speak)
# with several t-values to be converted into effect sizes:

# First, we will generate sample data:

dat <- data.frame(id=1:5,t=rnorm(5, 2, .5), 
                  n.t=round(rnorm(5, 25),0), 
                  n.c=round(rnorm(5, 25),0))

# Running the fuction as follows will generate a new 
# data.frame with several effect size estimates

tes(t=t, n.1=n.t, n.2=n.c, level=95, dig=2, id=id, data=dat)


####直接用函数计算
#https://stackoverflow.com/questions/15436702/estimate-cohens-d-for-effect-size
set.seed(45)                        ## be reproducible 
x <- rnorm(10, mean=10, sd = 1)                
y <- rnorm(10, mean = 5, sd = 5)

t.test(x,y)  #t = 1.9202
tes(t=1.9202,n.1=10,n.2=10)

cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
}
 res <- cohens_d(x, y)
 res
# [1] 0.5199662


###lsr这个包也可以
install.packages("lsr")
library(lsr);?lsr
set.seed(45)
x <- rnorm(10, 10, 1)
y <- rnorm(10, 5, 5)
cohensD(x,y)
# [1] 0.5199662

###sffsize也可以
install.packages("effsize")
library(effsize); ?effsize
set.seed(45) 
x <- rnorm(10, 10, 1) 
y <- rnorm(10, 5, 5) 
cohen.d(x,y)
# Cohen's d
# d estimate: 0.5199662 (medium)
# 95 percent confidence interval:
#        inf        sup 
# -0.4353393  1.4752717

###pwr也可以
install.packages("pwr")
library(pwr);?pwr
pwr.t.test(d=0.5199662,n=10,sig.level=0.05,type="paired",alternative="two.sided")

cohen.ES(test="r", size="medium")


##Wiki
#https://en.wikipedia.org/wiki/Effect_size#Cohen's_d