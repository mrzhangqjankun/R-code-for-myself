#2019.3.10
#https://academic.oup.com/bioinformatics/advance-article-abstract/doi/10.1093/bioinformatics/btz159/5370175?redirectedFrom=fulltext
#aphid 隐马尔可夫模型分析

install.packages("aphid")
library(aphid) 
?aphid ##HMM models
?kmer  ##计算kmer的包


## the dishonest casino example from Durbin et al (1998)
states <- c("Begin", "Fair", "Loaded")
residues = paste(1:6)
A <- matrix(c(0, 0, 0, 0.99, 0.95, 0.1, 0.01, 0.05, 0.9), nrow = 3)
dimnames(A) <- list(from = states, to = states)
E <- matrix(c(rep(1/6, 6), rep(1/10, 5), 1/2), nrow = 2, byrow = TRUE)
dimnames(E) <- list(states = states[-1], residues = residues)
x <- structure(list(A = A, E = E), class = "HMM")
plot(x, main = "Dishonest casino hidden Markov model")
