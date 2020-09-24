# Canonical Correspondence Analysis
##li 2017.7.12  和statistics中的相同
suppressMessages(library(vegan))
library(getopt)

# Make an option specification, with the following arguments:
# 1. long flag
# 2. short flag
# 3. argument type: 0: No argument, 1: required, 2: optional
# 4. target data type
option_specification = matrix(c(
  'file_in', 'f', 1, 'character',
  'en_file', 'f1', 2, 'character',
  'output1',  'o1', 1, 'character',
  'output2',  'o2', 1, 'character',
  'output3',  'o3', 1, 'character',
  'plotfile',  'p', 1, 'character'
), byrow=TRUE, ncol=4);
# Parse options
options1 = getopt(option_specification);
inputFile <- options1$file_in
en_file <- options1$en_file
cca_result <- options1$output1
inflation_factors <- options1$output2
Fp_value  <- options1$output3
plotFile <- options1$plotfile

x<-read.table(file=inputFile,sep="\t",header=T,row.names=1)  
y<-read.table(file=en_file,sep="\t",header=T,row.names=1)  
y[is.na(y)] <- 0
x.cca<-cca(t(x),y)
output1 = summary(x.cca)

inf_factor = vif.cca(x.cca)
output2 = inf_factor

ind.p = array(0,dim=c(1,ncol(y)))
ind.F = array(0,dim=c(1,ncol(y)))
for(j in 1:ncol(y)){
  ind.cca = cca(t(x)~y[,j], data=y)        #ind.cca = cca(fg.dat, env.st[,j], env.st[,-j])  #
  ind.sig = anova(ind.cca,step=1000)
  ind.p[1,j] = ind.sig$Pr[1]
  ind.F[1,j] = ind.sig$F[1]
}
colnames(ind.p) = colnames(y)
Fp = rbind(ind.F, ind.p)
rownames(Fp) = c("F","p")
output3 = Fp

     
x.sig = anova(x.cca)
x.p = x.sig$Pr[1]
png(plotFile)
plot(x.cca,dis=c('wa','cn'), main = paste("(p=",x.p,")",sep=''))
dev.off()  


sink(cca_result)
print(output1)

sink(inflation_factors)
print(output2)

sink(Fp_value)
print(output3)
sink()	






