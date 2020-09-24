# run tnull function with OTU table and treatment file
require(vegan)
require(parallel)
# read OTU table and treatment files
setwd("E:/桌面/null/")
com.file= read.table("OTU_table.txt",sep="\t",header=T,row.names=1)
treat=read.table("treatment.txt",sep="\t",header=T,row.names=1)

comm=t(com.file)

# stochasticity based on taxonomic null model analysis

source("taxnull_function.r")

tnull.res=taxnull(comm = comm, treat = treat, dist.method = "bray",abundance.weighted = TRUE,
                rand = 1000, nworker = 4, trace = TRUE, betadisper.type = "centroid")
write.csv(tnull.res$RC, "TaxoNull.RC.csv")
write.csv(tnull.res$SES, "TaxoNull.SES.csv")
write.csv(tnull.res$ST.Ratio, "TaxoNull.ST.Ratio.csv")
write.csv(tnull.res$summary, "TaxoNull.summary.csv")
write.csv(tnull.res$PERM.test, "TaxoNull.Comparison.csv")

