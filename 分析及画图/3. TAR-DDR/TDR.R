library(ieggr)
library(mCAM)
library(vegan)

setwd("G:/IEG/2018.6-Delong-data/GeoChip/TDR-fengkai program/Gepchip all")

com.file="b.csv"
treat.file="treatment.csv"
time.file="time.points.csv"

# load data, set work directory in res.wd
comm=t(lazyopen(com.file))
treat=lazyopen(treat.file)

times=lazyopen(time.file)
comm [is.na(comm)]<-0

# match sample ID
sampc=ieggr::match.name(rn.list=list(comm=comm,treat=treat,times=times))
comm=sampc$comm
dim(comm)
comm=comm[,colSums(comm)>0] # sometimes, removing samples will make some OTUs have no read across remained samples.
dim(comm)
treat=sampc$treat

times=sampc$times

# match OTU ID
#spc=match.name(cn.list=list(comm=comm))

##abundance.weighted = TRUE(=FALSE,Sorensen)

time.decay=tdecay(time=times, comm = comm, dist.method = "bray", abundance.weighted = TRUE,
                  treat=treat, perm.test = TRUE, boot.strap = TRUE, rand = 999, Dmax = 1)
save.file(time.decay$summary, filename = "TimeDecay all bray.summary")
save.file(time.decay$data, filename = "TimeDecay all bray.Details")
save.file(time.decay$comparison, filename = "TimeDecay all bray.Comparison")


  

