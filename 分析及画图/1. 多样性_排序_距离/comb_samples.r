
setwd('V:/my work in OU/All eCO2 samples/180samples')

rm(list=ls(all=TRUE))
fg.dat = read.table("all180_corrected.txt", sep="\t", row.names=1, header=T)

samp.name = colnames(fg.dat)

samp.pair = read.table("sample_combine.txt", sep="\t", header=T)

left.col = c()
for(i in 1:nrow(samp.pair)){
	left.col = c(left.col, as.character(samp.pair[i, 1]))
	if(as.character(samp.pair[i, 1]) != as.character(samp.pair[i, 2])){
		samp1 = fg.dat[,c(as.character(samp.pair[i, 1]))]
		samp2 = fg.dat[,c(as.character(samp.pair[i, 2]))]
		col.avg = rowMeans(cbind(samp1,samp2),na.rm=TRUE)
		fg.dat[,c(as.character(samp.pair[i, 1]))] = col.avg
	}
}

fg.dat2 = fg.dat[,left.col]

# scaling the data
colMean = colMeans(fg.dat2, na.rm=TRUE)
fg.dat3 = fg.dat2/colMean

