
long_lat = read.table("Long_lat.txt", sep="\t", header=T, row.names=1)

#
library(Imap)
# calculate by latitude and longitude
n = nrow(env.st2)
geodis = matrix(0, n, n)
for(i in c(1:n)){
	for(j in c(1:n)){
		geodis[i, j] = gdist(long_lat[i,1], long_lat[i,2], long_lat[j, 1], long_lat[j,2], units = "m") # Long1, Lat1, Long2, Lat2)
	}
}
geodis = as.dist(t(geodis))

