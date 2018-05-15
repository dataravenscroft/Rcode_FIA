#Catherine Ravenscroft 
#Method to extract FIA plot soils data in eastern USA
# Averages soil from samples within individual plots (multiple/plot)
# averages from reported values different soil horizons.


library(reshape)
library(nortest)
library(moments)
library(vegan)
library(fossil)


##READ IN PLOT DATA
#setwd("G:/Projects/FIA_TRAIT/DATA/")
#coords = read.csv(file.choose())
coords = read.csv("plot.coords.csv")

#Check to see plots read correctly
#xy.plot = xy.coords(coords$LON, coords$LAT)
#plot(xy.plot)


## clean up soil data

# import data
soil= read.csv("soil.csv")[,-1]

# remove extra columns
soils= soil[,c(2,9,20,22,23,26,27,30,34,35)]
head(soils)

# look at layers measured in each plot
layers= table(soils$PLT_CN, soils$LAYER_TYPE)
melt(apply(layers,1,sum))

# keep only mineral layer 1
soils= soils[(soils$LAYER_TYPE=="MIN_1"), ]

# calculate plot means
plot.mean= function(i) { tapply(soils[,i], soils$PLT_CN, mean) }
plot.soil= sapply(3:10, function(i) plot.mean(i))
colnames(plot.soil)= names(soils)[3:10]

# search for NAs 
find.na= apply(plot.soil, 2, function(i) which(is.na(i)))
count.na= apply(plot.soil, 2, function(i) length(which(is.na(i))))
plot.na= melt(apply(plot.soil, 1, function(i) sum(is.na(i))))

# remove plots with 3 or more NAs
goodplot= rownames(plot.na)[which(plot.na<3)]
plot.soil= plot.soil[rownames(plot.soil) %in% goodplot, ]

# replace NAs with column means
for(i in 1:8)	{ 
  plot.soil[,i][is.na(plot.soil[,i])]= mean(plot.soil[,i], na.rm=T)
}

# graph histograms of all variables
par(mfrow=c(2,4), mar=c(1,1,1,1), oma=c(1,1,1,1))
for(i in 1:8)	{
  hist(plot.soil[,i], xaxt="n", yaxt="n", main=colnames(plot.soil)[i],
       cex.main=0.8)
}

# graph kernel density plots
par(mfrow=c(2,4), mar=c(1,1,1,1), oma=c(1,1,1,1))
for(i in 1:8)	{
  plot(density(na.omit(plot.soil[,i])), xaxt="n", yaxt="n", cex.main=0.8,
       main=colnames(plot.soil)[i])
}

# check skewness and normality of raw, sqrt, and log-transformed data
log.means= log(plot.soil+1)
sqrt.means= sqrt(plot.soil)

skew= sapply(list(plot.soil, sqrt.means, log.means), function(i) 
  sapply(1:ncol(i), function(j) skewness(i[,j])))
colnames(skew)= c("untr","sqrt","log")

sw.test= sapply(list(plot.soil, sqrt.means, log.means), function(i) 
  sapply(1:ncol(i), function(j) shapiro.test(i[,j])[[2]]))
colnames(sw.test)= c("untr","sqrt","log")

# use best transformation for each variable
plot.soil= cbind(log.means[,c(1:3,5:8)],sqrt.means[,4])
colnames(plot.soil)[8]= colnames(sqrt.means)[4]




