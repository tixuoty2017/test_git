url="http://vlsstats.ifsttar.fr/rawdata/RawData/data_all_Paris.jjson"
data<-fromJSON(sprintf("[%s]", paste(readLines(url), collapse=",")))
data[[10]]

url="http://vlsstats.ifsttar.fr/data/input_Paris.json"
stations<-fromJSON(sprintf("[%s]", paste(readLines(url), collapse=",")))
stations<-stations[[1]]
stations$lat<-stations$position$lat
stations$lon<-stations$position$lng

fulldt <-as.data.frame(data[1])
fulldt<-merge(fulldt, stations[,c(1,13,14)],by="number")

fulldt$prop<-data[[1]][8]/data[[1]][4]
#for (i in 2:length(data))
for (i in 2:20)
{
  temp<-as.data.frame(data[i])
  temp$prop<-data[[i]][8]/data[[i]][4]
  fulldt <- merge(fulldt, temp[,c(5,9)], by="number")
}


#CAH
t2[[2]]

t2<-fulldt[,-(1:10)]
t3<-as.data.frame(lapply(t2, FUN=cbind))
t4<-as.matrix(t3)

library(cluster)
CAH <- agnes(t3, diss=F, metric="euclidian", stand=T, method="ward")
plot(CAH)

#k-means

reskmeans<-kmeans(t4, centers = 3, nstart=1)
summary(reskmeans)
reskmeans$cluster

require(RgoogleMaps)
center<-c(mean(range(fulldt[,9])),mean(fulldt[,10]))
zoom<-MaxZoom(range(fulldt[,9])*1.1,range(fulldt[,10])*1.1 )
carte<-GetMap(center=center, zoom=zoom)
PlotOnStaticMap(carte, lat=fulldt[,9], lon=fulldt[,10], pch=16, cex=2, col=as.numeric(reskmeans$cluster))

