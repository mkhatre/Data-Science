# http://tech.nitoyon.com/en/blog/2013/11/07/k-means/
getwd()
setwd("D:\\Others\\XLRI\\Sessions\\Decision Making - SKDe")
getwd()
#rfm<-read.csv("rfm.csv", header=TRUE)
rfm<-read.csv("full_rfm.csv", header=TRUE)#full_rfm
rfm1<-rfm # make a copy
summary(rfm)

#scaling as data in differen tvariables are in different range
rfm$Cust.ID<-NULL
rfm$Recency<-scale(rfm$Recency)
rfm$Freq<-scale(rfm$Freq)
rfm$Monetory<-scale(rfm$Monetory)
summary(rfm)

ncl<-0
for(i in 2:20)
{
  ncl[i]<-kmeans(rfm, centers = i,nstart=30, iter.max=50)$tot.withinss
}
ncl
plot(1:20, ncl, type="b")

set.seed(12345)
km<-kmeans(rfm,6,nstart=40)
km$size

ndata<-cbind(rfm1,km$cluster) # find all the cluster from rfm1 which has original data
summary(ndata)
write.csv(ndata, "cluster_data_rfm.csv")

boxplot(Recency~km$cluster, ndata) # group by km$cluster
boxplot(Freq~km$cluster, ndata)
boxplot(Monetory~km$cluster, ndata)

clus1<-subset(ndata,km$cluster==1)
head(clus1)
summary(clus1)
paste ("Count of rows: ", nrow(clus1))
write.csv(clus1, "clus1.csv")

clus2<-subset(ndata,km$cluster==2)
summary(clus2)
paste ("Count of rows: ", nrow(clus2))

clus3<-subset(ndata,km$cluster==3)
summary(clus3)
paste ("Count of rows: ", nrow(clus3))

clus4<-subset(ndata,km$cluster==4)
summary(clus4)
nrow(clus4)
paste ("Count of rows: ", nrow(clus4))

clus5<-subset(ndata,km$cluster==5)
summary(clus5)
paste ("Count of rows: ", nrow(clus5))

clus6<-subset(ndata,km$cluster==6)
summary(clus6)
paste ("Count of rows: ", nrow(clus6))

write.csv(clus3, "clus3.csv")
write.csv(clus2, "clus2.csv")
write.csv(clus4, "clus4.csv")
