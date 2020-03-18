custseg<-read.csv("D:\\Others\\XLRI\\Sessions\\Decision Making - SKDe\\customersseg.csv",header=TRUE)
summary(custseg)
cust<-custseg

#dropiing Cust id and Gender
custseg$CustomerID<-NULL
custseg$Gender<-NULL
head(custseg)

#z-transform scaling
custseg$Age<-scale(custseg$Age)
custseg$Annual.Income..k..<-scale(custseg$Annual.Income..k..)
custseg$Spending.Score..1.100.<-scale(custseg$Spending.Score..1.100.)
summary(custseg)

ncl<-0
for(i in 2:15)
{
  ncl[i]<-kmeans(custseg, centers = i,nstart=30)$tot.withinss
}
ncl
plot(1:15,ncl,type="b")

km<-kmeans(custseg,4,nstart=40)
km$size

ndata<-cbind(cust,km$cluster)
head(ndata)
write.csv(ndata, "cust_seg_clust.csv")

#boxplot(Gender~km$cluster, ndata) # cannot plot Categorical var
boxplot(Age~km$cluster, ndata)
boxplot(Annual.Income..k..~km$cluster, ndata)
boxplot(Spending.Score..1.100.~km$cluster, ndata)

clus1<-subset(ndata,km$cluster==1)
summary(clus1)

clus2<-subset(ndata,km$cluster==2)
summary(clus2)

clus3<-subset(ndata,km$cluster==3)
summary(clus3)

clus4<-subset(ndata,km$cluster==4)
summary(clus4)

### Label the clusters for business targets

write.csv(clus4, "D:/Others/Documents/XLRI/Sessions/Decision Making - SKDe/clus4.csv")
