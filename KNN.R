clus <- c(1,10,12,3,2,20,11,15,25)
mean(clus)
set.seed(1234)
km <- kmeans(clus, 3, nstart = 30)
?kmeans
km
km$cluster
km$centers
km$totss
km$withinss
km$tot.withinss
km$betweenss
km$size
###############

km2 <- kmeans(clus,2,nstart=30)
km
km4 <- kmeans(clus,4,nstart=30)
km5 <- kmeans(clus,5,nstart=30)
km2$tot.withinss
km$tot.withinss
km4$tot.withinss
km5$tot.withinss
######################
ncl<-0
for(i in 2:6)
{
  ncl[i]<-kmeans(clus, centers = i,nstart=30)$tot.withinss
}
ncl
plot(1:6,ncl,type="b")
##############################