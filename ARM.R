library(arules)
setwd("D:\\Others\\XLRI\\Sessions\\Decision Making - SKDe")
asso<-read.csv("assodatasem.csv", header=TRUE)
asso

#converting as transaction data
assotr<-as(split(asso$PRODUCT, asso$CUSTOMER), "transactions")
assotr

#Mine frequent itemsets with the Eclat algorithm
?eclat
ec<-eclat(assotr, parameter = list(support=0.1))
inspect(ec)

ec1<-sort(ec,by="support", decreasing = TRUE)
ec1

top10<-head(ec1, n=10)
inspect(top10)

inspect(ec1)

?itemFrequencyPlot
itemFrequencyPlot(assotr, topN=10)

?apriori
mod<-apriori(assotr,parameter=list(support=.2,confidence=.7,minlen=2))

inspect(mod)

library("arulesViz")
plot(mod,method="graph")


install.packages("rattle")
install.packages("RGtk2")
library("rattle")
rattle()
  
