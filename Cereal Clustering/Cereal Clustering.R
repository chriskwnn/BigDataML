library(dummies)
cereal.df <-read.csv("/Users/chriskwan/Documents/R/RLabs/Cereals-1.csv")
#Remove rows with NA
cereal.df <-na.omit(cereal.df)
#Name rows
row.names(cereal.df) <- cereal.df[,1]
#Remove names row
cereal.df <- subset(cereal.df, select = -c(name))
#Create dummy variables
cereal.df.dummy <- cbind(cereal.df,dummy(cereal.df$mfr, sep = "_"),dummy(cereal.df$type, sep = "_"))
#normalize values
cereal.df.norm <- sapply(cereal.df.dummy[3:15],scale)
#insert mfr and type back into cereal.df.norm
cereal.df.norm<-cbind(cereal.df.dummy[16:24],cereal.df.norm)
head(cereal.df.norm)
#compute norm distances
d.norm<- dist(cereal.df.norm, method="euclidean")
#clustering
hc1<-hclust(d.norm,method="single")
hc2<-hclust(d.norm,method="complete")
plot(hc1, hang=-1,ann=FALSE)
plot(hc2, hang=-1,ann=FALSE)
clusters2<-data.frame(cutree(hc2,k=9))
colnames(clusters2) <- "cluster"
clusters2<-cbind(clusters2,cereal.df)
clusters21<-subset(clusters2,clusters2$cluster==1)
clusters22<-subset(clusters2,clusters2$cluster==2)
clusters23<-subset(clusters2,clusters2$cluster==3)
clusters24<-subset(clusters2,clusters2$cluster==4)
clusters25<-subset(clusters2,clusters2$cluster==5)
clusters26<-subset(clusters2,clusters2$cluster==6)
clusters27<-subset(clusters2,clusters2$cluster==7)
clusters28<-subset(clusters2,clusters2$cluster==8)
clusters29<-subset(clusters2,clusters2$cluster==9)
clusters21
clusters22
clusters23
clusters24
clusters25
clusters26
clusters27
clusters28
clusters29
x1<-colMeans(clusters21[4:16])
x2<-colMeans(clusters22[4:16])
x3<-colMeans(clusters23[4:16])
x4<-colMeans(clusters24[4:16])
x5<-colMeans(clusters25[4:16])
x6<-colMeans(clusters26[4:16])
x7<-colMeans(clusters27[4:16])
x8<-colMeans(clusters28[4:16])
x9<-colMeans(clusters29[4:16])
cluster2mean.df<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9)
colnames(cluster2mean.df)<-c("Bran and Fibre","Bran","Higher Sugar/For Children","Nut based and Raisin","Wheat Based","Healthier","Diet","Corn and Rice Based","Puffed")
cluster2mean.df



