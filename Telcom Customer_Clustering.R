library(clustMixType)
library(factoextra)
library(cluster)
library(ggplot2)
library(MASS)
library(kernlab)
library(kknn)
library(ClusterR)

telco <- read.csv("C:/Users/User/Desktop/NCCU/Multivariate Analysis/Final/WA_Fn-UseC_-Telco-Customer-Churn.csv")
telco=telco[-21]
#======Data Processing======
telco=telco[-c(1,8,10:15,20)]
telco$TotalCharges[is.na(telco$TotalCharges)]=0
telco$gender<-as.factor(telco$gender)
telco$SeniorCitizen<-as.factor(telco$SeniorCitizen)
telco$Partner<-as.factor(telco$Partner)
telco$Dependents<-as.factor(telco$Dependents)
telco$PhoneService<-as.factor(telco$PhoneService)
telco$InternetService<-as.factor(telco$InternetService)
telco$Contract<-as.factor(telco$Contract)
telco$PaperlessBilling<-as.factor(telco$PaperlessBilling)
telco$PaymentMethod<-as.factor(telco$PaymentMethod)



telco=as.data.frame(telco)

#====== Distance Test
test=as.data.frame(telco[,c(5,11)])
testmds<-cmdscale(dist(test),k=2,eig = TRUE)
nonmetricMDS<-isoMDS(dist,k=2)
#===== K-prototype=====
kprototype<-kproto(telco,k=3)
x<-kprototype$cluster

fviz_cluster(x,
  data=telco,
  geom = c("point", "text"))

# Initialize total within sum of squares error: wss
wss <- c()

# For 1 to 20 cluster centers
for (i in 1:10) {
  km.out <- kproto(telco, k = i)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}
wss
# Plot total within sum of squares vs. number of clusters
plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

kprototype<-kproto(telco,k=6)


#### ======= MDS =====


dist<-daisy(telco,metric="gower")
mds<-cmdscale(dist,eig=TRUE)
mds2<-cmdscale(dist,k = 3,eig = TRUE)
mds3<-cmdscale(dist,k = 4,eig = TRUE)
mds4<-cmdscale(dist,k = 5,eig = TRUE)
mds5<-cmdscale(dist,k = 6,eig = TRUE)
mds10<-cmdscale(dist,k=10,eig=TRUE)

wss=c()
for (i in 1:10) {
  km.out <- kmeans(mds10$points, centers  = i)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}
wss
# Plot total within sum of squares vs. number of clusters
plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

km<-kmeans(mds$points,centers = 4)
km10<-kmeans(mds10$points,centers = 8)

fviz_cluster(km,
             data=data.frame(mds$points),
             geom = c("point", "text"))

telco_cluster<- cbind(telco,km$cluster) 
colnames(telco_cluster)[12]<-('Cluster')

telco_cluster10<- cbind(telco,km10$cluster) 
colnames(telco_cluster10)[12]<-('Cluster')

sc<-silhouette(km$cluster,dist)
sc10<-silhouette(km10$cluster,dist)

#==== Spectral Clustering====
specluster<-specc(x=mds,centers=4,kernel="rbfdot")
spe<-specClust(mds$points)
spe10<-specClust(mds10$points)
sc_spe<-silhouette(spe$cluster,dist)
#===== kernel k-Means======
kk<-kkmeans(x=mds$points,centers=4,kernel="rbfdot",kpar = "automatic")
dist_mds<-dist(mds$points)
sc_kk<-silhouette(kk,dist_mds)
#====== MiniBatchK-Means======
MiniB<-MiniBatchKmeans(mds$points,clusters = 4,batch_size=20)
MninBatch<-predict_MBatchKMeans(mds$points,MiniB$centroids)
SC_MB<-silhouette(MninBatch,dist)
#=======Graph EDA After Clustring =====
telco_cluster$Cluster<-as.factor(telco_cluster$Cluster)



options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=gender,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=SeniorCitizen,fill=Cluster))+ 
  geom_bar(position = 'fill')


options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=Partner,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=Dependents,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=PaperlessBilling,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=PaymentMethod,fill=Cluster))+ 
  geom_bar(position = 'fill')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=PhoneService,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=InternetService,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=Contract,fill=Cluster))+ 
  geom_bar(position = 'fill')


options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=PhoneService,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster, aes(x=InternetService,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width =6, repr.plot.height = 2)
ggplot(telco_cluster, aes(y= tenure, x = "", fill = Cluster)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

options(repr.plot.width =6, repr.plot.height = 2)
ggplot(telco_cluster, aes(y= MonthlyCharges, x = "", fill = Cluster)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#==== Varibles Not In Clustering=====
telco2 <- read.csv("C:/Users/User/Desktop/NCCU/Multivariate Analysis/Final/WA_Fn-UseC_-Telco-Customer-Churn.csv")
telco_cluster2<- cbind(telco2,km$cluster) 
colnames(telco_cluster2)[22]<-('Cluster')
telco_cluster2$Cluster<-as.factor(telco_cluster2$Cluster)
telco_cluster2<-telco_cluster2[,-1]

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster2, aes(x=MultipleLines,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster2, aes(x=OnlineSecurity,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster2, aes(x=OnlineBackup,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster2, aes(x=DeviceProtection,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster2, aes(x=StreamingTV,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster2, aes(x=StreamingMovies,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width = 12, repr.plot.height = 8)
ggplot(telco_cluster2, aes(x=Churn,fill=Cluster))+ 
  geom_bar(position = 'fill')

options(repr.plot.width =6, repr.plot.height = 2)
ggplot(telco_cluster2, aes(y= TotalCharges, x = "", fill = Cluster)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")
