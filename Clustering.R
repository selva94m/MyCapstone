data=read.csv(file.choose())

#install.packages("dummies")
library(dummies)
str(data)
d<-data[,-c(6:8)]
d$Call.Drop.Category<-as.numeric(d$Call.Drop.Category)
d$Rating<-NULL
#d$State.Region<-NULL
#d$Customer.id<-NULL
str(d)
names(d)


 d.new <- dummy.data.frame(d, sep = ".")
 names(d.new)
 
 d.new$Operator.BSNL<-NULL
 d.new$Indoor_Outdoor_Travelling.Travelling<-NULL
 d.new$Network.Type.Unknown<-NULL
 d.new$State.Region.West<-NULL
 d.new$Customer.id<-NULL
 dim(d.new)
 
 kdata=d.new
 # non-hierarchical clustering - Kmeans
 
 ## Identifying the optimal number of clusters form WSS
 
 wssplot <- function(kdata, nc=25, seed=123){
   wss <- (nrow(kdata)-1)*sum(apply(kdata,2,var))
   for (i in 2:nc){
     set.seed(seed)
     wss[i] <- sum(kmeans(kdata, centers=i)$withinss)
   }
   plot(1:nc, wss, type="b", xlab="Number of Clusters",
        ylab="Within groups sum of squares")}
 
 
 wssplot(kdata, nc=25)
 
 ## Identifying the optimal number of clusters
 
 library(NbClust)
 
 set.seed(123)
 kmeans.clus = kmeans(x=kdata, centers = 5, nstart = 25)
 kmeans.clus$withinss
 kmeans.clus$size
 kmeans.clus
 kdata$Clusters <- kmeans.clus$cluster
 aggr1 = aggregate(kdata,list(kdata$Clusters),mean)
 aggr1
 k.clus.profile <- data.frame(Cluster=aggr1[,1],
                              Freq=as.vector(table(kdata$Clusters)),
                              aggr1[,-1])
 write.csv(k.clus.profile,file="clusprofile.csv")
 d.new.1<-data
 d.new.1$cluster<-kmeans.clus$cluster
library(cluster)
table(kmeans.clus$cluster)
clusplot(kdata, kmeans.clus$cluster,color=TRUE,lines=2,shade=TRUE)
write.csv(d.new.1,file="kmeans_output.csv")

result.kmean.mm <- table(data$Call.Drop.Category, kmeans.clus$cluster)
purity.kmean <- sum(apply(result.kmean.mm, 2, max)) / nrow(d.new)
purity.kmean




require(caTools)  
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(d,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 =subset(d,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test1=subset(d, sample==FALSE)


train=dummy.data.frame(train1, sep = ".")
names(train)

train$Operator.Idea<-NULL
train$Indoor_Outdoor_Travelling.Travelling<-NULL
train$Network.Type.Unknown<-NULL
train$State.Region.West<-NULL

test=dummy.data.frame(test1, sep = ".")
names(test)

test$Operator.Idea<-NULL
test$Indoor_Outdoor_Travelling.Travelling<-NULL
test$Network.Type.Unknown<-NULL
test$State.Region.West<-NULL

scale(train)
scale(test)
require("class")
model1<- knn(train=train, test=test, cl=train$Customer.id, k=4)

output=table(test$Customer.id, model1)
write.csv(output,"Outputknn.csv")


?kmode
install.packages("klaR")
library(klaR)
str(kmodes_data)

kmodes_data=data
kmodes_data$Rating<-as.factor(kmodes_data$Rating)
kmodes_data$Latitude<-as.factor(kmodes_data$Latitude)
kmodes_data$Longitude<-as.factor(kmodes_data$Longitude)

?kmodes
kmodes_data$Customer.id<-NULL
kmode.clus=kmodes(kmodes_data,5)

kmodes_data$cluster<-kmode.clus$cluster
library(cluster)
table(kmode.clus$cluster)
clusplot(kmodes_data, kmode.clus$cluster,color=TRUE,lines=2,shade=TRUE)
write.csv(kmodes_data,file="kmodes_output.csv")


install.packages("cba")
library(cba)
data1=data
subset(data, select=-c(Customer.id))
str(data1)
data1$Customer.id<-NULL
model_data<-model.matrix(~.-1, data=data1)
str(model_data)



set.seed(20) 
result.kmean = kmeans(model_data, 2, nstart = 50, iter.max = 15) 
result.kmean.mm <- table(data$Call.Drop.Category, result.kmean$cluster)
purity.kmean <- sum(apply(result.kmean.mm, 2, max)) / nrow(data1)
purity.kmean

rock<-data[,-c(6:7)]
dim(rock)
rock$Rating<-NULL
rock$Customer.id<-NULL
str(rock)
rock1<-as.dummy(rock)
#data
str(rock1)
rockCluster(rock1, 2, theta=0.5,fun="dist",funArgs = list(method="binary"))
rockLink(rock1,beta=0.5)

