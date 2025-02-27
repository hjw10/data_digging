setwd("C://Users//penguin//Desktop//学习资料//数据挖掘//Lab3")

#k-means
km <- kmeans(iris[,-5],3)
table(iris$Species,km$cluster)
plot(iris[,-5],col=km$cluster)

#k-medoids
library(cluster)
#pam小数据集
pc <- pam(iris[,-5],3,medoids = c(2,3,8))
table(iris$Species,pc$cluster)
plot(iris[,-5],col=pc$clustering)
#clara大数据集
clac <- clara(iris[,-5],3)
table(iris$Species,clac$cluster)
plot(iris[,-5],col=clac$clustering)

library(fpc)
pkc <- pamk(iris[,-5])
table(iris$Species,pkc[[1]]$clustering)
plot(iris[,-5],col=pkc[[1]]$clustering)

#层次聚类
library(stats)
hc <- hclust(dist(iris[,-5]),method="ave")
group<-cutree(hc,k=4)
plot(hc,hang=-1,labels=group)
rect.hclust(hc,k=7)
#dbscan
library(dbscan)
db <- dbscan(iris[,-5],eps=0.42,MinPts=5)
db$cluster = db$cluster + 1
table(iris$Species,db$cluster)
plot(db,iris[,-5])
plot(iris[,-5],col=db$cluster)


id <- sample(1:nrow(iris),10)
newdata <- iris[id,-5]
newpre <- predict(db,iris[,-5],newdata)
table(newpre,iris$Species[id])
plot(db,newdata)



#som
library(som)



