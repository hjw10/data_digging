setwd("C:\\Users\\penguin\\Desktop\\学习资料\\数据挖掘\\lab")

data("iris")
ind <- sample(2,nrow(iris),replace = TRUE,prob = c(0.7,0.3))
trainSet <- iris[ind == 1,]
testSet <- iris[ind ==2,]

#J48
treeC45 = J48(Species~.,data = trainSet)

table(trainSet$Species,predict(treeC45))
testPre <- predict(treeC45,newdata = testSet)
table(testPre,testSet$Species)
plot(treeC45)

#tree
treeTree <- tree(Species~.,data = trainSet)
plot(treeTree)

treePre <- predict(treeTree,newdata = testSet)
treePre <- apply(treePre,1,function(x){names(x)[which.max(x)]})
table(treePre,testSet$Species)

#ctree
treeC <- ctree(Species~.,data = trainSet)
plot(treeC,type = "simple")

CPre <- predict(treeC,newdata = testSet)
table(CPre,testSet$Species)

#RandomForest
rf <- randomForest(Species~.,data = trainSet,ntree = 100)
rfPre <- predict(rf,newdata = testSet)
table(rfPre,testSet$Species)

#K-CV
k = 5
setosa <- iris[which(iris$Species == "setosa"),]
versicolor <- iris[which(iris$Species == "versicolor"),]
virginica <- iris[which(iris$Species == "virginica"),]
ind <- sample(5,nrow(setosa),replace = TRUE,prob = c(0.2,0.2,0.2,0.2,0.2))


for(i in 1:k){
  testSet <- rbind(setosa[ind == i,],versicolor[ind == i,],virginica[ind == i,])
  trainSet <- rbind(setosa[ind != i,],versicolor[ind != i,],virginica[ind != i,])
  
  
}

#confusionMatrix
iris$Species <-as.factor(iris$Species)
confusionMatrix(testPre,testSet$Species)



  
  
