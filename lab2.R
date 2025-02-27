setwd("C://Users//penguin//Desktop//学习资料//数据挖掘//Lab2")

############
ind <- sample(2,nrow(iris),replace = TRUE,prob = c(0.7,0.3))
trainSet <- iris[ind == 1,]
testSet <- iris[ind ==2,]


#朴素贝叶斯
library(e1071)
nby <- naiveBayes(Species~.,trainSet)
pre_nby <- predict(nby,testSet)
table(testSet$Species,pre_nby)
confusionMatrix(testSet$Species,pre_nby)


#BP网
library(nnet)
class.ind(iris$Species) -> C

bp <- nnet(trainSet[,1:4],class.ind(trainSet$Species),size = 1, rang=0.1, decay=5e-4,maxit=300)
pre_bp <- predict(bp,testSet[,1:4])
pre_bp <- apply(pre_bp,1,function(x) colnames(pre_bp)[which.max(x)])
table(testSet$Species,pre_bp)
confusionMatrix(testSet$Species,pre_bp)


#svm
library(e1071)
svmclass <- svm(Species~.,trainSet,na.action = na.omit,kernel = "sigmoid")
pre_svm <- predict(svmclass,testSet)
table(testSet$Species,pre_svm)
confusionMatrix(testSet$Species,pre_svm)

#BP newff
library(AMORE)
trainSet$Species <- class.ind(trainSet$Species)
bp_newff <- newff(n.neurons = c(dim(trainSet[,1:4])[2],3,dim(trainSet$Species)[2]),learning.rate.global = 0.0001,momentum.global = 0.001,Stao = NA, hidden.layer = "sigmoid",output.layer = "sigmoid",method = "ADAPTgdwm")
bp_train <- train(bp_newff,trainSet[,1:4],trainSet$Species,error.criterium = "LMS",report = TRUE,n.shows = 20,show.step = 10000)
bp_sim <- sim(bp_train$net,testSet[,1:4])

bp_sim_out <- apply(bp_sim,1,function(x) c("setosa","versicolor","virginica")[which.max(x)])
table(testSet$Species,bp_sim_out)

#knn
library(kknn)
knn <- kknn(Species~.,trainSet,testSet,distance = 2) 
fit.kknn <- fitted(knn)
table(testSet$Species, fit.kknn)

#ROC plot
library(gplots)
library(ROCR)
score<-c(0.9,0.8,0.7,0.6,0.55,0.54,0.53,0.51,0.50,0.40)
label<-c(1,1,-1,1,1,-1,-1,-1,1,-1)
pred<-prediction(score,label)
perf<-performance(pred,"tpr","fpr")
AUC<-performance(pred,"auc")
 wa<-AUC@y.values[[1]]
plot(perf,col="blue")


