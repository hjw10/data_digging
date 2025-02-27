setwd("C://Users//penguin//Desktop//学习资料//数据挖掘//bigwork")
load("big.Rdata")
save.image("big.Rdata")

#数据读入和预处理用的是老师的代码
data<-read.table('GSE4498_series_matrix.txt',skip=74,sep='\t',row.names=1,header=T,fill=T)
anno_data<-read.table('GPL570-55999.txt',skip=16,sep='\t',header=T,fill=T,quote='\"',row.names=1)

anno_data<-anno_data[,c(1,10)]

#如有一个探针对应多个基因，保留第一个基因
anno_data$Gene.Symbol<-gsub(' .+$','',anno_data$Gene.Symbol)
#将探针和基因对应
expr<-merge(anno_data,data,by=0,all=F)
row.names(expr)<-expr[,1]
expr<-expr[-1:-2]

#一个基因对应多个探针，对探针求均值
expr<-aggregate(expr[,2:23], list(expr$Gene.Symbol), mean)
expr<-expr[-1,]
#去掉没有名字的基因
na_row<-which(expr[,1]=="") 
row.names(expr)<-expr[,1]
expr<-expr[-1]

#fold change 提取特征基因1-12不吸13-22吸
FoldChange = c()
for(i in 1:length(rownames(expr))){
  FC = (sum(expr[i,1:12]) / 12) / (sum(expr[i,13:22]) / 10)
  FoldChange = append(FoldChange,FC)
}
expr2 <- cbind(expr,FoldChange)

#t-value提取特征基因1-12不吸13-22吸
P_value <- c()
for(i in 1:length(rownames(expr))){
  t = t.test(expr[i,1:12],expr[i,13:22])
  P_value <- append(P_value,t[[3]])
}
expr3 <- cbind(expr2,P_value)

#提取差异表达基因FC<1/4 or FC>4, P_value < 0.01
gene <- c()
for(i in 1 : dim(expr3)[1]){
  if(expr3[i,23] > 4 & expr3[i,24] < 0.01){
    gene <- append(gene,i)
  }else if(expr3[i,23] < 1/4 & expr3[i,24] < 0.01){
    gene <- append(gene,i)
  }
}
expr4 = c()
expr4 <- rbind(expr4,expr3[gene,])

#共13个差异基因，对表达值做log2处理
expr5 <- log2(expr4[,1:22])
dim(expr5)

#添加类别标签
expr6 <- cbind(t(expr5),"non_somker")
expr6[13:22,14] <- "somker"
colnames(expr6)[14] <- "Species"
View(expr6)
expr6 <- as.data.frame(expr6)
expr6[,1:13] <- as.numeric(unlist(expr6[,1:13]))
expr6[,14] <- as.factor(expr6[,14])

#分类器
library(caret)
ind <- sample(5,nrow(expr6),replace = TRUE)


#决策树
library(tree)
library(party)
acc = c()
for(i in 1:5){
  trainSet <- expr6[ind != i,]
  testSet <- expr6[ind == i,]
  mytree <- tree(Species~.,trainSet)
  pre_tree <- predict(mytree,testSet)
  pre_tree <- apply(pre_tree,1,function(x) colnames(pre_tree)[which.max(x)])
  print(table(testSet$Species,pre_tree))
  print(confusionMatrix(testSet$Species,factor(pre_tree))[[3]][1])
  acc = append(acc,confusionMatrix(testSet$Species,factor(pre_tree))[[3]][1])
}
print(mean(acc))

#朴素贝叶斯
library(e1071)
acc = c()
for(i in 1:5){
  trainSet <- expr6[ind != i,]
  testSet <- expr6[ind == i,]
    nby <- naiveBayes(Species~.,trainSet)
    pre_nby <- predict(nby,testSet)
    print(table(testSet$Species,pre_nby))
    print(confusionMatrix(testSet$Species,pre_nby)[[3]][1])
    acc = append(acc,confusionMatrix(testSet$Species,pre_nby)[[3]][1])
}
print(mean(acc))
#支持向量机svm
library(e1071)
acc = c()
for(i in 1:5){
  trainSet <- expr6[ind != i,]
  testSet <- expr6[ind == i,]
  svmclass <- svm(Species~.,trainSet,na.action = na.omit,kernel = "sigmoid")
  pre_svm <- predict(svmclass,testSet)
  print(table(testSet$Species,pre_svm))
  print(confusionMatrix(testSet$Species,pre_svm)[[3]][1])
  acc = append(acc,confusionMatrix(testSet$Species,pre_svm)[[3]][1])
}
print(mean(acc))
#BP newff
library(AMORE)
acc = c()
for(i in 1:5){
  trainSet <- expr6[ind != i,]
  testSet <- expr6[ind == i,]
  trainSet$Species <- class.ind(trainSet$Species)
  bp_newff <- newff(n.neurons = c(dim(trainSet[,1:13])[2],3,dim(trainSet$Species)[2]),learning.rate.global = 0.0001,momentum.global = 0.001,Stao = NA, hidden.layer = "sigmoid",output.layer = "sigmoid",method = "ADAPTgdwm")
  bp_train <- train(bp_newff,trainSet[,1:13],trainSet$Species,error.criterium = "LMS",report = TRUE,n.shows = 20,show.step = 10000)
  bp_sim <- sim(bp_train$net,testSet[,1:13])
  bp_sim_out <- apply(bp_sim,1,function(x) c("non_somker","smoker")[which.max(x)])
  print(table(testSet$Species,bp_sim_out))
  acc = append(acc,confusionMatrix(testSet$Species,factor(bp_sim_out))[[3]][1])
}
print(mean(acc))

#BP网
library(nnet)
acc = c()
for(i in 1:5){
  trainSet <- expr6[ind != i,]
  testSet <- expr6[ind == i,]
  trainSet$Species <- class.ind(trainSet$Species)
  bp <- nnet(trainSet[,1:13],trainSet$Species,size = 1, rang=0.1, decay=5e-4,maxit=300)
  pre_bp <- predict(bp,testSet[,1:13])
  pre_bp <- apply(pre_bp,1,function(x) colnames(pre_bp)[which.max(x)])
  print(table(testSet$Species,pre_bp))
  acc = append(acc,confusionMatrix(testSet$Species,as.factor(pre_bp))[[3]][1])
}
print(mean(acc))

#双向聚类
library(pheatmap)
hmcols <- colorRampPalette(c("blue","grey","red"))(20)
pheatmap(expr6[,1:13], col=hmcols,cutree_rows = 2,cutree_cols = 2)

