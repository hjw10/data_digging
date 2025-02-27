setwd("C:\\Users\\penguin\\Desktop\\学习资料\\数据挖掘\\数据描述及预处理")
methylation <- read.table("CHOL_methylation.data",header =  TRUE)
methylation <- as.matrix(methylation)

j = 1
for(i in 1:length(rownames(methylation))){
  if(length(which(is.na(methylation[j,]))) >= 10){
    methylation = methylation[-j,]
  }
  else j = j+1
}

for(j in 1 : length(colnames(methylation))){
  methylation[which(is.na(methylation[,j])),j] = mean(methylation[,j],na.rm = TRUE)
}

print("列：")
print(length(colnames(methylation)))
print("行：")
print(length(rownames(methylation)))
#检查处理后的数据是否还有NA
print(which(is.na(methylation[,])))

#最大-最小规范化
aa <- matrix(ncol = length(colnames(methylation)),nrow = length(rownames(methylation)) ,dimnames = dimnames(methylation))
for(i in 1 : length(colnames(methylation))){
  a <- (methylation[,i] - min(methylation[,i])) / (max(methylation[,i]) - min(methylation[,i]))
  aa[,i] = a
} 
View(aa)



