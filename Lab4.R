setwd = "C://Users//penguin//Desktop//学习资料//数据挖掘//Lab4"
library(som)
library(RSNNS)
library(pheatmap)

#som
somc <- som::som(iris[,-5], 5, 6,topol="rect")

#RSNNS
somc <- RSNNS::som(iris[,-5],5,6,topol=1000)
plotActMap(somc$map)
save.image("Lab4.Rdata")