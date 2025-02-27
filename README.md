# data_digging
数据挖掘课程实战代码

期末大作业
一、 实验目的： 
1． 应用表达谱数据构建多种分类器并比较分类器的效能 
2． 对表达谱数据进行双向聚类并绘制热图 

二、实验数据： 
从GEO下载表达谱数据及其注释数据，编号分别为GSE4498和GPL50-55999 

三、实验内容： 
1．通过对表达谱数据和注释数据进行预处理，得到数据文件 expr6
2．构建分类器并比较其性能 
（1）构建（朴素贝叶斯）分类器
分类效果非常好，5倍交叉证实有100%的正确率。
（2） 构建（支持向量机）分类器
5 倍交叉证实后平均准确率有96%，分类效果不错。
（3） 构建（BP神经网络）分类器
5倍交叉证实的混淆矩阵结果跟支持向量机的结果是一样的，分类效果不错。
（4） 构建（决策树）分类器
分类效果不错，5倍交叉证实有95%的正确率。
3．对表达谱数据进行双向聚类并绘制热图

四、实验结果： 
1.各分类器的分类效能比较结果 
朴素贝叶斯5倍交叉证实后平均准确性达到了100%，支持向量机和BP网达到96%，决策树
95%，分类效能都不错。但是由于BP网的构建参数是人为设置的，所以也有一些主观因素导
致BP网的效果的差异，修改参数后结果可能不一样。总的来说朴素贝叶斯方法在此次实验
中表现比较好，略优于支持向量机和BP网，决策树方法排在最后。 
2.双向聚类热图 
数据本身的区分不明显，按照数据标签11号应该放在吸烟组，但是它放到不吸烟组去了。
