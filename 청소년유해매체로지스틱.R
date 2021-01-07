setwd("C:\\Users\\minji\\Desktop\\P커리어캐치\\청소년매체위험실태조사\\총괄_20201223_65806_데이터")
mdis <- read.csv("총괄_2018_20201223_65806.csv",sep=",",header=F,skip=1,na.strings=9)

mdis$factor1<-scale(mdis[,402])*0.851+scale(mdis[,403])*0.877+scale(mdis[,404])*0.869+scale(mdis[,405])*0.906+scale(mdis[,406])*0.885
mdis$factor2<-scale(mdis[,49])*-0.153+scale(mdis[,73])*-0.108+scale(mdis[,50])*0.353+scale(mdis[,51])*0.898+scale(mdis[,74])*0.363+scale(mdis[,75])*0.852
mdis$factor3<-scale(mdis[,49])*0.734+scale(mdis[,73])*0.946+scale(mdis[,50])*-0.213+scale(mdis[,51])*-0.155+scale(mdis[,74])*-0.205+scale(mdis[,75])*-0.146
mdis$factor4<-scale(mdis[,49])*-0.154+scale(mdis[,73])*0+scale(mdis[,50])*0.832+scale(mdis[,51])*0.221+scale(mdis[,74])*0.841+scale(mdis[,75])*0.255

mdis$V112<-relevel(factor(mdis$V112),ref="2")
mdis$V134<-relevel(factor(mdis$V134),ref="2")

fit<-glm(mdis$V112~factor1+factor2+factor3+factor4,family=binomial)
summary(fit)

library(pROC)

fitted(fit)

mdis1<-na.omit(mdis[,c("V112","factor1","factor2","factor3","factor4")])
rocplot<-roc(V112~ fitted(fit),data=mdis1)
plot.roc(rocplot,legacy.axes=T)
auc(rocplot)

library(MASS)

fit<-glm(V112~factor1+factor2+factor3+factor4+factor(V412)+factor(V413)+
             V391+V395+V396+V25+V34+V80+V93+V96+V101+V159+V169+V176+V177+V179,family=binomial,data=mdis)

summary(fit)

vif(fit)

anova(fit,fit1,test="LRT")

summary(fit)

fit1<-stepAIC(fit)
summary(fit1)

mdis1<-na.omit(mdis[,c("V112","factor1","factor2","factor3","factor4","V413","V412","V391","V395","V396","V25","V34","V80","V93","V96","V101","V159","V169","V176","V177","V179")])

nrow(mdis1)
rocplot<-roc(V112~ fitted(fit),data=mdis1)
plot.roc(rocplot,legacy.axes=T)
auc(rocplot)




#####
fit<-glm(V134~factor1+factor2+factor3+factor4+factor(V412)+factor(V413)+
             V391+V395+V396+V25+V34+V80+V93+V96+V101+V159+V169+V176+V177+V179,family=binomial,data=mdis)
summary(fit)

mdis1<-na.omit(mdis[,c("V134","factor1","factor2","factor3","factor4","V413","V412","V391","V395","V396","V25","V34","V80","V93","V96","V101","V159","V169","V176","V177","V179")])

nrow(mdis1)
rocplot<-roc(V134~ fitted(fit),data=mdis1)
plot.roc(rocplot,legacy.axes=T)
auc(rocplot)