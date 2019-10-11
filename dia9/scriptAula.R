sul <- c("RS","SC","PR")
regiao <- c()
for(i in 1:length(estados)){
  if(estados[i] %in% norte) regiao[i] <- "NO"
  if(estados[i] %in% sul) regiao[i] <- "S"
}

dados <- read.csv("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/Regressao/SIOP_adj.csv")

for(i in 1:6){
  dados[,i] <- as.factor(dados[,i])
}
summary(dados)
dados <- dados[,-1]


modelo <- glm(Grupo.de.Despesa~.,data=dados,family="binomial")
modfim <- step(modelo,scope=list(lower=~1))
summary(modfim)

table(modfim$fitted.values>0.5,dados$Grupo.de.Despesa)

require(ROCR)
pred <- predict(modfim,type = "response")
rocpre <- prediction(pred,labels = dados$Grupo.de.Despesa)
aa <- performance(rocpre,"fpr","fnr")
plot(aa,print.cutoffs.at=seq(.1,.9,by=.05))


summary(modelo)

as.formula(dados)

install.packages("rpart")
require(rpart)
?rpart
?kyphosis

modelo <-  rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
summary(modelo)


gini <- function(dados){
  1-sum((summary(dados)/length(dados))^2)
}
summary(kyphosis$Start)
xx <- seq(1,18,by=.5)
xx
resultado <- c()
for(i in 1:length(xx)){
  direita <- kyphosis$Kyphosis[kyphosis$Start<=xx[i]]
  esquerda <- kyphosis$Kyphosis[kyphosis$Start>xx[i]]
  resultado[i] <- length(direita)*(ginipai-gini(direita))+length(esquerda)*(ginipai-gini(esquerda))
}
data.frame(resultado,xx)
plot(modelo)
text(modelo, use.n = TRUE)
install.packages("rattle")
require(rattle)
fancyRpartPlot(modelo)

predict(modelo,type="prob")
install.packages("randomForest")
require(randomForest)
modelo
modflor <- randomForest(Kyphosis ~ Age + Number + Start, data = kyphosis)
modflor
table(predict(modelo,type="prob")[,1]<.5,kyphosis$Kyphosis)


modquant <- rpart(mpg~wt+hp+drat,data=mtcars,method = "anova")
summary(modquant)
fancyRpartPlot(modquant)
random

aa <- randomForest(mpg~wt+hp+drat,data=mtcars)
aa
summary(aa)
predict(aa)
