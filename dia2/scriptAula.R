exportacoes <- read.csv(file="https://raw.githubusercontent.com/cursoRunb/regressao2019/master/dia2/exportacoes.csv")
head(exportacoes)
dim(exportacoes)
apply(exportacoes,2,typeof)
for(i in 1:ncol(exportacoes)){
  print(class(exportacoes[,i]))
}
class(exportacoes$VL_FOB)
exportacoes$CO_PAIS <- as.factor(exportacoes$CO_PAIS)
summary(exportacoes)
ordenado <- sort(summary(exportacoes$SG_UF_NCM),decreasing = T)
ordenado[1]
names(ordenado)[1]
max(summary(exportacoes$SG_UF_NCM))
ordenado["SP"]
tapply(exportacoes$KG_LIQUIDO,exportacoes$CO_VIA,summary)
tapply(exportacoes$KG_LIQUIDO,exportacoes$CO_VIA,mean)
boxplot(exportacoes$KG_LIQUIDO~exportacoes$CO_VIA)

leves <- exportacoes[exportacoes$KG_LIQUIDO<1000,]
boxplot(leves$KG_LIQUIDO~leves$CO_VIA)
sem_1 <- exportacoes[exportacoes$CO_VIA!=1,]
boxplot(sem_1$KG_LIQUIDO~sem_1$CO_VIA)
paises <- names(sort(summary(exportacoes$CO_PAIS),decreasing = T)[1:5])
paises
is.element(el=exportacoes$CO_PAIS,set = paises)
socinco <- exportacoes[is.element(el=exportacoes$CO_PAIS,set = paises),]
socinco
tapply(socinco$VL_FOB,socinco$CO_PAIS,summary)
socinco$CO_PAIS
socinco <- droplevels(socinco)
summary(socinco)
tapply(socinco$VL_FOB,socinco$CO_PAIS,summary)
boxplot(socinco$VL_FOB~socinco$CO_PAIS)
which.max(exportacoes$VL_FOB)
exportacoes[427,]
meuvetor <- sort(tapply(exportacoes$VL_FOB,exportacoes$CO_PAIS,sum),decreasing = T)
meuvetor[1]
somas <- tapply(exportacoes$VL_FOB,exportacoes$CO_PAIS,sum)
somas
names(somas)[which.max(somas)]
exportacoes$CO_PAIS %in% paises


###Revisao

pnorm(-1)
qnorm(.95)
rnorm(10)
pnorm(102,mean = 100, sd = 5)
pnorm(.4)
rnorm(10,mean = 100, sd = 5)

populacao <- rexp(10000,rate = 1/20)
populacao
hist(populacao)
mean(populacao)
elementos <- sample(x = 1:10000,10)
elementos
mean(populacao[elementos])
medias <- c()
for(i in 1:1000){
  amostra <- sample(populacao,250)
  medias[i] <- mean(amostra)
}
medias
hist(medias)
par(mfrow=c(1,2))


exportacoes
t.test(x = exportacoes$KG_LIQUIDO,alternative = "less",mu = 500000)
