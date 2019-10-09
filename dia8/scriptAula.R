with(mtcars,plot(mpg,wt))

dados <- read.csv2("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/Regressao/DADOS_2.csv"
)
head(dados)
dados$CO_MES <- as.factor(dados$CO_MES)
modelo <- lm(N_PRODUTOS~.,data=dados)
summary(modelo)
selecao <- step(modelo,scope=list(lower = ~ 1),direction = "backward")
summary(modelo)
summary(selecao)
VIF(selecao)
cor(dados[,-(1:2)])

shapiro.test(selecao$residuals)
qqnorm(selecao$residuals)
qqline(selecao$residuals)
plot(selecao$fitted.values,selecao$residuals)
summary(dados$N_PRODUTOS)
modglm <- glm(N_PRODUTOS~.,data=dados,family="poisson")


dados <- read.csv("https://raw.githubusercontent.com/cursoRunb/regressao2019/master/dia8/pima-indians-diabetes.csv")
names(dados) <- c("number_preg","plasma_gluc","diastolic_pressure",
                  "skin_thick","insulin","bmc","diabete_pedigree","age","diag")
summary(dados)

indices <- sample(1:nrow(dados),floor(nrow(dados)*.8))
indices
treino <- dados[indices,]
teste <- dados[-indices,]

modelo <- glm(diag~.,data=treino,family=binomial(link="probit"))
summary(modelo)
modfinal <- step(modelo,scope = list(lower=~1))
summary(modfinal)
modfinal$fitted.values
tabela <- table(modfinal$fitted.values>.3,treino$diag)
sum(diag(tabela))/sum(tabela)

install.packages("ROCR")
require(ROCR)
result <- predict(modfinal,treino,"response")
pred <- prediction(result,treino$diag)
perf <- performance(pred,"tnr","fnr")
plot(perf,print.cutoffs.at=seq(.1,.9,by=.05))


previsao_teste <- predict(modfinal,teste,"response")
tabelateste <- table(previsao_teste>.3,teste$diag)
sum(diag(tabelateste))/sum(tabelateste)



titan <- read.csv("https://raw.githubusercontent.com/cursoRunb/regressao2019/master/dia8/Titanic1.csv")
head(titan)
names(titan)
barplot(table(titan$Survived,titan$Sex),beside = T)
barplot(table(titan$Survived,titan$Pclass),beside = T)
barplot(table(titan$Survived,titan$Embarked),beside = T)
boxplot(titan$Age~titan$Survived)
boxplot(titan$Fare~titan$Survived)
barplot(table(titan$Survived,titan$SibSp),beside=T)
barplot(table(titan$Survived,titan$Parch),beside=T)

limpo <- titan[,-c(1,2,5,10,12)]
mod <- glm(Survived~.,data=limpo,family="binomial")
summary(mod)
modtitan <- step(mod,scope = list(lower=~1))
summary(modtitan)
names(modtitan$fitted.values)

verdade <- limpo$Survived[as.numeric(names(modtitan$fitted.values))]
table(verdade,modtitan$fitted.values>.5)
table(verdade,mod$fitted.values>.5)


