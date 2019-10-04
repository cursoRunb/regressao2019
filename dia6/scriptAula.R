dados <- read.csv("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/Regressao/SIOP.csv")
head(dados)
plot(dados$Dotação.Inicial,dados$Pago)
mod <- lm(((dados$Pago))~dados$Dotação.Inicial)
summary(mod)
abline(mod$coefficients)
piores <- order(abs(mod$residuals),decreasing = T)





piores[1:5]
mod2 <- lm(Pago ~ Dotação.Inicial,data=dados[-piores[1:3],])
mod
mod2
abline(mod2$coefficients,col=3)
dados <- dados[order(dados$Pago),]
tail(dados)

shapiro.test(mod$residuals)
qqnorm(mod$residuals)
qqline(mod$residuals)
require(MASS)
modgambiarra <- lm(I(dados$Pago+1)~dados$Dotação.Inicial)
boxcox(modgambiarra)

modtran <- lm(I(log(dados$Pago+1))~dados$Dotação.Inicial)
summary(modtran)
shapiro.test(modtran$residuals)
ytran <- ((dados$Pago+1)^lambda-1)/lambda
plot(dados$Dotação.Inicial,mod$residuals)
modtran <- lm(ytran~dados$Dotação.Inicial)
plot(modtran$residuals)

mtcars
plot(mtcars$wt,mtcars$mpg)
modcars <- lm(mtcars$mpg~mtcars$wt)
modcars
X <- model.matrix(modcars)
solve(t(X)%*%X)%*%t(X)%*%mtcars$mpg

plantas <- iris[1:100,]
plot(plantas$Petal.Length,plantas$Petal.Width,col=as.numeric(plantas$Species),pch=16)

modplanta <- lm(plantas$Petal.Width~plantas$Species)
summary(modplanta)
plot(plantas$Petal.Length,plantas$Petal.Width)
abline(a=-.21,b=.31)
abline(a=-.21+.19,b=.31,col=2)
modplanta2 <- lm(plantas$Petal.Width~plantas$Species*plantas$Petal.Length)
modplanta3 <- lm(plantas$Petal.Width~plantas$Species+plantas$Petal.Length+plantas$Petal.Length:plantas$Species)
summary(modplanta2)
summary(modplanta3)
summary(lm(iris$Petal.Width~iris$Species))
relevel(iris$Species,"versicolor")
model.matrix(lm(iris$Petal.Width~iris$Species))
modplanta2$coefficients
abline(a=-.04,b=.20)
abline(a=-.04-.03,b=.20+.13)
tarifa <- read.csv("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/TarifaFornecimentoResidencial.csv")
tarifa
modelo <- lm(tarifa$VlrTotaTRFConvencional~tarifa$nomConcessao+tarifa$SigRegiao)
dist(tapply(tarifa$VlrTotaTRFConvencional,tarifa$SigRegiao,mean))
confint(modplanta2)
