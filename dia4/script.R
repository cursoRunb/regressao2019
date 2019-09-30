
dados <- read.csv("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/Regressao/Abertura_CO.csv")
head(dados)
summary(dados[,1:10])
plot(x = dados$X2..FEC.Apurada,y = dados$X1..DEC.Apurada)
modelo <- lm(dados$X1..DEC.Apurada~dados$X2..FEC.Apurada)
abline(modelo$coefficients)
so18 <- dados[dados$ANO==2018,]
so18 <- so18[!is.na(so18$X1..DEC.Apurada) & !is.na(so18$X2..FEC.Apurada),]
plot(so18$X1..DEC.Apurada,so18$X2..FEC.Apurada)
mod2 <- lm(so18$X2..FEC.Apurada~so18$X1..DEC.Apurada)
abline(mod2)
so18[which.max(abs(mod2$residuals)),"Empresa"]
identify(so18$X1..DEC.Apurada,so18$X2..FEC.Apurada,labels = so18$Empresa)
text(locator(1) ,label="Boa Vista Energia")
locator(1)
xx <- so18$X1..DEC.Apurada
yy <- so18$X2..FEC.Apurada
nn <- length(yy)
b1 <- (sum(xx*yy)-(sum(xx)*sum(yy))/nn)/(sum(xx^2)-sum(xx)^2/nn)
b1
mod2$coefficients
b0 <- mean(yy)-b1*mean(xx)
c(b0,b1)
mod2$coefficients

vero <- function(params,xx,yy){
  b0 <- params[1]
  b1 <- params[2]
  sigma <- params[3]
  -sum(dnorm(yy,mean = b0 + b1 * xx,sd = sigma,log = T))
}
optim(c(10,.1,15),
      vero,method = "BFGS",
      xx=xx,yy=yy,control = list(maxit=1000))
?optim

mod3 <- lm(so18$X1..DEC.Apurada~so18$X2..FEC.Apurada)
mod3$residuals-mod2$residuals
par(mfrow=c(1,2))
plot(so18$X1..DEC.Apurada,so18$X2..FEC.Apurada)

abline(mod2$coefficients)

plot(so18$X2..FEC.Apurada,so18$X1..DEC.Apurada)
abline(mod3)
plot(mod2$residuals,mod3$residuals)
par(mfrow=c(1,1))
plot(mod2$residuals)
plot(mod2)
round(sum(mod2$residuals),8)
sum(yy)
sum(mod2$fitted.values)
sum(mod2$residuals*xx)
sum(mod2$fitted.values*mod2$residuals)


abline(h=mean(yy))
abline(v = mean(xx))
cor(xx,yy)
names(summary(mod2))
qmres <- summary(mod2)$sigma
qmres
names(mod2)
vb1 <- qmres^2/sum((xx-mean(xx))^2)
vb1
b1
sqrt(vb1)
summary(mod2)
b1-qt(.975,63)*sqrt(vb1)
b1+qt(.975,63)*sqrt(vb1)
confint(mod2)

pvalor <- 2*pt((b1-0)/sqrt(vb1),63,lower.tail = FALSE)
pvalor
summary(mod2)

x <- rpois(50,lambda = 100)
resultados <- c()
for(i in 1:1000){
  y <- 10 + 2.5 * x + rnorm(50,0,5)
  mod <- lm(y~x)
  resultados[i] <- mod$coefficients[2]
}
mean(resultados)
hist(resultados)
shapiro.test(resultados)
