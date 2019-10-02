dados <- read.csv("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/TarifaFornecimentoResidencial.csv")
head(dados)

mod <- lm(VlrTRFBrancaPonta ~ VlrTUSDConvencional,data=dados)
summary(mod)

plot(dados$VlrTUSDConvencional, dados$VlrTRFBrancaPonta,xlim=c(-1,.8),ylim=c(0,2))
abline(mod$coefficients)
mod$coefficients
confint(mod,level = .9)

hist(mod$residuals,freq=F)
xx <- seq(-.5,.5,by=.001)
lines(xx,dnorm(xx,mean(mod$residuals),sd(mod$residuals)))
qqnorm(mod$residuals)
qqline(mod$residuals)
plot(dados$VlrTUSDConvencional,mod$residuals)
plot(mod$fitted.values,mod$residuals)
shapiro.test(mod$residuals)

which.max(mod$residuals)

text(dados[55,5]-.1,dados[55,8],labels = dados[55,3])
which.min(mod$residuals)
text(dados[74,5]+.1,dados[74,8],labels = dados[74,3])
?predict

mod1 <- lm(dados$VlrTRFBrancaPonta ~ dados$VlrTUSDConvencional)
aa <- seq(.16,.73,length.out = nrow(dados))
predict(object = mod1,
        newdata = data.frame(VlrTUSDConvencional= aa))

teste <- predict(object = mod1,
                 newdata = data.frame(VlrTUSDConvencional=aa))

teste1 <- mod$coefficients[2]*aa+mod$coefficients[1]
plot(teste,teste1)
names(mod)
mod$call
summary(mod)

y <- runif(5000)
plot(y[c(T,F)],y[c(F,T)])
mod <- lm(y[c(F,T)]~y[c(T,F)])
summary(mod)
abline(mod$coefficients,col=2)

plot(y[c(T,F)],y[c(T,F)])
mod3 <- lm(y[c(T,F)]~y[c(T,F)])
summary(mod)
abline(mod$coefficients,col=2)

plot(dados$VlrTUSDConvencional)
boxplot(dados$VlrTUSDConvencional)

boxplot(mod$residuals)
hist(mod$residuals)

ks.test(mod$residuals,
        y = "pnorm",
        mean=mean(mod$residuals),
        sd=sd(mod$residuals))
qqnorm(mod$residuals)
qqline(mod$residuals)

result <- c()
result1 <- c()
for(i in 1:1000){
  x <- rnorm(50)
  result[i] <- lillie.test(x)$p.value
  result1[i] <- shapiro.test(x)$p.value
}
mean(result<.05)
mean(result1<.05)
install.packages("car")
install.packages("EnvS")
require(MASS)
boxcox(mod)
