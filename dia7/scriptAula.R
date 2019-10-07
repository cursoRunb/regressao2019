dados <- read.csv("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/TarifaFornecimentoResidencial.csv")
head(dados)
names(dados)
limpos <- dados[,-c(1,3,11,12,13)]
summary(limpos)
plot(limpos)
cor(limpos[,-(1:2)])
limpos$SigRegiao <- relevel(limpos$SigRegiao,"S ")
modelo <- lm(VlrTRFBrancaPonta~.,data=limpos)
summary(modelo)
modelo1 <- lm(VlrTRFBrancaPonta~nomConcessao + VlrTUSDConvencional + VlrTEConvencional + VlrTRFBrancaIntermediaria + VlrTRFBrancaForaPonta,data=limpos)
summary(modelo1)
summary(lm(limpos$VlrTRFBrancaPonta~limpos$SigRegiao))

shapiro.test(modelo1$residuals)
qqnorm(modelo1$residuals)
qqline(modelo1$residuals)
plot(limpos$VlrTRFBrancaIntermediaria,modelo1$residuals)
identify(limpos$VlrTRFBrancaIntermediaria,modelo1$residuals)
modelo3 <- lm(VlrTRFBrancaPonta~nomConcessao+ + VlrTUSDConvencional + VlrTEConvencional + VlrTRFBrancaIntermediaria + VlrTRFBrancaForaPonta+ SigRegiao,data=limpos)
shapiro.test(modelo3$residuals)
qqnorm(modelo3$residuals)
qqline(modelo3$residuals)



x1 <- rpois(50,170)
x2 <- 2* x1 + rnorm(50,0,1)
x3 <- 2 * x1
y <- 10 + 3*x1 + 2 * x2 + rnorm(50,0,15)
mod <- lm(y ~ x1 + x2)
summary(mod)
par(mfrow=c(1,2))
plot(x1,y)
plot(x2,y)
plot(x1,x2)
summary(lm(y~x1+x3))
plot(x1,x3)
summary(lm(y~x1))
summary(lm(y~x2))
VIF(mod)
VIF(modelo1)



###Fazendo seleção de variáveis
m1 <- lm(VlrTRFBrancaPonta ~ .,data=limpos)
step(m1,scope = list(lower= ~ 1),direction = "backward")
m2 <- lm(VlrTRFBrancaPonta ~ 1, data = limpos)
step(m2,
     scope = list(upper= ~nomConcessao + SigRegiao + VlrTUSDConvencional + 
                    VlrTEConvencional + VlrTRFBrancaIntermediaria + VlrTRFBrancaForaPonta),
     direction = "forward")

m3 <- lm(VlrTRFBrancaPonta ~nomConcessao, data=limpos)
step(m3,scope=list(upper=~nomConcessao + SigRegiao + VlrTUSDConvencional + 
                     VlrTEConvencional + VlrTRFBrancaIntermediaria + VlrTRFBrancaForaPonta, lower= ~1),
     direction = "both")
