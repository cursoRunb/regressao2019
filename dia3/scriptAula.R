dados <- read.table("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/IMP_2019_MUN.txt
",header = TRUE,sep=";")


head(dados)
summary(dados)
t.test(dados$KG_LIQUIDO)
media <- mean(dados$KG_LIQUIDO)
variancias <- var(dados$KG_LIQUIDO)
n <- nrow(dados)
media+qt(.025,n-1)*sqrt(variancias/n)
media-qt(.025,n-1)*sqrt(variancias/n)

modelo <- aov(dados$KG_LIQUIDO~1)
confint(modelo)

trim1 <- dados[dados$CO_MES<4,"VL_FOB"]
trim2 <- dados[(dados$CO_MES>3) & (dados$CO_MES<7),"VL_FOB"]

t.test(x = trim1,y = trim2,
       alternative = "two.sided",
       paired = F,var.equal = T)
m1 <- mean(trim1)
m2 <- mean(trim2)
v1 <- var(trim1)
v2 <- var(trim2)
n1 <- length(trim1)
n2 <- length(trim2)
s2c <- ((n1-1)*v1+(n2-1)*v2)/(n1+n2-2)
(m1-m2)/sqrt(s2c/n1+s2c/n2)

var.test(trim1,trim2)
t.test(x = trim1,y = trim2,
       alternative = "two.sided",
       paired = F,var.equal = F)


sort(summary(as.factor(dados$CO_PAIS)),decreasing = T)

vlrchina <- dados[dados$CO_PAIS == 160,"VL_FOB"]
t.test(x = vlrchina,alternative = "greater",mu = 120000)
tobs<- (mean(vlrchina)-120000)/sqrt(var(vlrchina)/length(vlrchina))
1-pt(tobs,length(vlrchina)-1)
resultados <- c()
for(i in 1:1000){
  resultados[i] <- t.test(x = sample(vlrchina,size = length(vlrchina)/2),alternative = "greater",mu = 120000)$p.value
}
hist(resultados)
aa <- t.test(x = sample(vlrchina,size = length(vlrchina)/2),alternative = "greater",mu = 120000)
names(aa)
n <- length(vlrchina)
xcrit <- qt(.95,n-1)*sqrt(var(vlrchina)/n)+120000
xcrit
pt((144773.1-130000)/sqrt(var(vlrchina)/n),n-1)

xcrit1 <- qt(.95,n/2-1)*sqrt(var(vlrchina)/(n/2))+120000
xcrit
pt((xcrit1-130000)/sqrt(var(vlrchina)/(n/2)),n/2-1)


plot(iris$Petal.Length,iris$Petal.Width)

modelo <- lm(iris$Petal.Width~iris$Petal.Length)
(modelo)
x <- iris$Petal.Length
y <- iris$Petal.Width
n <- length(y)
b1 <- (sum(x*y)-(sum(x)*sum(y)/n))/(sum(x^2)-sum(x)^2/n)
b0 <- mean(y)-b1*mean(x)
b0
b1
abline(a = b0,b = b1)

vero <- function(params){
  b0 <- params[1]
  b1 <- params[2]
  sigma <- params[3]
  -sum(dnorm(y,mean=b0+b1*x,sd=sigma,log = T))
}
optim(c(.1,.2,2),vero)
erro <- function(b0,b1,x,y){
  sum((y-b0-b1*x)^2)
}
errovet <- Vectorize(erro,"b0")
b0
betas <- seq(-1,1,by=.05)
plot(betas,errovet(betas,b1,x,y))
