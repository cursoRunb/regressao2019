tarifa <- read.csv(file="https://raw.githubusercontent.com/gustavopompeu/ENAP/master/TarifaFornecimentoResidencial.csv")
tarifa
dim(tarifa)
nrow(tarifa)
ncol(tarifa)
names(tarifa)
summary(tarifa)

tarifa$VlrTEConvencional
tarifa[,"VlrTEConvencional"]
tarifa[,6]

variaveis_interesse <- c("VlrTUSDConvencional","VlrTEConvencional")
variaveis_interesse
tarifa[,variaveis_interesse]
tarifa[c(1,2,3),]
boxplot(tarifa$VlrTEConvencional ~ tarifa$nomConcessao)
boxplot(tarifa$VlrTEConvencional ~ tarifa$SigRegiao)
table(tarifa$nomConcessao,tarifa$SigRegiao)

tapply(tarifa$VlrTotaTRFConvencional,paste(tarifa$nomConcessao,tarifa$SigRegiao),mean)

notas <- data.frame(nomes=c("ana","bia","carol","dani","erica"),
                    nota=c(5,7,8,3,10),
                    bolsista=c(T,F,F,F,T))
notas

3 < 5
3 > 10
3 == 3
notas[c(1,4),]
notas[c(T,F,F,T,F),]
notas$nota<7
notas[notas$nota<7,]

tarifa$nomConcessao=="Concessionária"
so_concessionarias <- tarifa[tarifa$nomConcessao=="Concessionária", ]
boxplot(so_concessionarias$VlrTEConvencional ~ so_concessionarias$SigRegiao)
names(tarifa)
plot(tarifa$VlrTUSDConvencional,tarifa$VlrTotaTRFConvencional,
     col=as.numeric(tarifa$nomConcessao),pch=16)
cor(tarifa$VlrTUSDConvencional,tarifa$VlrTotaTRFConvencional)

datas <- substr(as.character(tarifa$DthInicioVigencia),start=1,stop = 10)
datas <- as.Date(datas,"%d/%m/%Y")
?as.Date
plot(datas,tarifa$VlrTotaTRFConvencional)

plot(tarifa$VlrTotaTRFConvencional)
abline(h=mean(tarifa$VlrTotaTRFConvencional))

plot(so_concessionarias$VlrTotaTRFConvencional,
     col=as.numeric(so_concessionarias$SigRegiao),pch=16)
legend(locator(1),legend = levels(so_concessionarias$SigRegiao),
       fill=1:5)
identify(1:nrow(so_concessionarias),so_concessionarias$VlrTotaTRFConvencional,labels = so_concessionarias$SigDistribuidora)
