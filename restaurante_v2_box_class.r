
restaurante <- scan("/media/r2/8800BCCA00BCC08C1/Pasta de Trabalho/Regressão linear simples/Relatório/restaurante.dat", list(faturamento=0, gasto=0))
# restaurante <- scan("F:/Pasta de Trabalho/Regressão linear simples/Relatório/restaurante.dat")

attach(restaurante)

### Obtendo as medida-resumo

a <- summary(restaurante$faturamento)
a
b <- summary(restaurante$gasto)
b

hist(restaurante$faturamento, freq=FALSE)


fix(a)
fix(b)

sd(restaurante$faturamento)
sd(restaurante$gasto)

#### Obtendo a densidade

par(new=F, mfrow=c(1,1))
plot(d<-density(restaurante$faturamento), xlab="faturamento", ylab="", main="Densidade de faturamento")
polygon(d, col="gray")

#### Obtendo os box-plots

boxplot(restaurante$faturamento, main="Boxplot de faturamento",ylab="faturamento", col="gray")

                                
par(new=F, mfrow=c(1,2))
boxplot(restaurante$faturamento, main="Boxplot de faturamento",xlab="", ylab="faturamento", col="gray")
boxplot(restaurante$gasto, main="Boxplot de gasto",xlab="", ylab="gasto", col="gray")

#### Obtendo o gráfico de dispersão e a correlação

plot(gasto,faturamento,pch=16,main="Dispersão faturamento x gasto", )

X<-cbind(gasto,faturamento)
cor(X)

####  Obtendo os ajustes

fit.model<-lm(faturamento~gasto, data = restaurante)
summary(fit.model)


# Intervalos de confian?a para mu
confint(fit.model)

# Intervalos de confiança para mu
conf.int =  predict(fit.model,interval="confidence")
conf.int

#Intervalos de predição
pred.int =  predict(fit.model,interval="prediction")
pred.int

fitted.values = pred.int[,1]

pred.lower = pred.int[,2]
pred.upper = pred.int[,3]
conf.lower = conf.int[,2]
conf.upper = conf.int[,3]   


plot(gasto,faturamento)
lines(gasto,fitted.values, col="red",lwd=2)
lines(gasto,conf.lower, lwd=3,col="darkgreen")
lines(gasto,conf.upper, lwd=3,col="darkgreen")
lines(gasto,pred.lower, lwd=2,col="blue")
lines(gasto,pred.upper, lwd=2,col="blue")

#IP para gasto igual a 12

predict(fit.model, newdata =  data.frame(gasto = 12), interval = "prediction")

#IC para gasto igual a 12
predict(fit.model, newdata =  data.frame(gasto = 12), interval = "confidence")



#### Obtendo o envelope e os gráficos de diagnóstico

source("/media/r2/8800BCCA00BCC08C1/Pasta de Trabalho/Regressão linear simples/Relatório/envel_norm.txt")

source("/media/r2/8800BCCA00BCC08C1/Pasta de Trabalho/Regressão linear simples/Relatório/diag_norm.txt", encoding = "latin1")

#### Boxcox

require(MASS)
boxcox(fit.model)
            
####  Obtendo os ajustes, excluindo os pontos

summary(fit.model<-lm(faturamento~gasto,  subset = -1))

summary(fit.model<-lm(faturamento~gasto, subset = - 2))

summary(fit.model<-lm(faturamento~gasto,  subset = -3))

summary(fit.model<-lm(faturamento~gasto, subset = - 29))

summary(fit.model<-lm(faturamento~gasto,  subset = -15))

summary(fit.model<-lm(faturamento~gasto,  subset = c(-15, -29)))


c <- summary(fit.model<-lm(faturamento~gasto,  subset = c(-1, -29)))
c

c <- summary(fit.model<-lm(faturamento~gasto,  subset = c(-2, -29)))
c

c <- summary(fit.model<-lm(faturamento~gasto,  subset = c(-3, -29)))
c

c <- summary(fit.model<-lm(faturamento~gasto,  subset = c(-15, -1)))
c

c <- summary(fit.model<-lm(faturamento~gasto,  subset = c(-15, -2)))
c

c <- summary(fit.model<-lm(faturamento~gasto,  subset = c(-15, -3)))
c






