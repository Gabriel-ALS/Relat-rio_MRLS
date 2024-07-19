library(readr)
library(dplyr)
library(MASS)


dados <- read.csv("Base_NBB.csv")

dados_2pts_ef <- dados[,c("DD_TOTAL","EF_TOTAL")]

sumDD <- c(summary(dados_2pts_ef$DD_TOTAL),var(dados_2pts_ef$DD_TOTAL),
           sd(dados_2pts_ef$DD_TOTAL))

sumef <- c(summary(dados_2pts_ef$EF_TOTAL),var(dados_2pts_ef$EF_TOTAL),
           sd(dados_2pts_ef$EF_TOTAL))

sumDD #estatisticas basicas das variaveis 
sumef



plot(dados_2pts_ef$DD_TOTAL,dados_2pts_ef$EF_TOTAL, type = "p",
     xlab = "Duplo-Duplos dos jogadores",
     ylab = "Eficiência Total",
     main = "Gráfico de pontos dos jogadores da NBB")

plot(i <- density(dados_2pts_ef$EF_TOTAL),
     xlab = "Eficiencia Total dos jogadores",
     ylab = "",main = "Densidade da Eficiência") #probabilidade
polygon(i, col = "#B0C4DE")



plot(j <- density(dados_2pts_ef$DD_TOTAL),
                  xlab = "Duplo-duplos total dos jogadores",
                  ylab = "",main = "Densidade dos Duplo-Duplos") #probabilidade
polygon(j, col = "#ADD8E6")

par(new = FALSE, mfrow=c(1,2))
boxplot(dados_2pts_ef$EF_TOTAL)
boxplot(dados_2pts_ef$DD_TOTAL)
cor(dados_2pts_ef$DD_TOTAL,dados_2pts_ef$EF_TOTAL)

#EXPLICAR SOBRE EFICIENCIA E ESTATISTICA DE BASQUETE
#Com esse summary podemos observar como a grande maioria dos jogadores
#não realiza duplo duplos, como observados no grafico de pontos
#porém observamos que ainda sim a media ainda se mantem positiva
#Observamos uma enorme presença de outliers pelo boxplot de eficiencia,
#diversos outros fatores influenciam isso, jogadores que ficam pouco tempo
#e produzem muito tendem a ficar com estatisticas buffadas geralmente sexto homem
#observamos que a var de DD_total é bem pequena em comparação a EF total
#isso nos indica que mesmo a presença de poucos duploduplos ja inflam bastante
#a eficiencia dos jogadores, mas também conseguimos ver que consequentemente tem
#uma baixa quantidade de jogadores com duplo-duplos na temporada

 
fit.model <- lm(EF_TOTAL ~DD_TOTAL, data = dados_2pts_ef)

fit.model <- lm(EF_TOTAL ~DD_TOTAL + I(DD_TOTAL^2), data = dados_2pts_ef)

dados_2pts_ef$EF_TOTAL <- dados_2pts_ef$EF_TOTAL


summary(fit.model)

fit.model <- lm(EF_TOTAL + 1 ~DD_TOTAL + I(DD_TOTAL^2),
                data = dados_2pts_ef)
boxcox(fit.model, lambda = seq(0, 1, 1/10))
boxcox(fit.model)

fit.model <- lm(EF_TOTAL^0.4 ~DD_TOTAL + I(DD_TOTAL^2), data = dados_2pts_ef)
summary(fit.model)

#---------------------------------------------------------------#
# Para  rodar este programa deixe no objeto fit.model a sa?da 
# do ajuste da regress?o do modelo normal linear.Deixe tamb?m
# os dados dispon?veis atrav?s do comando attach(...). Depois
# use o comando source(...) no R ou S-plus para executar o 
# programa. A sequ?ncia de comandos ? a seguinte:
#
#       fit.model <- ajuste
#       attach(dados)
#       source("envel_norm")
#
# A sa?da ser? o gr?fico de envelope para o res?duo padronizado.
# Para colocar  um  t?tulo no gr?fico ap?s a sa?da use o comando
# title("...").
#---------------------------------------------------------------#
#
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
si <- lm.influence(fit.model)$sigma
r <- resid(fit.model)
tsi <- r/(si*sqrt(1-h))
#
ident <- diag(n)
epsilon <- matrix(0,n,100)
e <- matrix(0,n,100)
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:100){
  epsilon[,i] <- rnorm(n,0,1)
  e[,i] <- (ident - H)%*%epsilon[,i]
  u <- diag(ident - H)
  e[,i] <- e[,i]/sqrt(u)
  e[,i] <- sort(e[,i]) }
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2 }
#
med <- apply(e,1,mean)
faixa <- range(tsi,e1,e2)
#
par(pty="s")
qqnorm(tsi,xlab="Percentis da N(0,1)",
       ylab="Residuo Studentizado", ylim=faixa, pch=16)
par(new=TRUE)
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1)
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1)
par(new=TRUE)
qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2)

#--------------------------------------------------------------#








