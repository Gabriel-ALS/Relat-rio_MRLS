library(readr)
library(dplyr)
library(MASS)


#analises básicas
#####
dados <- read.csv("Base_NBB.csv")


top2_jogadores <- dados %>%
  group_by(Equipe) %>%
  arrange(desc(EF_TOTAL)) %>%
  slice(1:2)

dados_filtrados <- top2_jogadores[,c("PTS_TOTAL","EF_TOTAL")]

cor(dados_filtrados$PTS_TOTAL,dados_filtrados$EF_TOTAL)

sumPTS <- c(summary(dados_filtrados$PTS_TOTAL),
            var(dados_filtrados$PTS_TOTAL),
            sd(dados_filtrados$PTS_TOTAL))

sumEF <- c(summary(dados_filtrados$EF_TOTAL),
            var(dados_filtrados$EF_TOTAL),
           sd(dados_filtrados$EF_TOTAL))
sumEF_PTS <- summary(dados_filtrados)
sumEF_PTS

plot(dados_filtrados$PTS_TOTAL,dados_filtrados$EF_TOTAL, type = "p",
     xlab = "Total de pontos dos jogadores da NBB",
     ylab = "Eficiência Total",
     main = "Figura 3")


plot(i <- density(dados_filtrados$EF_TOTAL),
     xlab = "Eficiencia Total dos jogadores",
     ylab = "",main = "Densidade da Eficiência") #probabilidade
polygon(i, col = "#B0C4DE")



plot(j <- density(dados_filtrados$PTS_TOTAL),
     xlab = "Duplo-duplos total dos jogadores",
     ylab = "",main = "Densidade dos Duplo-Duplos") #probabilidade
polygon(j, col = "#ADD8E6")

par(new = FALSE, mfrow=c(1,2))
boxplot(dados_filtrados$EF_TOTAL, main = "Boxplot de Eficiencia dos Jogadores")
boxplot(dados_filtrados$PTS_TOTAL, main = "Boxplot de Pontos totais dos Jogadores")

fit.model <- lm(EF_TOTAL ~ PTS_TOTAL, data = dados_filtrados)
fit.model
summary(fit.model)
#####

#tranformação log(y)
##### 
boxcox(fit.model)
boxcox_melhor <- boxcox(fit.model)

lambda <-  boxcox_melhor$x[which.max(boxcox_melhor$y)]

if (lambda == 0) {
  dados_filtrados$EF_TOTAL_LOG <- log(y)
} else {
  dados_filtrados$EF_TOTAL_LOG <- (dados_filtrados$EF_TOTAL^lambda - 1) / lambda
}

fit.model <- lm(EF_TOTAL_LOG ~ PTS_TOTAL, data = dados_filtrados)
coef_det <- summary(fit.model)
coef_det$r.squared

##### 

#diag_normal
#####
#---------------------------------------------------------------#
# Para rodar este programa  deixe no objeto fit.model a sa?da 
# do  ajuste  da  regress?o com  erro normal. Deixe  os dados 
# dispon?veis  atrav?s do comando attach(...). Depois  use  o 
# comando source(...) no S-Plus ou R para executar o programa. 
# A sequ?ncia de comandos ? a seguinte:
#
#        > fit.model <- ajuste
#        > attach(dados)
#        > source("diag_norm")
#
# A sa?da ter? quatro gr?ficos: de pontos de alavanca, de pontos
# influentes  e  dois de res?duos. Para identificar os pontos
# que  mais  se destacam usar o comando identify(...). Se por
# exemplo se destacam tr?s pontos no plot(fitted(fit.model),h,...), 
# ap?s esse comando coloque
#     
#        > identify(fitted(fit.model),h,n=3)
#
# O mesmo pode ser feito nos demais gr?ficos. Nos gr?ficos de 
# res?duos foram tra?ados os limites ylim=c(a-1,b+1), onde a
# ? o menor valor e b o maior valor para o res?duo..Mude esses 
# limites  se  necess?rio.Para voltar a ter apenas um gr?fico 
# por tela fa?a o seguinte:
#
#        > par(mfrow=c(1,1))
# 
#---------------------------------------------------------------#
#
# Para rodar este programa  deixe no objeto fit.model a sa?da 
# do  ajuste  da  regress?o com  erro normal. Deixe  os dados 
# dispon?veis  atrav?s do comando attach(...). Depois  use  o 
# comando source(...) no S-Plus ou R para executar o programa. 
# A sequ?ncia de comandos ? a seguinte:
#
#        > fit.model <- ajuste
#        > attach(dados)
#        > source("diag_norm")
#
# A sa?da ter? quatro gr?ficos: de pontos de alavanca, de pontos
# influentes  e  dois de res?duos. Para identificar os pontos
# que  mais  se destacam usar o comando identify(...). Se por
# exemplo se destacam tr?s pontos no plot(fitted(fit.model),h,...), 
# ap?s esse comando coloque
#     
#        > identify(fitted(fit.model),h,n=3)
#
# O mesmo pode ser feito nos demais gr?ficos. Nos gr?ficos de 
# res?duos foram tra?ados os limites ylim=c(a-1,b+1), onde a
# ? o menor valor e b o maior valor para o res?duo..Mude esses 
# limites  se  necess?rio.Para voltar a ter apenas um gr?fico 
# por tela fa?a o seguinte:
#
#        > par(mfrow=c(1,1))
# 
#---------------------------------------------------------------#
#
# Configuração inicial dos gráficos
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X %*% solve(t(X) %*% X) %*% t(X)
h <- diag(H)
lms <- summary(fit.model)
s <- lms$sigma
r <- resid(lms)
ts <- r / (s * sqrt(1 - h))
di <- (1/p) * (h / (1 - h)) * (ts^2)
si <- lm.influence(fit.model)$sigma
tsi <- r / (si * sqrt(1 - h))
a <- max(tsi)
b <- min(tsi)

# Layout de 2x2 para os gráficos
par(mfrow = c(2, 2))

# Gráfico de pontos de alavanca
plot(h, main = "Pontos de alavanca", xlab = "Indice", ylab = "Medida h", pch = 16, ylim = c(0, 1))
abline(h = 2 * p / n, lty = 2)  # Linha de corte
identify(h,tol = 0.5, n = 4)  # Identifica pontos de interesse

# Gráfico de Distância de Cook
plot(di, main = "Pontos Influentes", xlab = "Indice", ylab = "Distancia de Cook", pch = 16)
identify(di, tol = 0.5,n = 2)

# Gráfico de Resíduos Padronizados
plot(tsi, main = "Pontos Aberrantes", xlab = "Indice", ylab = "Resíduo Padronizado", ylim = c(b - 1, a + 1), pch = 16)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
identify(tsi, n = 3)

# Gráfico de Valores Ajustados vs. Resíduos Padronizados
plot(fitted(fit.model), tsi, main = "Homocedasticidade", xlab = "Valores Ajustados", ylab = "Residuo Padronizado", ylim = c(b - 1, a + 1), pch = 16)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)
identify(fitted(fit.model), tsi, n = 3)

# Volta para um único gráfico por tela
par(mfrow = c(1, 1))


#---------------------------------------------------------------#
fit.model <- lm(EF_TOTAL ~ PTS_TOTAL, data = dados_filtrados)
summary(fit.model<-lm(dados_filtrados$EF_TOTAL ~ dados_filtrados$PTS_TOTAL,
                      subset = c(-1)))
fit.model <- lm(EF_TOTAL ~ PTS_TOTAL, data = dados_filtrados)
summary(fit.model<-lm(dados_filtrados$EF_TOTAL ~ dados_filtrados$PTS_TOTAL,
                      subset = c(-25)))
fit.model <- lm(EF_TOTAL ~ PTS_TOTAL, data = dados_filtrados)
summary(fit.model<-lm(dados_filtrados$EF_TOTAL ~ dados_filtrados$PTS_TOTAL,
                      subset = c(-34)))
fit.model <- lm(EF_TOTAL ~ PTS_TOTAL, data = dados_filtrados)
summary(fit.model<-lm(dados_filtrados$EF_TOTAL ~ dados_filtrados$PTS_TOTAL,
                      subset = c(-42)))


#---------------------------------------------------------------#
#####

#envel_normal
#####
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
#####

#examinando distancia de cook 
#####
cook <- cooks.distance(fit.model)
p <- length(coef(fit.model))

f_critical <- qf(0.5, p, 81 - p)
cook_cutoff <- f_critical / p

influential_points <- which(cook > cook_cutoff)

cat("Distâncias de Cook:\n", cook, "\n")
cat("Valor crítico de Cook's Distance:", cook_cutoff, "\n")
cat("Pontos influentes:", influential_points, "\n")
#####

#intervalo de confiança e predição
#####
# Intervalos de confiança para mu
confint(fit.model)

# Intervalos de confiança para mu
conf.int =  predict(fit.model,interval="confidence")
conf.int

#Intervalos de predição
pred.int =  predict(fit.model,interval="prediction")
pred.int

fitted.values = pred.int[,1]

pred.lower = pred.int[,"lwr"]
pred.upper = pred.int[,"upr"]
conf.lower = conf.int[,"lwr"]
conf.upper = conf.int[,"upr"]   


plot(dados_filtrados$PTS_TOTAL,dados_filtrados$EF_TOTAL)
lines(dados_filtrados$PTS_TOTAL,fitted.values, col="#483D8B",lwd=2)
lines(dados_filtrados$PTS_TOTAL,conf.lower, lwd=2,col="#A0522D")
lines(dados_filtrados$PTS_TOTAL,conf.upper, lwd=2,col="#A0522D")
lines(dados_filtrados$PTS_TOTAL,pred.lower, lwd=3,col="#66CDAA")
lines(dados_filtrados$PTS_TOTAL,pred.upper, lwd=3,col="#66CDAA")


novo_x <- data.frame(PTS_TOTAL = 450)

# Previsão com intervalo de predição
pred_intervalo <- predict(fit.model, newdata = novo_x, interval = "prediction")
print(pred_intervalo)

# Previsão com intervalo de confiança
conf_intervalo <- predict(fit.model, newdata = novo_x, interval = "confidence")
print(conf_intervalo)
#####


