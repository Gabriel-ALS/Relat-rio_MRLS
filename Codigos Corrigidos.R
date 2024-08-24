library(readr)
library(dplyr)
library(MASS)



dados <- read.csv("Base_NBB.csv")

dados

top2_jogadores <- dados %>%
  group_by(Equipe) %>%
  arrange(desc(EF_PORJOGO)) %>%
  slice(1:2)

dados_filtrados <- top2_jogadores[,c("DD_TOTAL","EF_PORJOGO")]

sumDD <- c(summary(dados_filtrados$DD_TOTAL),
           var(dados_filtrados$DD_TOTAL),sd(dados_filtrados$DD_TOTAL))
sumEF <- c(summary(dados_filtrados$EF_PORJOGO),
           var(dados_filtrados$EF_PORJOGO),sd(dados_filtrados$EF_PORJOGO))

plot(dados_filtrados$DD_TOTAL,dados_filtrados$EF_PORJOGO, type = "p",
     xlab = "Duplo-Duplos dos jogadores",
     ylab = "Eficiência Total",
     main = "Gráfico de pontos dos jogadores da NBB")

plot(i <- density(dados_filtrados$EF_PORJOGO),
     xlab = "Eficiencia Total dos jogadores",
     ylab = "",main = "Densidade da Eficiência") #probabilidade
polygon(i, col = "#B0C4DE")



plot(j <- density(dados_filtrados$DD_TOTAL),
     xlab = "Duplo-duplos total dos jogadores",
     ylab = "",main = "Densidade dos Duplo-Duplos") #probabilidade
polygon(j, col = "#ADD8E6")

par(new = FALSE, mfrow=c(1,2))
boxplot(dados_filtrados$EF_PORJOGO)
boxplot(dados_filtrados$DD_TOTAL)
cor(dados_filtrados$DD_TOTAL,dados_filtrados$EF_PORJOGO)


fit.model_basico <- lm(EF_PORJOGO ~I(DD_TOTAL^2), data = dados_filtrados)
fit.model_basico

boxcox_result <- boxcox(fit.model_basico)

# Encontrar o melhor lambda (λ)
lambda <- boxcox_result$x[which.max(boxcox_result$y)]
cat("Melhor lambda (λ):", lambda, "\n")

# Transformar a variável resposta EF_PORJOGO usando o lambda estimado
if (lambda == 0) {
  dados_filtrados$EF_PORJOGO_transformed <- log(dados_filtrados$EF_PORJOGO)
} else {
  dados_filtrados$EF_PORJOGO_transformed <- (dados_filtrados$EF_PORJOGO^lambda - 1) / lambda
}

# Ajustar o modelo novamente com a variável transformada
fit.model_transformed <- lm(EF_PORJOGO_transformed ~ I(DD_TOTAL^2),
                            data = dados_filtrados)
summary(fit.model_transformed)


boxcox(fit.model_transformed)


fit.model <- fit.model_transformed

boxcox(fit.model)
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
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
lms <- summary(fit.model)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(fit.model)$sigma
tsi <- r/(si*sqrt(1-h))
a <- max(tsi)
b <- min(tsi)
par(mfrow=c(2,2))
plot(h,xlab="Indice", ylab="Medida h", pch=16, ylim=c(0,1))
cut <- 2*p/n
abline(cut,0,lty=2)
#identify(h, n=5)
#title(sub="(a)")
#
plot(di,xlab="Indice", ylab="Distancia de Cook", pch=16)
#identify(di, n=3)
#title(sub="(b)")
#
plot(tsi,xlab="Indice", ylab="Res?duo Padronizado",
     ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(tsi, n=3)
#title(sub="(c)")
#
plot(fitted(fit.model),tsi,xlab="Valores Ajustados", 
     ylab="Residuo Padronizado", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#title(sub="(d)")
#identify(fitted(fit.model),tsi, n=3)
#---------------------------------------------------------------# diag_normal
#####

summary(fit.model <-lm(dados_filtrados$EF_PORJOGO_transformed ~ I(dados_filtrados$DD_TOTAL^2),
                      subset = c(-54, -66, - 26, - 30, -3, -24, -25, -64, -59)))

new_data <- data.frame(DD_TOTAL = dados_filtrados$DD_TOTAL)
new_data$DD_TOTAL_squared <- I(new_data$DD_TOTAL^2)

fitted.values <- predict(fit.model_transformed, newdata = new_data)
conf.int <- predict(fit.model_transformed, newdata = new_data, interval = "confidence")
pred.int <- predict(fit.model_transformed, newdata = new_data, interval = "prediction")

# Extraindo os valores dos intervalos de confiança e predição
conf.lower <- conf.int[, "lwr"]
conf.upper <- conf.int[, "upr"]
pred.lower <- pred.int[, "lwr"]
pred.upper <- pred.int[, "upr"]

# Plotar os dados e os valores ajustados com intervalos de confiança/predição
plot(I(dados_filtrados$DD_TOTAL^2), dados_filtrados$EF_PORJOGO_transformed, xlab = "DD_TOTAL^2", ylab = "EF_PORJOGO_transformed")
lines(I(dados_filtrados$DD_TOTAL^2), fitted.values, col = "red", lwd = 2)
lines(I(dados_filtrados$DD_TOTAL^2), conf.lower, lwd = 3, col = "darkgreen")
lines(I(dados_filtrados$DD_TOTAL^2), conf.upper, lwd = 3, col = "darkgreen")
lines(I(dados_filtrados$DD_TOTAL^2), pred.lower, lwd = 2, col = "blue")
lines(I(dados_filtrados$DD_TOTAL^2), pred.upper, lwd = 2, col = "blue")


