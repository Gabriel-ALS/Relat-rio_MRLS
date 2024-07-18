library(readr)
library(dplyr)

dados <- read.csv("Base_NBB.csv")

dados_2pts_ef <- dados[,c("Jogador","DD_TOTAL","EF_TOTAL")]

dados_2pts_ef_agrupado <- dados_2pts_ef %>%
  group_by(Jogador) %>%
  summarise(DD_TOTAL = sum(DD_TOTAL, na.rm = TRUE),
            EF_TOTAL = sum(EF_TOTAL, na.rm = TRUE))

summary(dados_2pts_ef_agrupado$DD_TOTAL)
summary(dados_2pts_ef_agrupado$EF_TOTAL)
var(dados_2pts_ef_agrupado$DD_TOTAL)
var(dados_2pts_ef_agrupado$EF_TOTAL)

plot(dados_2pts_ef_agrupado$DD_TOTAL,dados_2pts_ef_agrupado$EF_TOTAL, type = "p",
     xlab = "Duplo-Duplos dos jogadores",
     ylab = "Eficiência Total",
     main = "Gráfico de pontos dos jogadores da NBB")

plot(i <- density(dados_2pts_ef_agrupado$EF_TOTAL),
     xlab = "Eficiencia Total dos jogadores",
     ylab = "",main = "Densidade da Eficiência") #probabilidade
polygon(i, col = "#B0C4DE")



plot(j <- density(dados_2pts_ef_agrupado$DD_TOTAL),
                  xlab = "Duplo-duplos total dos jogadores",
                  ylab = "",main = "Densidade dos Duplo-Duplos") #probabilidade
polygon(j, col = "#ADD8E6")

boxplot(dados_2pts_ef_agrupado$EF_TOTAL)
cor(dados_2pts_ef_agrupado$DD_TOTAL,dados_2pts_ef_agrupado$EF_TOTAL)

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

