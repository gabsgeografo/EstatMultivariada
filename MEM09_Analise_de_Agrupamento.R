# UEPG - Programa de Pos-Graduacao em Agronomia
# Metodos em Estatista Multivariada

################################################################################
####                     Análise de Agrupamento (Clusters)
################################################################################

#' # Carregar pacotes
library(factoextra)


#' # Carregar dados
dados <- read.csv("Euroemp.csv", header = T, row.names = 1);  # A coluna '1' sera lida como rotulo
head(dados)
df <- scale(dados[,-1]) # Padronização dos dados (media = 0, variancia = 1)

#' # Análise de agrupamento hierárquico (Dendrograma) usando a base da Linguagem "R"
res.dist <- dist(df, method = "euclidean") # Distancia Euclidiana

res.hc <- hclust(res.dist, method = "single") # Vizinho mais proximo
plot(res.hc, cex = 0.5)
a <- plot(res.hc, cex = 0.5)

res.hc <- hclust(res.dist, method = "ward.D2") 
plot(res.hc, cex = 0.5)
b <- plot(res.hc, cex = 0.5)

res.hc <- hclust(res.dist, method = "complete") # Vizinho mais longe
plot(res.hc, cex = 0.5)
c <- plot(res.hc, cex = 0.5)

res.hc <- hclust(res.dist, method = "average") # Vizinho medio
plot(res.hc, cex = 0.5)
d <- plot(res.hc, cex = 0.5)

# Para unir os quatro graficos, vamos usar a programacao abaixo
par(mfrow=c(2,2))
a
b
c
d
par(mfrow=c(1,1))

#' # K-means (k-medias)
res.km <- eclust(df, "kmeans", k = 4)
fviz_cluster(res.km, repel = TRUE, xlab = "Observacao", ylab = "Variabilidade")
fviz_silhouette(res.km, repel = TRUE)
res.km$nbclust
# fviz_gap_stat(res.km$gap_stat) #número de ótimo de grupos




#' # Análise de agrupamento hierárquico usando o 'factoextra'
res.dist <- dist(df, method = "euclidean") #ward.d2 método
fviz_dend(res.hc, rect = TRUE, horiz = FALSE, 
          type = "rectangle", 
          xlab = "Observacoes", 
          ylab = "Variabilidade") # dendrogama
fviz_dend(res.hc, rect = TRUE, horiz = FALSE, 
          type = "phylogenic", 
          xlab = "Observacoes", 
          ylab = "Distancia") # dendrogama




#' ## Método do vizinho mais próximo
res.hc <- eclust(df, "hclust", k=10, hc_method = "single") # O valor de "k" eh arbitrario pelo usuario
fviz_dend(res.hc, rect = TRUE) # dendrogama
fviz_dend(res.hc, rect = TRUE,type = "phylogenic") # dendrogama


#' ## Usando o pacote 'Metan'
library(metan)
d1 <- clustering(dados[,-1],
           scale = TRUE,
           clustmethod = "single",
           nclust = 4)
plot(d1, horiz = TRUE, ylab = "Distância euclidiana")

d1$cophenetic
