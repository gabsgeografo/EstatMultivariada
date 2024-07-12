#' ---
#' title: "Métodos de ordenação: Componentes principais"
#' subtitle: "Espécie de plantas na Reserva Natural de Steneryd"
#' author: "Werlleson Nascimento e Carlos Tadeu dos Santos Dias"
#' date: "11/07/2024"
#' output: html_document
#' ---


#' # Carregar pacotes
library(vegan)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(GGally)


#' # Carregar dados
plantas <- read.csv("Plants Steneryd Reserve.csv",header=TRUE)

plantas1 <- data.frame(plantas[,2:18])
dados <- data.frame(t(data.frame(plantas1)))
colnames(dados) <- dados[,1]
rownames(dados) <- as.character(1:17)
dados


#' # PCA
res.pca <- PCA(dados, scale.unit = TRUE, graph = F)


#' # Extraindo os autovalores
aut.val <- round(get_eigenvalue(res.pca),6);aut.val


#' # Extraindo os autovetores
autovetores <- res.pca$svd$V
colnames(autovetores) <- c("CP1","CP2","CP3","CP4","CP5") # por linha
rownames(autovetores) <- plantas$Species
autovetores


#' # Gráfico de Draftsman
plot <- c(1:nrow(dados))
coordenadas <- res.pca$ind$coord[,1:3] 
mat.plot <- data.frame(plot, coordenadas)
ggpairs(mat.plot, columns = 1:4)



