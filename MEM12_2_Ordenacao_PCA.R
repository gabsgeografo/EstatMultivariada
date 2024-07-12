#' ---
#' title: "Métodos de ordenação: Componentes principais"
#' subtitle: "Túmulos em Bannadi"
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
dados <- read.csv("Bannadi burials.csv",header=TRUE)
summary(dados[,1])


#' # PCA
res.pca <- PCA(dados[3:40], scale.unit = TRUE, graph = F)


#' # Extraindo os autovalores
aut.val <- round(get_eigenvalue(res.pca),6);aut.val
# write.csv(aut.val,"Eigenvalues Bannadi burials.csv",row.names=FALSE)


#' # Extraindo os autovetores
autovetores <- res.pca$svd$V
colnames(autovetores) <- c("CP1","CP2","CP3","CP4","CP5") # por linha
autovetores


#' # Gráfico de Draftsman
total.bens <- rowSums(burials[3:40])
coordenadas <- res.pca$ind$coord[,1:3]
mat.plot <- data.frame(total.bens, burials[,2], coordenadas)
ggpairs(mat.plot, columns = 1:5)


#' Diagrama de Draftsman para 47 sepulturas de Bannadi. Asvariáveis plotadas 
#' são o número total de diferentes tipos de bens, o tipo de 
#' restos mortais (1 = adulto masculino, 2 = adulto feminino, 3 = filho) 
#' e os quatro primeiros componentes principais



