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


#' # Criando uma matriz de distâncias
Dist.euc <- dist(dados[3:40],method="euclidean")


# cmscale implicitly double-centers the resulting similarity matrix   
(pcoa.dados <- cmdscale(Dist.euc, k = 4, eig = TRUE)) 
round(pcoa.dados$eig,2) # autovalores 


# Autovalores
sum.eigpcoa <- sum(pcoa.dados$eig)
perc.var <- (pcoa.dados$eig/sum.eigpcoa)*100
cumvar.bannadi <- cumsum(perc.var)
matlambdas <- cbind(1:length(pcoa.dados$eig),pcoa.dados$eig,perc.var,cumvar.bannadi)
colnames(matlambdas) <- c("Coordenada","Autovalor não negativo","% Total","Acumulada %")
(mateigen.csv <- round(matlambdas,1))
#write.csv(mateigen.csv,"Non negative eigenvalues PCoA Steneryd.csv",row.names=FALSE)                                     # Eigenvalues



#' Gráfico de Draftsman

# Organizando os dados
total.bens <- rowSums(dados[3:40])
mat.plotPCOs <- cbind(total.bens, dados[,2], pcoa.dados$points[,1], 
                      -pcoa.dados$points[,2:3], pcoa.dados$points[,4])
colnames(mat.plotPCOs) <- c("No. Bens", "Tipo", "PCO1", "PCO2", "PCO3", "PCO4")
mat.plotPCOs <- as.data.frame(mat.plotPCOs) # Converter para um data frame

ggpairs(mat.plotPCOs)+ # Gráfico de Draftsman
  theme_test()



