#' ---
#' title: "Métodos de ordenação: Coordenadas Principais"
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
library(ggrepel)



#' # Carregar dados
plantas <- read.csv("Plants Steneryd Reserve.csv",header=TRUE)
plantas1 <- data.frame(plantas[,2:18])
dados <- data.frame(t(data.frame(plantas1)))
colnames(dados) <- dados[,1]
rownames(dados) <- as.character(1:17)
dados



stress.vec=rep(NA,5)
MDSdim1dados <- metaMDS(dados,distance="euclidean",autotransform=FALSE,k=1,trymax=50)
stress.vec[1] <- MDSdim1dados$stress
MDSdim2dados <- metaMDS(dados,distance="euclidean",autotransform=FALSE,trymax=100)
stress.vec[2] <- MDSdim2dados$stress
MDSdim3dados <- metaMDS(dados,distance="euclidean",autotransform=FALSE,k=3,trymax=150)
stress.vec[3] <- MDSdim3dados$stress
MDSdim4dados=metaMDS(dados,distance="euclidean",autotransform=FALSE,k=4,trymax=200)
stress.vec[4] <- MDSdim4dados$stress
MDSdim5dados <- metaMDS(dados,distance="euclidean",autotransform=FALSE,k=5,trymax=250)
stress.vec[5] <- MDSdim5dados$stress



# Screeplot
plot(c(1:5),stress.vec,type="b",xlab="Dimension",ylab="Stress",col="blue",
     main="Scree plot \n 25 plant species on 17 plots in dados Reserve Nature, Sweden")
matscores.dim3 <- scores(MDSdim3dados,display="sites")


# Graficos similares a figura 12.5
matscores.dim3[,3] <- -matscores.dim3[,3]
matscores.dim3
rownames(matscores.dim3) <- paste("P",1:nrow(matscores.dim3),sep="")
par(mfrow = c(2, 2))
par(cex = 0.7)
par(mar = c(3, 3, 1, 1), oma = c(2, 2, 2, 2))
for (j in 2:3) {
  for (i in 1:2) {
    if (i != j) { plot(matscores.dim3[,i],matscores.dim3[,j],type="n")}
    else {plot(0,0, type="n",xaxt="n",yaxt="n", bty="n")
    }
    text(matscores.dim3[,i],matscores.dim3[,j],rownames(matscores.dim3))
  }
}
mtext("Dim-1",side=1,line=-0.4, outer=TRUE, at=c(0.27,-1), cex=0.9)
mtext("Dim-2",side=1,line=-0.4, outer=TRUE, at=c(0.77,-1), cex=0.9)
mtext("Dim-3",side=2,line=-0.4, outer=TRUE, at=c(-1,0.27), cex=0.9)
mtext("Dim-2",side=2,line=-0.4, outer=TRUE, at=c(-1,0.77), cex=0.9)

