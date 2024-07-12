#' # Carregar pacotes
library(vegan)    # EMD
library(ggplot2)  # Gráficos
library(ggrepel)  # Ajuste nos graficos
library(gridExtra) # Exibicao dos graficos

#' # Carregar dados
Congmen <- read.csv("Voting NJ Congressmen.csv",header=TRUE,row.names=1)
attach(Congmen)

#' # Convertendo os dados em matriz
Vote.mat <- as.matrix(Congmen) 
colnames(Vote.mat) <- rownames(Vote.mat)
dist.Vote <- as.dist(Vote.mat)
dist.Vote   

# Realizacao do Escalonamento Multidimensional, sendo "k" o numero de dimensoes
(MDSdim2Vote.meta <- metaMDS(dist.Vote, k=2, autotransform=FALSE))
(stress.dim2 <- MDSdim2Vote.meta$stress)

(MDSdim3Vote.meta <- metaMDS(dist.Vote, k=3, autotransform=FALSE))
(stress.dim3 <- MDSdim3Vote.meta$stress)

(MDSdim4Vote.meta <- metaMDS(dist.Vote, k=4, autotransform=FALSE))
(stress.dim4 <- MDSdim4Vote.meta$stress)

scores.sites3 <- scores(MDSdim3Vote.meta, display="sites")



# Gráfico de Dimensão 1 vs Dimensão 2
p1 <- ggplot(data = as.data.frame(scores.sites3), aes(x = NMDS1, y = NMDS2, label = rownames(scores.sites3))) +
  geom_point(color = "blue", shape = 16) +
  geom_text_repel(size = 3, box.padding = 0.5) +
  labs(x = "Dimension 1", y = "Dimension 2", title = "metaMDS function") +
  # theme_minimal() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)+
  theme_test()

p1

# Gráfico de Dimensão 1 vs Dimensão 3
p2 <- ggplot(data = as.data.frame(scores.sites3), aes(x = NMDS1, y = NMDS3, label = rownames(scores.sites3))) +
  geom_point(color = "blue", shape = 16) +
  geom_text_repel(size = 3, box.padding = 0.5) +
  labs(x = "Dimension 1", y = "Dimension 3", title = "isoMDS function") +
  # theme_minimal() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)+
  theme_test()
p2

# Mostrar os gráficos lado a lado
grid.arrange(p1, p2, nrow = 1)


detach(Congmen) 