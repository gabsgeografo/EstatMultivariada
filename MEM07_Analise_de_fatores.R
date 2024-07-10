# UEPG - Programa de Pos-Graduacao em Agronomia
# Metodos em Estatista Multivariada
# Analise de Fatores

#' # Carregar pacotes que serao utilizados
library(corrplot)    # Construcao do heatmap (mapa de calor)
library(FactoMineR)  # Faz as Análises de Fator (FA)
library(factoextra)  # Plota os gráficos
library(ggplot2)
library(cowplot)     # Compara os graficos

#' # Carregar os dados
dados <- read.csv("Euroemp.csv", header = T, row.names = 1); # 'row.names' lê a coluna como rotulo
head(dados)
str(dados)

dados <- transform(dados, Group = as.factor(Group)) # Transforma a variavel em fator
str(dados) # Confere se a variavel foi transformada

#' Realiza a Matriz de correlação e gráficos heatmap
R <- cor(dados[,-1]) # O [,-1] desconsidera a primeira coluna 
R                    # Verifica a matriz no console (caso prefira)
                     # Por padrao, eh calculado o R de Pearson, mas eh possivel selecionar outros metodos

# Plota os graficos de correlacao - a saida eh rigorosamente a mesma, o que muda eh a forma de visualizacao
corrplot(R,  method="circle")
corrplot(R,  method="pie")
corrplot(R,  method="color")
corrplot(R,  method="number")
corrplot(R,  method="square")
corrplot(R,  method="ellipse") # Mostra a direcao da correlacao (se eh positiva ou negativa)

#' Execucao da Análise Fatorial (AnFat)
res.anfat <- PCA(dados[,-1], 
                 scale.unit = TRUE, 
                 graph = F)
print(res.anfat)               # Mostra tudo o que a analise eh capaz de fazer


#' Obtendo os autovalores
autovalores <- get_eigenvalue(res.anfat)
autovalores


#' # Extraindo os autovetores
autovetores <- res.anfat$svd$V # autovetores lidos linha a linha
autovetores

#' # Gráfico Biplot (para PCA)
A <- fviz_pca_biplot(res.anfat,
                addEllipses = FALSE, 
                label = c("ind","var"),
                col.var = "blue", 
                repel = TRUE,               # O comando 'repel' impede que uma variavel sobrescreva a outra
                legend.title = "Grupo")+
  theme_test()
A 

#' # Calculando as cargas fatoriais

# Multiplicando cada coluna da matriz de autovetores pela
# raiz quadrada do autovalor correspondente para obter
# as cargas fatoriais

autovetores. <- autovetores[,1:4]          # O objeto 'autovetores.' eh diferente do objeto 'autovetores'
desvios <- sqrt(autovalores)
cargas <- sweep(autovetores.,
                MARGIN=2,
                desvios[1:4,1],`*`) # O simbolo '*' indica 'produto'
cargas


# Calculando as comunalidades
comunalidades <- rowSums(cargas^2)
comunalidades


#' # Varimax (Variancia Maxima)
rot.fact.emp <- varimax(cargas)
rot.fact.emp


fact.load.emp <- rot.fact.emp$loadings[1:9,1:4] # Seleciona linhas e colunas a serem salvas no objeto
fact.load.emp

colnames(fact.load.emp) <- paste0("Factor", 1:ncol(fact.load.emp)) # Renomeando o objeto 'linha'
rownames(fact.load.emp) <- colnames(dados[,-1])                    # Renomeando o objeto 'coluna'
fact.load.emp


#' # Valores rotacionados dos fatores calculando de acordo com a equação 7.4 (Manly e Alberto, 2017)
scale.emp <- scale(dados[-1])
escores <- as.matrix(scale.emp)%*%fact.load.emp%*%solve(t(fact.load.emp)%*%fact.load.emp) # Realizamos o produto de matrizes
rownames(escores) <- dados[,1]
escores

rotated_loadings <- as.data.frame(fact.load.emp) # Convertendo os valores rotacionados em um 'data.frame'

# Função auxiliar para criar um objeto de classe PCA a partir dos resultados rotacionados
create_pca_object <- function(loadings, pca) {
  pca$var$coord <- as.matrix(loadings)
  pca$var$cos2 <- loadings^2
  pca$var$contrib <- (loadings^2) / rowSums(loadings^2) * 100
  return(pca)
}

# Criar um objeto PCA com as cargas rotacionadas
res.anfat_rot <- create_pca_object(fact.load.emp, res.anfat)

# Gráfico com os valores rotacionados
B <- fviz_pca_biplot(res.anfat_rot, 
                repel = TRUE,
                title = "Fatorial")+
  theme_test()

B

# Comparando os graficos
plot_grid(A, B)







################################################################################
####                       Usando o pacote psych
################################################################################

#' # Carregando pacotes
library(psych)


#' Carregando dados
euroemp <- read.csv("Euroemp.csv", header=TRUE, row.names=1)
attach(euroemp)


#' # Análise defatores por PCA
fit.pc <- principal(euroemp[-1], nfactors=4, rotate="varimax")
fit.pc                  # (ver pag 131)
round(fit.pc$values, 3) # Autovalores   (ver tabela 7.2)
fit.pc$loadings         # Cargas
for (i in c(1,3,2,4)) { print(fit.pc$loadings[[1,i]])}   # Loadings with more digits
fit.pc$communality      # Comunalidades (ver pagina 129)


#' Escores rotacionados 
fit.pc$scores
detach(euroemp)


