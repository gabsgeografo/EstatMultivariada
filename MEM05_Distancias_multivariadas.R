# UEPG - Programa de Pos-Graduacao em Agronomia
# Metodos em Estatista Multivariada

################################################################################
####                       Distancias Multivariadas
################################################################################


#' #  Carregar pacotes
library(MultivariateAnalysis) # Distancias
library(dplyr)    
library(biotools)             # Matriz de variância agrupada
library(ape)                  # Teste de Mantel


#' # Carregar dados
dados <- read.csv("caes.csv", header=TRUE, row.names = 1); head(dados)


#' Calculo da Distancia euclidiana pelo pacote 'MultivariateAnalysis'
dist.euc <- Distancia(dados,4)
dist.euc$Distancia

#' # Leitura da base de dados para o calculo da Dist. Penrose
#' Usamos o comando 'read.csv'
dados <- read.csv("cranios_egipicios.csv",header=TRUE);
head(dados)


# Função para calcular a distância de Penrose e rotular saídas
calcular_penrose <- function(dados, a, b) {
  # Converte a primeira coluna para fator
  fator. <- as.factor(dados[, 1])
  
  # Calcular médias dos atributos para cada período
  means.df <- aggregate(as.matrix(dados[, a:b]), list(var.clas = fator.), mean)
  
  # Lista de matrizes de covariância por período
  covs.list <- by(dados[, a:b], fator., cov)
  
  # Número de unidades amostrais
  n <- nrow(dados)
  # Número de variáveis
  p <- b - a + 1
  # Número de grupos
  m <- nlevels(fator.)
  # Número de observações por período
  Period.list <- table(fator.)
  # Matriz de covariância agrupada
  V.pool <- boxM(dados[, a:b], fator.)$pooled
  
  # Inicializando a matriz de distância de Penrose
  P <- matrix(rep(0, m * m), nrow = m, ncol = m)
  for (j in 1:m) { 
    for (i in 1:m) {
      if (i == j) {
        P[i, i] <- 0
      } else {
        for (k in 1:p) {
          P[i, j] <- P[i, j] + (((means.df[i, (k + 1)] - means.df[j, (k + 1)])^2) / (p * V.pool[k, k]))
        }
      }
    }
  }
  
  # Nomear as linhas e colunas da matriz de distância com os níveis de período
  rownames(P) <- levels(fator.)
  colnames(P) <- levels(fator.)
  
  return(P)
}

# Supondo que seus dados estejam no dataframe 'dados'
distancias_penrose <- calcular_penrose(dados, 2, 5)
distancias_penrose


#' # Calculo da Distância de Mahalanobis
#' Vamos ler os dados dos cranios egipcios
dados <- read.csv("cranios_egipicios.csv",header=TRUE);head(dados)
dados$rept <- rep(1:5, each=30); # Simula um dado de DIC
head(dados)

dados <- dados %>% 
  select(Period,rept,
         Maximum.breadth,
         Basibregmatic.height,
         Basialveolar.length,
         Nasal.height)


m <- MANOVA(dados,2)
Med <- apply(dados[, -c(1:2)],2,function(x) tapply(x,as.factor(dados[,1]),mean))
CRE <- m$CovarianciaResidual
dist_mahalanobis <- Distancia(Med,7,CRE)

dist_mahalanobis$Cov #matriz de variâncias e covariâncias combinada
dist_mahalanobis$Distancia



#' # Aleatorização de mantel
# Definindo a matriz M simétrica
M <- matrix(c(0,   1.0, 1.4, 0.9,
              1.0, 0,   1.1, 1.6,
              1.4, 1.1, 0,   0.7,
              0.9, 1.6, 0.7, 0), nrow = 4, byrow = TRUE)

# Definindo a matriz R simétrica
R <- matrix(c(0,   0.5, 0.8, 0.6,
              0.5, 0,   0.5, 0.9,
              0.8, 0.5, 0,   0.4,
              0.6, 0.9, 0.4, 0), nrow = 4, byrow = TRUE)



mantel.test(R, M, graph = TRUE,
            main = "Mantel test: a random example with 6 X 6 matrices
representing asymmetric relationships",
            xlab = "z-statistic", ylab = "Density",
            sub = "The vertical line shows the observed z-statistic")



#' # Outras medidas
Local <- c(1:10)
Especie1 <- c(0, 0, 1, 1, 1, 0, 1, 1, 1, 0)
Especie2 <- c(1, 1, 1, 1, 0, 0, 0, 0, 1, 1)

dados <- data.frame(Local, Especie1, Especie2)
dados


#' # dissimilaridade de Jaccard
dist.jac <- Distancia(dados,12)
dist.jac$Distancia


