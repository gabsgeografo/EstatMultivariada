# UEPG - Pós-graduação em Agronomia
# Métodos em Estatística Multivariada (2024/2) - Prof. Dr. Carlos T S Dias
# Autoria: Equipe da Disciplina - Modificacoes: Gabriel Passos

#' Carregar pacotes
library("FactoMineR") # Realiza a PCA
library("factoextra") # Gera os gráficos da PCA
library("ggplot2")    # Gera os gráficos 

#' Carregar dados das Pardocas (enunciado do exercício) e exibe as informacoes
dados <- read.csv("pardocas.csv", header = T) 
head(dados)

#' Calculando as correlações da base de dados
cor(dados[,-1]) # A primeira coluna nao eh numerica

#' #  Realizando a Analise dos Componentes Principais (PCA) 
res.pca <- PCA(dados[,-1], scale.unit = TRUE, graph = F) # O 'scale.unit' padroniza as unidades (variância unitária)
                                                         # O 'graph' gera gráficos automaticamente, porem, queremos elaborar
                                                         # os nossos graficos
print(res.pca)        # Exibe tudo o que a funcao executa. No entanto, queremos executar somente algumas tarefas especificas

#' # Encontrando os autovalores
aut.val <- round(get_eigenvalue(res.pca),6); aut.val     # Indicamos de qual objeto queremos extrair os autovalores

#' # Encontrando os autovetores
res.pca$svd$V # Extrai os autovetores por linha
              # O pacote pode inverter o sinal do autovetor (mas isso NAO eh problema)

#' # Contribuições de variáveis 
(res.pca$var$contrib) # em porcentagem
sqrt(res.pca$var$contrib/100) #autovalores


#' # Screeplot - Grafico
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))+
  theme_test()

#' Considerando-se o gráfico 'Screeplot' também pode-se perceber nitidamente que
#' os dois primeiros componentes principais explicam a maior parte da variação
#' existente no conjunto de dados

#' # Resultados para variáveis
var <- get_pca_var(res.pca)
var

#' # Correlação entre variável e componente
res.pca$var$coord #Nomes dos componentes


#' A partir de um estudo de correlação entre as variáveis e os componentes 
#' principais, é possível perceber que...

#' # Circulo de correlação entre as variáveis
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


#' # Exibe o quanto cada variavel contribuiu (graficamente) para a composicao do PCA
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

#' # Gráficos Biplots

#' # Biplot (CP1 E CP2) com elipses de concentração
fviz_pca_biplot(res.pca, 
                col.ind = dados$Survivorship, addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE, legend.title = "Sobreviência")+
  theme_test()

# O comando 'repel' impede que uma variavel sobrescreva a outra

#' Carregar dados dos países da Europa
dados <- read.csv("Euroemp.csv", header = T, row.names = 1)
head(dados)

#' Calculando as correlações
cor(dados[,-c(1:2)])

#' Realizando a PCA 
res.pca <- PCA(dados[,-1], scale.unit = TRUE, graph = F)
print(res.pca)

#' # Extraindo os autovalores
aut.val <- round(get_eigenvalue(res.pca),6)
aut.val

#' # Extraindo os autovetores
res.pca$svd$V # Extrai os autovetores linha a linha

#' # Contribuições de variáveis (O quanto cada variavel influencia)
(res.pca$var$contrib) # em porcentagem
sqrt(res.pca$var$contrib/100) # em autovalores

#' Plota o Screeplot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 40))+
  theme_test()

#' Considerando-se o gráfico screeplot também pode-se perceber nitidamente que
#' os dois primeiros componentes principais explicam a maior parte da variação
#' existente no conjunto de dados

#' # Resultados para variáveis
var <- get_pca_var(res.pca)
var

#' # Correlação entre variável e componente
res.pca$var$coord #Nomes dos componentes

#' A partir de um estudo de correlação entre as variáveis e os componentes 
#' principais, é possível perceber que ...

#' # Circulo de correlação entre as variáveis
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#' # Variáveis que mais contribuiram (graficamente)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

#' # Gráficos Biplot's

#' # Biplot (CP1 E CP2) com elipses de concentração
fviz_pca_biplot(res.pca,
                addEllipses = FALSE, label = c("ind","var"),
                col.var = "blue", repel = TRUE,
                legend.title = "Grupo")+
  theme_test()
