# UNIVERSIDADE ESTADUAL DE PONTA GROSSA
# Estatistica Multivariada
# Autor: Gabriel Passos (2024/2)

#' # Carregar pacotes
library(rstatix) # Vamos usar o pacote para o teste 'M', de Box
library(MVN)     # Vamos usar o pacote para testar a normalidade - uni e multivariada
library(GGally)  # Vamos usar o pacote para visualizar dados 
library(AgroR)
library(car)     # Vamos usar o pacote para o teste de Levene
library(ggstats)
library(MultivariateAnalysis)


#' # Carregar dados
dados <- read.csv("cranios_egipicios.csv",header=TRUE)
str(dados) # Visualiza a estrutura de dados 
dados$Period <- as.factor(dados$Period) # Transforma a variavel 'Period' em fator
str(dados) # Visualiza a estrutura de dados (para saber se a variavel foi transformada)

#' # ANOVA univariada
dados.aovMB <- aov(Maximum.breadth ~ Period, data=dados) # Estrutura (variavel resosta ~ em funcao de)
summary(dados.aovMB) # Exibe a analise de dados

# De maneira analoga podemos executar o seguinte comando, usando pacotes (o pacote usado aqui eh o 'AgroR' p/ DIC)
# with(dados, DIC(Period, Maximum.breadth, ylab = "y")) # Este comando ja realiza o Teste de Tukey 

# Retornando para o metodo original (em 'R') 
dados.aovBH <- aov(Basibregmatic.height ~ Period, data=dados)
summary(dados.aovBH)

dados.aovBL <- aov(Basialveolar.length ~ Period, data=dados)
summary(dados.aovBL)

dados.aovNH <- aov(Nasal.height ~ Period, data=dados)
summary(dados.aovNH) 


# MANOVA - ANÁLISE MULTIVARIADA DA VARIANCIA
#' # Verificando as pressuposições da MANOVA
#' ## Passo 1: Verificão da normalidade MULTIVARIADA - por grupo: Teste de Henze-Zirkler - pacote MVN:
mvn(data = dados, subset = "Period", mvnTest = "hz") # Indica quais sao os dados, variavel classif. e o teste a ser usado

## No entanto, o teste da funcao 'mvn' eh feito grupo a grupo (ATENCAO)

#' ## Passo 2: Verifição da presença de outliers MULTIVARIADOS
## Pela distancia de Mahalanobis (outlier = p<0,001)
dados %>% group_by(Period) %>% 
  doo(~mahalanobis_distance(.)) %>%  # Aqui vamos usar o pacote 'rstatix'
  filter(is.outlier == TRUE)


#' ## Passo 3: Verificação da homogeneidade das matrizes de covariâncias e variâncias
box_m(dados[,c(2:5)], dados$Period) # Teste de Box



#' ### Verificação da homogeneidade de variâncias - teste de Levene (pacote car)
leveneTest(residuals(lm(Maximum.breadth ~ Period, dados))~Period, dados)
leveneTest(residuals(lm(Basibregmatic.height ~ Period, dados))~Period, dados)
leveneTest(residuals(lm(Basialveolar.length ~ Period, dados))~Period, dados)
leveneTest(residuals(lm(Nasal.height ~ Period, dados))~Period, dados)


# Passo 4: Verifição da presença de multicolinearidade (r > 0,9)
## Matriz de correlação
matriz <- cor(dados[,c(2:5)]); matriz




# Passo 5: Verifição da relação linear entre as variáveis dependentes por grupo
ggpairs(dados[,c(2:5)], aes(colour = dados$Period, alpha = 0.5))+
  theme_bw()

ggsave("DAC_cor.png", dpi = 300) # Salva a imagem do grafico em 'x'dpi


# MANOVA
dados.mnv <- manova(as.matrix(dados[,-1])~ Period, data=dados)
summary.manova(dados.mnv) # Teste de Pillai eh o default in R
summary.manova(dados.mnv,test="Wilks") # Seleciona o teste de Wilks
summary.manova(dados.mnv,test="Hotelling-Lawley") # Analogo para Hotteling-Lawley
summary.manova(dados.mnv,test="Roy")# # Analogo para Roy

summary.aov(dados.mnv) # Faz a MANOVA para cada variavel 

# Realiza a MANOVA a partir do pacote 'MultivariateAnalysis'
data("Dados.DBC")
MANOVA(Dados.DBC,2)










