# UEPG - Programa de Pos-Graduacao em Agronomia
# Metodos em Estatista Multivariada
# Teste de Significancia

#' # Carregar os pacotes que serao utilizados
library(AgroR)
library(Hotelling)
library(corpcor)

#' # Carregar dados
dados <- read.csv("pardocas.csv", header = T);head(dados)
str(dados)  # Confere a estrutura dos dados
dados <- transform(dados, Survivorship = as.factor(Survivorship))
str(dados)  # Para conferir se os dados foram transformados
head(dados) # Exibe as seis primeiras linhas da base de dados (em formato de tabela)

#' # Teste "t" (Comprimento total)
with(dados, test_two(Survivorship, 
                     Total_length, 
                     var.equal = TRUE))

#' # Teste T2 multivariado
t2_pardocas <- hotelling.test(Total_length + Alar_extent + L_beak_head + 
                                L_humerous + L_keel_sternum ~ Survivorship, 
                              data=dados)

t2_pardocas



# library(Hotelling)
# matstand <- scale(sparrows[,2:6])
# matstand.all <- data.frame(Survivorship, matstand)
# colnames(matstand.all) <- colnames(sparrows[1:6]) 
# t2testsparr <- hotelling.test(Total_length+Alar_extent+L_beak_head+
#                                 L_humerous+L_keel_sternum ~ 
#                                 Survivorship,data=matstand.all) 
# 
# print(t2testsparr) 



#' # Test F para Total length (não recomendado)
with(dados, var.test(Total_length[Survivorship=="S"],Total_length[Survivorship=="NS"]))




#' # Teste de Levene (para verificar a homogeneidade de variância)
# dados <- read.csv("pardocas.csv",header=TRUE)
attach(dados)


matstand <- scale(dados[,2:6])
matsurv <- matstand[Survivorship == "S",]
matnosurv <- matstand[Survivorship == "NS",]
vecmediansurv <- apply(matsurv, 2, median)
vecmediannosurv <- apply(matnosurv, 2, median)
matabsdevsurv <- abs(matsurv - matrix(rep(vecmediansurv,nrow(matsurv)),
                                      nrow=nrow(matsurv), byrow=TRUE))
matabsdevnosurv <- abs(matnosurv - matrix(rep(vecmediannosurv,nrow(matnosurv)),
                                          nrow=nrow(matnosurv), byrow=TRUE))
matabsdev.all <- rbind(matabsdevsurv,matabsdevnosurv)
matabsdev.all <- data.frame(Survivorship, matabsdev.all)
t.test(matabsdev.all$Total_length[Survivorship == "S"], 
       matabsdev.all$Total_length[Survivorship == "NS"], alternative="less",
       var.equal = TRUE)
t.test(matabsdev.all$Alar_extent[Survivorship == "S"], 
       matabsdev.all$Alar_extent[Survivorship == "NS"], alternative="less",
       var.equal = TRUE)
t.test(matabsdev.all$L_beak_head[Survivorship == "S"], 
       matabsdev.all$L_beak_head[Survivorship == "NS"], alternative="less",
       var.equal = TRUE)
t.test(matabsdev.all$L_humerous[Survivorship == "S"], 
       matabsdev.all$L_humerous[Survivorship == "NS"], alternative="less",
       var.equal = TRUE)
t.test(matabsdev.all$L_keel_sternum[Survivorship == "S"], 
       matabsdev.all$L_keel_sternum[Survivorship == "NS"], alternative="less",
       var.equal = TRUE)




#' # Teste de Van (Para verificar a homogeneidade de variância)
matstand <- scale(dados[,2:6])
matsurv <- matstand[Survivorship == "S",]
matnosurv <- matstand[Survivorship == "NS",]
vecmediansurv <- apply(matsurv, 2, median)
vecmediannosurv <- apply(matnosurv, 2, median)
matabsdevsurv <- abs(matsurv - matrix(rep(vecmediansurv,nrow(matsurv)),
                                      nrow=nrow(matsurv), byrow=TRUE))
matabsdevnosurv <- abs(matnosurv - matrix(rep(vecmediannosurv,nrow(matnosurv)),
                                          nrow=nrow(matnosurv), byrow=TRUE))
matabsdev.all <- rbind(matabsdevsurv,matabsdevnosurv)
matabsdev.all <- data.frame(Survivorship, matabsdev.all)
d.all <- data.frame(Survivorship,sqrt(rowSums(matabsdev.all[,-1]^2)))
colnames(d.all)[2] <- "dij"   
with(d.all, t.test(dij[Survivorship=="S"], dij[Survivorship=="NS"],
                   var.equal=TRUE, alternative="less"))
sprintf("d-values for Survivors: Mean = %2.3f, Variance = %2.3f",
        mean(d.all$dij[Survivorship=="S"]),var(d.all$dij[Survivorship=="S"]))
sprintf("d-values for Non-survivors: Mean = %2.3f, Variance = %2.3f",
        mean(d.all$dij[Survivorship=="NS"]),var(d.all$dij[Survivorship=="NS"]))

detach(dados)

