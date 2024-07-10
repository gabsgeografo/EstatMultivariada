# Codigo Visualização Grafica Multivariada - Simplificada
# Autor: Gabriel Passos (PPGCA/UEPG)

#' # Carregar pacotes
library(ggplot2)       # Plotagem de gráficos
library(esquisse)      # Criação de gráficos 
library(scatterplot3d) # Gráficos 3D
library(GGally)        # Representação de Draftsman
library(aplpack)       # Faces de Chernoff
library(TeachingDemos) # Faces de Chernoff
library(graphics)      # Gráfico de estrelas
library(reshape2)      # Gráfico de perfis multivariados
library(RColorBrewer)  # Paleta de cores


#' # Carregar os dados a serem analisados
dados <- read.csv("pardocas.csv", header = T);head(dados)
str(dados)
dados <- transform(dados,
                   Survivorship = as.factor(Survivorship))
str(dados) # Exibe os dados a partir da sua estrutura
head(dados) # Exibe as 6 primeiras linhas de dados (em formato de tabela)

#' # Visualização de dados
# esquisser(dados)


#' ## 1) Grafico de pontos - usando o pacote 'ggplot'
ggplot(dados) +
  aes(x = Total_length, y = Alar_extent, 
      fill = Survivorship, colour = Survivorship) +
  geom_point(shape = "circle", size = 2) +
  labs(x = "Comprimento total (mm)", y = "Extensão alar (mm)")+
  theme_bw()+
  theme(legend.position = "bottom")



#' ## 2) Visualização do Grafico 3D - com o pacote 'scatterplot3d'
colors <- c("#56B4E9", "#999999")
colors <- colors[as.numeric(dados$Survivorship)]
shapes <- c(17, 18) 
shapes <- shapes[as.numeric(dados$Survivorship)]

scatterplot3d(dados[,2:4], pch = shapes, color=colors,
              xlab= "Comprimento total (mm)", ylab="Comprimento alar (mm)",
              zlab="Comprimento bico e cabeça (mm)")
legend("bottom", legend = levels(dados$Survivorship),
       col =  c("#56B4E9","#999999"), pch = c(17, 18), 
       inset = -0.25, xpd = TRUE, horiz = TRUE)


#' ## 3) Representação Gráfica de Draftsman - utilizando o pacote 'ggpairs'
ggpairs(dados[,-1], aes(color = dados$Survivorship),
        columnLabels = c("Comprimento total","Extensão alar","C. bico e cabeça",
                         "C. Umero","C. Esterno"))

#' ## 4) Representação das Faces de Chernoff
caes <- read.csv("caes.csv", header = T);
head(caes)
caes$Group <-  as.factor(caes$Group) # Transforma para fator
str(caes)

# Criando o objeto 'label'
label <- caes$Group
caes2 <- caes[,-1] # Remove a primeira coluna
rownames(caes2) <- label # Rotula as linhas, portanto, nao le mais como variavel 

attach(caes)
canmat <- data.matrix(caes2)
faces2(canmat, scale="center")

## 5) Gráfico de estrelas
stars(caes2,draw.segments=TRUE,nrow=2,ncol=6,labels=row.names(caes2),
      key.loc=c(10,0.5))


#' ## 6) Gráfico de perfis multivariados (linhas)
dados_long <- melt(caes, id.vars = "Group") # A funcao 'melt' organiza o conjunto de dados, bastando indicar a var classificatoria

ggplot(dados_long, aes(x = variable, y = value, color = Group, group = Group)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Gráfico de Perfis Multivariados", x = "Variáveis",
       y = "Medidas de mandibula (mm)")


#' ### Define a nova ordem das variáveis
nova_ordem <- c("Mol1_Breadth", "Mand_Breadth", "Mol1_Length", "Mand_Height", "Mol1.3_Length", "Mol1.4_Length")
dados_long$variable <- factor(dados_long$variable, levels = nova_ordem)


ggplot(dados_long, aes(x = variable, y = value, color = Group, group = Group)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Gráfico de Perfis Multivariados", x = "Variáveis", y = "Valores")


#' ## 7) Gráfico de perfis multivariados (barras)
ggplot(dados_long, aes(Group, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
  scale_fill_grey(start = 0.2, end = 0.8) +  # Escala de cinza e ajusta os valores de start e end conforme necessário
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


# Grafico de perfis multivariados (em barras) monocromático
ggplot(dados_long, aes(Group, value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "BluGn") +  # Usando uma paleta do RColorBrewer
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

display.brewer.all() # Abre a opcao de paleta de cores

