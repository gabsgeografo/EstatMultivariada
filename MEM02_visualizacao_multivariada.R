# Carregar pacotes
library(ggplot2)
library(esquisse)
library(scatterplot3d) # Gráficos 3D
library(GGally)        # Representação de Draftsman
library(aplpack)       # Faces de Chernoff
library(TeachingDemos) # Faces de Chernoff
library(graphics)      # Gráfico de estrelas
library(reshape2)      # Gráfico de perfis multivariados


# Carregar dados
dados <- read.csv("pardocas.csv", header = T);head(dados)
str(dados)

dados <- transform(dados,
                   Survivorship = as.factor(Survivorship))
str(dados)


# Visualização de dados
# esquisser(dados)

# Grafico de pontos
ggplot(dados) +
 aes(x = Total_length, y = Alar_extent, 
     fill = Survivorship, colour = Survivorship) +
 geom_point(shape = "circle", size = 2) +
 labs(x = "Comprimento total (mm)", y = "Extensão alar (mm)")+
 theme_bw()+
 theme(legend.position = "bottom")


# Grafico 3D
colors <- c("#56B4E9", "#999999")
colors <- colors[as.numeric(dados$Survivorship)]
scatterplot3d(dados[,2:4], pch = 16, color=colors)


colors <- c("#56B4E9", "#999999")
colors <- colors[as.numeric(dados$Survivorship)]
scatterplot3d(dados[,2:4], pch = 16, color=colors,
              grid=TRUE, box=FALSE)
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
addgrids3d(dados[,2:4], grid = c("xy", "xz", "yz"))
legend("right", legend = levels(dados$Survivorship),
       col =  c( "#56B4E9", "#999999"), pch = 16)


# Algumas customizações
shapes <- c(17, 18) 
shapes <- shapes[as.numeric(dados$Survivorship)]
scatterplot3d(dados[,2:4], pch = shapes, color=colors)
legend("bottom", legend = levels(dados$Survivorship),
       col =  c("#56B4E9","#999999"), pch = c(17, 18), 
       inset = -0.25, xpd = TRUE, horiz = TRUE)



colors <- c("darkblue", "darkgreen")
colors <- colors[as.numeric(dados$Survivorship)]
variaveis <- dados[,c(2,4,3)]
scatterplot3d(variaveis, pch = 16,color = colors)
legend("right", legend = levels(dados$Survivorship),
                     col =  c( "darkblue", "darkgreen"), pch = 16)



# Representação de Draftsman
ggpairs(dados[,-1])
ggpairs(dados[,-1], aes(color = dados$Survivorship),
        columnLabels = c("Comprimento total","Extensão alar","C. bico e cabeça",
                         "C. Umero","C. Esterno"))



# Faces de Chernoff
caes <- read.csv("caes.csv", header = T);head(caes)

label <- caes$Group
caes2 <- caes[,-1]
rownames(caes2) <- label

## usando o pacote aplpack
fa <- faces(caes2,labels=rownames(caes2),plot=FALSE)
plot(fa,face.type=1)
plot(fa,face.type=2)


## usando o pacote TeachingDemos
attach(caes)
canmat <- data.matrix(caes2)
faces2(canmat, scale="center")



# Gráfico de estrelas
stars(caes2,nrow=2, ncol=6,labels=row.names(caes2),key.loc=c(10,0,5))
stars(caes2,draw.segments=TRUE,nrow=2,ncol=6,labels=row.names(caes2),
      key.loc=c(10,0.5))




# Gráfico de Perfis multivariados
dados_long <- melt(caes, id.vars = "Group")


# Criar o gráfico de perfis multivariados
ggplot(dados_long, aes(x = variable, y = value, color = Group, group = Group)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Gráfico de Perfis Multivariados", x = "Variáveis",
       y = "Medidas de mandibula (mm)")



# Definir a nova ordem das variáveis
nova_ordem <- c("Mol1_Breadth", "Mand_Breadth", "Mol1_Length", "Mand_Height", "Mol1.3_Length", "Mol1.4_Length")
dados_long$variable <- factor(dados_long$variable, levels = nova_ordem)

# Criar o gráfico de perfis multivariados
ggplot(dados_long, aes(x = variable, y = value, color = Group, group = Group)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Gráfico de Perfis Multivariados", x = "Variáveis", y = "Valores")


