# Carregando as matrizes
A <- matrix(c(2,-1,
              1,1), nrow = 2, byrow = T);A # O comando 'byrow' imprime a linha exatamente como foi escrita, impede que linha vire coluna (e vice-versa)

B <- matrix(c(1,1,
              0,1), nrow = 2, byrow = T);B


# Soma de matrizes
A+B # Soma elemento a elemento da matriz, bastando colocar o operador '+' 


# Subtração de Matrizes
A-B # Ocorre de forma análoga à soma


# Multiplicação por escalar
2*A
3*B


# Dimensão de A e B
dim(A)
dim(B)


# Multiplicação de matrizes (Cuidado com a ordem)
A%*%B # Utiliza o operador '%' antes e após do operador '*', pois não ocorre elemento-a-elemento
B%*%A #

t(A)%*%A #soma de quadrados - basta pegar a transposta de 'A' e multiplicar por si mesma (A^T*A)


# Transposta de uma matriz
t(A)
t(B)


# Matriz identidade - a diagonal principal é preenchida por '1' e os demais elementos tornam-se 'zero'
I <- diag(2)
A %*% I

# Traço de uma matriz
sum(diag(A))


# Determinante de A
det(A)


# Inversa de uma matriz
solve(A)
A%*%solve(A) # Propriedade da inversa: para averiguar se A^-1 é verdadeira, basta multiplicar 'A' por inversa, devendo retornar a identidade
round(A%*%solve(A),1) # Arredonda os valores


# Calculo de autovalores e autovetores (2x2)
C <- matrix(c(4,5,
              2,1), nrow = 2, byrow = T);C
eigen(C)


# Calculo de autovalores e autovetores (3x3)
D <- matrix(c(3,-1,1,
              -1,5,1,
              1,-1,3), nrow = 3, byrow = T);D
eigen(D)


# Calculo de autovalores e autovetores (números complexos)
D <- matrix(c(sqrt(3),-1,
              1,sqrt(3)), nrow = 2, byrow = T);D

eigen(D)


# Correlação
cor(A) #R


# Covariância - As diagonais principais apresentam a covariância
cov(A) #S


# Méida de linhas e colunas
colMeans(A)
rowMeans(A)


# Padronização das variáveis
(A[,1] - mean(A[,1]))/sd(A[,1])

p <- matrix(c((A[,1]-mean(A[,1]))/sd(A[,1]),
(A[,2]-mean(A[,2]))/sd(A[,2])), nrow = 2, byrow = T)
p

colMeans(p)

#Quando os dados são padronizados temos que a cov(p) = cor(p)
cov(p)
cor(p)
