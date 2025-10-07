# Arquivo: primeiros_passosI.R
# Autor: Fabio A. F. Molinares
# email: fabio.molinares@gmail.com
# Atualização: 08/08/2016

#Operações aritméticas
1+2*3

3/2+1

4*3**3

4*3^3

sqrt(2)

sin(pi)

c(-1,0,1)/0

str(c("a", 1)) # Estrutura do dado

a<-c(1,2,3)
is.atomic(a)

m<-matrix(1:4,2,2)
is.atomic(m)

b<-list(1,2)
is.atomic(b)

c<-list(1,list(2,3))
is.atomic(c)

c(1, c(2, c(3, 4)))

c(1, 2, 3, 4)

x <- c(FALSE, FALSE, TRUE)
as.numeric(x)

is.integer(2)

is.integer(2L)

is.integer(as.integer(c(1,2)))

as.character(1.23) 

as.numeric(c("1", "2"))

x1 <- 10; x1

x2 <- c(1, 3, 6); x2

x2[1]

x2[2]

length(x2)

is.vector(x2)

is.atomic(x2)

is.list(x2)

is.matrix(x2)

is.numeric(x2)

is.character(x2)

x3 <- 1:10; x3

x4 <- seq(0, 1, by = 0.1); x4

x4[x4 > 0.5]

x4 > 0.5

x5 <- seq(0, 1, len = 11); x5

x6 <- rep(1, 5); x6

x7 <- rep(c(1, 2), c(3, 5)); x7

x8 <- rep(1:3, rep(5, 3))

x2 + 3

x2 + 1:3

x2 + 1:6

(1:3) * x2

x2/(1:6)

x2^(1:3)

x9 <- round(rnorm(10, mean = 40, sd = 10)); x9

sum(x9)

mean(x9)

var(x9)

min(x9)

max(x9)

summary(x9)

# vetores repetidos
rep(1:4, each = 12)

rep(1:4, rep(12, 4))

rep(rep(1:3, each = 4), 4)

rep(1:4, 12)

# Vetores de caracteres
let5 <- letters[1:5]; let5

let10 <- LETTERS[11:20]; let10

nomes <- c("fulano", "beltrano", "cicrano")

paste(nomes, 1:3)

paste("fulano", 2, sep = "")

paste(letters[1:8], 2, sep = "")

rep(paste("T", 1:3, sep = ""), c(4, 4, 3))

m1 <- matrix(1:12, ncol = 3)

matrix(1:12, ncol = 3, byrow = T)

length(m1)

dim(m1)

nrow(m1)

ncol(m1)

m1[1, 2]

m1[2, 2]

m1[, 2]

m1[3, ]

m1[1:2, 2:3]; dimnames(m1)

dimnames(m1) <- list(c("L1", "L2", "L3", "L4"), c("C1", "C2", "C3")); dimnames(m1)

m2 <- cbind(1:5, 6:10); m2

m3 <- cbind(1:5, 6); m3


# Outros exemplos
x <- c(2,3,7,9)
y <- c(9,7,3,2)
ano <- 1990:1993
nomes <- c("joao", "maria", "marina", "tiririca")
# mostra o primeiro elemento de y
y[1]
y[length(y)]

# Criar listagem de pessoas
pessoa <- list(nome="maria", x=2, y=9, ano=1990)
pessoa
# Accessing things inside a list --
pessoa$nome
pessoa$x

# Criar matriz com colunas "ano" "x" and "y"
cbind(ano, x, y)

# Cria uma tabela de datos
D <- data.frame(nomes, ano, x, y)
nrow(D)
# Mostra o conteúdo de uma coluna da tabela
D$names
# Mostra a última coluna da tabela
D$names[nrow(D)]
# Mostra a última coluna da tabela
D$names[length(D$nomes)]


X <- list(altura=175, peso=74)
print("olá mundo")
print(X)
cat("Sua altura é ", X$altura, " e seu peso é ", X$peso, "\n")

# funções --
square <- function(x) {
  return(x*x)
}
cat("O quadrado de 3 é ", square(3), "\n")

# valor do argumento na função
cube <- function(x=5) {
  return(x*x*x);
}
cat("O cubo de 2 é : ", cube(2), "\n")
cat("O cubo de 5 é : ", cube(), "\n")

# Funções que retornam múltiples valores
powers <- function(x) {
  parcel <- list(x2=x*x, x3=x*x*x, x4=x*x*x*x);
  return(parcel);
}

X = powers(3);
print("Mostra potências de 3 "); print(X);

# Forma compacta

powerful <- function(x) {
  return(list(x2=x*x, x3=x*x*x, x4=x*x*x*x));
}
print("Mostra potências de 3 "); print(powerful(3));

powerful <- function(x) {list(x2=x*x, x3=x*x*x, x4=x*x*x*x)}
