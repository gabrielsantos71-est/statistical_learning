rm(list = ls())

if(!require(graphics)) install.packages("graphics", repos = "https://cloud.r-project.org/")
if(!require(lattice)) install.packages("lattice", repos = "https://cloud.r-project.org/")

######## Exemplos de otimização univariada
# P() and dP() from Ex 1.1 in Mathematical Modeling by M. M. M.

P <- function(x=0, w0=200, g=5, p0=0.65, r=0.01, c=0.45) {
  return( (p0-r*x)*(w0+g*x)-c*x )
}

dP <- function(x=0, w0=200, g=5, p0=0.65, r=0.01, c=0.45) {
  return(-2*g*r*x+(p0*g-r*w0-c))
}

x<-seq(-2,50,length=100)
plot(x,P(x),type="l", 
     main="Otimização da função odjetibo", xlab="Variável")
abline(v=8,col="red")

# Qual o ponto de maximiza a função?
optimize(P,c(0,50)) # Putz! Ele está encontrando o mínimo da função

# Como resolver?
obj <- function(x) {-P(x);}
optimize(obj,c(0,50))

# ou também pode usar a função optimize() para maximizar
optimize(P,c(0,50), maximum=TRUE)

## Podemos encontrar as raízes da função ao invés de otimizar
uniroot(dP,c(0,50))



# Funcao univariada
f<-function(x){ 
	-exp(-((x-2)^2))
}

gr<- function(x){
	-2*(x-2)*f(x)
}
curve(f,0,4)
optim(1,f,method="BFGS")
optim(1,f,gr,method="BFGS")

f<-function(x){
	sin(x*cos(x))
}
curve(f,0,10)
optim(2,f,method="BFGS")
optim(4,f,method="BFGS")
optim(6,f,method="BFGS")
optim(8,f,method="BFGS")

#--------------------------------
# Grafico da funcao de Rosenbrock
#--------------------------------
Rosenbrock <- function(x){
	g <- (1 - x[1])^2 + 100*(x[2] - x[1]^2)^2
	g1 <- -2*(1 - x[1]) - 400*(x[2] - x[1]^2)*x[1]
	g2 <- 200*(x[2] - x[1]^2)
	g11 <- 2 - 400*x[2] + 1200*x[1]^2
	g12 <- -400*x[1]
	g22 <- 200
	return(list(g, c(g1, g2), matrix(c(g11, g12, g12, g22), 2, 2)))
}
 
x <- seq(-2, 3, .1)
y <- seq(-2, 5, .1)
xyz <- data.frame(matrix(0, length(x)*length(y), 3))
names(xyz) <- c('x', 'y', 'z')
n <- 0
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    n <- n + 1
    xyz[n,] <- c(x[i], y[j], Rosenbrock(c(x[i], y[j]))[[1]])
  }
}
wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE),zlab = 'f(x, y)', drape = T)
graphics::contour(x,y,outer(x,y))

# Rotate plot
wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE),zlab = 'f(x, y)', drape = T, screen=list(x=-60))
wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE),zlab = 'f(x, y)', drape = T, screen=list(x=-60, y=70)) 
wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE),zlab = 'f(x, y)', drape = T, screen=list(x=10, y=-135, z=-40))

f<- function(x,y){
	100 * (y - x * x)^2 + (1 - x)^2
}
z<-outer(x,y,f)
contour(x,y,z)

## Defina a funcao para optimizar
fr <- function(x){
	x1 <- x[1]
	x2 <- x[2]
	100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
 ## Gradiente de 'fr'
grr <- function(x){
	x1 <- x[1]
	x2 <- x[2]
	c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
	200 * (x2 - x1 * x1))
}

(res <- optim(c(-1.2,1), fr, grr, method = "BFGS"));optimHess(res$par, fr, grr)
optim(c(-1.2,1), fr, NULL, method = "BFGS", hessian = TRUE)

## Exemplos de nao convergencia
optim(c(-1.2,1), fr, grr, method = "CG")
optim(c(-1.2,1), fr, grr, method = "CG", control = list(type = 2))
optim(c(-1.2,1), fr, grr, method = "L-BFGS-B")

#--------------------------------
# Grafico da funcao de Himmelblau
#--------------------------------
f<-function(x,y){
	(x^2+y-11)^2+(x+y^2-7)^2
}
x <- seq(-4.5,4.5,by=.2)
y <- seq(-4.5,4.5,by=.2)
z <- outer(x,y,f)
persp(x,y,z,phi=-45,theta=45,col="yellow",shade=.65 ,ticktype="detailed")

## Defina funcao para optimizar
f <- function(x){
	(x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
}
optim(c(-4,-4), f)
optim(c(2,-2), f)
optim(c(2,2), f)
optim(c(-4,4), f)

