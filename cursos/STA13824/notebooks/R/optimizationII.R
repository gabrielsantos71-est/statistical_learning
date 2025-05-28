rm(list = ls())

if(!require(stats4)) install.packages("stats4", repos = "https://cloud.r-project.org/")
if(!require(MASS)) install.packages("MASS", repos = "https://cloud.r-project.org/")
if(!require(car)) install.packages("car", repos = "https://cloud.r-project.org/")

RNGkind("Marsaglia")
set.seed(c(1234,5678))
# Exemplo 1
n <- 100
x <- rnorm(n, mean = 3, sd = 2)
mean(x)
sd(x)

# Funcao de log-verossimilhanca
LL <- function(mu, sigma) {
    R <-suppressWarnings(dnorm(x, mu, sigma))
    -sum(log(R))
}

mle(LL, start = list(mu = 1, sigma=1))
mle(LL, start = list(mu = 1, sigma=1))

# Exemplo 2
xvec <- rnorm(1000,2,10)
fn <- function(theta) {
sum ( 0.5*(xvec - theta[1])^2/theta[2] + 0.5* log(theta[2]) )
}
optim(theta <- c(0,1), fn, hessian=TRUE)

# Exemplo 3: Poisson
y <- rpois(1000,10)
poisson.lik<-function(mu){
logl<-dpois(y,mu,log=T)
return(-sum(logl))
}
optimize(poisson.lik,interval=c(0,20))

  
x <- rgamma(100, shape = 5, rate = 0.1)
suppressWarnings(fitdistr(x, "gamma")) # Encontra as estimativas de máx. verossim.


# Beta
dados<-rbeta(10000,1,3)
L.beta <- function(theta, y) 
	-sum(dbeta(y, theta[1], theta[2], log=TRUE)) 
optim(c(1,1), L.beta, y=dados, method="L-BFGS-B", lower=c(.1, .1), upper=c(40, 40)) 

# Alternativa

L.beta2 <- function(t1, t2) 
	-sum(dbeta(dados, t1, t2, log=TRUE)) 
fit <- mle(L.beta2, start=list(t1=1, t2=1), method="L-BFGS-B", lower=rep(.1, 2)) 
plot(profile(fit))
confint(fit) 


# Exemplo 3: Ajuste de um modelo lineal

n <- 100
x <- runif(n)
y <- 15 + 4 * x + rnorm(n)

# Usando minimos quadrados ordinarios
fit <- lm(y ~ x)
summary(fit)

plot(x, y)
abline(fit, col = "red")


# Função de log-verossimilhança
LL <- function(beta_0, beta_1, sigma){
  -sum(dnorm(y,(beta_0+beta_1*x), sigma, log = TRUE))
}

# Estimando os coeficientes da regressão
result_1 <- suppressWarnings(mle(LL, start = list(beta_0 = 1, beta_1=1, sigma = 9)))
summary(result_1)

# calculando o sigma quadrado
(result_1@coef[3])^2
