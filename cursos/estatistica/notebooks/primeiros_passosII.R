# Arquivo: primeiros_passosI.R
# Autor: Fabio A. F. Molinares
# email: fabio.molinares@gmail.com
# Atualização: 04/09/2012

# Calculo das raizes de um polinomio de segundo grau
a0<-2
a1<-5
a2<-1
discrim <- a1^2 - 4*a2*a0
if (discrim > 0) {
roots <- c( (-a1 + sqrt(a1^2 - 4*a2*a0))/(2*a2),
(-a1 - sqrt(a1^2 - 4*a2*a0))/(2*a2) )
} else {
if (discrim == 0) {
roots <- -a1/(2*a2)
} else {
roots <- c()
}
}
show(roots)

# Calcula o fatorial de um enteiro
n <- 6
n_factorial <- 1
for (i in 1:n) {
n_factorial <- n_factorial * i
}
show(n_factorial)

# Geração de valores pseudo aleatorios de um gerador
# congruencial multiplicativo dado por:
# x_n=171*x_{n-1} mod 30269
# u = x_n/30269. Valor inicial = 27218
n<-10000
aleatorio<-numeric(n)
random.seed<-272128
for(i in 1:n){
random.seed<-(171*random.seed) %% 30269
aleatorio[i]<-random.seed/30269
}

# Forma 1
n<-100000 
x<-runif(n) 
y<-runif(n) 
z<-rep(0,n) 
system.time(for(i in 1:n){ 
z[i]<-x[i]+y[i] 
}
)

# Forma 2
n<-100000 
x<-runif(n) 
y<-runif(n)
z<-1 
system.time(for(i in 1:n){ 
z[i]<-x[i]+y[i] 
} 
)

# Forma 3
system.time(z<-x+y)

# Funções
soma<-function(a,b){
	return(a+b)
}
n<-1000000
x<-c(1:n)
y<-x^2
system.time(
for(i in 1:n){
	x[i]<-soma(x[i],y[i])
}
)

system.time(x+y)


library(magrittr)
3 %>% soma(4)
x <- 1:10

system.time({for(i in 1:1e5){x; identity(x)}})
#   user  system elapsed 
#   0.07    0.00    0.08 
system.time({for(i in 1:1e5) x %>% identity})
#   user  system elapsed 
#  15.39    0.00   16.68 


# Geração de valores de uma dist. Bernoulli
bernoulli<-function(n,p){
	valores<-function(p){
		x<-ifelse(runif(1)<=p,1,0)
	}
replicate(n,valores(p))
}

# Geração de valores de uma dist. Binomial
binomial<-function(n,N,p){
	valores<-function(N,p){
		x<-sum(runif(N)<p)
	}
replicate(n,valores(N,p))
}

# Lei dos grandes numeros
REP<-10
n<-5000
media<-rep(0,REP)
for(i in 1:REP){
	media[i]<-mean(bernoulli(n,0.1))
}
mean(media)

