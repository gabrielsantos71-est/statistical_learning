# Arquivo: primeiros_passosIII.R
# Autor: Fabio A. F. Molinares
# email: fabio.molinares@gmail.com
# Atualização: 04/08/2013

RNGkind("Marsaglia-Multicarry") # Gerador de número pseudo-aleatórios
set.seed(c(362436069,521288629)) # Fixando semente

experimento <- function(n){ 
	x <- runif(n) # Geração de valores pseudo-aleatorios uniformes em (0,1)
	return(c(mean(x), median(x)))
}

n<-100 # Tamanho de amostra
REP<-10000
# experimentos de Monte Carlo
resultados <- replicate(REP, experimento(n)) # Gives back a 2x100000 matrix

# distribuição da média e a mediana 
k1 <- density(resultados[1,]) # results[1,] mostra a primeira linha
k2 <- density(resultados[2,]) # results[2,] mostra a segunda linha

# Grafico dos dados
postscript("figura.eps",width=8,height=5,horizontal=T)
xrange <- range(k1$x, k2$x)
plot(k1$x, k1$y, xlim=xrange, type="l", xlab="Dados amostrais", ylab="")
grid()
lines(k2$x, k2$y, col="red")
abline(v=.5)
legend(x="topleft", bty="n", lty=c(1,1), col=c("black", "red"), legend=c("Media", "Mediana"))
dev.off()

