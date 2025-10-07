# filename: descritiva_aula_I.R
# Author: Fabio Fajardo Molinares <fabio.molinares@ufes.br>
# Update: 
# Ter 02 Mai 2017 20:24:13 BRT :pu=strftime('%c')
# Ter 13 Oct 2020 23:30:32 BRT
# Qua 21 Oct 2020 13:01:05 BRT
rm(list = ls()) # Deleta as variáveis previamente declaradas na sessão 
# Libraries
if(!require(descr)) install.packages("descr", repos = "https://cloud.r-project.org/")
if(!require(EnvStats)) install.packages("EnvStats", repos = "https://cloud.r-project.org/")
if(!require(psych)) install.packages("psych", repos = "https://cloud.r-project.org/")
if(!require(httr)) install.packages("httr", repos = "https://cloud.r-project.org/")
if(!require(jsonlite)) install.packages("jsonlite", repos = "https://cloud.r-project.org/")
if(!require(plotly)) install.packages("plotly", repos = "https://cloud.r-project.org/")
if(!require(tidyverse)) install.packages("tidyverse", repos = "https://cloud.r-project.org/")

# Basic statistics
x <- c(20, 15, 17, 20, 13, 17, 17, 25, 30, 13) # Create vector
x
mean(x) # Média amostral
median(x) # Mediana amostral
var(x);sd(x) # Variância e desvio padrão 
sqrt(var(x)) # Raiz da variância
psych::describe(x)
# Dotplot de x
stripchart(x, method = "stack", offset = .5, at = .15, pch = 19, main = "Dotplot de x", xlab = "")
# Bar plot de x
barplot(x)
# Histograma de x
hist(x, main="Histograma do Fabio",ylab="Frequência",xlab="Dados")
descr::histkdnc(x)

# Substituindo o último valor
x <- c(20, 15, 17, 20, 13, 17, 17, 25, 30, 13);x
mean(x)
median(x)
stripchart(x, method = "stack", offset = .5, at = .15, pch = 19, main = "Dotplot de x", xlab = "")
boxplot(x)

# Algumas propriedades da média
x <- c(20, 15, 17, 20, 13, 17, 17, 25, 30, 13) # Create vector
y<-x+10 # Somando uma constante a todos os elementos de x
x;y
mean(x); mean(y)
median(x);median(y)

stripchart(x, method = "stack", offset = .5, at = .15, pch = 19, main = "Dotplot de x", xlab = "")


# If there are missing values:
x <- c(1,3,5,2,9,NA,7,10)
mean(x)
mean(x, na.rm=T)

x<-c(155, 160, 171, 182, 162, 153, 190, 167, 168, 165, 191)
median(x)

x<-c(155, 160, 171, 182, 162, 153, 190, 167, 168, 165, 191, 175)
median(x)

x<-c(3,4,5,6,7)
var(x) # variance
sd(x) # standard deviation

x<-c(1,4,5,7,8,110) # change the last observation from 11 to 110
mean(x)
median(x)
# [1] 6
var(x)
# [1] 1843.5
sd(x)
# [1] 42.936
fivenum(x) # Resumo dos 5 números


# calcular a media nas variaveis de um conjunto de dados
# excluding missing values
dados<-matrix(rpois(2000,10),40,5)
apply(dados,2,mean, na.rm=TRUE,)
# outras funcoes usadas com sapply: mean, sd, var, min, max, median, range, quantile.

# Estatisticas basicas

# mean,median,25th and 75th quartiles,min,max
summary(dados)

# Tukey min,lower-hinge, median,upper-hinge,max
fivenum(dados)

# item name ,item number, nvalid, mean, sd, median, mad, min, max, skew, kurtosis, se
psych::describe(dados) 

#######################
# Tabela de frequencias
#######################
x<-c(17, 17, 17, 17, 17, 17, 17, 17, 16, 16, 16, 16, 16, 18, 18, 18, 10, 12, 17, 17, 17,
17, 17, 17, 17, 17, 16, 16, 16, 16, 16, 18, 18, 18, 10, 12, 15, 19, 20, 22, 20, 19, 19,
19)
length(x) # Tamanho de amostra
cbind(Frequencia=table(x), Acumulada=cumsum(table(x)), relativa=prop.table(table(x)))
# ou
tbl <- table(x)
cbind( Freq=tbl, Cumul=cumsum(tbl), relative=prop.table(tbl))
# ou
#library("descr")
descr::freq(x, plot = FALSE)
# ordenado
descr::freq(ordered(x), plot = FALSE)
# Com faltantes
y<- c(sample(10:20, 44, TRUE), NA, NA)
descr::freq(ordered(y), plot = F)

###################
#
# valores com frequencias zero
z<-c(5,10,3,5,10,10,3,6,10,1,10,3,7,1,10,3,6,9,3,3) 
table(z)
table(factor(z, levels = 0:10)) 

##################################
# A Dotplot in R  - use stripchart
##################################
# Exemplo:
x<- rbinom(100,5,0.5)
stripchart(x, method = "stack", offset = .5, at = .15, pch = 19, main = "Dotplot de x", xlab = "Valores pseudo-aleatorios")

n<-300
data1<-c(rbinom(n,50,0.1))
data2<-c(rbinom(n*0.05,5,0.5),rbinom(n*0.9,30,0.1),rbinom(n*0.05,5,0.3)+25)
data3<-c(rbinom(n*0.5,15,0.1),rbinom(n*0.5,30,0.7))
data4<-c(rep(0,5),rbinom(15,15,0.2),rpois(n*0.8,40))
data5<-c(rpois(n*0.25,15),rbinom(n*0.25,5,0.1),rpois(n*0.25,50),rbinom(n*0.25,50,0.1))
data6<-c(rep(0,10),rpois(n-10,50))

psych::describe(data1) 
psych::describe(data2) 
psych::describe(data3) 
psych::describe(data4) 
psych::describe(data5) 
psych::describe(data6) 


par(mfrow=c(2,3))
stripchart(data1, method = "stack", offset = .1, at = .15, pch = 19,cex=0.7, main = "Dotplot de data1", xlab = "", xlim=c(0,20))
stripchart(data2, method = "stack", offset = .03, at = .15, pch = 19,cex=0.7, main = "Dotplot de data2", xlab = "", xlim=c(0,30))
stripchart(data3, method = "stack", offset = .1, at = .15, pch = 19,cex=0.7, main = "Dotplot de data3", xlab = "", xlim=c(0,30))
stripchart(data4, method = "stack", offset = .1, at = .15, pch = 19,cex=0.7, main = "Dotplot de data4", xlab = "", xlim=c(0,65))
stripchart(data5, method = "stack", offset = .1, at = .15, pch = 19,cex=0.7, main = "Dotplot de data5", xlab = "", xlim=c(0,70))
stripchart(data6, method = "stack", offset = .25, at = .15, pch = 19,cex=0.7, main = "Dotplot de data6", xlab = "", xlim=c(0,75))


par(mfrow=c(2,3))
boxplot(data5, horizontal=T)
boxplot(data3, horizontal=T)
boxplot(data6, horizontal=T)
boxplot(data2, horizontal=T)
boxplot(data4, horizontal=T)
boxplot(data1, horizontal=T)

# Histograms
par(mfrow=c(2,3))
descr::histkdnc(data1, main="", xlab="A")
descr::histkdnc(data2, main="", xlab="B")
descr::histkdnc(data3, main="", xlab="C")
descr::histkdnc(data4, main="", xlab="D")
descr::histkdnc(data5, main="", xlab="E")
descr::histkdnc(data6, main = "", xlab="F")


## Dados simetricos
y<- rnorm(1000)
hist(y)
describe(y)
fivenum(y)
quantile(y)

########################################
# A Stem-and-leaft plot in R  - use stem
########################################
stem(rnorm(100,0,1))
stem(rnorm(100,0,100))


# Consultando API do IBGE

# res  <- GET("https://servicodados.ibge.gov.br/api/v2/censos/nomes/ranking", type="basic")
res  <- GET("https://servicodados.ibge.gov.br/api/v2/censos/nomes/bruno", type="basic")
#res

get_budget_text <- content(res, "text")
#get_budget_text

data  <- fromJSON(get_budget_text, flatten = TRUE)
names(data)

data <- as.data.frame(data$res)
data <- data[2:9,]


## Cronologia dos nomes - Ex. Bruno
fig <- plot_ly(data=data, x = data$periodo, y = data$frequencia, 
               type = 'scatter', mode = 'lines+markers')%>%
  layout(plot_bgcolor='#e5ecf6', title="Bruno",
         xaxis = list(title=list(text='Periodo', font = list(size = 20), standoff = 25),
           zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'),
         yaxis = list(title=list(text='Frequência', standoff = 25),
                      zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'))

fig


## Carregando banco de dados portal https://dados.gov.br/dados/conjuntos-dados/alunos

alunos <- read.csv("/home/fajardo/Dropbox/biblioteca/bussab_morettin/dados/alunos202402.csv", sep=",")
head(alunos); names(alunos)
table(alunos$CURSO)

alunos %>% filter(str_detect(CURSO, "Ciências Sociais -")) %>% 
  filter(ANO.DE.INGRESSO=='2011')


barplot(alunos %>% filter(str_detect(CURSO, "Ciências Sociais -")) %>% 
  select("ANO.DE.INGRESSO") %>% table, main="Número de ingressantes x ano")

##################


cursos <- read.csv("/home/fajardo/Dropbox/biblioteca/bussab_morettin/dados/ponto2.csv", sep=",", dec=",")
head(cursos)

fix(cursos)

cv<-function(x){
  coef<-sd(x)/mean(x)*100 
  return(coef)
}


names(cursos)

summary(cursos$estatística)
sd(cursos$estatística); cv(cursos$estatística)
describe(cursos$estatística)

summary(cursos$redação)
sd(cursos$redação); cv(cursos$redação)
describe(cursos$redação)

par(mfrow=c(2,1))
stripchart(cursos$estatística, method = "stack", offset = 1.1, at = .15, pch = 19,cex=1.7, main = "Dotplot de Estatística", xlab = "")
stripchart(cursos$redação, method = "stack", offset = 1.1, at = .15, pch = 19,cex=1.7, main = "Dotplot de Redação", xlab = "")



boxplot(cbind(cursos$estatística, cursos$redação))




###################################

# Create data 
my_variable <- data1#c(rnorm(1000 , 0 , 2) , rnorm(1000 , 9 , 2))

summary(my_variable)

# Layout to split the screen
graphics::layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(my_variable , horizontal=TRUE , ylim=c(-10,20), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(my_variable , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="valores", xlim=c(-10,20))




#####################################
# Morettin CD-notas

notas <- read.csv("/home/ffajardo/Dropbox/UFES/UFES2025/I/sociais/ProvaI/notas.csv", sep=",", dec=",")
head(notas)
summary(notas)

par(mfrow=c(2,2))
stripchart(notas, method = "stack", offset = .5, at = .15, pch = 19,cex=0.7, main = "Dotplot de notas", xlab = "", xlim=c(0,11))
boxplot(notas, horizontal = T)
hist(notas$notas, breaks = 5, main="", xlab="notas", ylab="Frequência")



library(car)
x <- rnorm(300)
par(mfrow=c(2,2), bg=NA)
plot(x, type="l", xlab="Tempo", ylab="")
car::qqPlot(x,ylab = "")
acf(x, main="")
pacf(x, main="")



B <- c(2.7, 7.1,7.6,6.2,4.7,3.3,2.5,2.0,7.5,5.6,8.9,3.3,2.7,6.5,1.3)
L <- c(5.0,  4.3,  7.6,  5.6,  1.4,  2.5,  5.1,  5.7,  4.4,  2.0, 6.8)


par(mfrow=c(2,2))
stripchart(c(B,L), method = "stack", offset = 0.7, at = .15, pch = 19,cex=0.9, 
           main = "Dotplot de notas P1", xlab = "", xlim=c(0,11))
boxplot(cbind(B,L,c(B,L)), horizontal=T, main="Notas P1", col=c('skyblue',rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.3)))
hist(B, xlim=c(0,10), col=scales::alpha('skyblue',.5), border=F,xlab="")
hist(L, add=T, col=rgb(0.8,0.1,0.3,0.6), border=F)
hist(c(B,L), breaks=5,main="", xlab="notas P1", ylab="Frequência", , col=rgb(0.1,0.1,0.7,0.3))



B.Prev <-c(7.0, 4.5, 8.0, 9.0, 7.5, 8.0, 9.0, 7.0, 8.0, 5.0,10.0,8.0) 
L.Prev <- c(5.0,7.0,7.0,6.5,6.0,7.0,7.0,8.0,3.0,9.5)

par(mfrow=c(2,2))
stripchart(c(B.Prev,L.Prev), method = "stack", offset = 0.7, at = .15, pch = 19,cex=0.9, 
           main = "Dotplot de notas P1", xlab = "", xlim=c(0,11))
boxplot(cbind(B.Prev,L.Prev,c(B.Prev,L.Prev)), horizontal=T, main="Notas P1", col=c('skyblue',rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.3)))
hist(B.Prev, xlim=c(0,10), col=scales::alpha('skyblue',.5), border=F,xlab="")
hist(L.Prev, add=T, col=rgb(0.8,0.1,0.3,0.6), border=F)
hist(c(B.Prev,L.Prev), breaks=5,main="", xlab="notas P1", ylab="Frequência", , col=rgb(0.1,0.1,0.7,0.3))


P1 <- c(B,L)
P1.Prev <- c(B.Prev, L.Prev)

par(mfrow=c(2,2))
stripchart(P1, method = "stack", offset = 0.7, at = .15, pch = 19,cex=0.9, 
           main = "Dotplot de notas P1", xlab = "", xlim=c(0,11), col=scales::alpha('skyblue',.5))
stripchart(P1.Prev, method = "stack", offset = 0.7, at = .15, pch = 19,cex=0.9, 
           main = "Dotplot de notas P1", xlab = "", xlim=c(0,11), add=T, col=rgb(0.8,0.1,0.3,0.6))

boxplot(cbind(P1,P1.Prev), horizontal=T, main="Notas P1", col=c('skyblue',rgb(0.8,0.1,0.3,0.6),rgb(0.1,0.1,0.7,0.3)))
hist(P1, xlim=c(0,10), col=scales::alpha('skyblue',.5), border=F,xlab="")
hist(P1.Prev, add=T, col=rgb(0.8,0.1,0.3,0.6), border=F)



