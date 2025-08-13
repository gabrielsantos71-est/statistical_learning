if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(TSA)) install.packages("TSA", repos = "http://cran.us.r-project.org")
if(!require(lmtest)) install.packages("lmtest", repos = "http://cran.us.r-project.org")
if(!require(olsrr)) install.packages("olsrr", repos = "http://cran.us.r-project.org")
if(!require(nlme)) install.packages("nlme", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")


n <- 1000
ar.sim<-arima.sim(list(order=c(1,0,0),ar=c(0.2)),n=n)
#ma.sim<-arima.sim(list(order=c(0,0,1),ma=0.2),n=n)
#arma.sim<-arima.sim(list(order=c(1,0,1),ar=0.2,ma=0.2),n=n)
par(mfrow=c(2,2))
plot.ts(ar.sim)
TSA::periodogram(ar.sim)
acf(ar.sim, lag.max = 100)
pacf(ar.sim, lag.max = 100)

ar.sim.mod <- ar.sim
ar.sim.mod[500:n] <- ar.sim[500:n] + 5
par(mfrow=c(2,2))
plot.ts(ar.sim.mod)
TSA::periodogram(ar.sim.mod)
acf(ar.sim.mod, lag.max = 100)
pacf(ar.sim.mod, lag.max = 100)

ar.sim.nest<-arima.sim(list(order=c(1,0,0),ar=c(0.99)),n=n)
par(mfrow=c(2,2))
plot.ts(ar.sim.mod)
plot.ts(ar.sim.nest)
acf(ar.sim.mod, lag.max = 100)
acf(ar.sim.nest, lag.max = 100)



b0 <- 4.0
b1 <- 5.2
b2 <- 10.0
b3 <- -3.5

betas<-c(b0,b1,b2,b3)

# Vetor de coeficientes
b<-matrix(betas,nrow=length(betas),ncol=1)

# Matrix de covariaveis
x<-matrix(c(rep(1,n),rnorm(n*(length(betas)-1),mean=0,sd=20)),nrow=n,ncol=length(betas))

# Vetor de erros
ar.sim<-arima.sim(list(order=c(1,0,0),ar=c(0.2)),n=n)

# Vetor de Y
y<-x%*%betas+ar.sim

fit <- lm(y ~ x-1); summary(fit)

car::durbinWatsonTest(fit)

plot(fit)
acf(fit$residuals)
pacf(fit$residuals)
car::qqPlot(fit$residuals)
cpgram(fit$residuals)

res <- residuals(fit)
yhat <- fitted(fit)
plot(y,res, xlab="y", ylab="residuals")
plot(yhat,res, xlab="fitted values", ylab="residuals")

lmtest::bptest(fit)
olsrr::ols_test_breusch_pagan(fit)


fit2 <- nlme::gls(y ~ x -1, corr=corAR1(0.2, form = ~1))
summary(fit2)
lmtest::coeftest(fit2)


fit3 <- forecast::auto.arima(y, xreg=x)
summary(fit3)
lmtest::coeftest(fit3)

