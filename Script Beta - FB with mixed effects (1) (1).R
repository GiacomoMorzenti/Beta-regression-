library(rstan)
library(loo)
library(MCMCpack)
library(FlexReg)

setwd("G:/Il mio Drive/Progetti/Modelli Stan") # Cartella dove hai i file .stan

FB_Mixed <- rstan::stan_model("FB_mixed.stan")
Beta_Mixed <- rstan::stan_model("Beta_Mixed.stan")


# Genero dei dati di prova
set.seed(369)

n <- 100
x <- runif(n,0,2)
X <- cbind(rep(1,n),x=x)
beta0 <- -2
beta1 <- 2


J <- 5   # Numero di gruppi
sig <- 2 # Deviazione standard dell'effetto casuale
groups <- sample(1:J, n, T) 
rand.eff <- rnorm(J, 0, sig)


p <- .6
w <- .3
phi <- 30

eta <- beta0 + beta1*x
mu <- 1/(1+exp(-(eta + rand.eff[groups])))

y <- sapply(1:n,function(i)  rFB(1,mu[i], phi, p, w))
plot(y~x, pch=20)


# creo una lista con i dati necessari per la stima
data.stan <- list(
  N = n,  y = y,
  K = ncol(X),  X = X,
  sd_prior = 10, g=0.001,
  J = 5, subject = groups
)

n.iter <- 15000 # lunghezza della catena

fit.FB <- rstan::sampling(
  object = FB_Mixed,   # Stan program
  data = data.stan,       # named list of data
  chains = 1,             # number of Markov chains
  warmup = 0.5*n.iter,          # number of warmup iterations per chain
  iter = n.iter,            # total number of iterations per chain
  cores = 1,              # number of cores (using 2 just for the vignette)
  thin=1,
  #control = list(adapt_delta = .8),
  refresh = n.iter/100           # show progress every 'refresh' iterations
)
print(fit.FB, pars=c("beta", "phi", "p", "w", "sigma_u", "U"))


fit_Beta <- rstan::sampling(
  object = Beta_Mixed,   # Stan program
  data = data.stan,       # named list of data
  chains = 1,             # number of Markov chains
  warmup = 0.5*n.iter,          # number of warmup iterations per chain
  iter = n.iter,            # total number of iterations per chain
  cores = 1,              # number of cores (using 2 just for the vignette)
  thin=1,
  #control = list(adapt_delta = .8),
  refresh = n.iter/100           # show progress every 'refresh' iterations
)
print(fit_Beta, pars=c("beta", "phi", "sigma_u", "U"))


# WAIC: Misura simile all'AIC per confrontare modelli. Piu' basso e', meglio e'
waic(extract_log_lik(fit_Beta))
waic(extract_log_lik(fit))
