#MCMC from his book
# install.packages("mvtnorm")
# install.packages("devtools")
# install.packages("dagitty")
# install.packages("coda")

# install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
# devtools::install_github("rmcelreath/rethinking")

library(rethinking)

n_samples <- 1000
p <- rep(NA,n_samples)
p[1] <- 0.5
W  <- 6
L <- 3
for(i in 2:n_samples){
  p_new <- rnorm(1, p[i-1], 0.1)
  # print(paste0( "p_new is ",p_new))
  if(p_new<0) p_new <- abs(p_new)
  if(p_new>1) p_new <- 2-p_new
  q0 <- dbinom(W,W+L,p[i-1])
  q1 <- dbinom(W,W+L,p_new)
  # print(paste0("q0 is ",q0))
  # print(paste0("q1 is ",q1))
  p[i] <- ifelse(runif(1) < q1/q0, p_new,p[i-1])

    # print(paste0("new p[i] is ",p[i]))
  }
p
hist(p)
dens(p)
curve(dbeta(x,W+1,L+1), lty=2, add=T)

# plot(density(p))
