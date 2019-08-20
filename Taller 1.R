library("magrittr")
library("ggplot2")

####################
# Uniforme discreta
####################
x <- seq(1,5,1)
n <- x %>% length() 
i <- rep(5,5)

# PMF
# Forma 1
fx <- sapply(i, function(i) 1/i)
X <- cbind(x, fx) %>% as.data.frame()

X  %>%  ggplot() + 
  aes(y=0, x=x, xend=x, yend=fx) + 
  geom_segment() 

# Forma 2
u <- dunif(1:5, 1, 5)

cbind(X,u) %>% as.data.frame()  %>%  ggplot() + 
  aes(y=0, x=x, xend=x, yend=u) + 
  geom_segment() 

# CDF
t <- c(1:4)
cdf <- punif(1:5, 1, 5) # CDF n=5
plot(stepfun(t, x), xlab="x", ylab="F(x)", main="CDF UNIFORM (n=5)",
     do.points = FALSE, pch = 16,verticals = FALSE)

#CDF
X <- cbind(x, fx) %>% as.data.frame()
ggplot() +
  geom_step(data=X, mapping=aes(x=x, y=cdf), linetype=5) +
  geom_point(data=X, mapping=aes(x=x, y=cdf), color="red") 


unif_discreta <- function(n){
  u <- dunif(1:n, 1, n)
  X <- 1:n
  p1 <- cbind(X,u) %>% as.data.frame()  %>%  ggplot() + 
    aes(y=0, x=X, xend=X, yend=u) + 
    geom_segment() 
  
  cdf <- punif(1:n, 1, n)
  X2 <- cbind(X, cdf) %>% as.data.frame()
  p2 <- ggplot() +
    geom_step(data=X2, mapping=aes(x=X, y=cdf), linetype=5) +
    geom_point(data=X2, mapping=aes(x=X, y=cdf), color="red") 
  
  ggpubr::ggarrange(p1,p2)
}

unif_discreta(5)
unif_discreta(50)


####################
# Bernoulli
####################
bernoulli <- function(p){
  x <- c(0,1)
  fx <- dbinom(x, 1, p)
  
  cbind(x,fx) %>% as.data.frame()  %>%  ggplot() + 
    aes(y=0, x=x, xend=x, yend=fx) + 
    geom_segment() 
  
}

bernoulli(.7)
bernoulli(.3)
bernoulli(.5)

####################
# Binomial
####################
binomial <- function(n, p){
  x <- 0:n
  fx <- dbinom(x, n, p)
  
  cbind(x,fx) %>% as.data.frame()  %>%  ggplot() + 
    aes(y=0, x=x, xend=x, yend=fx) + 
    geom_segment() 
  
}

binomial(8, .8)
binomial(8, .5)
binomial(8, .3)

####################
# Hipergeometrica
####################
hyper <- function(n, N, k){
  # valores que puede tomar x
  low <- max(0, n -(N-k))
  up <- min(n, k)
  x <- seq(low,up,1)
  fx <- dhyper(x,k,N-k,n)
  # x: vector de exitos
  # K: caracteristica de interés
  # N-k: sin caracteristica de interés
  # n: muestra del conjunto N
  
  p1 <- cbind(x,fx) %>% as.data.frame()  %>%  ggplot() + 
    aes(y=0, x=x, xend=x, yend=fx) + 
    geom_segment() 
  
  cdf <- phyper(x,k,N-k,n)
  
 p2 <- cbind(x, cdf) %>%  as.data.frame() %>%  ggplot() +
    geom_step( mapping=aes(x=x, y=cdf), linetype=5) +
    geom_point(mapping=aes(x=x, y=cdf), color="red") + ylim(0,1)
  
  ggpubr::ggarrange(p1,p2)
}

N <- 10 
k <- 5
n <- 4
hyper(n, N, k)


############################################################
# aproximación binomial a hipergeométrica
############################################################

N <- 1000
k <- 400
n <- 50
n/N
p <- 0.4


binomial(n, p)
hyper(n, N, k)

aprox_BH <- function(n, p, N, k){
  x <- 0:n
  fx <- dbinom(x, n, p)
  
  
  low <- max(0, n -(N-k))
  up <- min(n, k)
  x2 <- seq(low,up,1)
  fx2 <- dhyper(x,k,N-k,n)
  # x: vector de exitos
  # K: caracteristica de interés
  # N-k: sin caracteristica de interés
  # n: muestra del conjunto N
  if(length(x2) != length(x)){
    temp <- length(x) - length(x2)
    x2 <- c(seq(0,temp -1 ,1),x2)
  }else{
    cbind(x, x2,fx, fx2) %>% as.data.frame()  %>%  ggplot() + 
    aes(y=0, x=x, xend=x, yend=fx) + 
    geom_segment() +   
    geom_segment(aes(y=0, x=x2+.1, xend=x2+.1, yend=fx2) , col ="red") 
  } 
  
      
    cbind(x, x2,fx, fx2) %>% as.data.frame()  %>%  ggplot() + 
    aes(y=0, x=x, xend=x, yend=fx) + 
    geom_segment() +   
    geom_segment(aes(y=0, x=x2+.1, xend=x2+.1, yend=fx2) , col ="red") 
  

}

n/N
aprox_BH(n, p, N, k)

N <- 12
k <- 8
n <- 6
p <- 0.67
n/N

aprox_BH(n, p, N, k)

###############
# Geometrica 
###############

exponential <- function(p){
  x <- 1:30
  fx <- dgeom(x, p) 
 
 p1 <- cbind(x,fx) %>% as.data.frame()  %>%  ggplot() + 
    aes(y=0, x=x, xend=x, yend=fx) + 
    geom_segment() 
  
 cdf <- pgeom(x, p)
 
 p2 <- cbind(x, cdf) %>%  as.data.frame() %>%  ggplot() +
   geom_step( mapping=aes(x=x, y=cdf), linetype=5) +
   geom_point(mapping=aes(x=x, y=cdf), color="red") + ylim(0,1)
 
 ggpubr::ggarrange(p1,p2)
}


exponential(0.1)
exponential(0.8)
exponential(0.5)



###############
# Poisson 
###############

poiss <- function(n, lambda){
  x <- 0:n
  fx <- dpois(x, lambda) 
  
  cbind(x,fx) %>% as.data.frame()  %>%  ggplot() + 
    aes(y=0, x=x, xend=x, yend=fx) + 
    geom_segment() 
  
 
}

lambda <- 0.5
poiss(6, 0.5)
poiss(6, 2)
poiss(14, 5)
poiss(25, 15)



####################################
# aproximación poisson a binomial
####################################

aprox_PB <- function(n, p, lam){
  # binomial
  x <- 0:n
  fx <- dbinom(x, n, p)
  # poisson
  x2 <- 0:n
  fx2 <- dpois(x, lam) 
  
  
  cbind(x, x2,fx, fx2) %>% as.data.frame()  %>%  ggplot() + 
    aes(y=0, x=x, xend=x, yend=fx) + 
    geom_segment() +   
    geom_segment(aes(y=0, x=x2+.1, xend=x2+.1, yend=fx2) , col ="red") +
    xlim(0,8)
  
  
}


n <- 100
p <- .01
lam <- 1

aprox_PB(n, p, lam)
aprox_PB(20, .2, 4)
