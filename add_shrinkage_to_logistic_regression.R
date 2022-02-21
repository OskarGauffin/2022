# Custom link function GLM

# Rewrite and include the shrinkage
vlog <- function() {
  ## link
  linkfun <- function(p){ 
    log((p)/(1-p))
  }
  
  ## inverse link
  linkinv <- function(eta){  
    exp(eta)/(1+exp(eta))
  }
  
  
  ## derivative of invlink wrt eta
  mu.eta <- function(eta) { exp(eta)/(1+exp(eta))^2 }
  valideta <- function(eta) TRUE
  link <- "log(p/1-p)"
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta, 
                 name = link),
            class = "link-glm")
}


vv <- vlog()
vv$linkfun(vv$linkinv(27))  ## check invertibility
# install.packages("numDeriv")
# library("numDeriv")
all.equal(grad(vv$linkinv,2), vv$mu.eta(2))  ## check derivative

x <- runif(50)
y <- as.logical(x + runif(50)*0.1 < 0.5 )

set.seed(101)
glm(y~x, family=binomial)                       

glm(y~x, family=binomial(vv)) 
