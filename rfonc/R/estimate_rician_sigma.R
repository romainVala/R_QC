estimate_rician_sigma = function(y){
  
suppressMessages(require(pracma))
  
  phi_0 <- function(x, inv_a) digamma(x) - digamma(inv_a) - log(x) + log(inv_a)
  dphi <- function(x) psigamma(x, 1) - 1/x
  
  ## ----sigma---------------------------------------------------------------
  J <- length(y)
  
  # méthode 1
  n1 <- 1
  
  
  sigmahat1 <- sqrt( sum(y**2)/(2*J*n1) ) 
  
  res=data.frame(sigmahat1)
  
  # méthode 2
  tryCatch({
    phi <- function(x) digamma(abs(x)) - log(abs(x))
    a <- 2/J * sum(log(y)) - log(1/J * sum(y**2))
    res$n2 <- newtonRaphson(function(x) phi(x)-a, x0=n1, dfun=dphi)$root
    res$sigmahat2 <- sqrt( sum(y**2)/(2*J*res$n2) ) 
  },
  error=function(ee){
    message("Problem with rician noise estimation")
    message(ee)
  }    )
  
  # méthode 3 
  dens <- density(y, bw = 0.5)
  
  fy <- dens$y[dens$x>0]
  yyy <- dens$x[dens$x>0]
  
  nlmod <- nls(fy ~ 2/((ssigma*sqrt(2))^(2*nn)*factorial(nn-1)) * yyy^(2*nn-1) *
                 exp(- (yyy^2)/(2*ssigma^2) ) , start = list(nn=1, ssigma= res$sigmahat1))
  res$n3 <- coef(nlmod)["nn"]
  res$sigmahat3 <- coef(nlmod)["ssigma"]
  
  return(res)
}
