# Reproducing calculations from
# Containing 2019-nCov(Wuhan) conoravirus, Edward H. Kaplan
# 
# initial version Andreas 4/3/2020
require(stats)


# compute gamma parameters a and b from mean m and variance v (!not using std!)

gamma_parameters <- function(m,v){
  a = m*m/v
  b = m/v
  lst = c(a=a,b=b)
  return(lst)
}

# compute lognormal parameters mu and sigma from mean m and variance v

lognormal_parameters <- function(m,v){
  sigma <- sqrt(log(1+v/(m^2)))
  mu <- log(m)-(sigma^2)/2
  lst <-  c(mu=mu,sigma=sigma)
  return(lst)
}

# Integral over exp(r*t)*gamma(t,a,b)
# from t0 to t0+dt
# using exp(r*t)*gamma(t,a,b)=gamma(t,a,b-r)*(b/(b-r))^a

integral_rgamma <- function(t0,dt,a,b,r){
  res <- pgamma(t0+dt, a, (b-r)) - pgamma(t0, a, (b-r))
  res <- res*(b/(b-r))^a
  return(res)
} 

# Compute R0  Eq. 5
# gamma mean 7.5 days, std-dev = 3.4

r <- 0.1
g <- gamma_parameters(7.5, 3.4^2)
a <- g[["a"]]
b <- g[["b"]]

cat("Eq. (5) R0:", integral_rgamma(0,1000, a, b, r),"\n")

# Compute R adjustment due to isolation time Eq 6

# log normal incubation distribution
# mean = 5.2 days, std=3.9 days  (not to use variance below)

param_ln <-lognormal_parameters(5.2, 3.9^2) 
mu = param_ln[["mu"]]
sigma = param_ln[["sigma"]]

# isolation time 
dt = 0
integrant_isolation <- function(x){
  return(dlnorm(x,mu,sigma)*integral_rgamma(x,dt,a,b,r))
}
dt=7
cat("Eq. (6) BetaIsolation ",dt," days:", (integrate(integrant_isolation,0,1000)$value),"\n")
dt=14
cat("Eq. (6) BetaIsolation ",dt," days:", (integrate(integrant_isolation,0,1000)$value),"\n")
dt=1000
cat("Eq. (6) BetaIsolation ",dt," days:", (integrate(integrant_isolation,0,1000)$value),"\n")

# Effect of quarantine Eq 7
integrant_quarantine <- function(x){
  return(dgamma(x,a,b)*integral_rgamma(x,dt,a,b,r))
}

dt = 7
integrate(integrant_quarantine,0,1000)
cat("Eq. (7) BetaQuarantine ",dt," days:", (integrate(integrant_quarantine,0,1000)$value),"\n")
dt = 14
cat("Eq. (7) BetaQuarantine ",dt," days:", (integrate(integrant_quarantine,0,1000)$value),"\n")
dt = 1000
cat("Eq. (7) BetaQuarantine ",dt," days:", (integrate(integrant_quarantine,0,1000)$value),"\n")
