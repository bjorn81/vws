# y is a non-linear (sinus shaped) function of x plus an error component
x = runif(100, -1.5, 1.5)
y1 = (sin(x) + 1) / 2 
y = y1 + rnorm(100, mean = 0, sd = 0.1)

plot(x, y)
lines(x[order(x)], y1[order(x)], lty = 2, col = "red2", lwd = 2)
title("True function and observed values")
library(rstan)

# Put data in a list for stan
data = list(tt=x,NN=y, T=length(x))


# Model code
grmodel <- "
  data {
    int<lower=1> T; // num observation
    vector[T] NN;
    vector[T] tt;
}
    parameters {
    real<lower=0> K; // own TV lag 3
    real<lower=0> tm; // comp TV lag 2
    real<lower=0> Lc; // comp TV lag 2
    real eight1;
    real<lower=0> sigma; // noise scale
}
  model {
    vector[T] nu;
    vector[T] epsilon;
    for (i in 1:T){
      // relationship is estimated by diffusion of innovation model
      nu[i] = K / (1 + exp((log(eight1)*(tt[i] - tm) / (Lc/2))));
      epsilon[i] = nu[i] - NN[i];
    }
  K ~ normal(0.5, 1);  
  tm ~ normal(0.5, 1);
  Lc ~ normal(0.5, 1);
  eight1 ~ normal(1,1);
  epsilon ~ normal(0, sigma); // likelihood
  sigma ~ cauchy(0, 10);
}
"
# Run stan model
gr.stan <- stan(model_code = grmodel, model_name = "example",
                 data = data, warmup = 500, iter = 2000, chains = 3, cores = 3,
                 verbose = TRUE)
summary(gr.stan)
# Extract data for plots by coda
gr.stan.coda<-mcmc.list(lapply(1:ncol(gr.stan),function(x) mcmc(as.array(gr.stan)[,x,])))
plot(gr.stan.coda)
# Extract mean parameters from simulations
params = lapply(extract(gr.stan), mean)

library(data.table)
stan.coef = data.frame(extract(gr.stan))
setDT(stan.coef)

stan.coef[, id := .I]
stan.coef[, rsq := sum((K / (1 + exp(((log(eight1)*(data$tt - tm) / (Lc/2))))) - data$NN)**2), by = id]
stan.coef[, min(rsq)]
stan.coef[, which.min(rsq)]

setorder(stan.coef, rsq)
stan.coef[1:20,]
nu3 = with(stan.coef[1,], K / (1 + exp(((log(eight1)*(data$tt - tm) / (Lc/2))))))


Lc = params$Lc
tm = params$tm
K = params$K
eight1 = params$eight1

nu = K / (1 + exp(((log(eight1)*(data$tt - tm) / (Lc/2)))))


sum((nu-y)**2)
sum((nu3-y)**2)

plot(x, y, pch =4)
lines(x[order(x)], y1[order(x)], lty = 2, col = "red2", lwd = 2)
lines(x[order(x)], nu3[order(x)], col = "green4", lwd = 2)
legend("topleft", legend = c("Actual", "Fitted"), col = c("red2", "green4"), lty = c(2, 1))
title("Estimated and actual")
points(x, nu, col = "blue2")

# Check parameter intetrdependence
pairs(gr.stan)
