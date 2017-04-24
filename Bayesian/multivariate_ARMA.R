library(forecast)
library(data.table)
library(rstan)
library(coda)

# Simulate data
error.model=function(n){rnorm(n, sd=.3)}
y0 <- arima.sim(model=list(ar=0.6, ma = -0.9), n=10000,
                                     n.start=200, start.innov=rnorm(200, sd=.2),
                                     rand.gen=error.model )

x1 <- arima.sim(model=list(ar=0.95), n=10000,
                                     n.start=200, start.innov=rnorm(200, sd=.2),
                                     rand.gen=error.model)

x2 <- arima.sim(model=list(ar=0.95), n=10000,
                                     n.start=200, start.innov=rnorm(200, sd=.2),
                                     rand.gen=error.model)
# Model for y
y <- y0 + 0.5*x2 + 0.2 * x1

# Create list of data
sdata2 <- list(
  y = as.numeric(y), 
  x1 = as.numeric(x1), 
  x2 = as.numeric(x2), 
  T = length(y))

# Stan model code
mmmodel <- "
    data {
    int<lower=1> T; // num observation
    real y[T]; // observed outputs
    real x1[T];
    real x2[T];
  }
  parameters {
    real mu; // mean coeff
    real phi; // autoregression coeff
    real<lower = -1, upper = 1> theta; // moving avg coeff
    real bx1; // own TV lag 3
    real bx2; // comp TV lag 1
    real<lower=0> sigma; // noise scale
  }
  model {
    vector[T] nu; // prediction for time t
    vector[T] eta; // ar error for time t
    vector[T] peta; // ar error for time t
    vector[T] epsilon; // model error for time t
    nu[1] = mu + bx1*x1[1] + bx2*x2[1];
    epsilon[1] = 0;
    eta[1] = y[1] - nu[1];
    peta[1] = eta[1];
    for (t in 2:T) {
      // this is the long run relationship between variables
      nu[t] = mu + bx1*x1[t] + bx2*x2[t];
      // eta is composite error term (AR and MA) which describes temporary deviations
      // from long run relationships
      eta[t] =  y[t] - nu[t];
    // peta is expected value of eta  
    peta[t] = phi*eta[t-1] + theta*epsilon[t-1];
      // epsilon is difference between expected and actual deviation from long run
      // relationship between model variables
      epsilon[t] = eta[t] - peta[t];
    }
    mu ~ normal(0, 100);  
    phi ~ normal(0, 2);
    theta ~ normal(0, 2);
    // error term of model is normally distributed around zero
    epsilon ~ normal(0, sigma); // likelihood
    bx1 ~ normal(10, 20); // linear trend
    bx2 ~ normal(-10, 20); // relative price
    sigma ~ cauchy(0, 1);
  }
"

#Compile and fit the model
mmm.stan <- stan(model_code = mmmodel, model_name = "example",
                 data = sdata2, warmup = 500, iter = 2000, chains = 3, cores = 3,
                 verbose = TRUE)
summary(mmm.stan)
mmm.stan.coda<-mcmc.list(lapply(1:ncol(mmm.stan),function(x) mcmc(as.array(mmm.stan)[,x,])))
plot(mmm.stan.coda)
