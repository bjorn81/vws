# Data table is not necessary for the actual modelling, only used for transformations
library(data.table)
# Rstan needs to be installed to run model
library(rstan)
# Coda is needed for graphics
library(coda)


# Read the IHT dataset (data on meter installations from energy supplier)
# Nps measures the score customers assigned
# Could differ systematically by installer, area, season etc.
ihtDT <- read.csv("L:/Jobnos/Maconomy/TBA/Ovo/IHTdata.csv", 
                  stringsAsFactors = FALSE, na.strings = c("N/A", "", "NA", "N\\/A"))
# Set to data table
setDT(ihtDT)
# some checks:
head(ihtDT)
dim(ihtDT)
names(ihtDT)
sapply(ihtDT, class)

# Set proper date
ihtDT[, theDate := as.Date(created, format = "%d/%m/%Y", tz = "GMT")]
# Create a m/Y factor variable
ihtDT[, ym := paste(month(theDate), year(theDate), sep ="/")]
# Create a dummy for Christmas (week 53)
ihtDT[, isxmas := 0]
ihtDT[week(theDate)==53, isxmas := 1]

# Remove rows that are missing data
DT <- ihtDT[!is.na(area) & ! is.na(nps), .(
  nps, area, installer, jobtype, Feedback.Sentiment.Score, ym, isxmas, channelid)]

# Calculate market share for installer in four steps:
# 1. Sum number of jobs per area
DT[, areasum := sum(.N), by = area]
# 2. Get the number for each installer for each area
DT[, N := .N, by = list(area, installer)]
# 3. Divide sum for each installer in each area by total sum in area
DT[, share := N / areasum, by = installer]
# Remove incomplete cases
DT <- DT[complete.cases(DT), ]

#Remove strange obs (should not be data here)
DT <- DT[installer != "10/11/2015", ]

nps = DT$nps
# Random effects variables
Area <- as.integer(as.factor(DT$area))
Period <- as.integer(as.factor(DT$ym))
# Normal variables
Share <- DT$share
Isxmas <- DT$isxmas
# Matrixes
Jobtype <- model.matrix((~ -1 + DT$jobtype))[, -1]
Installer <- model.matrix(~-1+DT$installer)[, -1]
Channel <- model.matrix(~-1+DT$channelid)[, -1]

colnames(Installer) <- gsub("DT\\$installer", "", colnames(Installer))
head(Installer)

# Rstan takes a list of data
# In addition to data, it needs to have dimensions of vectors and matrixes passed
sdata <- list(
  nps = nps,
  area = Area,
  channel = Channel,
  installer = Installer,
  period = Period,
  jobtype = Jobtype,
  isxmas = Isxmas,
  share = Share,
  N = nrow(DT),
  K = ncol(Installer),
  I = ncol(Jobtype),
  J = length(unique(Area)),
  L = length(unique(Period)))

# Stanmodel: data, parameters, model
# Data dimensions and type has to be specified. The each dimension is itself a parameter too
# Upper / lower is used to speed up execution when applicable
# Matrixes have [nrow, ncol] dimension
# Vectors are needed for matrix multiplication
varying_intercept = "
  data {
      // Dimensions of the vectors and matrixes parameter that are within data list
      int<lower=0> N; 
      int<lower=0> K;
      int<lower=0> I;
      int<lower=0> J; 
      int<lower=0> L; 
      // upper / lower is used to speed up execution when applicable
      // channel and isxmas are both dummies, therefore integers
      int<lower=0, upper=1>channel[N];
      int<lower=0, upper=1> isxmas[N];
      // Share is a numeric value, therefore real
      real<lower=0> share[N];
      // installer is an N*K matrix, jobtype N*I
      matrix[N, K] installer;
      matrix[N, I] jobtype;
      // There are L different periods, L shows which period an obs belong to
      int<lower=1,upper=L> period[N];
      // Similar for area
      int<lower=1,upper=J> area[N];
      // nps is stored in N-dimensional vector. Would probably work with real too
      vector<lower=0, upper=10>[N] nps;
    } 
    parameters {
      // the matrix variables need parameter vectors for matrix multiplication to work
      vector[K] b_installer;
      vector[I] b_jobtype;
      // other parameters are just real scalars (numbers)
      real b_isxmas;
      real b_channel;
      real b_share;
      vector[J] a_area;
      vector[L] a_period;
      real mu;
      // all parameters to be estimated needs to be created here, also standard deviations
      real<lower=0,upper=10> sigma_y;
      real<lower=0,upper=10> sigma_a;
      real<lower=0,upper=10> sigma_p;
    } 
    model {
      // describe the model:
      // y_hat is condtional expectation of dependent variable (can have any name)
      vector[N] y_hat;
      // this loop says that each for each observation (i) the expected value
      // is described by the formula below
      for (i in 1:N) {
      //                below is a partial matrix multiplication:
      //                row i of matrix installer times beta-vector
        y_hat[i] = mu + installer[i, 1:K]*b_installer + a_period[period[i]] + 
          a_area[area[i]] + channel[i]*b_channel + jobtype[i, 1:I]*b_jobtype + 
          isxmas[i]*b_isxmas + share[i]*b_share;
      } 
      // description of model priors for random effects variable coefficients:
      sigma_a ~ uniform(0, 10);
      a_area ~ normal (0, sigma_a);
      sigma_p ~ uniform(0, 10);
      a_period ~ normal(0, sigma_p);
      mu ~ normal(5,10);
      sigma_y ~ uniform(0, 10);
      // here is core of model, nps is normally distributed around y_hat with sd sigma_y
      nps ~ normal(y_hat, sigma_y);
      // priors for fixed effects variable coefficients
      b_channel ~ normal(0, 10);
      b_share ~ normal(0, 2);
      b_isxmas ~ normal(0, 5);
      // for parameter vectors, priors needs to be set by loop
      for (i in 1:K) { 
        b_installer[i] ~ normal(0, 10);
      }
      for (s in 1:I) { 
        b_jobtype[s] ~ normal(0, 10);
      }

  }
"
# Model (this takes 2 hours to run!)
mcmc_full <- stan(model_code = varying_intercept, model_name = "area",
                  data = sdata, warmup = 1000, iter = 3000, chains = 3, cores = 3,
                  verbose = TRUE)
# Summary statistics
summary(mcmc_full)

# Extract for plotting
mcmc_full.coda<-mcmc.list(lapply(1:ncol(mcmc_full),function(x) mcmc(as.array(mcmc_full)[,x,])))
# Plot
plot(mcmc_full.coda)

saveRDS(mcmc_full, "L:/Work/Bjorn/Meters/full_stanm.rds")
test <- readRDS("L:/Work/Bjorn/Meters/full_stanm.rds")
summary(test)
