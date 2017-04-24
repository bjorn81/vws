library(poLCA)


data(election)
f <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
           MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~1

nes1 <- poLCA(f,election,nclass=1) # log-likelihood: -18647.31
nes2 <- poLCA(f,election,nclass=2) # log-likelihood: -17344.92
nes3 <- poLCA(f,election,nclass=3) # log-likelihood: -16714.66


# Three-class model with a single covariate (party)
f2a <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
             MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~PARTY
# Function call, nrep is to avoid finding local maxima
nes2a <- poLCA(f2a,election,nclass=5,nrep=5) # log-likelihood: -16222.32
# Predicted (posterior) class probabilities
head(cbind(nes2a$posterior, sum.probs = rowSums(nes2a$posterior)))

par(mfrow = c(5,1), mar=c(2.1,4.1,2.1,2.1))
for (i in 1:5){
  hist(nes2a$posterior[,i], breaks = 20, main = paste0("class probabilities, latent class ", i))  
}


plot(nes2a) # Plot latent classes (clusters)
pidmat <- cbind(1,c(1:7))
exb <- exp(pidmat %*% nes2a$coeff)
matplot(c(1:7),(cbind(1,exb)/(1+rowSums(exb))),ylim=c(0,1),type="l",
        main="Party ID as a predictor of candidate affinity class",
        xlab="Party ID: strong Democratic (1) to strong Republican (7)",
        ylab="Probability of latent class membership",lwd=2,col=1)
text(5.9,0.35,"Other")
text(5.4,0.7,"Bush affinity")
text(1.8,0.6,"Gore affinity")
