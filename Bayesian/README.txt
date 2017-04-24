I’ve created some Bayesian models that could be of re-use at some point. They written are in R, but can also be used in python, as the essential code is just a string that is passed to something called STAN, which compiles the model to c++ and runs it. You can use either Rstan or Pystan (“py” as in python).
 
Setting up a model can be a little complicated, but I’ve added some comments in the examples.
 
There are currently three different models:
 
1.            Multivariate time-series model with ARMA errors
2.            Non-linear optimization problem
3.            Multilevel mixed-effects model
 
The third model is best commented for greater understanding
 
The advantage of these models over other estimation techniques, is that you get very deep insights about parameter and prediction uncertainty. These models will take some time to run on long  datasets (for example, 90,000 observation and 170 parameters took 2 hours) and should mainly be used as a supplement to more simplistic and much faster models like least squares or maximum likelihood, nonlinear least squares, in special cases.
 
For example, the time-series model (1) (with the actual data, not the made up data in example) gave insight about the combined effect of parameter uncertainty and showed that a model should possibly not be modelled in its current form (it was quite likely that the true autoregressive term was > 1). 
 
The non-linear optimization (2) can detect if there are multiple near optima for a non-linear optimisation problem, while something like Excel’s solver or R’s nls will in the first case give no information about the uncertainty and in the second only about uncertainty around the global optimum found.
 
The multilevel mixed-effects model (3) was able to answer questions about mean customer satisfaction in different areas. For an area with very small sample size and an extreme observation (maybe 2 out of 10) it could infer by assuming a distribution for the true means of different areas and between individuals that the true mean is more likely to be in the range 8-9.
 
Hopefully these models will come of use in the future.
