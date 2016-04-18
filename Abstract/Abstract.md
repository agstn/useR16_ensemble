The Use of Ensemble Learning Methods in Open Source Data Challenges
As data collection grows in size and complexity across a variety of industries, open source data challenges are 
becoming more widespread.  We present our experience developing prediction models within the context of data challenges. 
With the goal of maximizing predictive performance, we explored ensemble learning methods to train our models.  
We demonstrate the use of these methods using R packages such as h2o and h2oEnsemble and cloud computing platforms. 
In order to obtain an approximation of our predictive ability prior to challenge submission, we developed wrapper code 
to perform cross validation on the H2O ensembles. We display our process for determining the expected level of performance 
of the trained model on external data sources.

References:
  
Spencer Aiello, Tom Kraljevic, Petr Maj and with contributions from the H2O.ai team (2015). 
h2o: R Interface for H2O.  R package version 3.8.1.3. 
https://CRAN.R-project.org/package=h2o

Erin LeDell (2016). h2oEnsemble: H2O Ensemble Learning. R package version 0.1.6. 
https://github.com/h2oai/h2o-3/tree/master/h2o-r/ensemble/h2oEnsemble-package
