### Example script to run MCMC using R package adnuts for a stock
### assessment model in ADMB. Started January 2021 by Cole
### Monnahan (cole.monnahan@noaa.gov | AFSC)

library(adnuts)                         # 1.1.0
library(shinystan)
### ------------------------------------------------------------
### Step 0: Set up model for running. This requires pointing to a
### folder and executable. The folder needs to contain all
### sufficient input files. Temporary copies will be made in the
### working directory during execution

## Define the path and model name
m <- './fm'                               # model name
p <- '/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/m22_0_noSR/'                                   # path to model
## Assumes current working directory is where this R script is
setwd("/Users/ingridspies/admbmodels/BSAI_YFS/assessments/yfs/runs/m22_0_noSR")
(wd <- getwd())

## It also assumes the model was optimized with successful
## inversion of the matrix to create the admodel.cov file. If not
## rerun it.
setwd(p)
## system(paste(m, '-nox -iprint 1000'))
## If an SS model better to optimize with -mcmc flag
system(paste(m, '-nox -iprint 1000 -mcmc 10'))
setwd(wd)

## How many parallel chains to use? I recommend using all cores
## but one for laptops. Probably not needed to run more than 8 in
## most cases.
(chains <- parallel::detectCores()-1)

### ------------------------------------------------------------
### Step 1: Run and demonstrate MCMC convergence diagnostics.

## I recommend using 1000-2000 iterations, with first 25%
## warmup. Start with thin=1, then increase thin rate until
## convergence diagnostics passed (ESS>200 & Rhat<1.1).
## !! Note in some environments like RStudio parallel output is
## not printed to screen !!

thin <- 1000
iter <- 1000*thin
fit <- sample_rwm(model=m, path=p, iter=iter, warmup=iter/4,
                  chains=chains, thin=thin)#This one takes a while
saveRDS(fit, 'm22_0_noSR.RDS')

## Key information from run. Including the two recommended
## convergence diagnostics:
fit <- readRDS('m22_0.RDS')
fit <- readRDS('m22_0_noSR.RDS')
summary(fit)

## Stored internally like this but you rarely need to access
## these.
fit$monitor[1:5, c('n_eff', 'Rhat')]

## Interactive tools (must close out browser to regain console)
launch_shinyadmb(fit)

## If more thinning is needed, increase and rerun. Good idea to
## save the output, I recommend RDS format.
saveRDS(fit, file='fits/myfit.RDS')
fit$cmd[1]

### ------------------------------------------------------------
### Step 2: Model diagnostics using failed convergence
### diagnostics. When the MLE and MCMC estimate completely
### different things that is usually a parameterization issue.

## fit <- readRDS('fits/snowcrab2.RDS')    # thin=50

## The 5 slowest/fastest mixing parameters
pairs_admb(fit, pars=1:10)
pairs_admb(fit, pars=1:10, order='slow')
pairs_admb(fit, pars=1:10, order='fast')


## Can also specify names or use grep
par.names <- dimnames(fit$samples)[[3]]
pairs_admb(fit, pars=grep('log_rec_dev', par.names)[64:68])


## Look at MLE vs MCMC marginal standard deviations of snowcrab
## fit
fit <- readRDS('fits/snowcrab2.RDS')
sd.post <- apply(extract_samples(fit), 2, sd)
sd.mle <- fit$mle$se[1:length(sd.post)]
plot(log10(sd.post), log10(sd.mle)); abline(0,1)
pars <- which(sd.post > 10^(.15))
pairs_admb(fit, pars=pars)

### ------------------------------------------------------------
### Step 3: Posterior extraction for inference. mceval can be run
### from command line b/c post-warmup samples from all chains
### were merged into main folder, so any mceval output files
### contain all this information

## fit <- readRDS('fits/flathead2.RDS')    # thin=50
plot_marginals(fit, pars=1:6)
## Marginal comparisons as multipage PDF for easy scrolling
pdf('plots/marginals.pdf', onefile=TRUE, width=7,height=5)
plot_marginals(fit)
dev.off()

## See tab "Estimate"
launch_shinyadmb(fit)

post <- extract_samples(fit)
str(post)
