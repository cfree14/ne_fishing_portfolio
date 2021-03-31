
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(TMB)
library(tidyverse)
# devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")

# Directories
tmbdir <- "code/tmb_code"
datadir <- "data/ramldb/data/processed"
codedir <- "code"
outputdir <- "output"

# Read data
data <- readRDS(file.path(datadir, "RAM_NE_data_w_sst_trimmed_prepped.Rds")) %>% 
  rename(tb_sd=tb_scaled, sp_sd=sp_scaled)

# Helper functions
source(file.path(codedir, "helper_functions.R"))



# Function to fit model
################################################################################

covariate <- "sst_c_scaled"; covariate_name <- "sst"

# Fit surplus production model
fit_sp <- function(data, p, covariate, covariate_name){

  # 1. Format data
  ######################################
  
  # Parameters
  stocks <- unique(data$stockid)
  nstocks <- length(stocks)
  
  
  # 2. Fit production model
  ######################################
  
  # Compile TMB code
  # Only run once to compile code
  origdir <- getwd()
  setwd(tmbdir)
  if(FALSE){
    dyn.unload(paste(tmbdir, dynlib("pella_prey_fixed"), sep="/"))
    file.remove(paste(tmbdir, c("pella_prey_fixed.o", "pella_prey_fixed.dll"), sep="/"))
    compile("pella_prey_fixed.cpp")
  }
  
  # Load TMB code
  dyn.load(dynlib("pella_prey_fixed"))
  
  # Input data and parameter starting values
  params <- list(ln_B0=rep(1.5, nstocks),
                 ln_r=rep(log(0.4), nstocks),
                 BetaT=rep(0.0, nstocks),
                 ln_sigmaP=rep(-2.5, nstocks)) # -3 before, -1.25 based on model fits
  input.data <- list(Nstocks=nstocks,
                     Nobs=nrow(data),
                     p=p,
                     StockID=as.factor(data$stockid),
                     B_t=data$tb_sd,
                     P_t=data$sp_sd,
                     Prey_t=as.numeric(unlist(data[,covariate])))
  
  # Initialization
  model <- MakeADFun(data=input.data, parameters=params, DLL="pella_prey_fixed")
  # model$control <- list(trace=1, parscale=rep(1,13), REPORT=1, reltol=1e-12, maxit=100)
  # model$hessian <- F
  # newtonOption(model, smartsearch=TRUE)
  
  # Run model
  output <- TMBhelper::fit_tmb(obj=model, lower=-Inf, upper=Inf, loopnum=3, newtonsteps=3, bias.correct=FALSE, getsd=FALSE)
  
  
  # 3. Check fit
  ######################################
  
  # Use hessian to diagnose fixed effects that might cause a problem
  hess <- optimHess(par=output$par, fn=model$fn, gr=model$gr)
  problem.vals <- which(eigen(hess)$values<0)
  if(length(problem.vals)>0 ){
    display <- eigen(hess)$vectors[,problem.vals]
    names(display) = (output$diagnostics$Param)
    cbind(1:length(output$par), output$par, display)
  }
  
  # Calculate SD
  sd <- try(sdreport(model, hessian.fixed=hess))
  
  # AIC of model
  TMBhelper::TMBAIC(output)
  
  
  # 4. Plot results
  ######################################
  
  # Format output
  results <- format_output(sd, stocks, covariate_name)
  
  # Plot results
  plot_ests(results)
  plot_thetas(results)
  
  
  # 5. Export results
  ######################################

  # Outfile name
  outfile <- paste0("pella_", "", format(p, nsmall=2), "p_fixed_", covariate_name, ".Rdata")
  
  # Export model objects
  setwd(origdir)
  save(data, stocks, nstocks,
       input.data, params,
       model, output, sd, hess, results,
       file=paste(outputdir, outfile, sep="/"))

}

# Fit models
################################################################################

fit_sp(data, covariate="sst_c_scaled", covariate_name="sst", p=1)


