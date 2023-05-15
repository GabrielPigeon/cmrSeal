# script for estimating preweaning survival in harbour seal using a cjs model 
# code adapted from Kéry and Schaub 2012.
# model fitting a constant phi and a constant p (mean.p)(m2 in model selection table)
# the code also add a year*sex interaction on the capture probabiilty 


library(dplyr)
library(magrittr)
library(nimble)
library(coda)
library(boot)
library(nimbleEcology)
library(lubridate)# function now to calculate time to run model

# mydat for all yrs -------------------------------------------------------
# load("~/projects/def-pelleti2/renl2702/phoques/2023-04-04_revisedDf.RData")
load('data/mine/2023-04-04_revisedDf.RData')
# nimble model
cjs <- nimbleCode({
    
    # add linear growth curve
    # 10 kg at birth # changed to 8 since 10 occurred before first bd
    for (j in 1:Nw) {
        wt.hat[j] <- 6 + beta.wt * (julianDay[j] - bDate[nimbleID[j]]) 
        mass[j] ~ dnorm(wt.hat[j], sd = sd.mass)
    }

    # truncated distn between minimal bdate and first entry
    for (i in 1:nind) {
        bDate[i] ~ T(dnorm(mu.bd, sd = sd.bd), min.bd, first.bd[i]) 
    }
    
    # Priors and constraints
    for (i in 1:nind) { # individuals
        for (t in f[i]:(n.occasions - 1)) { # time
            logit(phi[i,t]) <- phi.B.YearSite[site_int[i]+1,year_int[i]]+
              phi.B.weaned*weaned[i,t]
              
        }
        
        for(t in 1:n.occasions){
            logit(p[i,t]) <- p.mean + 
                weaned[i, t] * p.betaWeaned +
               site_int[i] * p.betaSite +
              p.betaYear[year_int[i]] # additive 
               # site_int[i] * betaYearSite[year_int[t]] # interaction
               # betaYearSite[year_int[i]]   # fixed site; random yrsite
        }
        
        # age # vector of 10 dates # weanedAge=constant, specified below
        for (t in 1:n.occasions) {
            weaned[i, t] <- (captureJJ[t] - bDate[i]) > weanedAge 
        } #t
    } #i
    

  # prior wt-BD model
    beta.wt ~ dnorm(0.5, 0.001)
    mu.bd ~ dnorm(130, 0.001) # increased precision since was too low and underestimated birthdate 
    sd.mass ~ dunif(0, 10)
    sd.bd ~ dunif(1, 20)
    
    # prior P
    p.betaWeaned ~ dnorm(0, 0.001)
    p.mean ~ dlogis(0, 1)

    p.betaSite~dlogis(0,1) # with * 0 will be reference level - additive effect of yr and site
    sd.p~dunif(0, 1.5)
    # interaction year*site
    for(i in 1:16) {
        p.betaYear[i]~dnorm(0,sd=sd.p)
    }
    
    
    phi.mean[1] ~dlogis(0, 1)
    phi.mean[2] ~dlogis(0, 1)
    for(i in 2:16) {
      phi.betaYear[1,i]~dnorm(phi.mean[1],sd=1.5)
      phi.betaYear[2,i]~dnorm(phi.mean[2],sd=1.5)
      }
    phi.betaWeaned~ dnorm(0, sd=4)
    
    # sd.yr~ dunif(0, 5)
    #int.wt ~ dnorm(0,0.001) # à mettre + informatif

    # Likelihood
    
    # trueOcc is a matrix of whether an animal was really seen on that day - to space out unequal time intervals
    # new predefined distribution with NimbleEcology. Former was manually done in nimble
    
    for (i in 1:nind) {
       # Define latent state at first capture
        z[i, f[i]] <- 1
        for (t in (f[i] + 1):n.occasions) {
            # State process
            z[i, t] ~ dbern(mu1[i, t])
            mu1[i, t] <- phi[i, t - 1] * z[i, t - 1]
              # Observation process
            y[i, t] ~ dbern(mu2[i, t])
            mu2[i, t] <- p[i, t] * z[i, t]*trueOcc[i,t] # 0=not truly observed
        } #t
       # y[i,f[i]:n.occasions] ~ dCJS_vv(phi[i, f[i]:n.occasions],
        #                                p[i, f[i]:n.occasions]*trueOcc[i,f[i]:n.occasions])
    } #i
    

    # derived survival from unequal occasions
   #  daily surv takes the average surv not the random time variation
    logit(dailySurv[1]) <- phi.mean[1]
    weanSurv[1] <- dailySurv[1]^weanedAge
    logit(dailySurv[2]) <- phi.mean[2] 
    weanSurv[2] <- dailySurv[2]^weanedAge
})

# Define a function to get the first non-zero value in a vector (the earliest possible entry date)
get.first <- function(x) min(which(x != 0)) 
    
# Defince vector of all capture occasions, ordered
# Get the unique Julian days in the current site's pup data
captureJJ <- unique(pvData_filtered$julianDay)

# Create a vector of all possible Julian days (min to max)
allJJ <- seq(min(captureJJ), max(captureJJ))

# Store the capture history data and pup mass in a list, along with latent variable matrix z to estimate surv
df <- list()
df <- list(
    data = list(y = as.matrix(obs),
                mass = pvData_filtered$mass,
                z=data.z), # data z is created in data 
    const = list()
)
    # sex will be in data since NA
# Store additional constants in the list - no NA allowed
df$const <- list(
    f = apply(df$data$y, 1, function(x) get.first(x)),
    nind = nrow(df$data$y),
    n.occasions = ncol(df$data$y),
    captureJJ = allJJ,
    firstOcc = min(allJJ),
    Nw = nrow(pvData_filtered),
    weanedAge = 30,
    julianDay = pvData_filtered$julianDay,
    nimbleID = pvData_filtered$nimbleID,
    first.bd = NA,
    trueOcc=trueOcc,
    min.bd = 100,
    site_int=site_int, # no NA thus const
    year_int=year_int # no NA thus const
    #OccuN = NULL,
    #nfakeOcc = NULL,
    #nrealOcc = NULL
    )
    
# Pull a vector of minimal dates for existing ID
tmptmp <- pvData_filtered %>% 
    group_by(myID) %>% 
    summarise(min.bd = min(julianDay))
    
# Match the minimal dates to the obs matrix rows using their myID
tmptmp <- tmptmp$min.bd[match(rownames(df$data$y), tmptmp$myID)]
    
# Set the 'first.bd' constant value to the minimum date for each individual in the data set
df$const$first.bd <- ifelse(
    is.na(tmptmp),
    max(unique(pvData_filtered$julianDay)),
    tmptmp
)
    
# Function to create a matrix of initial values for latent state z (Kery & Schaub 2011)
cjs.init.z <- function(ch,f){ for (i in 1:dim(ch)[1]){
    if (sum(ch[i,])==1) next
    n2 <- max(which(ch[i,]==1)) 
    ch[i,f[i]:n2] <- NA
}
    for (i in 1:dim(ch)[1]){ ch[i,1:f[i]] <- NA
    }
    return(ch)
}

# provide other initial values for computing efficiency
inits <- function() {
    list(
        phi.mean = rnorm(2, 4, 1),
        mean.p = runif(1, -1, 0.5),
        z = cjs.init.z(df$data$y,df$const$f), # to check
        bDate=sample(138:142,size = nrow(df$data$y),replace = T),
        sd.bd=runif(1,1,2),
        sd.mass=runif(1,0,1),
        mu.bd=round(rnorm(1,140,sd = 2)),
        beta.wt=rnorm(1,0.6,0.02),
        p.betaWeaned = runif(1, 0, 1),
        p.betaSite = rnorm(1, 0, 1.5),
        p.betaYear=rnorm(16, 0, 0.2),
        sd.p=runif(1,0.3,0.8),
        phi.betaYear=matrix(rnorm(2*16,0,0.3),2,16),
        # phi.betaYearSite=
        phi.betaWeaned=rnorm(1, 1, .5)# 1st reference level is fixed in models, no init
        # beta = runif(constants$n.occasions,0,1),
        # wt.hat=ifelse(is.na(dflist[[i]]$data$mass),rnorm(dflist[[i]]$const$Nw,16,1),1),
        )
}

# parameters monitored
parameters <-
    c(
        "mean.p",
        "phi.mean",
      #  "z",
        "betaWeaned",
        "beta.wt",
        "bDate",
        "mu.bd",
        "sd.bd",
        "sd.mass",
        "weanSurv",
      #  "ranef.t",
        'dailySurv',
      'betaSite',
      'betaYear',"sd.p"
      # "delta.occ",
        # "phi"
    ) # added w and z to WAIC - here z is shitty


# run model

# run model
chainOut <- nimbleMCMC(
    code = cjs,
    constants = df$const,
    data = df$data,
    inits = inits(),
    monitors = parameters,
    nchains = 2,
    # niter = 500000, thin = 400,nburnin = 100000, # 1000 iterations left to estimate parameters
    niter = 1*500+10, thin = 1,nburnin = 10, # 1000 iterations left to estimate parameters
    WAIC=TRUE,
    summary = TRUE,
    samplesAsCodaMCMC = TRUE
)


write_rds(chainOut,file = 'cache/202304060943_m9.rds',compress = 'xz')


traceplot(chainOut$samples[,c('mean.p',"mean.phi","betaSite",
                         "betaYear[4]","betaYear[12]","sd.p",
                         "bDate[6]","mu.bd","sd.bd")])