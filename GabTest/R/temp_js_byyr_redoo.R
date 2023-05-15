myarg = commandArgs(trailingOnly=TRUE)
cy=ifelse(length(myarg)==0,1,args[1])

# script for estimating preweaning survival in harbour seal using a cjs model 
# code adapted from Kéry and Schaub 2012.
# model fitting a constant phi and a constant p (mean.p)(m1 in model selection table)

library(tidyverse )
library(magrittr)
library(nimble)
library(coda)
library(boot)
library(nimbleEcology)
library(lubridate)# function now to calculate time to run model

# mydat for all yrs -------------------------------------------------------
# load("~/projects/def-pelleti2/renl2702/phoques/2023-04-04_revisedDf.RData")
df.syjs <- read_rds("cache/df_syjs.rds")
# nimble model
js <- nimbleCode({
    # Priors and constraints
    # Priors and constraints
    
    for (i in 1:M) { # individuals
        for (t in 1:(n.occasions - 1)) { # time
            logit(phi[i,t]) <- phi.betaYear[site_int[i]+1]+
                # phi.occ[year_int[i],t]+
                # phi.sex*sex_int[i]+
                phi.betaWeaned*weaned[i,t]
        }
        for(t in 1:n.occasions){
            logit(p1[i,t]) <- p.betaYear[site_int[i]+1]
            p[i,t] <- p1[i,t] *trueOcc[i,t]
            
            weaned[i, t] <- ((captureJJ[t] - bDate[i]) > 30 )
            # born[i,t] <- (captureJJ[t] - bDate[i]) > 0
        }
        
    } #i
    
    # add linear growth curve
    for (j in 1:Nw) {
        wt.hat[j] <- 5.9 + beta.wt * (julianDay[j] - bDate[nimbleID[j]])
        mass[j] ~ dnorm(wt.hat[j], sd = sd.mass)
    }
    
    # truncated distn between minimal bdate and first entry
    for (i in 1:M2) {
        bDate[i] ~ T(dnorm(mu.bd, sd = sd.bd), 100, first.bd[i])
        sex_int[i] ~ dbern(0.5)
    }
    
    
    # prior wt-BD model
    beta.wt ~ dnorm(0.5, 0.001)
    sd.mass ~ dunif(0, 5)
    sd.bd ~ dunif(1, 15)     # at least a 4 day range in BD , max 60
    mu.bd~ dnorm(130, 0.001)
    p.betaWeaned ~ dnorm(0, 0.001)
    phi.betaWeaned~ dnorm(0, sd=4)
    
    # phi.mean~dlogis(0,1)
    # p.mean~dlogis(0,1)
    phi.betaYear[1]~dlogis(0,1)
    phi.betaYear[2]~dlogis(0,1)
    p.betaYear[1]~dlogis(0,1)
    p.betaYear[2]~dlogis(0,1)
    
    
    # Dirichlet prior for entry probabilities
    for (t in 1:n.occasions){
        beta[t] ~ dgamma(1, 1)
        b[t] <- beta[t] / sum(beta[1:n.occasions]) 
    }
    
    # Convert entry probs to conditional entry probs
    nu[1] <- b[1]
    for (t in 2:n.occasions){
        nu[t] <- b[t] / (1 - sum(b[1:(t-1)])) 
    } #t
    
    # Likelihood
    psi ~ dunif(0, 1) 
    for (i in 1:M){ # first
        # First occasion
        # State process
        w[i] ~ dbern(psi)   # Draw latent inclusion
        z[i,1] ~ dbern(nu[1]) # conditional entry prob
        # Observation process
        mu1[i] <- z[i,1] * p[i,1] * w[i] 
        y[i,1] ~ dbern(mu1[i])
        
        # Subsequent occasions
        for (t in 2:n.occasions){ # after first
            # State process
            q[i,t-1] <- 1 - (z[i,t-1])
            mu2[i,t] <- (phi[i,t-1] * z[i,t-1] + nu[t] * prod(q[i,1:(t-1)]) ) 
            z[i,t] ~ dbern(mu2[i,t])
            # Observation process
            mu3[i,t] <- z[i,t] * p[i,t] * w[i]
            y[i,t] ~ dbern(mu3[i,t])
        } #t
    } #i
    
    # Calculate derived population parameters
    for (i in 1:M){
        for (t in 1:n.occasions){
            u[i,t] <- z[i,t] * w[i]     # Deflated latent state (u)
        } 
    }
    for (i in 1:M){
        recruit[i,1] <- u[i,1] 
        for (t in 2:n.occasions){
            recruit[i,t] <- (1 - u[i,t-1]) * u[i,t]
        } #t
    } #i
    for (t in 1:n.occasions){
        N[t] <- sum(u[1:M,t]) # Actual population size 
        B[t] <- sum(recruit[1:M,t]) # Number of entries
    } #t
    for (i in 1:M){
        Nind[i] <- sum(u[i,1:n.occasions]) 
        Nalive[i] <- 1 - equals(Nind[i], 0)
        Nalive1[i] <- Nalive[i]*site_int[i]
    } #i
    Nsuper <- sum(Nalive[1:M]) # Superpopulation size
    NsuperS1 <- sum(Nalive1[1:M])
})




# inits -------------------------------------------------------------------


# provide other initial values for computing efficiency
# w.init=rep(1,nrow(df.syjs[[cy]]$data$y)) # added this after example in Kery 
# 
# inits <- function() {
#     l=list(
#         bDate=sample(135,size =  df.syjs[[cy]]$const$M2,replace = T),
#         sd.bd=runif(1,3,6),
#         sd.mass=runif(1,0,1),
#         mu.bd=round(rnorm(1,140,sd = 2)),
#         beta.wt=rnorm(1,0.6,0.02),
#         p.mean = runif(1, -1, 0.5),
#         phi.betaYear=rnorm(2,0,0.5),
#         p.betaWeaned = runif(1, 0, 1),
#         sd.p=runif(1,0.3,0.8),
#         phi.betaYear=rnorm(2,0,0.5),
#         phi.occ=rnorm(55,0,0.25),
#         phi.betaWeaned= rnorm(1, 0, 1.5),
#         phi.sex= rnorm(1, 0, 1),
#         sd.phi.yr=runif(1,0,0.8),
#         sd.occ=runif(1,0,0.8),
#         w=w.init,#z = z.init,
#         psi = runif(1, 0, 1)
#     )
#     l$bDate=ifelse(l$bDate>df.syjs[[cy]]$const$first.bd,
#                    df.syjs[[cy]]$const$first.bd-2,
#                    l$bDate)
#     return(l)
# }

z.init=df.syjs[[cy]]$data$y # added this 
z.init[z.init==0] <- 1 # added this 
w.init=rep(1,nrow(df.syjs[[cy]]$data$y)) # added this after example in Kery 
inits <- function(){
    l=list(psi = runif(1, 0, 1), w=w.init,z = z.init,
           sex=ifelse(is.na(df.syjs[[cy]]$data$sex)),
           beta.wt=rnorm(1,0.6,0.02),
           sd.bd=runif(1,3,6),
           sd.mass=runif(1,0,1),
           mu.bd=round(rnorm(1,140,sd = 2)),
           phi.betaYear=rnorm(2,0,0.5),
           p.betaYear=rnorm(2,0,0.5),
           phi.betaWeaned = runif(1, 0, 1),
           p.betaWeaned = runif(1, 0, 1),
           bDate=sample(135,size =  df.syjs[[cy]]$const$M2,replace = T))
    
    l$bDate=ifelse(l$bDate>df.syjs[[cy]]$const$first.bd,
                   df.syjs[[cy]]$const$first.bd-2,
                   l$bDate)
    return(l)
}
# parameters monitored
parameters <-c("p.mean",
               # "phi.mean",
               "p.betaWeaned",
               "beta.wt",
               "bDate",
               "mu.bd",
               "sd.bd",
               "sd.mass",
               # "weanSurv",
               # 'dailySurv',
               # 'p.betaSite',
               'p.betaYear',
               # "sd.p",
               # 'phi.sex',
               'phi.betaWeaned',
               'phi.betaYear',
               # 'phi.occ',
               'N','Nsuper','B','NsuperS1'
               # "delta.occ",
               # "phi"
) # added w and z to WAIC - here z is shitty


# run model   ######
df <- df.syjs[[cy]]
nmodel <- nimbleModel(
    code = js,
    constants = df$const,
    data = df$data,
    inits = inits()
)
# nmcmc <- buildMCMC(monitors = parameters)


newOut <- nimbleMCMC(
    code = js,
    constants = df.syjs[[cy]]$const,
    data = df.syjs[[cy]]$data,
    inits = inits(),
    monitors = parameters,
    nchains = 1,
    # niter = 50000, thin = 400,nburnin = 10000, # 1000 iterations left to estimate parameters
    niter = 5000, thin = 3,nburnin = 2000, # 1000 iterations left to estimate parameters
    WAIC=TRUE,
    summary = TRUE,
    samplesAsCodaMCMC = TRUE
)

# change model name in rds object
saveRDS(newOut,file=paste0('cache/20230515_js1c_',cy,'.rds'),compress = 'xz')
