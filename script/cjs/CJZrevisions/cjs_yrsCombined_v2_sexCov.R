# script for estimating preweaning survival in harbour seal using a cjs model # code adapted from Kéry and Schaub 2012.
# this is NOT equivalent to superpop 4.
# model fitting a daily survival variation and constant p (mean.p)


# this model has been revised to include a sex covariate (rev 1), a site effect (rev2) and pool all years together. 

# not yet clear on how to include this year effect. 


library(dplyr)
library(magrittr)
library(nimble)
library(coda)
library(boot)

# mydat for all yrs -------------------------------------------------------
load("~/projects/def-pelleti2/renl2702/phoques/2022-12-05_pupDataCjsCombined.RData")

# nimble model
cjs <- nimbleCode({

    # add linear growth curve
    # 10 kg at birth # changed to 8 since 10 occurred before first bd
    # truncated distn between minimal bdate and first entry
    
    for (j in 1:Nw) {
        wt.hat[j] <- 8 + beta.wt * (julianDay[j] - bDate[nimbleID[j]]) 
        mass[j] ~ dnorm(wt.hat[j], sd = sd.mass)
    }
    for (i in 1:nind) {
        bDate[i] ~ T(dnorm(mu.bd, sd = sd.bd), min.bd, first.bd[i]) 
    }

    # Priors and constraints
    # new variable added for sex where beta.sex is the effect of sex on survival, and sex is the sex of individual i. 
    #  ranef.t[OccuN[t]]: added OccuN before [t] for daily surv variation
    for (i in 1:nind) { # individuals
        for (t in f[i]:(n.occasions - 1)) {
                logit(phi[i,t]) <- mean.phi +
                sbw*weaned[i,t] +
                beta.sex*sex[i] +
                ranef.t[OccuN[t]] 
        }
        # beta.sex is the effect of sex on capture probability. 
        for(t in 1:nrealOcc){
            logit(p[i, realOcc[t]]) <- mean.p + weaned[i, t] * betaWeaned + beta.sex*sex[i] 
        }
        # including dummy 0 between real capture occasions - even spaced out
        for(t in 1:nfakeOcc){ 
            p[i, fakeOcc[t]] <- 0
        }

        # age # vector of 10 dates # weanedAge=constant, specified below
        for (t in 1:n.occasions) {
            weaned[i, t] <- (captureJJ[t] - bDate[i]) > weanedAge 
        } #t
    } #i
    
    for(t in 1:(nrealOcc-1)){
        ranef.t[t]~ dnorm(0, sd=sd.yr)  # priors for time-spec. recapture # added here with addition of OccuN instead of l. 55
    }

    betaWeaned ~ dnorm(0, 0.001)
    mean.phi ~dlogis(0, 1)
    mean.p ~ dlogis(0, 1)
    sbw ~ dnorm(0, 0.001)
    sd.yr~ dunif(0, 5)
    beta.sex~dlogis(0,1)
    #int.wt ~ dnorm(0,0.001) # à mettre + informatif
    beta.wt ~ dnorm(0.5, 0.001)
    mu.bd ~ dnorm(130, 0.001) # increased precision since was too low and underestimated birthdate 
    sd.mass ~ dunif(0, 10)
    sd.bd ~ dunif(1, 20)

    # Likelihood
    for (i in 1:nind) {
        # Define latent state at first capture
        z[i, f[i]] <- 1
        for (t in (f[i] + 1):n.occasions) {
        # for (t in (f[i] + 1):(bDate[i]+weanedAge-firstOcc+1)){   # potential alternative to really truncate after weaning, but beta.weaned and sbw will not longuer be identifiaable

            # State process
            z[i, t] ~ dbern(mu1[i, t])
            mu1[i, t] <- phi[i, t - 1] * z[i, t - 1]
          
              # Observation process
            y[i, t] ~ dbern(mu2[i, t])
            mu2[i, t] <- p[i, t] * z[i, t]
        } #t
    } #i

    # derived survival from unequal occasions
   #  daily surv takes the average surv not the random time variation
    logit(dailySurv) <- mean.phi 
    weanSurv <- dailySurv^weanedAge
})


# loop over sites ---------------------------------------------------------

# this code matches the minimal dates of each individual to the corresponding rows in the ch_dum matrix, sets the first.bd constant value to the minimum date for each individual in the data set, and sets the number of fake and real occasions in the data set. Finally, it renames the list element to indicate which site it belongs to. This code is executed for each site in the pupData_list_2sites list.

# create vector of site
site <- c('bic','metis')

# Initialize empty lists to store data and output
dflist <- list()
outlist <- list()

# Loop over the 2 sites in the pupData_list_2sites
for (site in 1:2) {
    # Get the current site's pup data
    pupData <- pupData_list_2sites[[site]]
    
    # Get the unique Julian days in the current site's pup data
    captureJJ <- unique(pupData$julianDay)
    
    # Create a vector of all possible Julian days (min to max)
    allJJ <- seq(min(captureJJ), max(captureJJ))
    
    # Initialize a matrix to store capture history data
    ch_dum <- matrix(0, nrow = nrow(pup_ch_l[[site]]), ncol = length(allJJ))
    
    # Match the unique Julian days to their positions in the allJJ vector
    tmp <- match(captureJJ, allJJ)
    
    # Fill in the ch_dum matrix with the capture history data
    for (i2 in 1:length(tmp)) {
        ch_dum[,tmp[i2]] <- pup_ch_l[[site]][,i2]
    }
    
    # Set the row names of the ch_dum matrix to match the pup data
    rownames(ch_dum) <- rownames(pup_ch_l[[site]])
    
    # Define a function to get the first non-zero value in a vector (the earliest possible entry date)
    get.first <- function(x) min(which(x != 0)) 
    
    # Store the capture history data and pup mass in a list
    dflist[[site]] <- list(
        data = list(y = as.matrix(ch_dum),
                    mass = pupData$mass),
        const = list()
    )
    
    # Store additional constants in the list
    dflist[[site]]$const <- list(
        f = apply(dflist[[site]]$data$y, 1, function(x) get.first(x)),
        nind = nrow(dflist[[site]]$data$y),
        n.occasions = ncol(dflist[[site]]$data$y),
        captureJJ = allJJ,
        firstOcc = min(allJJ),
        Nw = nrow(pupData),
        weanedAge = 30,
        julianDay = pupData$julianDay,
        nimbleID = pupData$nimbleID,
        first.bd = NA,
        fakeOcc = which(colSums(dflist[[site]]$data$y) == 0),
        realOcc = which(colSums(dflist[[site]]$data$y) > 0),
        min.bd = 100,
        OccuN = NULL,
        nfakeOcc = NULL,
        nrealOcc = NULL
    )
    
    # Set the OccuN constant to force the model to ignore order of daily surv multiplication between occasions
    dflist[[site]]$const$OccuN <- rep(1:(length(dflist[[site]]$const$realOcc)), c(diff(dflist[[site]]$const$realOcc), 1))
    
    # Pull a vector of minimal dates for existing ID in the current site's pup data
    tmptmp <- pupData %>% 
        group_by(myID) %>% 
        summarise(min.bd = min(julianDay))
    
    # Match the minimal dates to the ch_dum matrix rows using their myID
    tmptmp <- tmptmp$min.bd[match(rownames(dflist[[site]]$data$y), tmptmp$myID)]
    
    # Set the 'first.bd' constant value to the minimum date for each individual in the data set
    dflist[[site]]$const$first.bd <- ifelse(
        is.na(tmptmp),
        max(unique(pupData_list_2sites[[site]]$julianDay)),
        tmptmp
    )
    
    # Set the number of fake and real occasions in the data set
    dflist[[site]]$const$nfakeOcc <- length(dflist[[site]]$const$fakeOcc)
    dflist[[site]]$const$nrealOcc <- length(dflist[[site]]$const$realOcc)
    
    # Rename the list element to indicate which site it belongs to
    names(dflist)[[site]] <- paste0("Site", site)
}   









known.state.cjs <- function(ch) {
    state <- ch
    for (i in 1:dim(ch)[1]) {
        n1 <- min(which(ch[i, ] == 1))
        n2 <- max(which(ch[i, ] == 1))
        state[i, 1:n2] <- 1 # added 1 but still NAs in Z
        state[i, n1] <- NA
    }
    state[state == 0] <- NA
    return(state)
}

inits <- function() {
    list(
        mean.phi = rnorm(1, 5, 1),
        mean.p = runif(1, -1, 0.5),
        z = known.state.cjs(dflist[[i]]$data$y),
        #beta = runif(constants$n.occasions,0,1),
        bDate=sample(138:142,size = nrow(dflist[[i]]$data$y),replace = T),
        sd.bd=runif(1,1,2),
        sd.mass=runif(1,0,1),
        mu.bd=round(rnorm(1,140,sd = 2)),
        # wt.hat=ifelse(is.na(dflist[[i]]$data$mass),rnorm(dflist[[i]]$const$Nw,16,1),1),
        beta.wt=rnorm(1,0.6,0.02),
        betaWeaned = runif(1, 0, 1)
    )
}
# parameters monitored
parameters <-
    c(
        "mean.p",
        "mean.phi",
        "z",
        "betaWeaned",
        "beta.wt",
        "bDate",
        "mu.bd",
        "sd.bd",
        "sd.mass",
        "weanSurv",
        "ranef.t",
        'dailySurv'
        # "delta.occ",
        # "phi"
    ) # added w and z to WAIC - here z is shitty

newOut <- nimbleMCMC(
    code = cjs,
    constants = dflist[[i]]$const,
    data = dflist[[i]]$data,
    inits = inits(),
    monitors = parameters,
    nchains = 3,
    niter = 500000, thin = 400,nburnin = 100000,
    WAIC=TRUE,
    summary = TRUE,
    samplesAsCodaMCMC = TRUE
)
outlist <- list()
outlist[[i]] <- newOut
print('done with:')
print(i)

saveRDS(outlist,file=paste0('~/projects/def-pelleti2/renl2702/phoques/outputs/cjs4_both_semiRanefTGrowth2.rds'),compress = 'xz')
