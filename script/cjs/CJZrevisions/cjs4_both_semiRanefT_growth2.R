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
cjs2 <- nimbleCode({

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

# the code calculates captureJJ and allJJ over all years, and concatenates all pup_ch matrices into one large matrix ch_dum. 
captureJJ = unique(unlist(lapply(pupData_list, function(x) x$julianDay)))
allJJ <- seq(min(captureJJ),max(captureJJ))

ch_dum <- matrix(0, nrow = sum(sapply(pup_ch, nrow)), ncol = length(allJJ))
idx_start <- 1

# the loop that was used to create ch_dum for each year is now replaced by a loop that iterates over each pup_ch matrix and copies the data to ch_dum (rows)
for (i in 1:length(pup_ch)) {
    pup_ch_i <- pup_ch[[i]]
    tmp <- match(pup_ch_i$julianDay, allJJ)
    for (i2 in 1:length(tmp)) {
        ch_dum[idx_start:(idx_start+nrow(pup_ch_i)-1), tmp[i2]] <- pup_ch_i[,i2]
    }
    idx_start <- idx_start + nrow(pup_ch_i)
}
rownames(ch_dum) <- unlist(lapply(pup_ch, rownames))

get.first <- function(x) min(which(x != 0))

dflist <- list(
    data=list(y=as.matrix(ch_dum),
              mass=unlist(lapply(pupData_list, function(x) x$mass))),
    const=list()
)

dflist$const <- list(
    f = lapply(dflist$data$y, function(x) get.first(x)),
    nind = nrow(dflist$data$y),
    n.occasions = ncol(dflist$data$y),
    captureJJ = allJJ,
    firstOcc = min(allJJ),
    Nw = sum(sapply(pupData_list, nrow)),
    weanedAge = 30,
    julianDay = do.call(c, lapply(pupData_list, function(x) x$julianDay)),
    nimbleID = do.call(c, lapply(pupData_list, function(x) x$nimbleID)),
    first.bd = NA,
    fakeOcc = which(colSums(dflist$data$y) == 0),
    realOcc = which(colSums(dflist$data$y) > 0),
    min.bd = 100
)

dflist$const$OccuN <- diff(c(0, dflist$const$realOcc))
tmptmp <- pupData_list |>
    group_by(myID) |> 
    summarise(min.bd = min(julianDay)) |> 
    pull(min.bd)[match(unlist(lapply(pup_ch, rownames)), pull(myID))]
dflist$const$first.bd <- ifelse(is.na(tmptmp), max(unique(do.call(c, lapply(pupData_list, function(x) x$julianDay)))), tmptmp)
dflist$const$nfakeOcc <- length(dflist$const$fakeOcc)
dflist$const$nrealOcc <- length(dflist$const$realOcc)



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
    code = cjs2,
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
