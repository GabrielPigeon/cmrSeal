dflist<-list()
for(i in 1:16){ # treat each year separately
  # i=6
  now=as.character(years[i])
  #mydata <- list_ch_metis[[now]]
  captureJJ = unique(metisData35_l[[now]]$julianDay)
  allJJ <- seq(min(captureJJ),max(captureJJ))
  ch_dum <- matrix(0,nrow = nrow(list_ch_metis[[now]]),ncol = length(allJJ))
  tmp <- match(captureJJ, allJJ)
  for(i2 in 1:length(tmp)){
    ch_dum[,tmp[i2]] <- list_ch_metis[[now]][,i2]
  }
  rownames(ch_dum) <- rownames(list_ch_metis[[now]])
  get.first <- function(x)    min(which(x != 0))
  
  dflist[[i]]<-list(
    data=list(y=as.matrix(ch_dum),
              mass=metisData35_l[[now]]$mass),
    const=list()
  )
  dflist[[i]]$const <- list(f=apply(dflist[[i]]$data$y, 1,function(x) get.first(x)),
                            nind = nrow(dflist[[i]]$data$y), #  nind = dim(mydata)[1],
                            n.occasions = ncol(dflist[[i]]$data$y), #  n.occasions = dim(mydata)[2],
                            captureJJ = allJJ,firstOcc=min(allJJ),
                            Nw =  nrow(metisData35_l[[now]]),
                            weanedAge = 30,# give a value
                            julianDay=metisData35_l[[now]]$julianDay,
                            # mass = metisData35_l[["2009"]]$mass,
                            nimbleID = metisData35_l[[now]]$nimbleID,
                            first.bd=NA,
                            fakeOcc=which(colSums(dflist[[i]]$data$y)==0),
                            realOcc=which(colSums(dflist[[i]]$data$y)>0),
                            min.bd=100 # pas de naissance en hiver
  )
  
  dflist[[i]]$const$OccuN=rep(1:(length(dflist[[i]]$const$realOcc)),c(diff(dflist[[i]]$const$realOcc),1))
  tmptmp=  metisData35_l[[i]] %>% group_by(myID) %>% summarise(min.bd = min(julianDay)) # pull vector of minimal dates for existing ID - their entry date
  tmptmp=  tmptmp$min.bd[match(rownames(dflist[[i]]$data$y),as.character(tmptmp$myID))]
  dflist[[i]]$const$first.bd <- ifelse(is.na(tmptmp),max(unique(metisData35_l[[i]]$julianDay)),tmptmp)
  dflist[[i]]$const$nfakeOcc=length(dflist[[i]]$const$fakeOcc)
  dflist[[i]]$const$nrealOcc=length(dflist[[i]]$const$realOcc)
  
  names(dflist)[[i]]=now
  
}

wtpart0 <- nimbleCode({
  
  # add linear growth curve
  for (j in 1:Nw) {
    # nb lines mass 1998
    wt.hat[j] <- 10 + beta.wt * (julianDay[j] - bDate[nimbleID[j]]) # Dubé 2003 - 10 kg at birth
    mass[j] ~ dnorm(wt.hat[j], sd = sd.mass)
  }
  for (i in 1:nind) {
    bDate[i] ~ T(dnorm(mu.bd, sd = sd.bd), min.bd, first.bd[i]) # truncated distn between minimal bdate and first entry
  }
  beta.wt ~ T(dnorm(0.5, 0.01),0,)
  mu.bd ~ dnorm(140, sd=10) # à checker dans la litt
  sd.mass ~ dunif(0, 10)
  sd.bd ~ dunif(0, 20)
})

wtpart2 <- nimbleCode({
  
  # add linear growth curve
  for (j in 1:Nw) {
    # nb lines mass 1998
    wt.hat[j] <- 10 + ranef.id[nimbleID[j]] + beta.wt[nimbleID[j]] * (julianDay[j] - bDate[nimbleID[j]]) # Dubé 2003 - 10 kg at birth
    mass[j] ~ dnorm(wt.hat[j], sd = sd.mass)
  }
  
  for (i in 1:nind) {
    ranef.id[i]~dnorm(0, sd = sd.idwt)
    bDate[i] ~ T(dnorm(mu.bd, sd = sd.bd), min.bd, first.bd[i]) # truncated distn between minimal bdate and first entry
    # bDate[i] ~ dunif( min.bd, first.bd[i]) # truncated distn between minimal bdate and first entry
    beta.wt[i] ~ T(dnorm(beta.wt.mu, sd = sd.bwt),0,)
    }
  
  #int.wt ~ dnorm(0,0.001) # à mettre + informatif
  beta.wt.mu ~ T(dnorm(0.5, sd=0.3),0,)
  sd.bwt ~ dnorm(0, sd=2)
  sd.idwt ~ dnorm(0, sd=2)
  mu.bd ~ dnorm(140, 0.01) # à checker dans la litt
  sd.mass ~ dunif(0, 10)
  sd.bd ~ dunif(0, 20)
})

wtpart <- nimbleCode({
  
  # add linear growth curve
  for (j in 1:Nw) {
    # nb lines mass 1998
    wt.hat[j] <- intercept + beta.wt * (julianDay[j]) +  ranef.wt[nimbleID[j]] # Dubé 2003 - 10 kg at birth
    mass[j] ~ dnorm(wt.hat[j], sd = sd.mass)
  }
  for (i in 1:nind) {
    ranef.wt[i] ~ dnorm(0, sd = sd.wt2)
    bDate.hat[i] <- (10-intercept-ranef.wt[i])/beta.wt
    # ~ dnorm(0, sd = sd.bd)
    # bDate[i] ~ T(dnorm(bDate.hat[i], sd = sd.bd), min.bd, first.bd[i]) # truncated distn between minimal bdate and first entry
  }
  
  #int.wt ~ dnorm(0,0.001) # à mettre + informatif
  beta.wt ~ T(dnorm(0.5, sd=0.3),0,)
  intercept ~ dnorm(10, 0.001)
  mu.bd ~ dnorm(140, 0.01) # à checker dans la litt
  sd.mass ~ dunif(0, 10)
  sd.wt2 ~ dunif(0, 10)
  sd.bd ~ dunif(0, 20)
})



inits <- function() {
  list(
    bDate=sample(138:142,size = nrow(dflist[[i]]$data$y),replace = T),
    sd.bd=runif(1,0,2),
    sd.mass=0.1,
    mu.bd=round(rnorm(1,140,sd = 2)),
    # wt.hat=ifelse(is.na(dflist[[i]]$data$mass),rnorm(dflist[[i]]$const$Nw,16,1),1),
    beta.wt.mu=rnorm(1,0.6,0.02)
  )
}
# parameters monitored
parameters <-
  c(#"beta.wt.mu","sd.bwt",
    "bDate",
  "beta.wt",
    # "bDate.hat",
    "mu.bd",
    "sd.bd",
    "sd.mass"
  )

i=1
newOut_0 <- nimbleMCMC(
  code = wtpart0 ,
  constants = dflist[[i]]$const,
  data = dflist[[i]]$data,
  inits = inits(),
  monitors = parameters,
  nchains = 3,
  # niter = 500000, thin = 400,nburnin = 100000,
  niter = 10000, thin = 10,nburnin = 1,
  WAIC=TRUE,
  summary = TRUE,
  samplesAsCodaMCMC = TRUE
)

newOut_0$summary$all.chains
plot(newOut$samples[,"beta.wt"])
plot(newOut_0$samples[,"beta.wt.mu"])
plot(newOut$samples[,"bDate.hat[6]"])
plot(newOut_0$samples[,"bDate[6]"])
plot(newOut_0$samples)
