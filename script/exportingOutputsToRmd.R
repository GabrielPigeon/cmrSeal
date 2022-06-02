#Values substantially above 1 indicate lack of convergence. If the chains have not converged, Bayesian credible intervals based on the t-distribution are too wide, and have the potential to shrink by this factor if the MCMC run is continued.
library(magrittr) # problems with tidyverse. Installed this instead
library(dplyr)
library(coda)
library(bookdown)
library(ggthemes)
library(pander)
library(cowplot)
library(lubridate)
library(coda)
library(boot)
library(purrr)
library(ggmcmc)

rm(list = ls())


years<- c(seq(as.numeric("1998"), as.numeric("2003"), by=1), seq(as.numeric("2008"), as.numeric("2016"), by=1), 2019)

#  get gelmanRubin and WAIC for Jolly Seber--------------------------------------------------------
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic1")

aicb1<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    aicb1[i,2]<-filename_uid
    aicb1[i,3]<-fd[[i]]['WAIC']
}


codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
  }


gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)

# get nicer df - stil has to pivot 
gelman1 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic2")
aicb2<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    #dfWAIC[i,2]<-filename_uid
    aicb2[i,3]<-fd[[i]]['WAIC']
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)


# get nicer df - stil has to pivot 
gelman2 <-map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic3")
aicb3<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    #dfWAIC[i,2]<-filename_uid
    aicb3[i,3]<-fd[[i]]['WAIC']
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)


# get nicer df - stil has to pivot 
gelman3 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)


setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic4")
aicb4<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    #dfWAIC[i,2]<-filename_uid
    aicb4[i,3]<-fd[[i]]['WAIC']
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)

gelman4 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)


# add no5 here #ERROR IN SD RUNIF -REPLACE BY DUNIF
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic5")
aicb5<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    aicb5[i,2]<-filename_uid
    aicb5[i,3]<-fd[[i]]['WAIC']
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)


# get nicer df - stil has to pivot 
gelman5 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)


# yr     y
# <dbl> <dbl>
#   1  1999  1.23
# 2  2003  2.70
# 3  2010  1.13
# 4  2012  1.13
# 5  2013  1.22


# 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210614/bic6")
aicb6<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    aicb6[i,2]<-filename_uid
    aicb6[i,3]<-fd[[i]]['WAIC']
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)

gelman6 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)

# yr     y
# <dbl> <dbl>
#   1  1998  1.51
# 2  2002  1.26
# 3  2008  1.17
# 4  2014  1.21
# 5  2015  1.15
# 6  2016  1.10


# m7
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210614/bic7")
aicb7<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
    filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
    aicb7[i,2]<-filename_uid
    aicb7[i,3]<-fd[[i]]['WAIC']
}

# quick check
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 14, 18))[[i]])
}

gelmanN_bic <- vector("list",length=16)

for(i in 1:length(gelmanN_bic)){
  gelmanN_bic[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_bic)
gelman7 <- map_dfr(1:length(gelmanN_bic),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_bic[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)

# yr     y
# <dbl> <dbl>
#   1  1998  1.51
# 2  2002  1.26
# 3  2008  1.17
# 4  2014  1.21
# 5  2015  1.15
# 6  2016  1.10



# get nice table to export
aicT<-data.frame(model = c(1:7), description=NA, WAIC = NA)
aicT[1,3]<-mean(aicb1$WAIC, na.rm = T) # need a specific name for model 1, bic
aicT[2,3]<-mean(aicb2$WAIC, na.rm = T) # here change object name to refer to model 2
aicT[3,3]<-mean(aicb3$WAIC, na.rm = T)
aicT[4,3]<-mean(aicb4$WAIC, na.rm = T)
aicT[5,3]<-mean(aicb5$WAIC, na.rm = T)
aicT[6,3]<-mean(aicb6$WAIC, na.rm = T)
aicT[7,3]<-mean(aicb7$WAIC, na.rm = T)



# metis
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis1")

aicm1<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  aicm1[i,2]<-filename_uid
  aicm1[i,3]<-fd[[i]]['WAIC']
}

# quick check
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman1 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis2")
aicm2<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicm2[i,3]<-fd[[i]]['WAIC']
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman2 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)




setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis3")
aicm3<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicm3[i,3]<-fd[[i]]['WAIC']
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman3 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # 2013-2016 doesn't look so good 



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis4")
aicm4<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicm4[i,3]<-fd[[i]]['WAIC']
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman4 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)



#  MODEL 5 HAS AN ERROR - REPLACE RUNIF BY DUNIF

setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis5")
aicm5<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicm5[i,3]<-fd[[i]]['WAIC'] # ATTENTION MIGHT NEED TO CHANGE 
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman5 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20220319/metis6")
aicm6<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #aicm6[i,2]<-filename_uid
  aicm6[i,3]<-fd[[i]]$WAIC$WAIC# NEW to this way of doing since nimble new version?? 
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman6 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20220319/metis7")
aicm7<-data.frame(year = c(1998:2003, 2008:2016, 2019), file_name=NA, WAIC = NA)
fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
  #filename_uid <- unlist(strsplit(file[[i]], "_"))[3]
  #dfWAIC[i,2]<-filename_uid
  aicm7[i,3]<-fd[[i]]$WAIC$WAIC # NEW to this way of doing
}
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # did not converge well in 2001, 2002 and 2014
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}

gelmanN_metis <- vector("list",length=16)

for(i in 1:length(gelmanN_metis)){
  gelmanN_metis[[i]] <- gelman.diag(codaSamp[[i]][,'Nsuper'])
}
print(gelmanN_metis)
gelman7 <- map_dfr(1:length(gelmanN_metis),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanN_metis[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)

# not so sure about mod 6 and 7 since Nsuper was not extracted correctly - all look the same 

aicTM<-data.frame(model = c(1:7), description=NA, WAIC = NA)
aicTM[1,3]<-mean(aicm1$WAIC, na.rm = T) # need a specific name for model 1, bic
aicTM[2,3]<-mean(aicm2$WAIC, na.rm = T) # here change object name to refer to model 2
aicTM[3,3]<-mean(aicm3$WAIC, na.rm = T)
aicTM[4,3]<-mean(aicm4$WAIC, na.rm = T)
aicTM[5,3]<-mean(aicm5$WAIC, na.rm = T)
aicTM[6,3]<-mean(aicm6$WAIC, na.rm = T)
aicTM[7,3]<-mean(aicm7$WAIC, na.rm = T)



# quick convergence check for model 7 - metis - DIDNT'T WORK
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y_", substr(file, 16, 20))[[i]])
  #plot(codaSamp[[i]][,'mean.phi'], main = paste0("mean.phi_y_", substr(file, 14, 18))[[i]]) # plot chain # my last year did not converge well
  #plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file, 14, 18))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  #plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file, 14, 18))[[i]])
  #plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 14, 18))[[i]])
}



# get gelmanRubin statistics and WAIC for Cormack-Jolly-Seber (survival estimates)--------------------------------------------------------

#sapply(outlist, function(x) x$WAIC$WAIC)

#export table with probabilities
# weanSurvOut %>%
#   mutate_if(is.numeric, round, digits = 2) 

# decompresser le rds avec une connection speciale
# ATTENTION CA DIT PAS TOUJOURS SI Y A UNE ERREUR D'EMPLACEMENT


con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/bic_cjs1.rds")
outlist <- readRDS(con)
WAIC.b.1<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
# print(gelmanCJS_b)

map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) 


con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/bic_cjs2_fewIT.rds")
#con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/bic_cjs_Gab2_fewIt.rds") # did not have WAIC?
outlist <- readRDS(con)
WAIC.b.2<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
# print(gelmanCJS_b)

map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # 2012 is very bad



con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220511/cjs3_ranef.rds")
outlist <- readRDS(con)
WAIC.b.3<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
# print(gelmanCJS_b)

map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # 2012  6.01



con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/cjs4.rds") # no good
outlist <- readRDS(con)
WAIC.b.4<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_b)

map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1)
# yr     y
# <dbl> <dbl>
#   1  1999  1.37
# 2  2001  1.49
# 3  2002  1.18
# 4  2003  1.13
# 5  2008  1.76
# 6  2009  2.16
# 7  2011  1.12
# 8  2012  1.13
# 9  2013  2.22
# 10  2016  1.23


con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/cjs5.rds")
outlist <- readRDS(con)
WAIC.b.5<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_b)

map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # a lot ! 2008-2009 are 4 and 5
# yr     y
# <dbl> <dbl>
#   1  1998  1.21
# 2  2000  1.12
# 3  2002  1.27
# 4  2008  4.42
# 5  2009  5.05
# 6  2012  1.21
# 7  2013  1.82
# 8  2016  1.11
# 9  2019  1.32




con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220511/bic_cjs6.rds")
outlist <- readRDS(con)

WAIC.b.6<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_b <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_b[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_b)

map_dfr(1:length(gelmanCJS_b),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_b[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # 2009 and 2013 are over 2



# summarise into one table for all years. means might not be good
waic.table<-expand.grid(model = c(1:6), description = NA,WAIC = NA, site=c('bic', 'metis'))

# fill it for bic 
waic.table[1,3]<-mean(WAIC.b.1$WAIC, na.rm = T) # need a specific name for model 1, bic
waic.table[2,3]<-mean(WAIC.b.2$WAIC, na.rm = T) # here change object name to refer to model 2
waic.table[3,3]<-mean(WAIC.b.3$WAIC, na.rm = T)
waic.table[4,3]<-mean(WAIC.b.4$WAIC, na.rm = T)
waic.table[5,3]<-mean(WAIC.b.5$WAIC, na.rm = T)
waic.table[6,3]<-mean(WAIC.b.6$WAIC, na.rm = T)

#rm(list=setdiff(ls(), c("years", "waic.table")))








# Metis -------------------------------------------------------------------

# the latest models were ran with 500K. It improved Gelman statistics

years<- c(seq(as.numeric("1998"), as.numeric("2003"), by=1), seq(as.numeric("2008"), as.numeric("2016"), by=1), 2019)

metis1 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220517/metis_cjs1.rds")
outlist <- readRDS(metis1)
class(outlist)
WAIC.m.1<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here

gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # all of which above 1.1, 1998 worst

# <dbl> <dbl>
#     1  2009  1.17
# 2  2011  1.33
# 3  2013  1.13


m2 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220517/metis_cjs2_fewIT.rds")
outlist <- readRDS(m2)
WAIC.m.2<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # all of which above 1.1, 2000 worst

# yr     y
# <dbl> <dbl>
#     1  2003  1.69
# 2  2009  1.12
# 3  2010  1.23
# 4  2011  1.14
# 5  2015  1.14

m3 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220517/cjs3_ranef_metis.rds")
outlist <- readRDS(m3)
WAIC.m.3<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # OK
# 2011  1.37


m4 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220517/cjs4_metis.rds")
outlist <- readRDS(m4)
WAIC.m.4<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) #
# yr     y
# <dbl> <dbl>
#     1  2003  1.15
# 2  2008  1.76
# 3  2011  1.10


m5 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220517/cjs5_metis.rds")
outlist <- readRDS(m5)
WAIC.m.5<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
#print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) # all of which above 1.1, 1998 worst
# yr     y
# <dbl> <dbl>
#     1  2000  1.11
# 2  2008  1.42
# 3  2009  1.22
# 4  2011  1.34



m6 <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220517/metis_cjs6.rds")
outlist <- readRDS(m6)
WAIC.m.6<- sapply(outlist,function(x)x$WAIC$WAIC) %>% as.data.frame() %>% rename(WAIC='.') %>% mutate(yr=years) # no need to transpose here
gelmanCJS_m <- vector("list",length=16)
for(i in 1:length(outlist)){
  gelmanCJS_m[[i]] <- gelman.diag(outlist[[i]]$samples[,'weanSurv'])
}
# print(gelmanCJS_m)

map_dfr(1:length(gelmanCJS_m),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(gelmanCJS_m[[x]]))# $samples[,'weanSurv']
  )
}) %>% as.data.frame() %>% group_by(yr) %>% slice(1) %>% filter(y>1.1) 
# yr     y
# <dbl> <dbl>
#     1  2008  1.15
# 2  2009  1.11
# 3  2011  1.27

# summarise into same table for all years and site. means might not be good
waic.table[7,3]<-mean(WAIC.m.1$WAIC, na.rm = T) # 311.5526 here change object name to refer to model 2
waic.table[8,3]<-mean(WAIC.m.2$WAIC, na.rm = T)# 311.4428

waic.table[9,3]<-mean(WAIC.m.3$WAIC, na.rm = T)#307.1165
waic.table[10,3]<-mean(WAIC.m.4$WAIC, na.rm = T)# 311.018
waic.table[11,3]<-mean(WAIC.m.5$WAIC, na.rm = T)#312.6037
waic.table[12,3]<-mean(WAIC.m.6$WAIC, na.rm = T) #312.3701


# clean up a bit - keep only results? 
# rm(list=setdiff(ls(), c("years", "waic.table")))




# get abundance estimates from the Jolly Seber  ------------------------------------------------

# prepare df to graph
tmp <- data.frame(yr = rep(c(1998:2003, 2008:2016, 2019),2), site = c(rep('bic',16),rep('metis',16)), N = NA, ymin = NA, ymax = NA, 
                  surv = NA, min.surv = NA, max.surv = NA)

# bst model for abundance 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic3") # lowest WAIC
# setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210614/bic6")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

for(i in 1:length(fd)){
  sample_df=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() # c long 
  N=sample_df[,grepl('Nsuper',colnames(sample_df))]
  tmp[tmp$site=='bic',]$N[i]=mean(N)
  tmp[tmp$site=='bic',]$ymin[i]= quantile(N, 0.025)
  tmp[tmp$site=='bic',]$ymax[i]=quantile(N, 0.975)
 }

# get plot of convergence # only one year at a time
# df_l <- sample_df %>% select(Nsuper) %>% # select the 'right' raw parameters you monitored in the corresponding model
#   mutate(chain=rep(1:length(fd[[i]]$samples),each=1000),
#          it=rep(1:1000,3)) %>% # ATTENTION bon seulement si tu gardes toujours le même nombre d'itérations - 1000 
#   gather(key="parameter", value="value",-chain,-it)
# 
# ps <- df_l %>% ggplot(aes(x=it, y = value,color=chain)) + geom_line()
# ps + facet_wrap(~parameter, scales = "free")
# p <- ggplot(df_l,aes(value)) + geom_histogram(aes( y= ..density..),bins = 60)
# p
# p + facet_wrap(~parameter, scales = "free")

# now extract with a time lag 
# tmp <- tibble(Nt1 = lead(tmp$N), tmp)
# tmp$lambda <- tmp$Nt1/tmp$N





# check on another type of model
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210614/bic7") # lowest WAIC

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
 #plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y_", substr(file, 14, 18))[[i]])
  #plot(codaSamp[[i]][,'mean.phi'], main = paste0("mean.phi_y_", substr(file, 14, 18))[[i]]) # plot chain # my last year did not converge well
  #plot(codaSamp[[i]][,grepl("B", colnames(codaSamp[[i]][[1]]))], main = paste0("B_y_", substr(file, 14, 18))[[i]]) # l nomb de la premieère chaine (pareille partout0)
  #plot(codaSamp[[i]][,grepl("b", colnames(codaSamp[[i]][[1]]))], main = paste0("b_y_", substr(file, 14, 18))[[i]])
  plot(codaSamp[[i]][,grepl("N", colnames(codaSamp[[i]][[1]]))], main = paste0("N_y_", substr(file, 14, 18))[[i]])
}


for(i in 1:length(fd)){
  sample_df=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() # c long 
  N=sample_df[,grepl('Nsuper',colnames(sample_df))]
  tmp[tmp$site=='bic',]$N[i]=mean(N)
  tmp[tmp$site=='bic',]$ymin[i]= quantile(N, 0.025)
  tmp[tmp$site=='bic',]$ymax[i]=quantile(N, 0.975)
}

ggplot(tmp,aes(x=yr,y=N,ymin=ymin,ymax=ymax))+geom_pointrange()





# metis
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis3")
# setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20220319/metis6")
# setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20220322")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

# check convergence 
codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                 
  plot(codaSamp[[i]][,'Nsuper'], main = paste0("N_y", substr(file, 16, 20))[[i]])
}


# get N
for(i in 1:length(fd)){
  sample_df=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() 
  N=sample_df[,grepl('Nsuper',colnames(sample_df))] # un seul N, sur la bonne échelle
  tmp[tmp$site=='metis',]$N[i]=mean(N)
  tmp[tmp$site=='metis',]$ymin[i]= quantile(N, 0.025)
  tmp[tmp$site=='metis',]$ymax[i]=quantile(N, 0.975)
}

# N <- sapply(fd,function(x) x$summary$all.chains['Nsuper',]) %>%
#   t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
# N$site <- "metis"
# ggplot(N,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()
# 

tmp1 <- data.frame(yr = rep(c(2004:2007, 2017:2018),2), site = c(rep('bic',6),rep('metis',6)), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)
results_all<-rbind(tmp, tmp1)

#export table with probabilities
results_all %>%
  mutate_if(is.numeric, round, digits = 2) 




# get survival from the best model  -----------------------------------------------------------

# this is the chunk to extract from gab's code with fake occasions spaced out every day.
load(here("data/mine/20211031_cmr_pup35.RData"))
years

con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/bic_cjs2_fewIT.rds")
outlist <- readRDS(con)
sapply(outlist, function(x) x$WAIC$WAIC) # double check with what's in WAIc.b2

# extract df to plot
weanSurvOut <- sapply(outlist,function(x) x$summary$all.chains['weanSurv',]) %>%
  t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
weanSurvOut$site <- "bic"
ggplot(weanSurvOut,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()

#export table with probabilities if necessary 
weanSurvOut %>%
  mutate_if(is.numeric, round, digits = 2) 



# metis: seems to be model 3
con_m <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220517/cjs3_ranef_metis.rds")
outlist_m <- readRDS(con_m)
sapply(outlist_m, function(x) x$WAIC$WAIC)

# prepare df 
weanSurvOut_m <- sapply(outlist_m,function(x) x$summary$all.chains['weanSurv',]) %>%
  t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)

weanSurvOut_m$site <- 'metis'
colnames(weanSurvOut_m)

# add NA 
tmp1 <- data.frame(Mean = NA, Median = NA,  St.Dev. = NA, CIL = NA, CIH = NA, yr = rep(c(2004:2007, 2017:2018),2), site = c(rep('bic',6),rep('metis',6)))
colnames(weanSurvOut)


# bind bic and metis
colnames(tmp1)
colnames(weanSurvOut_m)
colnames(weanSurvOut)

results_weanSurv<-rbind(weanSurvOut_m, weanSurvOut, tmp1)
#export table with probabilities
results_weanSurv %>%
  mutate_if(is.numeric, round, digits = 2) 

# 
ggplot(weanSurvOut_m,aes(x=as.factor(yr)))+
    geom_linerange(aes(ymin=CIL,ymax=CIH), position=position_dodge(w=0.75), size=0.75)+
    geom_point(aes(y=Mean),shape=18,size=4, position=position_dodge(w=0.75), size=0.75)+
    labs(x='Year',y='Pre-weaning survival')+
    scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.1)) +
    theme(panel.grid.minor = element_line(color="transparent"))+
    #scale_colour_manual(values = c("bic" = "#FFDB6D", "metis" = "#00AFBB")) + theme_cowplot(10) +
    theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')) 

# map_dfr(1:length(outlist_m),function(x) {
#   data.frame(yr=years[x],
#              y=as.numeric(unlist(outlist_m[[x]]$samples[,'weanSurv']))
#   )
# }) %>% as.data.frame() %>% ggplot(aes(x=yr,y=y))+geom_boxplot()
# 
# map_dfr(1:length(years),function(x) {
#   data.frame(yr=years[x],
#              y=as.numeric(unlist(outlist_m[[x]]$samples[,'dailySurv']))
#   )
# }) %>% as.data.frame() %>% ggplot(aes(x=yr,y=y))+geom_boxplot()



# visual convergence check CJS-------------------------------------------------------

#newOut$WAIC
#plot(newOut$samples[,'weanSurv'])
#plot(newOut$samples[,'mean.p'])

map(1:16,~ plot(outlist[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # bic
map(1:16,~ plot(outlist_m[[.x]]$samples[,'mean.phi'],main=years[[.x]])) # metis

map(1:16,~ plot(outlist[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # bic
map(1:16,~ plot(outlist_m[[.x]]$samples[,'weanSurv'],main=years[[.x]])) # metis
  
  

# extract correlations between N (JS) and Surv (CJS) from lists for bic ------------------------------------------

# for bic, seems that model 2 is best for surv
# model 3 is best for N
  
  
# extract surv as list
con_b <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/bic_cjs2_fewIT.rds")
outlist_b <- readRDS(con_b)
  
years<- c(seq(as.numeric("1998"), as.numeric("2003"), by=1), seq(as.numeric("2008"), as.numeric("2016"), by=1), 2019)

# this is the df for wean surv
weanSurvOut_b <- map_dfr(1:length(outlist_b),function(x) {
  data.frame(yr=years[x],
             weanSurv=as.numeric(unlist(outlist_b[[x]]$samples[,'weanSurv'])),
               it=1:3000
    )
  }) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# this is the summary for plotting and exporting as supp 
weanSurvOut_b_summa <-weanSurvOut_b %>% group_by(yr) %>% 
  summarise(weanSurv_mean=mean(weanSurv),
              weanSurv_cil=as.numeric(quantile(weanSurv,0.025)),
              weanSurv_c25=as.numeric(quantile(weanSurv,0.25)),
              weanSurv_c75=as.numeric(quantile(weanSurv,0.75)),
              weanSurv_cih=as.numeric(quantile(weanSurv,0.975))
              
    )
ggplot(weanSurvOut_b_summa,aes(x=as.factor(yr)))+
    geom_linerange(aes(ymin=weanSurv_cil,ymax=weanSurv_cih))+
    geom_linerange(aes(ymin=weanSurv_c25,ymax=weanSurv_c75),size=2,color='grey40')+
    geom_point(aes(y=weanSurv_mean),shape=18,size=4)+
    labs(x='Year',y='survival until weaning')
  
  # getwd()
  
# best JS is model3 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic3")
mod3.bic<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
   mod3.bic[[i]]<-readRDS(file[[i]])
}
  
N_bic.3 <- map_dfr(1:length(mod3.bic),function(x) {
   l=which(colnames(mod3.bic[[x]]$samples[[1]])=='Nsuper')
   #map_df(mod3.bic[[x]]$samples,~ .x[,l])
   data.frame(yr=years[x],
              Nsuper=  as.numeric(unlist(map(mod3.bic[[x]]$samples,~ .x[,l]))),
             it=1:3000
  )
 }) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

# sumamry 
N_bic.3_summa<-N_bic.3 %>% group_by(yr) %>% 
    summarise(Nsuper_mean=mean(Nsuper),
              Nsuper_cil=as.numeric(quantile(Nsuper,0.025)),
              Nsuper_c25=as.numeric(quantile(Nsuper,0.25)),
              Nsuper_c75=as.numeric(quantile(Nsuper,0.75)),
              Nsuper_cih=as.numeric(quantile(Nsuper,0.975))
    )
  

  ggplot(N_bic.3_summa,aes(x=as.factor(yr)))+
    geom_linerange(aes(ymin=Nsuper_cil,ymax=Nsuper_cih))+
    geom_linerange(aes(ymin=Nsuper_c25,ymax=Nsuper_c75),size=2,color='grey40')+
    geom_point(aes(y=Nsuper_mean),shape=18,size=4)+
    labs(x='Year',y='Population size')
  
  
  lbda_bic.3<- N_bic.3 %>% arrange(it,yr) %>% group_by(it) %>% 
    mutate(n_tp1=lead(Nsuper)) %>% filter(yr<2019) %>% # on pourrait enlever les ann/es crap en faisant un crap
    mutate(growth=n_tp1/Nsuper) %>% 
    left_join(weanSurvOut_b) %>% 
    as.data.frame()
 
  # lbda_bic.3 %>% #group_by(yr) %>% 
  #   summarise(lbda_mean=mean(growth),
  #             lbda_cil=as.numeric(quantile(growth,0.025)),
  #             lbda_c25=as.numeric(quantile(growth,0.25)),
  #             lbda_c75=as.numeric(quantile(growth,0.75)),
  #             lbda_cih=as.numeric(quantile(growth,0.975))
  #   )
  
  correlation_bic<- lbda_bic.3 %>% group_by(it) %>% 
    summarise(cor_sN=cor(weanSurv,Nsuper),
              cor_sgrowth=cor(weanSurv,growth))
  quantile(correlation_bic$cor_sN,probs = c(0.025,.5,0.975))
  # 2.5%          50%        97.5% 
  # -0.372452859 -0.003792789  0.426867806 
  hist(correlation_bic$cor_sN);abline(v=0,col='red')
  mean(correlation_bic$cor_sN>0)  # pseudo p-value 0.4916667
  
  quantile(correlation_bic$cor_sgrowth,probs = c(0.025,.5,0.975))
  hist(correlation_bic$cor_sgrowth);abline(v=0,col='red')
  mean(correlation_bic$cor_sgrowth>0)  # pseudo p-value
  
  
# extract correlations from best models for metis -------------------------

# model 3 CJS is best 
# model 3 JS is best since models 6 and 7 did not converge 
  
con_m <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220511/cjs3_ranef_metis.rds")
outlist_m <- readRDS(con_m)


years<- c(seq(as.numeric("1998"), as.numeric("2003"), by=1), seq(as.numeric("2008"), as.numeric("2016"), by=1), 2019)

weanSurvOut_m <- map_dfr(1:length(outlist_m),function(x) {
    data.frame(yr=years[x],
          weanSurv=as.numeric(unlist(outlist_m[[x]]$samples[,'weanSurv'])),
          it=1:3000
          )
    }) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()

weanSurvOut_m_summa <-weanSurvOut_m %>% group_by(yr) %>% 
  summarise(weanSurv_mean=mean(weanSurv),
            weanSurv_cil=as.numeric(quantile(weanSurv,0.025)),
            weanSurv_c25=as.numeric(quantile(weanSurv,0.25)),
            weanSurv_c75=as.numeric(quantile(weanSurv,0.75)),
            weanSurv_cih=as.numeric(quantile(weanSurv,0.975))
            
  )
ggplot(weanSurvOut_m_summa,aes(x=as.factor(yr)))+
  geom_linerange(aes(ymin=weanSurv_cil,ymax=weanSurv_cih))+
  geom_linerange(aes(ymin=weanSurv_c25,ymax=weanSurv_c75),size=2,color='grey40')+
  geom_point(aes(y=weanSurv_mean),shape=18,size=4)+
  labs(x='Year',y='survival until weaning')

# get abundance from JS metis
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis3")
mod3.metis<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  mod3.metis[[i]]<-readRDS(file[[i]])
}

N_metis.3 <- map_dfr(1:length(mod3.metis),function(x) {
  l=which(colnames(mod3.metis[[x]]$samples[[1]])=='Nsuper')
  #map_df(mod3.metis[[x]]$samples,~ .x[,l])
  data.frame(yr=years[x],
             Nsuper=  as.numeric(unlist(map(mod3.metis[[x]]$samples,~ .x[,l]))),
             it=1:3000
  )
}) %>% as.data.frame() #%>% ggplot(aes(x=yr,y=y))+geom_boxplot()
N_metis.3_summa<-N_metis.3 %>% group_by(yr) %>% 
  summarise(Nsuper_mean=mean(Nsuper),
            Nsuper_cil=as.numeric(quantile(Nsuper,0.025)),
            Nsuper_c25=as.numeric(quantile(Nsuper,0.25)),
            Nsuper_c75=as.numeric(quantile(Nsuper,0.75)),
            Nsuper_cih=as.numeric(quantile(Nsuper,0.975))
  )

ggplot(N_metis.3_summa,aes(x=as.factor(yr)))+
  geom_linerange(aes(ymin=Nsuper_cil,ymax=Nsuper_cih))+
  geom_linerange(aes(ymin=Nsuper_c25,ymax=Nsuper_c75),size=2,color='grey40')+
  geom_point(aes(y=Nsuper_mean),shape=18,size=4)+
  labs(x='Year',y='Population size')


lbda_metis.3<- N_metis.3 %>% arrange(it,yr) %>% group_by(it) %>% 
  mutate(n_tp1=lead(Nsuper)) %>% filter(yr<2019) %>% # on pourrait enlever les ann/es crap en faisant un crap
  mutate(growth=n_tp1/Nsuper) %>% 
  left_join(weanSurvOut_m) %>% 
  as.data.frame()

correlation_Metis<- lbda_metis.3 %>% group_by(it) %>% 
  summarise(cor_sN=cor(weanSurv,Nsuper),
            cor_sgrowth=cor(weanSurv,growth))
quantile(correlation_Metis$cor_sN,probs = c(0.025,.5,0.975))
# 2.5%        50%      97.5% 
# -0.5194995 -0.1353982  0.3595358 
hist(correlation_Metis$cor_sN);abline(v=0,col='red')
mean(correlation_Metis$cor_sN>0)  # pseudo p-value 0.2903333

quantile(correlation_Metis$cor_sgrowth,probs = c(0.025,.5,0.975))
# 2.5%        50%      97.5% 
# -0.4441526 -0.1190146  0.4316046 
hist(correlation_Metis$cor_sgrowth);abline(v=0,col='red')
mean(correlation_Metis$cor_sgrowth>0)  # pseudo p-value 0.3186667













# Supp Online Material  ---------------------------------------------------

# extract and plot p per sampling occasion BIC from the Jolly Seber -------------------------------
# get date from original data 
load("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/data/mine/20211031_cmr_pup35.RData")
data<-bicData35.

for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]]) # make sure dates are arranged
}

# extract coda sample from good model 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/bic/20210304/bic3")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # transform to coda # pour le summary coda 
}


# ATTENTION IL FAUT AJOUTER LES BONS P SELON LE MODÈLE. PAS DE PSI, PAS DE RANEF.P, ETC.
tmp <- list()
tmp1 <- list()
tmp2 <- list()
for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]])
  tmp[[t]] <-summary(codaSamp[[t]][,grepl("^p", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi
  tmp1[[t]]<-summary(codaSamp[[t]][,grepl("^p", colnames(codaSamp[[t]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
  tmp2[[t]]<-data.frame(days, tmp[[t]][1:length(days),1], tmp1[[t]][1:length(days),]) # knowing length day s= 10
  names(tmp2[[t]])<-c("date", "mean", "low", "high")
  tmp2[[t]]$year <- as.numeric(year(ymd(tmp2[[t]]$date))) # needs lubridate
}


p_bic <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + 
              geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
              ylab("Capture probability") + xlab("Date") +  ggtitle(paste("Year ", d$year, sep=""))+ 
              scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2)) + 
              #  theme_cowplot()+
              theme(axis.line = element_line(),
                    panel.grid.minor = element_line(color="transparent"),
                    panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')))

# # exporter tous les graphs
# pdf("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/2022-05-13_pPerOccasionsBic.pdf")
# do.call("grid.arrange", p_bic)
# dev.off()


# show only year 2019 (n=56 p. recapt of 0.052) and 1999 n=37, p=0.811)\
p_panel <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + 
                  geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
                  ylab("Capture probability") + xlab("Date") +  # ggtitle(paste("Year ", d$year, sep=""))+ 
                  scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2)) +
                  theme_cowplot()+
                  theme(axis.line = element_line(),
                        panel.grid.minor = element_line(color="transparent"),
                        panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')))

cowplot::plot_grid(p_panel[[16]],p_panel[[2]], labels=c('a)','b)'))

# ggsave("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/FIG_2panel_2022-05-13_pPerOccasionsBic.pdf",
#        height= 10, # max height of 23.7 cm
#        width = 18.2,
#        unit= "cm",
#        dpi = 300)#





# b - recruitment from JS model per occasions -----------------------------


# state process + observation process. the probability that a member of Ns enters the pop at occasion t is bt and is called the entry probability. 
# it is the probability than an indvidual is new in the population and has entered the population since the preceding occasion.
# entry coul result either from in situ recruitment (locally born individuals) or from immigration. 
# THI SIS NOT A RECUITMENT PROBABILITY. THE NUMBER OF IND ENTEREING THE POPULATION AT T IS BT=NSBT. THE FRACTION OF INDIVIDUALS ALREAD PRESENT AT TEH FIRST OCCASION IS B1 = THE ENTRY PROB HAS NO CLEAR ECOLOGICAL MEANING BECUASE IT IS A COMPLEX FUNCTION OF ALL ENTRIES BEFORE THE FIRST OCCASION. ALL ENTRY PROB MUST SUM TO 1 to ensure tha ll Ns individuals enter the population sometime during teh studyl 
# the superpop model uses entry probabilities b and an inclusion parameter psi. b are analogous to removal entry prob but not calculated the same way 
# the dirichlet prior is used. 
# allocate the entries of al indivdiuals uniformly over T occasions, alpha of 1 for all t. 
tmp <- list()
tmp1 <- list()
tmp2 <- list()

for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]])
  tmp[[t]] <-summary(codaSamp[[t]][,grepl("^b", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi
  tmp1[[t]]<-summary(codaSamp[[t]][,grepl("^b", colnames(codaSamp[[t]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
  tmp2[[t]]<-data.frame(days, tmp[[t]][1:length(days),1], tmp1[[t]][1:length(days),]) # knowing length day s= 10
  names(tmp2[[t]])<-c("date", "mean", "low", "high")
  tmp2[[t]]$year <- as.numeric(year(ymd(tmp2[[t]]$date))) # needs lubridate
  tmp2[[t]]$sum=sum(tmp2[[t]]$mean)# pent should sum to 1
}


b_bic <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + 
              geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
              ylab("Entry probability") + xlab("Date")+ ggtitle(paste("Year ", d$year, sep=""))+
              scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2))+
              theme(axis.line = element_line()))

# pdf("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/2022-05-13_bPerOccasion_bic.pdf", width = "8", height="8")
# do.call("grid.arrange", b_bic)
# dev.off()




# phi or daily surv from CJS per occasion at Bic ----------------------------------------------
con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/bic_cjs2_fewIT.rds")
outlist <- readRDS(con)

# this extracts daily surv at each iteration and plot them

sapply(outlist,function(x) x$summary$all.chains['dailySurv',]) 

map_dfr(1:length(years),function(x) {
  data.frame(yr=years[x],
             y=as.numeric(unlist(outlist[[x]]$samples[,'dailySurv']))
  )
}) %>% as.data.frame() %>% ggplot(aes(x=yr,y=y))+geom_boxplot()



map_dfr(1:length(years),function(x) {
    data.frame(yr=years[x],
               y=as.numeric(unlist(outlist[[x]]$samples[,'dailySurv']))
    )
}) %>% as.data.frame() %>% ggplot(aes(x=yr,y=y))+geom_boxplot()



# this creates a dataframe of daily surv PER year
dailySurv_bic <- sapply(outlist,function(x) x$summary$all.chains['dailySurv',]) %>% 
  t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
ggplot(dailySurv_bic,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()



# birthdates at bic per year from the CJS ----------------------------------------------
sapply(outlist,function(x) x$summary$all.chains['mu.bd',]) 
meanBd_bic <- sapply(outlist,function(x) x$summary$all.chains['mu.bd',]) %>% 
  t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)


# graph it?
ggplot(meanBd_bic,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()













# mean birthdate - Metis -----------------------------------------------------
con_m <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220511/cjs3_ranef_metis.rds")
outlist_m <- readRDS(con_m)

sapply(outlist_m,function(x) x$summary$all.chains['mu.bd',]) 

meanBd_metis <- sapply(outlist_m,function(x) x$summary$all.chains['mu.bd',]) %>% 
  t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)

# with mean
ggplot(meanBd_metis,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange(color = 'blue') + 
  geom_pointrange(data=meanBd_bic,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH), color='green')

# with median
ggplot(meanBd_metis,aes(x=yr,y=Median,ymin=CIL,ymax=CIH))+geom_pointrange(color = 'blue') + 
  geom_pointrange(data=meanBd_bic,aes(x=yr,y=Median,ymin=CIL,ymax=CIH), color='green')




















# p - Metis -------------------------------
# get date from original data 
data<-metisData35.

for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]]) # make sure dates are arranged
}

# extract coda sample from good model 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/js/metis/20210304/metis3")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
  fd[[i]]<-readRDS(file[[i]])
}

codaSamp<-list()
for(i in 1:length(fd)){
  codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list() # transform to coda # pour le summary coda 
}


# start binding things 
tmp <- list()
tmp1 <- list()
tmp2 <- list()
for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]])
  tmp[[t]] <-summary(codaSamp[[t]][,grepl("^p", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi
  tmp1[[t]]<-summary(codaSamp[[t]][,grepl("^p", colnames(codaSamp[[t]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
  tmp2[[t]]<-data.frame(days, tmp[[t]][1:length(days),1], tmp1[[t]][1:length(days),]) # knowing length day s= 10
  names(tmp2[[t]])<-c("date", "mean", "low", "high")
  tmp2[[t]]$year <- as.numeric(year(ymd(tmp2[[t]]$date))) # needs lubridate
}

p_metis <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + 
                    geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
                    ylab("Capture probability") + xlab("Date") +  ggtitle(paste("Year ", d$year, sep=""))+ 
                    scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.2)) + 
                    # theme_cowplot()+
                    theme(axis.line = element_line(),
                          panel.grid.minor = element_line(color="transparent"),
                          panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')))

# exporter tous les graphs
# pdf("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/2022-05-13_pPerOccasions_Metis.pdf")
# library(gridExtra)
# do.call("grid.arrange", p_metis)
# dev.off()



# b - metis ---------------------------------------------------------------
# get date from original data 
tmp <- list()
tmp1 <- list()
tmp2 <- list()

for (t in 1:length(years)) { # takes a while
  days<-unique(data$date[data$year==years[t]])
  tmp[[t]] <-summary(codaSamp[[t]][,grepl("^b", colnames(codaSamp[[t]][[1]]))])[[1]][,1:2] #tu commences avec les charactères “p", rien devant - ceci inclut psi
  tmp1[[t]]<-summary(codaSamp[[t]][,grepl("^b", colnames(codaSamp[[t]][[1]]))])[[2]][,c(1,5)] # en mettre min 2 pour savoir c quoi 
  tmp2[[t]]<-data.frame(days, tmp[[t]][1:length(days),1], tmp1[[t]][1:length(days),]) # knowing length day s= 10
  names(tmp2[[t]])<-c("date", "mean", "low", "high")
  tmp2[[t]]$year <- as.numeric(year(ymd(tmp2[[t]]$date))) # needs lubridate
  tmp2[[t]]$sum=sum(tmp2[[t]]$mean)# pent should sum to 1
}


b_metis <- lapply(tmp2, function(d) ggplot(data = d, aes(x=date, y=mean)) + 
                  geom_pointrange(data = d, aes(ymin=low, ymax=high), size = 0.3) + #geom_smooth(se=F) + 
                  ylab("Entry probability") + xlab("Date")+ ggtitle(paste("Year ", d$year, sep=""))+
                  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2))+
                  theme(axis.line = element_line()))
# 
# pdf("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/graph/2022-05-13_bPerOccasion_metis.pdf", width = "8", height="8")
# do.call("grid.arrange", b_metis)
# dev.off()








# save abundance and surv  ----------------------------------------

# remove all raw objects that are no longer useful 
rm(codaSamp, fd, data, tmp, tmp1, tmp2)

# save(list = ls(),
# file = "/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/2022-05-13_resultsCMR.RData")











# 
# # this is the chunk to extract from gab's code with fake occasions spaced out every day.
# load(here("data/mine/20211031_cmr_pup35.RData"))
# years
# 
# con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/bic_cjs2_fewIT.rds")
# outlist <- readRDS(con)
# sapply(outlist, function(x) x$WAIC$WAIC) # double check with what's in WAIc.b2
# 
# # extract df to plot
# weanSurvOut <- sapply(outlist,function(x) x$summary$all.chains['weanSurv',]) %>%
#   t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
# weanSurvOut$site <- "bic"
# ggplot(weanSurvOut,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()
# 
# #export table with probabilities if necessary 
# weanSurvOut %>%
#   mutate_if(is.numeric, round, digits = 2) 
# 
# 
# 
# # metis: seems to be model 3
# con_m <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/metis/20220511/cjs3_ranef_metis.rds")
# outlist_m <- readRDS(con_m)
# sapply(outlist_m, function(x) x$WAIC$WAIC)
# 
# # prepare df 
# weanSurvOut_m <- sapply(outlist_m,function(x) x$summary$all.chains['weanSurv',]) %>%
#   t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
# 
# weanSurvOut_m$site <- 'metis'
# colnames(weanSurvOut_m)
# colnames(results_weanSurv)
# 
# 





# # this is the chunk to extract from gab's original code with fake occasions spaced out every day.
# load(here("data/mine/20211031_cmr_pup35.RData"))
# years
# 
# con <- gzfile("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output.nosync/data/cjs/bic/20220422/bic_cjs2_fewIT.rds")
# outlist <- readRDS(con)
# sapply(outlist, function(x) x$WAIC$WAIC) # double check with what's in WAIc.b2
# 
# # extract df to plot
# weanSurvOut <- sapply(outlist,function(x) x$summary$all.chains['weanSurv',]) %>%
#   t() %>% as.data.frame() %>% rename(CIL=`95%CI_low`,CIH=`95%CI_upp`) %>% mutate(yr=years)
# weanSurvOut$site <- "bic"
# ggplot(weanSurvOut,aes(x=yr,y=Mean,ymin=CIL,ymax=CIH))+geom_pointrange()
# 
# #export table with probabilities
# weanSurvOut %>%
#   mutate_if(is.numeric, round, digits = 2) 
# 
# # for supp mat - all iterations
# map_dfr(1:length(outlist),function(x) {
#   data.frame(yr=years[x],
#              y=as.numeric(unlist(outlist[[x]]$samples[,'weanSurv']))
#   )
# }) %>% as.data.frame() %>% ggplot(aes(x=yr,y=y))+geom_boxplot()
# 
# # map_dfr(1:length(years),function(x) {
# #   data.frame(yr=years[x],
# #              y=as.numeric(unlist(outlist[[x]]$samples[,'dailySurv']))
# #   )
# # }) %>% as.data.frame() %>% ggplot(aes(x=yr,y=y))+geom_boxplot()
# 
# # extract birthdate 
# map_dfr(1:length(years),function(x) {
#   data.frame(yr=years[x],
#              y=as.numeric(unlist(outlist[[x ]]$samples[,'bDate']))
#   )
# }) %>% as.data.frame()

