# script for extracting clean outputs for ms 
library(ggplot2)
library(ggpubr)
library(coda)
library(tidyverse)
# extract survival from cjs
rm(list = ls())

#my custom theme  
mytheme <- theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position = "none",
                 axis.text=element_text(size=12, color="white"),
                 axis.title=element_text(size=14, color="white"),
                 axis.ticks.x =element_line(color="white"),
                 axis.ticks.y = element_line(color="white"),
                 rect = element_rect(fill = "white"),
                 axis.line = element_line(color="white"),
                 panel.grid.major = element_line(color="white"),
                 panel.grid.minor = element_line(color="transparent"),
                 panel.background = element_rect(fill = "transparent",colour = NA),
                 plot.background = element_rect(fill = "transparent",colour = NA))
background_grid(
    major = c("xy", "x", "y", "only_minor", "none"),
    minor = c("none", "xy", "x", "y"),
    size.major = 0.5,
    size.minor = 0.2,
    color.major = "grey85",
    color.minor = "grey85"
)
# prepare df to graph
tmp <- data.frame(yr = rep(c(1998:2003, 2008:2016, 2019),2), site = c(rep('bic',16),rep('metis',16)), N = NA, ymin = NA, ymax = NA, 
                  surv = NA, min.surv = NA, max.surv = NA)


# locate right model folder 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output/data/outputs.tmp.nosync/bic")

# extract the list of all files in that folder 

fd<-list()
file <-list.files(pattern = "rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
}

# explore what's in there
fd[[1]]$summary$all.chains # that gives you the summary for all chains, all iterations
fd[[1]]$samples$chain1
sample_df <- fd[[1]]$samples %>% map(as.data.frame) %>% bind_rows()
#weanSurv <- nimble::ilogit(sample_df[,grepl('wean.surv',colnames(sample_df))])
weanSurv <- sample_df[,grepl('wean.surv',colnames(sample_df))]
mean(weanSurv) # 0.6802454
var(weanSurv) # 0.001657796
quantile(weanSurv, 0.025)
quantile(weanSurv, 0.975)


for(i in 1:length(fd)){
    sample_df=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() 
    # mean.phi+ranef.phi[t] to add 
    #weanSurv=nimble::ilogit(sample_df[,grepl('wean.surv',colnames(sample_df))]) # un seul N, sur la bonne échelle
    #weanSurv=sample_df[,grepl('wean.surv',colnames(sample_df))] 
    weanSurv= nimble::ilogit(sample_df[,grepl('mean.phi',colnames(sample_df))])
    #weanSurv=sample_df[,grepl('wean.surv',colnames(sample_df))] # un seul N, sur la bonne échelle
    tmp[tmp$site=='bic',]$surv[i]=mean(weanSurv)
    tmp[tmp$site=='bic',]$min.surv[i]= quantile(weanSurv, 0.025)
    tmp[tmp$site=='bic',]$max.surv[i]=quantile(weanSurv, 0.975)
}

# for(i in 1:length(fd)){
#     ttt=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() 
#     # mean.phi+ranef.phi[t] to add 
#     tttn=ttt[,grepl('Nsuper',colnames(ttt))] # un seul N, sur la bonne échelle
#     tmp$N[i]=mean(tttn)
#     tmp$ymin[i]= quantile(tttn, 0.025)
#     tmp$ymax[i]=quantile(tttn, 0.975)
# }


setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output/data/outputs.tmp.nosync/metis/20220211")

# extract the list of all files in that folder 
fd<-list()
file <-list.files(pattern = "rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
}

#explore what's in there
# fd[[1]]$summary$all.chains # that gives you the summary for all chains, all iterations
# fd[[1]]$samples$chain1
# sample_df <- fd[[1]]$samples %>% map(as.data.frame) %>% bind_rows()
# weanSurv <- nimble::ilogit(sample_df[,grepl('wean.surv',colnames(sample_df))])
# weanSurv <- nimble::ilogit(sample_df[,grepl('mean.phi',colnames(sample_df))])
# mean(weanSurv) # 0.6802454
# var(weanSurv) # 0.001657796
# quantile(weanSurv, 0.025)
# quantile(weanSurv, 0.975)


# prepare df to export and graph 
for(i in 1:length(fd)){
    sample_df=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() 
    # mean.phi+ranef.phi[t] to add 
    #weanSurv=nimble::ilogit(sample_df[,grepl('wean.surv',colnames(sample_df))])
    #weanSurv=sample_df[,grepl('wean.surv',colnames(sample_df))] 
    weanSurv= nimble::ilogit(sample_df[,grepl('mean.phi',colnames(sample_df))])
    tmp[tmp$site=='metis',]$surv[i]=mean(weanSurv)
    tmp[tmp$site=='metis',]$min.surv[i]= quantile(weanSurv, 0.025)
    tmp[tmp$site=='metis',]$max.surv[i]=quantile(weanSurv, 0.975)
}

tmp1 <- data.frame(yr = c(2004:2007, 2017:2018), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)
results_M<-rbind(tmp, tmp1)
levels(factor(results_M$yr))
results_M$yr <- as.numeric(as.character(results_M$yr))


tmp1 <- data.frame(yr = rep(c(2004:2007, 2017:2018),2), site = c(rep('bic',6),rep('metis',6)), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)
results_all<-rbind(tmp, tmp1)

#export table with probabilities
results_all %>%
  mutate_if(is.numeric, round, digits = 3) %>%
 # write.csv(file = "/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal/output/data/modelCJS_randomYRID.csv", row.names = FALSE)

# graph both site next to each other
results_all %>% 
    ggplot(aes(x=yr, y=surv, ymin=min.surv, ymax=max.surv, color=site)) +
    geom_pointrange(position=position_dodge(w=0.5), size = 1) +
    labs(x=expression('Year')) + 
    labs(y="Preweaning survival") +  
    scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.1)) +
    scale_x_continuous(breaks = seq(from = 1998, to = 2019, by = 1)) +
    theme(panel.grid.minor = element_line(color="transparent"))+
    scale_colour_manual(values = c("bic" = "#FFDB6D", "metis" = "#00AFBB")) + cowplot::theme_cowplot()+
  theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'))
#ggsave('meanPhi_randomYRID.pdf')


# Delta method ------------------------------------------------------------
# Le package pour appliquer la delta méthode, c’est « emdbook » et la fonction précise c’est « deltavar ».
# Dans l’exemple ci-dessous, il est utilisé pour calculer la variance issue d’une moyenne de plusieurs distributions

library(emdbook)
test <- data.frame(a=0.05, b=0.03, c=0.03, d=0.05, a_var = 0.000823, b_var = 0.00011, c_var = 0.000157, d_var = 0.001535)
deltavar((a+b+c+d)/4, meanval=test[1:4], Sigma=test[5:8])


# me testint 
deltavar((a*b), meanval=test[1:4], Sigma=test[5:8])
deltavar((a^10), meanval=test[1:4], Sigma=test[5:8])



# powell 
# 900 * var(SD) * SD^58
# Surv : 1.62599732 
# st dev : 0.1752286
# need to take variance, not standard deviation 
# from the sample, take the ilogit of all iterations (for daily survival). 
# Take the mean of that and it's var (NOT the stadard deviation sd)
# 900 * 0.1752286 * (0.08330191^58) #3.943076e-61
# 0.08330191-(1.96*3.943076e-61)
# 0.08330191+(1.96*3.943076e-61)

# do i take the mean per id or total?
# is pho really the daily surv??

# the first number is 30x30 (for a monthly estimation), the other is (30-1)*2 = 58

# so for estimating 24 days of weaning, we get: 
#576*dailySurv*var^46

sample_df=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() 
# mean.phi+ranef.phi[t] to add 
#weanSurv=nimble::ilogit(sample_df[,grepl('wean.surv',colnames(sample_df))])
#weanSurv=sample_df[,grepl('wean.surv',colnames(sample_df))] 
weanSurv= nimble::ilogit(sample_df[,grepl('mean.phi',colnames(sample_df))])
tmp[tmp$site=='metis',]$surv[i]=mean(weanSurv) # moyenne et quantiles pour les iterations car j'ai juste un parametre. 
# pas la bonne affaire pour extraire tous les pho. 
tmp[tmp$site=='metis',]$min.surv[i]= quantile(weanSurv, 0.025)
tmp[tmp$site=='metis',]$max.surv[i]=quantile(weanSurv, 0.975)





