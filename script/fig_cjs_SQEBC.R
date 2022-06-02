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



# here 
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20211101/bic")
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20211031")

fd<-list()
file <-list.files(pattern = "rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
}
# codaSamp<-list()
# for(i in 1:length(fd)){
#     codaSamp[[i]] <- fd[[i]]$samples %>% map(~as.mcmc(.x)) %>% as.mcmc.list()                    # transform to coda
#    plot(codaSamp[[i]][,'mean.phi'], main = paste0("mean.phi_y_", substr(file, 13, 16))[[i]]) # plot chain # my last year did not converge well
#    # plot(codaSamp[[i]][,'wean.surv[1]'], main = paste0("wean.surv_y_", substr(file, 13, 16))[[i]]) # plot chain # my last year did not converge well
# }


tmp <- data.frame(yr = c(1998:2003, 2008:2016, 2019), surv = NA, CI_low = NA, CI_upp = NA)
tmp$id <- c(38,37,41,46,54,29,53,79,66,87,75,72,64,105,72,58)

for(i in 1:length(fd)){
    tmp$surv[i]=fd[[i]]$summary$all.chains["wean.surv[1]", ]
    tmp$CI_low[i]=fd[[i]]$summary$all.chains["wean.surv[1]", "95%CI_low"]
    tmp$CI_upp[i]=fd[[i]]$summary$all.chains["wean.surv[1]", "95%CI_upp"]
}
fd[[i]]$summary$all.chains["betaWeaned", "Median"]


#tmp[,2:4][i]=fd[[i]]$summary$all.chains["wean.surv[1]", c("Median", "95%CI_low", "95%CI_upp")]
tmp1 <- data.frame(yr = c(2004:2007, 2017:2018), surv = NA, CI_low = NA, CI_upp = NA, id=NA)# p = NA, min.p=NA, max.p=NA
results_B<-rbind(tmp, tmp1)
# results_B$wean_surv <- results_B$surv^30
# results_B$wean_lci<- results_B$CI_low^30
# results_B$wean_ici<- results_B$CI_upp^30

id <- list()
for (i in 1:length(years)){
id[i] <- dim(list_ch_bic[[i]])[1] 
}

p1=ggplot(results_B, aes(x = yr, y = surv)) + 
    geom_line(color="grey") +
    geom_pointrange(data = results_B, aes(x = yr, y = surv, ymin=CI_low, ymax=CI_upp),
                    shape=21, color="grey", fill="#FFDB6D", size = 1.5) + 
    labs(x=expression('Year')) + 
    labs(y="Preweaning survival") +
    mytheme+
    scale_x_continuous(limits=c(1998, 2019)) +
        geom_text(aes(label=id), position=position_dodge(width=0.5), vjust=-1, colour = 'white')


# ggsave("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Graphs/fig_sqebc_bic.png",
#        height= 14, # max height of 23.7 cm
#        width = 18.2,
#        unit= "cm",
#        dpi = 300)#



setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20211101/metis")

fd<-list()
file <-list.files(pattern = "rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
}
id <- list()
for (i in 1:length(years)){
    id[i] <- dim(list_ch_metis[[i]])[1] 
}

tmp <- data.frame(yr = c(1998:2003, 2008:2016, 2019), surv = NA, CI_low = NA, CI_upp = NA)
tmp$id <- c(21,24,32,20,25,30,27,30,24,22,33,6,27,31,28,18)
for(i in 1:length(fd)){
    tmp$surv[i]=fd[[i]]$summary$all.chains["wean.surv[1]", "Median"]
    tmp$CI_low[i]=fd[[i]]$summary$all.chains["wean.surv[1]", "95%CI_low"]
    tmp$CI_upp[i]=fd[[i]]$summary$all.chains["wean.surv[1]", "95%CI_upp"]
}
#tmp[,2:4][i]=fd[[i]]$summary$all.chains["wean.surv[1]", c("Median", "95%CI_low", "95%CI_upp")]
tmp1 <- data.frame(yr = c(2004:2007, 2017:2018), surv = NA, CI_low = NA, CI_upp = NA, id=NA)# p = NA, min.p=NA, max.p=NA
results_M<-rbind(tmp, tmp1)
p2=ggplot(results_M, aes(x = yr, y = surv)) + 
    geom_line(color="grey") +
    geom_pointrange(data = results_M, aes(x = yr, y = surv, ymin=CI_low, ymax=CI_upp),
                    shape=21, color="grey", fill="#00AFBB", size = 1.5) + 
    labs(x=expression('Year')) + 
    labs(y="Preweaning survival") +
    scale_x_continuous(limits=c(1998, 2019)) +
   mytheme +
geom_text(aes(label=id), position=position_dodge(width=0.5), vjust=-1, colour = 'white')

#ggarrange(p1, p2, ncol = 2, labels = c("Bic","Metis"))
# #cowplot::plot_grid(p1, p2, ncol=2, labels = c("Bic","Metis"))
# ggsave("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Graphs/fig_sqebc_m.png",
#        height= 14, # max height of 23.7 cm
#        width = 18.2,
#        unit= "cm",
#        dpi = 300)#

ggplot(results_M, aes(x = yr, y = surv)) + 
    geom_line(color="grey") +
    geom_point(data = results_M, aes(x = yr, y = surv),
                    shape=21, color="grey", fill="#00AFBB", size = 6, alpha = 0.8) + 
   geom_line(data = results_B, color="grey") +
    geom_point(data = results_B, aes(x = yr, y = surv),
                    shape=21, color="grey", fill="#FFDB6D", size = 6, alpha=0.8) + 
    labs(x=expression('Year')) + 
    labs(y="Preweaning survival") +
    scale_y_continuous(limits = c(0,1),breaks = seq(from = 0, to = 1, by = 0.25)) + 
    mytheme

# ggsave("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Graphs/fig_sqebc_surv_both.png",
#        height= 14, # max height of 23.7 cm
#        width = 18.2,
#        unit= "cm",
#        dpi = 300)#


p1
p2

# abundance ---------------------------------------------------------------
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210304/bic3")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
}

tmp <- data.frame(yr = c(1998:2003, 2008:2016, 2019), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)

for(i in 1:length(fd)){
    ttt=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() # instead of making a mcmc list # pas besoin de (i) parce quenregistre au fur et à mesure
    # mean.phi+ranef.phi[t] to add 
    tttn=ttt[,grepl('Nsuper',colnames(ttt))] # un seul N, sur la bonne échelle
    tmp$N[i]=mean(tttn)
    tmp$ymin[i]= quantile(tttn, 0.025)
    tmp$ymax[i]=quantile(tttn, 0.975)
 }
tmp1 <- data.frame(yr = c(2004:2007, 2017:2018), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)
resultsBic<-rbind(tmp, tmp1)
levels(factor(resultsBic$yr))
resultsBic$yr <- as.numeric(as.character(resultsBic$yr))

# figures for presentation 
ggplot(resultsBic, aes(x = yr, y = N)) + 
    geom_line(color="grey") +
    geom_pointrange(data = resultsBic, aes(x = yr, y = N, ymin=ymin, ymax=ymax),
                    shape=21, color="grey", fill="lightgray", size = 1.5) + 
    labs(x=expression('Year')) + 
    labs(y="Number of individuals (N)") +
    scale_y_continuous(limits = c(0,401),breaks = seq(from = 0, to = 401, by = 100)) + 
mytheme
# ggsave("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Graphs/fig_sqebc_b_nsuper.png", 
#        height= 14, # max height of 23.7 cm
#        width = 18.2, 
#        unit= "cm", 
#        dpi = 300)#
# 


# here from model 3 metis
setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Outputs.tmp/20210304/metis3")

fd<-list()
file <-list.files(pattern = ".rds")
for(i in seq_along(file)){
    fd[[i]]<-readRDS(file[[i]])
}

tmp <- data.frame(yr = c(1998:2003, 2008:2016, 2019), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)

for(i in 1:length(fd)){
    ttt=fd[[i]]$samples %>% map(as.data.frame) %>% bind_rows() # instead of making a mcmc list # pas besoin de (i) parce quenregistre au fur et à mesure
    tttn=ttt[,grepl('Nsuper',colnames(ttt))] # un seul N, sur la bonne échelle
    tmp$N[i]=mean(tttn)
    tmp$ymin[i]= quantile(tttn, 0.025)
    tmp$ymax[i]=quantile(tttn, 0.975)
   }

# plots Na nd surv Metis

colnames(tmp)
tmp1 <- data.frame(yr = c(2004:2007, 2017:2018), N = NA, ymin = NA, ymax = NA, surv = NA, min.surv = NA, max.surv = NA)# p = NA, min.p=NA, max.p=NA
resultsMetis<-rbind(tmp, tmp1)

ggplot(resultsMetis, aes(x = yr, y = N)) + 
    geom_line(color="grey") +
    geom_pointrange(data = resultsMetis, aes(x = yr, y = N, ymin=ymin, ymax=ymax),
                    shape=21, color="grey", fill="steelblue", size = 1.5) + 
    labs(x=expression('Year')) + 
    labs(y="Number of individuals (N)") +
    scale_y_continuous(limits = c(0,401),breaks = seq(from = 0, to = 401, by = 100)) + 
    mytheme
# ggsave("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Graphs/fig_sqebc_m_nsuper.png", 
#        height= 14, # max height of 23.7 cm
#        width = 18.2, 
#        unit= "cm", 
#        dpi = 300)#
ggplot(resultsMetis, aes(x = yr, y = N)) + 
    geom_pointrange(data = resultsMetis, aes(x = yr, y = N, ymin=ymin, ymax=ymax),
                    shape=21, color="black", fill="steelblue", size = 1.5) + 
    geom_pointrange(data = resultsBic, aes(x = yr, y = N, ymin=ymin, ymax=ymax),
                    shape=21, color="black", fill="lightgray", size = 1.5) + 
    labs(x=expression('Year')) + 
    labs(y="Number of individuals (N)") +
    scale_y_continuous(limits = c(0,401),breaks = seq(from = 0, to = 401, by = 100))# + 
    #mytheme
# ggsave("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Graphs/fig_sqebc_both_nsuper.png", 
#        height= 14, # max height of 23.7 cm
#        width = 18.2, 
#        unit= "cm", 
#        dpi = 300)#

ggsave("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Graphs/fig_sqebc_both_nsuper.pdf", 
       height= 14, # max height of 23.7 cm
       width = 18.2, 
       unit= "cm", 
       dpi = 300)#
# grey seal ---------------------------------------------------------------

nSeal_df <- read.csv("~/Downloads/doi_10/EAP18-0326.R2_Model_input_data.csv")

coeff <- 10

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- "steelblue"

ggplot(nSeal_df, aes(x=year)) +
    
    geom_line( aes(y=Iadt), color = "grey") + 
    geom_line( aes(y=nSeal / coeff), color = "grey") + # Divide by 10 to get the same range than the temperature
    geom_point( aes(x = year, y = Iadt),
               shape=21, color="grey", fill="#69b3a2", size = 4) + 
    geom_point(aes(x = year, y = nSeal / coeff),
               shape=21, color="grey", fill="steelblue", size = 4) + 
    
    scale_y_continuous(
        
        # Features of the first axis
        name = "Number of adult skates",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Number of grey seals")
    ) + 
theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
) + mytheme


ggsave("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Graphs/fig_sqebc_greyseal.png", 
       height= 14, # max height of 23.7 cm
       width = 18.2, 
       unit= "cm", 
       dpi = 300)#



# Delta method ------------------------------------------------------------
# 900 * var(SD) * SD^58
# Surv : 1.62599732 
# st dev : 0.1752286

# need to take variance, not standard deviation 
# from the sample, take the ilogit of all iteerations. Take the mean of that and it's var (NOT the stadard deviation sd)
900 * 0.1752286 * (0.08330191^58) #3.943076e-61
0.08330191-(1.96*3.943076e-61)
0.08330191+(1.96*3.943076e-61)


