# script for putting seal data altogether  - generates dataList and databySite objects
#changes in data frame Jan 30 2021 ---------------------------------------
# 

library(tidyverse)
library(lubridate)
library(stringr)
library(magrittr)


# modified 2022-08-22 to return to both sites within the same dataframe and rerun model selection 
# # Umbilicus degenaration stage - according to PhoquesCommuns_2019 (raw data)
# 1 = long, red, moist with possible traces of blood; 
# 2 = long and fresh without blood; 
# 3 = medium length and white; 
# 4 = small and dry; 
# 5 = umbilicus loss

# setwd("/Users/LimoilouARenaud/Documents/PostDocI/Projects/cmrSeal")

# directories need to be changed 


rm(list = ls())


# 1998-2003 - no ombilicus recorded?

pvData03 <- read.csv2("data/mine/CaptureData_Pv1998-2003.csv", na.strings = c("", "?", "-", "NA"))
pvData03$site # arrange site name later 

# arrange date  
pvData03$date <-ymd(pvData03$date) 
pvData03$year <-as.numeric(year(ymd(pvData03$date)))

# nlevels(pvData03$year) # 6
# levels(pvData03$year) 

pvData03$fliptag <- as.factor(as.character(pvData03$fliptag))
nlevels(pvData03$fliptag) # 398

pvData03 = pvData03[!is.na(pvData03$headtag),] # removes 3 ID - n  = 760
pvData03 = pvData03[!is.na(pvData03$fliptag),]

summary(pvData03)

# tidy table  # no umbilicus stage in this df
df1= tibble(date = pvData03$date, myID=as.factor(pvData03$fliptag), year = pvData03$year, sex = as.factor(pvData03$sex), site = pvData03$site, mass = pvData03$weight, bdate = NA)

df1=df1[!is.na(df1$myID),]
df1 = df1%>% arrange(date) # n= 760 observations

df1<-df1%>%group_by(myID)%>%
  droplevels() 
nlevels(unique(df1$myID))# n = 396 # some miss letters. who cares as long as there are unique ID. # now 397 with doublons corrected as of 2022 11 08

df1$mass<-as.character(as.factor(df1$mass)) # for now





# capture data 2008-2011
df2 <- read.csv2("data/mine/CaptureData_Pv2008-2011.csv", na.strings = c("", "?", "-", "NA")) # SEMBLE Y AVOIR UN FILTRE SUR LA DATE EFFECTUÉ ICI
df2$site
df2=df2[df2$site %in% c("metis", "bic", "Métis", "Bic"),]


# proportion with ombilicus stage 1 
table(df2$ombilicusstage)
# 1   2   3   4   5 
# 48 190 121  87 274 

prop.table(table(df2$ombilicusstage))
# 1          2          3          4          5 
# 0.06666667 0.26388889 0.16805556 0.12083333 0.38055556 


# arrange date
df2$date <-ymd(df2$date)
df2$year <-as.numeric(year(ymd(df2$date)))
sort(unique(df2$date), decreasing = T) # arrête en juillet

# IDs
df2 = df2[!is.na(df2$fliptag),] # n = 760
table(df2$fliptag)


# determine age # added oct 25 2021
df2$bdate <- NA

df2 <- df2 %>% mutate(bdate = case_when(
  ombilicusstage == "1" ~ date-1, 
))

df2= tibble(date = df2$date, myID=as.factor(df2$fliptag), year = df2$year, sex = as.factor(df2$sex), site = df2$site, mass= df2$weight, bdate = df2$bdate)
df2=df2[!is.na(df2$myID),]

df2 = df2%>% arrange(date) 
df2<-df2%>%group_by(myID)%>%
  droplevels() 
nlevels(unique(df2$myID))# n = 388

df2$mass<-as.character(as.factor(df2$mass))



# format 2019
df3 <- read.csv2("data/mine/myData2019.csv", na.strings = c("", "?", "-", "NA")) #
df3$site

# arrange date  
df3$date <-ymd(df3$date) 
df3$year <-as.numeric(year(ymd(df3$date)))

colnames(df3)


# proportion with ombilicus stage 1 
table(df3$UDS)
# 2  3  4  5 
# 8  5  4 47 

prop.table(table(df3$UDS))
# 2        3        4        5 
# 0.125000 0.078125 0.062500 0.734375 


#remove adult females
# df2<- df2[!is.na(df2$headtag),] NO - CHECK NOTES WITH UMBILICUS 
# determine age # added oct 25 2021
df3$bdate <- NA

df3 <- df3 %>% mutate(bdate = case_when(
  UDS == "1" ~ date-1, 
  )) # none aged 1 day old

df3= tibble(date = df3$date, myID=as.factor(df3$fliptag), year = df3$year, sex = as.factor(df3$sex), site = df3$site, mass= df3$weight, bdate = df3$bdate)
df3=df3[!is.na(df3$myID),]
df3 = df3%>% arrange(date) 
df3<-df3%>%group_by(myID)%>%
  droplevels() 
nlevels(unique(df3$myID))# n = 76

df3$mass<-as.character(as.factor(df3$mass))



# format 2012  + 
df4 <- read.csv2("data/mine/PvBase_2008-2016.csv", na.strings = c("", "?", "-", "NA"))
df4<-df4[-c(1,2),]
unique(df4$site)
df4=df4[df4$site %in% c("metis", "bic", "Métis",  "Metis","Bic"),]

# arrange date  
df4$date <-ymd(df4$date) 
df4$year <-as.numeric(year(ymd(df4$date)))

colnames(df4)
table(df4$tag_color)

# get an ID - first letter was no good - corrected nov.13 2020
df4$femaleT = substr(sapply(str_split(df4$tag_color, "/",n = 2), function(i)i[1]),1,1)
df4$maleT = substr(sapply(str_split(df4$tag_color, "/",n = 2), function(i)i[2]),1,1)

as.factor(unique(df4$femaleT))  #"r" "g" "b" "y"
as.factor(unique(df4$maleT))

df4  <- df4 %>% 
  mutate(newFemT = case_when(
    femaleT == "g" ~ "v", # some in english, some in french - all in french
    femaleT == "y" ~ "j",
    TRUE ~ femaleT
  ), newMaleT = case_when(
    maleT == "g" ~ "v",
    maleT == "y" ~ "j",
    TRUE ~ maleT
  ))
df4$newFemT
df4$newMaleT

df4$myID <- as.factor(paste0(toupper(df4$newFemT), toupper(df4$newMaleT), toupper(df4$letter), df4$tag))

# doublons avec 2008-2011
df4=df4[df4$year %in% c(2012:2016),]# n = 845
table(df4$myID) # kept 0
unique(df4$myID)

# determine age # added oct 25 2021
df4$bdate <- NA


table(df4$Umbilicus)
# 1   2   3   4   5 
# 35  87  61  89 478 

prop.table(table(df4$Umbilicus))
# 1          2          3          4          5 
# 0.04666667 0.11600000 0.08133333 0.11866667 0.63733333 


df4 <- df4 %>% mutate(bdate = case_when(
  Umbilicus == "1" ~ date-1, 
))


df4= tibble(date = df4$date, myID=as.factor(df4$myID), year = df4$year, sex = as.factor(df4$sex), site= df4$site,
            mass = df4$weight, bdate = df4$bdate)

df4=df4[!is.na(df4$myID),]
df4 = df4%>% arrange(date) # n= 779

df4<-df4%>%group_by(myID)%>%
  droplevels() 
table(df4$myID) # 0 gone 

nlevels(unique(df4$myID)) # 524 niveaux 

df4$mass<-as.character(as.factor(df4$mass))

# ATTENTION A FEW DEAD CONFIRMED. WHAT TO DO WITH THEM??


# bind all ----------------------------------------------------------------

pvData = rbind(df1, df2, df3, df4) %>% arrange(date)
pvData$site<-as.factor(pvData$site)
pvData$myID<-as.factor(pvData$myID) 

pvData=pvData[!is.na(pvData$myID),]
pvData$myID <- droplevels(pvData$myID)

nlevels(unique(pvData$myID)) # 1385 levels 

# arrange mass decimal 
pvData[, "mass"]<- as.character(as.factor(pvData$mass))
pvData[, "mass"]<-str_replace(pvData$mass, "[..]", ",")
pvData[, "mass"]<-str_replace(pvData$mass, "[,]", ".")
pvData[, "mass"]<-str_replace(pvData$mass, "[>]", "")
pvData[, "mass"]<-str_replace(pvData$mass, "[~]", "")
unique(pvData$mass)
str(pvData$mass)

# HOW MANY NA IN MASS
pvData[is.na(pvData$mass),] # 57 NA

# given it is unsure of wheter an animal was a mom or just a sighting without capture, delete missing mass
pvData <- pvData[!is.na(pvData$mass),] # now 2390 observations
pvData$myID <- droplevels(pvData$myID) # now 1378 levels

pvData$mass<-as.numeric(as.character(pvData$mass)) # just one more NA

# NOW HOW MANY NA IN MASS
# pvData$age<-as.numeric(as.character(pvData$age))
# str(pvData)

# how to be sure that missing mass is a capture? remove NAs

# arrange site
pvData$site = droplevels(pvData$site)
levels(pvData$site)
levels(as.factor(pvData$sex))

pvData$site <-as.character(pvData$site) # attention they must all be there even if correct 
pvData  <- pvData %>% 
  mutate(mySite= case_when(
    site == "Bic" ~ "bic",
    site == "Métis" ~ "metis",
    site == "Metis" ~ "metis",
    site == "M\x8etis" ~ "metis",
    site == "bic" ~ "bic",
    site == "metis" ~ "metis",
    TRUE ~ site
  ))

# arrange sex
pvData$sex <-as.character(pvData$sex)
pvData  <- pvData %>% 
    mutate(sex = na_if(sex, "NA")) %>% 
    mutate(mySex= case_when(
    sex == "f" ~ "female",
    sex == "m" ~ "male",
    sex == "F" ~ "female",
    sex == "M" ~ "male",
    sex == "female" ~ "female",
    sex == "male" ~ "male",
    sex == "male?" ~ "male",
    TRUE ~ sex
  )) 

pvData <- pvData %>%
    mutate(sex = na_if(sex, "NA")) 

# assign class and drop levels
pvData=pvData[!is.na(pvData$myID),]
pvData$mySex <-droplevels(as.factor(pvData$mySex))
pvData$mySite <-droplevels(as.factor(pvData$mySite))
pvData$year <-as.factor(pvData$year)
pvData$myID <-droplevels(as.factor(pvData$myID))

# check for NAs
levels(pvData$mySex) # sex has no NA
levels(pvData$mySite)
levels(pvData$myID)

# count (like a table)
fct_count(pvData$mySite)
# f         n
# <fct> <int>
#  1 bic    1794
# 2 metis   653

# revised
# 1 bic    1747
# 2 metis   643

fct_count(pvData$mySex) # 40 individuals with unknown sex? 
# female  1144
# 2 male    1263
# 3 NA        40

# revised 
# 1 female  1126
# 2 male    1256
# 3 NA         8


#clean database 
pvData= tibble(date = pvData$date, myID=as.factor(pvData$myID), year = as.factor(pvData$year), 
               sex = as.factor(pvData$mySex), mySite= as.factor(pvData$mySite), mass = as.numeric(pvData$mass), bdate = date(pvData$bdate))


# find duplicates 
pvData=pvData %>%
  distinct(date,myID,.keep_all = TRUE) # n=2388

# add julian day --------------------------------------------------------
# arrange capture date
pvData$julianDay<- as.POSIXlt(pvData$date, format = "%Y-%m-%d")$yday+1 

# to age # name it as in model as well 
pvData$bDate<- as.POSIXlt(pvData$bdate, format = "%Y-%m-%d")$yday+1 

# clear 
rm(df1, df2, df3, df4,tmp1, df2019)

# check for adult females or weaned pups at first capture ------------------------------------------------
# only remove those of 35 kg at first capture 

# Get the list of individuals to remove
to_remove <- pvData %>% 
    group_by(myID) %>% 
    arrange(date) %>% 
    filter(mass >= 35 & row_number() == 1) %>% 
    pull(myID)

# Remove the selected individuals from captureData
pvData_filtered <- pvData %>% 
    filter(!myID %in% to_remove)

# who's NA
pvData_filtered[is.na(pvData_filtered$mass),] # none
pvData_filtered[is.na(pvData_filtered$myID),] # none
pvData_filtered$myID <- droplevels(pvData_filtered$myID)
pvData_filtered =pvData_filtered[!is.na(pvData_filtered$myID),] # 2436, no NA in id

# find duplicates : same ID same date ??
pvData_filtered = pvData_filtered[!duplicated(paste(pvData_filtered$myID, pvData_filtered$date)),]

# that separates sites and df
bic=pvData_filtered %>% filter(mySite=='bic')
metis=pvData_filtered %>% filter(mySite=='metis')

temp=inner_join(bic, metis, by="myID")
#2000-06-01 JBW60… 2000   fema… bic         14 NA     

# NOW CORRECTED AND THESE DO NOT SHOW UP
# 2 WERE currently errors while the first has been seen at two sites but over more than a month interval

# 2 2003-06-13 VVV3474 2003   female bic          25
# 3 2003-06-13 VVV3474 2003   female bic          25
# 4 2003-06-13 VVV3378 2003   female bic          13


# revisions - create CJS df of capture history all years + site combined-----------------------------------------
# assign correct class 
site=levels(as.factor(unique(pvData_filtered$mySite)))
tmpSite=length(site)

# create table of capture histories, keeping all julian day bw min and max capture date
# some ID will have 0 captures at 'fake' capture date
obs <- pvData_filtered %>% # filter(mySite=='bic') 
    ungroup() %>% arrange(julianDay) %>% 
    mutate(myID=droplevels(myID),jj2=factor(julianDay,levels=seq(min(julianDay,na.rm = T),
                                                                 max(julianDay,na.rm=T),by=1))) %$% # with
    table(myID,jj2) 

id=rownames(obs)
jj2=colnames(obs)

obs2 <- matrix(as.numeric(obs), ncol=length(jj2), nrow=length(id))
obs <- obs2

colnames(obs) <- jj2
rownames(obs) <- id

# create nimble ID to align original data to mass data by iD
pvData_filtered$nimbleID <- match(pvData_filtered$myID, rownames(obs), nomatch = NA_integer_, incomparables = NULL)

# create vector of year, as long as the data, with ID
year=unique(select(ungroup(pvData_filtered),myID,year))

# match year with observations, with ID
year=year$year[match(rownames(obs),year$myID)]

# transform to numeric
year_int=as.numeric(as.factor(year))

# x=matrix(pup_ch_l[[site]],nrow = nrow(pup_ch_l[[site]]),ncol=ncol(pup_ch_l[[site]]))

# create matrix of true observations (1), based on capture histories that really happened
trueOcc=obs
trueOcc[,] <- NA
for(t in unique(year_int)){
    l=which(year_int==t)
    trueOcc[l,] <- rep(as.numeric(colSums(obs[l,])>0),each=length(l))
}
# table is 75460 long, made of ID and jj2, where a true occ is not 0


# split true occ and obs by sites -----------------------------------------
# create vector of year, as long as the data, with ID
site=unique(select(ungroup(pvData_filtered),myID,mySite))
# match year with observations, with ID
site=site$mySite[match(rownames(obs),site$myID)]
site_int <- as.numeric(as.factor(site))-1 # 0=bic, 1=metis

# create vector of year, as long as the data, with ID
sex=unique(select(ungroup(pvData_filtered),myID,sex))
# match year with observations, with ID
sex=sex$sex[match(rownames(obs),sex$myID)]
sex_int <- as.numeric(factor(sex, levels = c('male', 'female')))-1 # 0=male, 1=female

# create matrix of z ------------------------------------------------------

# Function to create a matrix with information about known latent state z - this creates the latent state variable z that needs to be updated at each MCMC iterations
# Define a function named known.state.cjs that takes a capture history matrix ch as input

known.state.cjs <- function(ch) {
    
    # Copy the capture history matrix to a new matrix named state
    state <- ch
    
    # Loop over each individual in the capture history matrix
    for (i in 1:dim(ch)[1]) {
        
        # Determine the first capture occasion (n1) and last capture occasion (n2) for the current individual
        n1 <- min(which(ch[i, ] == 1))
        n2 <- max(which(ch[i, ] == 1))
        
        # Set the state of the individual to 1 for all capture occasions up to and including the last capture occasion
        state[i, 1:n2] <- 1
        
        # Set the state of the individual to NA for the first capture occasion
        state[i, n1] <- NA
    }
    
    # Set the state of individuals that were never captured to NA for all capture occasions
    state[state == 0] <- NA
    
    # Return the state matrix
    return(state)
}

data.z=known.state.cjs(obs)

 # save(pvData_filtered, data.z, obs, trueOcc, site_int, year_int, sex_int, file = "data/mine/2023-04-04_revisedDf.RData")
 # if test for mass effect might need all pups including those>35kg (7 obs)




# data augmentation for JS models ----------------------------------------
load("cache/2023-04-04_revisedDf.RData")
NAug=30

# Define a function to get the first non-zero value in a vector (the earliest possible entry date)
get.first <- function(x) min(which(x != 0)) 
# Defince vector of all capture occasions, ordered
# Get the unique Julian days in the current site's pup data
captureJJ <- unique(pvData_filtered$julianDay)
# Create a vector of all possible Julian days (min to max)
allJJ <- seq(min(captureJJ), max(captureJJ))

# Store the capture history data and pup mass in a list, along with latent variable matrix z to estimate surv
df.js <- list()
# comment this line to get back to full df.js
# shorternl <- sample(1:nrow(obs),size = nrow(obs)*0.5) %>% sort
# create df.js to run model



df.js <- list(
    data = list(y = as.matrix(obs),
                mass = pvData_filtered$mass),
    const = list()
)


known.state.js <- function(ch) {
    
    # Copy the capture history matrix to a new matrix named state
    state <- ch
    state[,] <-  NA
    # Loop over each individual in the capture history matrix
    for (i in 1:dim(ch)[1]) {
        
        # Determine the first capture occasion (n1) and last capture occasion (n2) for the current individual
        n1 <- min(which(ch[i, ] == 1))
        n2 <- max(which(ch[i, ] == 1))
        
        # Set the state of the individual to 1 for all capture occasions up to and including the last capture occasion
        state[i, n1:n2] <- 1
        # Set the state of the individual to NA for the first capture occasion
        state[i, n1] <- NA
    }
    
    # Set the state of individuals that were never captured to NA for all capture occasions
    
    # Return the state matrix
    return(state)
}
df.js$data$z <- known.state.js(df.js$data$y)

str(df.js$data)
nb.fakeID <- NAug*16*2
new_yearInt <- rep(1:16,each=NAug*2)
new_site <-   rep(0:1,NAug*16)
new_sex <- rep(NA,length(new_site))

df.js$data$y <- rbind(df.js$data$y,matrix(0,ncol=ncol(df.js$data$y),nrow=nb.fakeID))
rownames(df.js$data$y) <- c(rownames(obs),paste0("fake_",1:nb.fakeID))
colnames(df.js$data$y)
df.js$data$z <- rbind(df.js$data$z,matrix(NA,ncol=ncol(df.js$data$z),nrow=nb.fakeID))
str(df.js$data)

str(df.js$const)

# sex will be in data since NA
# Store additional constants in the list - no NA allowed
df.js$const <- list(
    # f = apply(df.js$data$y, 1, function(x) get.first(x)),
    # nind = nrow(df.js$data$y),
    M= nrow(df.js$data$y),
    n.occasions = ncol(df.js$data$y),
    captureJJ = allJJ,
    julianDay = pvData_filtered$julianDay,
    first.bd = NA,
    sex_int=c(sex_int,new_sex),
    site_int=c(site_int,new_site), # no NA thus const
    year_int=c(year_int,as.numeric(new_yearInt)) # no NA thus const
)
l <- order(df.js$const$year_int)

df.js$data$y=df.js$data$y[l,]
df.js$data$z=df.js$data$z[l,]
df.js$const$first.bd=df.js$const$first.bd[l]
df.js$const$sex_int=df.js$const$sex_int[l]
df.js$const$site_int=df.js$const$site_int[l]
df.js$const$year_int=df.js$const$year_int[l]

df.js$const$nimbleID = match(pvData_filtered$myID,row.names(df.js$data$y))


df.js$const$trueOcc=df.js$data$y
df.js$const$trueOcc[,] <- NA
for(t in 1:16){
    l=which(df.js$const$year_int==t)
    df.js$const$trueOcc[l,] <- rep(as.numeric(colSums(df.js$data$y[l,])>0),each=length(l))
}



df.js$data$mass <- df.js$data$mass[!is.na(df.js$const$nimbleID)]
df.js$const$julianDay <- df.js$const$julianDay[!is.na(df.js$const$nimbleID)]
df.js$const$nimbleID <- df.js$const$nimbleID[!is.na(df.js$const$nimbleID)]
df.js$const$Nw = length(df.js$const$nimbleID)

# yrsites <- array(NA,dim=c(16,2,12572) )
# yrsites.nb <- matrix(NA,16,2)
# for(s in 1:2){
#     for(y in 1:16){
#         l=which(df.js$const$site_int==(s-1) & df.js$const$year_int==y)
#         yrsites[y,s,1:length(l)] <- l
#         yrsites.nb[y,s] <- length(l)
#     }
# }
# yrsites <- yrsites[,,1:max(yrsites.nb)]
yrRAnge <- matrix(NA,16,2)
for(i in 1:16){
    yrRAnge[i,] <- range(which(df.js$const$year_int==i))
}

df.js$data$yrRAnge=yrRAnge
# Pull a vector of minimal dates for existing ID
tmptmp <- pvData_filtered %>% 
    group_by(myID) %>% 
    summarise(min.bd = min(julianDay))

# Match the minimal dates to the obs matrix rows using their myID
tmptmp <- tmptmp$min.bd[match(rownames(df.js$data$y), tmptmp$myID)]

# Set the 'first.bd' constant value to the minimum date for each individual in the data set
df.js$const$first.bd <- ifelse(
    is.na(tmptmp),
    max(unique(pvData_filtered$julianDay)),
    tmptmp
)


write_rds(df.js,"cache/df_js.rds")



# data augmentation for  single year JS models ----------------------------------------
load("cache/2023-04-04_revisedDf.RData")
NAug=300

# Define a function to get the first non-zero value in a vector (the earliest possible entry date)
get.first <- function(x) min(which(x != 0)) 
# Defince vector of all capture occasions, ordered
# Get the unique Julian days in the current site's pup data
captureJJ <- unique(pvData_filtered$julianDay)
# Create a vector of all possible Julian days (min to max)
allJJ <- seq(min(captureJJ), max(captureJJ))

# Store the capture history data and pup mass in a list, along with latent variable matrix z to estimate surv
# comment this line to get back to full df.js
# shorternl <- sample(1:nrow(obs),size = nrow(obs)*0.5) %>% sort
# create df.js to run model


df.syjs=lapply(1:16,function(cy){
l=which(year_int==cy)
df.js <- list(
    data = list(y = as.matrix(obs[l,]),
                mass = pvData_filtered$mass),
    const = list()
)


known.state.js <- function(ch) {
    
    # Copy the capture history matrix to a new matrix named state
    state <- ch
    state[,] <-  NA
    # Loop over each individual in the capture history matrix
    for (i in 1:dim(ch)[1]) {
        
        # Determine the first capture occasion (n1) and last capture occasion (n2) for the current individual
        n1 <- min(which(ch[i, ] == 1))
        n2 <- max(which(ch[i, ] == 1))
        
        # Set the state of the individual to 1 for all capture occasions up to and including the last capture occasion
        state[i, n1:n2] <- 1
        # Set the state of the individual to NA for the first capture occasion
        state[i, n1] <- NA
    }
    
    # Set the state of individuals that were never captured to NA for all capture occasions
    
    # Return the state matrix
    return(state)
}
df.js$data$z <- known.state.js(df.js$data$y)

# str(df.js$data)
nb.fakeID <- NAug*2
new_site <-   rep(0:1,NAug)
new_sex <- rep(NA,length(new_site))

df.js$data$y <- rbind(df.js$data$y,matrix(0,ncol=ncol(df.js$data$y),nrow=nb.fakeID))
rownames(df.js$data$y) <- c(rownames(obs[l,]),paste0("fake_",1:nb.fakeID))
colnames(df.js$data$y)
df.js$data$z <- rbind(df.js$data$z,matrix(NA,ncol=ncol(df.js$data$z),nrow=nb.fakeID))

# sex will be in data since NA
# Store additional constants in the list - no NA allowed
df.js$const <- list(
    # f = apply(df.js$data$y, 1, function(x) get.first(x)),
    # nind = nrow(df.js$data$y),
    M= nrow(df.js$data$y),
    n.occasions = ncol(df.js$data$y),
    captureJJ = allJJ,
    julianDay = pvData_filtered$julianDay,
    # massYr =pvData_filtered$year,
    first.bd = NA,
    sex_int=c(sex_int[l],new_sex),
    site_int=c(site_int[l],new_site) # no NA thus const
)

df.js$const$nimbleID = match(pvData_filtered$myID,row.names(df.js$data$y))
tmp <- droplevels(pvData_filtered$myID[is.na(df.js$const$nimbleID)])
df.js$const$nimbleID[is.na(df.js$const$nimbleID)] <- as.numeric(tmp)+nrow(df.js$data$y)


df.js$const$M2 <- max(df.js$const$nimbleID)


df.js$const$trueOcc=df.js$data$y
df.js$const$trueOcc[,] <- NA
# for(t in 1:16){
#     l2=which(df.js$const$year_int==t)
df.js$const$trueOcc[,] <- rep(as.numeric(colSums(df.js$data$y,na.rm = T)>0),
                              each=nrow(df.js$data$y))
# }



df.js$const$Nw = length(df.js$const$nimbleID)
# Pull a vector of minimal dates for existing ID
tmptmp <- pvData_filtered %>%mutate(nimbleID=df.js$const$nimbleID) %>% 
    group_by(nimbleID) %>% 
    summarise(min.bd = min(julianDay,na.rm = T)) %>% full_join(tibble(nimbleID=1:df.js$const$M2))

# Set the 'first.bd' constant value to the minimum date for each individual in the data set
df.js$const$first.bd <- ifelse(
    is.na(tmptmp$min.bd),
    max(unique(pvData_filtered$julianDay)),
    tmptmp$min.bd
)

# str(df.js)
return(df.js)
})

str(df.syjs[[3]])

write_rds(df.syjs,"cache/df_syjs.rds")










# 
# 
# 
# # all data needs to be split by year
# obs_aug <- pvData_filtered %>%
#     # filter(mySite == 'bic') %>%
#     ungroup() %>%
#     arrange(julianDay) %>%
#     mutate(myID = droplevels(myID),
#            jj2 = factor(julianDay, levels = seq(min(julianDay, na.rm = TRUE), max(julianDay, na.rm = TRUE), by = 1))) %>%
#     split(., .$year)
# 
# years <- levels(as.factor(unique(pvData_filtered$year)))
# yrs <- length(years)
# 
# reps <- rep(0, yrs)
# 
# num_augmented_rows <- 100
# 
# for (j in 1:length(years)) {
#     x <- obs_aug[[j]]
#     y <- as.character(years[j])
#     x$myID <- droplevels(x$myID)
#     df <- data.frame(
#         date = rep(unique(x$date), num_augmented_rows),
#         myID = paste0('x.', seq(1:100)),
#         year = rep(NA, num_augmented_rows),
#         sex = rep(NA, num_augmented_rows),
#         mySite = rep(NA, num_augmented_rows),
#         mass = rep(NA, num_augmented_rows),
#         bdate = rep(NA, num_augmented_rows),
#         julianDay = rep(NA, num_augmented_rows),
#         bDate = rep(NA, num_augmented_rows),
#         nimbleID = rep(NA, num_augmented_rows),
#         jj2=rep(NA, num_augmented_rows)
#     )
#     obs_aug[[j]] <- rbind(x, df) # this is a list
# }
# 
# # Combine the modified data frames for all years
# obs_aug.df <- data.frame(do.call(rbind, obs_aug))
# 
# # for data aug
# for(j in 1:length(years)){
#     x=obs_aug.df[which(obs_aug.df$year==years[j]),]
#     y=as.character(years[j])
#     x$myID<-droplevels(x$myID)
#     ch=with(x,table(myID,date)) # this is what transforms into a matrix of 0-1
#     colnames(ch) <- NULL
#     obs_aug[[j]]$nimbleID <- match(obs_aug[[j]]$myID, rownames(ch), nomatch = NA_integer_, incomparables = NULL)
#     # Add dummy occasion before the first real occasion, augment data set and recode that data 
#     # to match the codes of the observed states 
#     
#     #CH.du <- cbind(rep(0, dim(ch)[1]), ch) removed for NEW superpop model
#     z <- ch                           # Grab counts
#     z[z > 1] <- 1                             # Counts to det-nondetections
#     ever.observed <- apply(z, 1, max)
#     C <- sum(apply(z, 1, max)) ; print(C)     # Number of observed ID
#     table(apply(z, 1, sum))                   # Capture-frequencies
#     yy <- as.matrix(z[ever.observed == 1,]) # Detection histories
#   
#       # Augment data set
#     nz <- 100
#     new_rows <- as.character(paste0('x.', seq(1:100)))  # Row names from x:1 to x:100
#     CH.ms <- rbind(ch, matrix(0, ncol = dim(ch)[2], nrow = nz))
#     rownames(CH.ms) <- c(rownames(ch), new_rows)  # Assign row names
#     
#     # # Recode CH matrix: a 0 is not allowed in WinBUGS!
#     #CH.ms[CH.ms==0] <- 2    
#     name=paste("obs",y, sep="")
#     assign(name, CH.ms, inherits=T)
#     print(dim(CH.ms))
# }
# 
# id=rownames(obs_aug.df)
# jj2=colnames(obs_aug.df)
# 
# obs2 <- matrix(as.numeric(obs_aug.df), ncol=length(jj2), nrow=length(id))
# obs <- obs2
# 
# colnames(obs) <- jj2
# rownames(obs) <- id
# 
# # create nimble ID to align original data to mass data by iD
# pvData_filtered$nimbleID <- match(pvData_filtered$myID, rownames(obs), nomatch = NA_integer_, incomparables = NULL)
# 
# # create vector of year, as long as the data, with ID
# year=unique(select(ungroup(pvData_filtered),myID,year))
# 
# # match year with observations, with ID
# year=year$year[match(rownames(obs),year$myID)]
# 
# # transform to numeric
# year_int=as.numeric(as.factor(year))
# 
# # x=matrix(pup_ch_l[[site]],nrow = nrow(pup_ch_l[[site]]),ncol=ncol(pup_ch_l[[site]]))
# 
# # create matrix of true observations (1), based on capture histories that really happened
# trueOcc=obs
# trueOcc[,] <- NA
# for(t in unique(year_int)){
#     l=which(year_int==t)
#     trueOcc[l,] <- rep(as.numeric(colSums(obs[l,])>0),each=length(l))
# }
# # table is 75460 long, made of ID and jj2, where a true occ is not 0
# 
# 
# # split true occ and obs by sites -----------------------------------------
# # create vector of year, as long as the data, with ID
# site=unique(select(ungroup(pvData_filtered),myID,mySite))
# # match year with observations, with ID
# site=site$mySite[match(rownames(obs),site$myID)]
# site_int <- as.numeric(as.factor(site))-1 # 0=bic, 1=metis
# 
# # create vector of year, as long as the data, with ID
# sex=unique(select(ungroup(pvData_filtered),myID,sex))
# # match year with observations, with ID
# sex=sex$sex[match(rownames(obs),sex$myID)]
# sex_int <- as.numeric(factor(sex, levels = c('male', 'female')))-1 # 0=male, 1=female
# 
# data.z=known.state.cjs(obs)
# 
# 
# 
# 
# 
# 
# 
# 
# # save(list = ls(), file = "cache/2023-05-12_dataJS_pup35.RData") # by site for pup less than 35 kg, males and females 
# 
# 
# 
# 
# 
