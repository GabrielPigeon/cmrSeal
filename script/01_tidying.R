# script for putting seal data altogether  - generates dataList and databySite objects
#changes in data frame Jan 30 2021 ---------------------------------------
# 
# install.packages("readxl", "lubridate", "stringr", "ggplot2", 
#                  "r2jags", "coda", "boot")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("pander")
# install.packages("ggthemes")
library(tidyverse)
# library(lubridate)
# library(stringr)


# CHECK PUPS WITH 30 KG OR SO

setwd("/Users/LimoilouARenaud/Documents/PostDocI/Phoque")

rm(list = ls())

# 1998-2003
pvData03 <- read.csv2("Data/Mine/CaptureData_Pv1998-2003.csv", na.strings = c("", "?", "-", "NA"))
pvData03$site # arrange site name later 

# arrange date  
pvData03$date <-ymd(pvData03$date) 
pvData03$year <-as.numeric(year(ymd(pvData03$date)))

nlevels(pvData03$year) # 6
levels(pvData03$year) 

pvData03$fliptag <- as.factor(as.character(pvData03$fliptag))
nlevels(pvData03$fliptag) # 398

pvData03 = pvData03[!is.na(pvData03$headtag),] # removes 3 ID - n  = 760
pvData03 = pvData03[!is.na(pvData03$fliptag),]

summary(pvData03)

# tidy table 
df1= tibble(date = pvData03$date, myID=as.factor(pvData03$fliptag), year = pvData03$year, sex = as.factor(pvData03$sex), site = pvData03$site, mass = pvData03$weight, bdate = NA)

df1=df1[!is.na(df1$myID),]
df1 = df1%>% arrange(date) # n= 760 observations

df1<-df1%>%group_by(myID)%>%
  droplevels() 
nlevels(unique(df1$myID))# n = 396 # some miss letters. who cares as long as there are unique ID.

df1$mass<-as.character(as.factor(df1$mass)) # for now





# capture data 2008-2011
df2 <- read.csv2("Data/Mine/CaptureData_Pv2008-2011.csv", na.strings = c("", "?", "-", "NA")) # SEMBLE Y AVOIR UN FILTRE SUR LA DATE EFFECTUÉ ICI
df2$site
df2=df2[df2$site %in% c("metis", "bic", "Métis", "Bic"),]

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
df3 <- read.csv2("Data/Mine/myData2019.csv", na.strings = c("", "?", "-", "NA")) #
df3$site

# arrange date  
df3$date <-ymd(df3$date) 
df3$year <-as.numeric(year(ymd(df3$date)))

colnames(df3)

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
df4 <- read.csv2("Data/Mine/PvBase_2008-2016.csv", na.strings = c("", "?", "-", "NA"))
df4<-df4[-c(1,2),]
df4$site 
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




# bind all ----------------------------------------------------------------

pvData = rbind(df1, df2, df3, df4) %>% arrange(date)
pvData$site<-as.factor(pvData$site)
pvData$myID<-as.factor(pvData$myID) 

pvData=pvData[!is.na(pvData$myID),]
pvData<-pvData%>%group_by(myID)%>%
  droplevels() 

nlevels(unique(pvData$myID)) # 1384 niveaux 

# arrange mass decimal 

pvData[, "mass"]<- as.character(as.factor(pvData$mass))
pvData[, "mass"]<-str_replace(pvData$mass, "[,]", ".")
pvData[, "mass"]<-str_replace(pvData$mass, "[>]", "")
pvData[, "mass"]<-str_replace(pvData$mass, "[~]", "")
unique(pvData$mass)
str(pvData)

# convert as numeric possible interesting var
# tmp[, "length"]<- as.character(as.factor(tmp[, "length"]))
# tmp[, "axialgirth"]<- as.character(as.factor(tmp[, "axialgirth"]))
# cols.num <- c("weight","length", "axialgirth")
# tmp[cols.num] <- sapply(tmp[cols.num],as.numeric)
# sapply(tmp[cols.num], class)

pvData$mass<-as.numeric(as.character(pvData$mass))
pvData[is.na(pvData$mass),] # n=60 missing mass? 
pvData$age<-as.numeric(as.character(pvData$age))
str(pvData)

#uniformiser les sites et sex 
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

pvData$sex <-as.character(pvData$sex)
pvData  <- pvData %>% 
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


# check for NAs
levels(pvData$mySite)
levels(pvData$myID)
levels(pvData$mySex) # sex has NA

pvData$mySite = fct_explicit_na(pvData$mySite, na_level = "NA")
pvData$myID = fct_explicit_na(pvData$myID, na_level = "NA")
pvData$mySex = fct_explicit_na(pvData$mySex, na_level = "NA")
levels(pvData$mySite)
fct_count(pvData$mySite)
# f         n
# <fct> <int>
#   1 bic    1794
# 2 metis   653

fct_count(pvData$mySex) # 40 individuals with unknown sex? 
# female  1144
# 2 male    1263
# 3 NA        40
pvData$mySex <-as.factor(pvData$mySex)
pvData$mySite <-as.factor(pvData$mySite)
pvData$year <-as.factor(pvData$year)




#clean database 
pvData= tibble(date = pvData$date, myID=as.factor(pvData$myID), year = as.factor(pvData$year), 
               sex = as.factor(pvData$mySex), mySite= as.factor(pvData$mySite), mass = as.numeric(pvData$mass), bdate = date(pvData$bdate))
pvData=pvData[!is.na(pvData$myID),]
pvData<-pvData%>%group_by(mySite)%>%
  droplevels() 
colnames(pvData)

# find duplicates 
pvData=pvData %>%
  distinct(date,myID,.keep_all = TRUE) # n=2444

# add julian day --------------------------------------------------------
# arrange capture date
pvData$julianDay<- as.POSIXlt(pvData$date, format = "%Y-%m-%d")$yday+1 

# to age # name it as in model as well 
pvData$bDate<- as.POSIXlt(pvData$bdate, format = "%Y-%m-%d")$yday+1 

# tidy 35 kg data 
pvData$sex <-as.character(pvData$sex)
pvData  <- pvData %>% 
  mutate(sex = case_when(
    sex == "male?" ~ "male",
    TRUE ~ sex
  ))
pvData$sex <-as.factor(pvData$sex)
levels(pvData$sex)

# check for adult females  ------------------------------------------------

# Dubé et al. 2003 - The largest change in growth
# rate was observed among pups captured five or more times. If
# this class is removed, the difference in growth rates between
# captures was no longer significant (ANOVA by rank, F[2,74] =
# #  2.00, P > 0.05).
# Weaning occurred near the end of June or at the beginning
# of July (Table 2). The duration of lactation estimated from
# the difference between the median date of birth and the median
# date of weaning was 34, 36, and 31 days in 1998, 1999,
# and 2000, respectively (mean = 33 days, SE = 1.8, N = 3).


# no filter on duration of lactation since first capture, not bdate. Fig. 1 dubé et al. 2003
pup.data35 = pvData[!(pvData$mass>35),] # n = 2437 ceci enlève des mâles qu'on sait pas si c des pups ou des adults 
pup.data40 = pvData[!(pvData$mass>40),]
rm(df1, df2, df3, df4,tmp1, df2019)

# c qui qui a des NA

pup.data35[is.na(pup.data35$mass),]
pup.data35 =pup.data35[!is.na(pup.data35$myID),] # 2380, no NA - lost 64 individuals, data conversion? 

pup.data40[is.na(pup.data40$mass),]
pup.data40 =pup.data40[!is.na(pup.data40$myID),] # 2380, no NA - lost 64 individuals, data conversion? 


# find duplicates : same ID same date ??
pup.data35 = pup.data35[!duplicated(paste(pup.data35$myID, pup.data35$date)),]
pup.data40 = pup.data40[!duplicated(paste(pup.data40$myID, pup.data40$date)),]



# split data by site for pup 35 kg------------------------------------------------------
# makes a list 
metisData35_l <- pup.data35 %>% filter(mySite == "metis") %>% split(., .$year) 
bicData35_l <- pup.data35 %>% filter(mySite == "bic") %>% split(., .$year) 

# metisData35. <- pup.data35%>%
#   filter(mySite == "metis")
# 
# years=levels(as.factor(unique(metisData35.$year)))
# yrs=length(years)
# 
# reps=rep(0,yrs)
# 
# for(i in 1:length(years)){
#   x=metisData35.[which(metisData35.$year==years[i]),]
#   y=as.character(years[i])
#   name=paste("metisData35.",y, sep="")
#   assign(name, x, inherits=T)	
#   days=length(unique(x$date))
#   nfish=length(unique(x$myID)) # changed myID
#   reps[i]=days
#   #create the count table with id for rows and years for columns
# }
# 
# 
# bicData35. <- pup.data35%>%
#   filter(mySite == "bic")
# 
# years=levels(as.factor(unique(bicData35.$year)))
# yrs=length(years)
# 
# reps=rep(0,yrs)
# 
# for(i in 1:length(years)){
#   x=bicData35.[which(bicData35.$year==years[i]),]
#   y=as.character(years[i])
#   name=paste("bicData35.",y, sep="")
#   assign(name, x, inherits=T)	
#   days=length(unique(x$date))
#   nfish=length(unique(x$myID))
#   reps[i]=days
#   #create the count table with id for rows and years for columns
# }
# 
# getwd()

# rm junk
rm(pup.data40, pvData, pvData03)
#save it as rdata
#save(list = ls(), file = "Data/Mine/20210608dataBySitep_Pup35.RData") # by site
# save(list = ls(), file = "Data/Mine/20211025_dataList.Rdata")
# done save



# split data by site for pup 40 kg------------------------------------------------------

pup.data40$sex <-as.character(pup.data40$sex)
pup.data40  <- pup.data40 %>% 
  mutate(sex = case_when(
    sex == "male?" ~ "male",
    TRUE ~ sex
  ))
pup.data40$sex <-as.factor(pup.data40$sex)
levels(pup.data40$sex)

# find duplicates : same ID same date ??
pup.data40 = pup.data40[!duplicated(paste(pup.data40$myID, pup.data40$date)),]

metisData40. <- pup.data40%>%
  filter(mySite == "metis")

years=levels(as.factor(unique(metisData40.$year)))
yrs=length(years)

reps=rep(0,yrs)

for(i in 1:length(years)){
  x=metisData40.[which(metisData40.$year==years[i]),]
  y=as.character(years[i])
  name=paste("metisData40.",y, sep="")
  assign(name, x, inherits=T)	
  days=length(unique(x$date))
  nfish=length(unique(x$myID)) # changed myID
  reps[i]=days
  #create the count table with id for rows and years for columns
}


bicData40. <- pup.data40%>%
  filter(mySite == "bic")

years=levels(as.factor(unique(bicData40.$year)))
yrs=length(years)

reps=rep(0,yrs)

for(i in 1:length(years)){
  x=bicData40.[which(bicData40.$year==years[i]),]
  y=as.character(years[i])
  name=paste("bicData40.",y, sep="")
  assign(name, x, inherits=T)	
  days=length(unique(x$date))
  nfish=length(unique(x$myID))
  reps[i]=days
  #create the count table with id for rows and years for columns
}

getwd()


rm(adult.data, data, data2, dataPv, pvData, pvData03, tmp, x)
#save(list = ls(), file = "Data/Mine/20210210dataBySite.RData") # by site 
#save(list = ls(), file = "Data/Mine/20210608dataBySitep_Pup40.RData") # by site 




# create each dataset for CJS only  ---------------------------------------------------
rm(list=ls())
#load("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Data/Mine/20210210dataBySite.RData") # by site 
load("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Data/Mine/20211025_dataList.Rdata") # by site 

# subset into site to augment 
metisData35. <- pup.data35 %>% filter(mySite == "metis") # %>% split(., .$year) 
bicData35. <- pup.data35 %>% filter(mySite == "bic") # %>% split(., .$year) 

years=levels(as.factor(unique(metisData35.$year)))
yrs=length(years)

reps=rep(0,yrs)

# # for metis 
# for(j in 1:length(years)){
#   x=metisData35.[which(metisData35.$year==years[j]),]
#   y=as.character(years[j])
#   x$myID<-droplevels(x$myID)
#   ch=with(x,table(myID,date)) # this is what transforms into a matrix of 0-1
#   colnames(ch) <- NULL
#   metisData35_l[[j]]$nimbleID <- match(metisData35_l[[j]]$myID, rownames(ch), nomatch = NA_integer_, incomparables = NULL)
#   z <- ch                           # Grab counts
#   z[z > 1] <- 1                             # Counts to det-nondetections
#   ever.observed <- apply(z, 1, max)
#   C <- sum(apply(z, 1, max)) ; print(C)     # Number of observed ID
#   table(apply(z, 1, sum))                   # Capture-frequencies
#   yy <- as.matrix(z[ever.observed == 1,]) # Detection histories
#   # Augment metisData set
#   # nz = 500
#   #CH.ms <- rbind(ch, matrix(0, ncol = dim(ch)[2], nrow = nz))
#   name=paste("metis_",y, sep="")
#   assign(name, ch, inherits=T)	
#   print(dim(ch))
# }
list_ch_metis <- list()
for(j in 1:length(years)){
  x=metisData35.[which(metisData35.$year==years[j]),]
  y=as.character(years[j])
  x$myID<-droplevels(x$myID)
  list_ch_metis[[j]]=with(x,table(myID,date))
  colnames(list_ch_metis[[j]]) <- NULL# this is what transforms into a matrix of 0-1
  names(list_ch_metis)[[j]]=paste(y)
  print(dim(list_ch_metis[[j]]))
  metisData35_l[[j]]$nimbleID <- match(metisData35_l[[j]]$myID, rownames(list_ch_metis[[j]]), nomatch = NA_integer_, incomparables = NULL)
}

list_ch_bic <- list()
for(j in 1:length(years)){
  x=bicData35.[which(bicData35.$year==years[j]),]
  y=as.character(years[j])
  x$myID<-droplevels(x$myID)
  list_ch_bic[[j]]=with(x,table(myID,date))
  colnames(list_ch_bic[[j]]) <- NULL# this is what transforms into a matrix of 0-1
  names(list_ch_bic)[[j]]=paste(y)
  print(dim(list_ch_bic[[j]]))
  bicData35_l[[j]]$nimbleID <- match(bicData35_l[[j]]$myID, rownames(list_ch_bic[[j]]), nomatch = NA_integer_, incomparables = NULL)
  
}

# # for bic
# for(j in 1:length(years)){
#   x=bicData35.[which(bicData35.$year==years[j]),]
#   y=as.character(years[j])
#   x$myID<-droplevels(x$myID)
#   ch=with(x,table(myID,date))
#   colnames(ch) <- NULL
#   bicData35_l[[j]]$nimbleID <- match(bicData35_l[[j]]$myID, rownames(ch), nomatch = NA_integer_, incomparables = NULL)
#   z <- ch                           # Grab counts
#   z[z > 1] <- 1                             # Counts to det-nondetections
#   ever.observed <- apply(z, 1, max)
#   C <- sum(apply(z, 1, max)) ; print(C)     # Number of observed ID
#   table(apply(z, 1, sum))                   # Capture-frequencies
#   yy <- as.matrix(z[ever.observed == 1,]) # Detection histories
#   name=paste("bic_",y, sep="")
#   assign(name, ch, inherits=T)	
#   print(dim(ch))
# }
# save(list = ls(), file = "Phoque/Data/Mine/20211031_cmr_pup35.RData") # by site for pup less than 35 kg, males and females


# augment each dataset for jolly seber only ---------------------------------------------------
rm(list=ls())
#load("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Data/Mine/20210210dataBySite.RData") # by site 
load("/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Data/Mine/20211025_dataList.Rdata") # by site 

# subset into site to augment 
metisData35. <- pup.data35 %>% filter(mySite == "metis") # %>% split(., .$year) 
bicData35. <- pup.data35 %>% filter(mySite == "bic") # %>% split(., .$year) 

years=levels(as.factor(unique(metisData35.$year)))
yrs=length(years)

reps=rep(0,yrs)
# for metis 
for(j in 1:length(years)){
  x=metisData35.[which(metisData35.$year==years[j]),]
  y=as.character(years[j])
  x$myID<-droplevels(x$myID)
  ch=with(x,table(myID,date)) # this is what transforms into a matrix of 0-1
  colnames(ch) <- NULL
  metisData35_l[[j]]$nimbleID <- match(metisData35_l[[j]]$myID, rownames(ch), nomatch = NA_integer_, incomparables = NULL)
  
  # Add dummy occasion before the first real occasion, augment data set and recode that data 
  # to match the codes of the observed states 
  
  #CH.du <- cbind(rep(0, dim(ch)[1]), ch) removed for NEW superpop model
  z <- ch                           # Grab counts
  z[z > 1] <- 1                             # Counts to det-nondetections
  ever.observed <- apply(z, 1, max)
  C <- sum(apply(z, 1, max)) ; print(C)     # Number of observed ID
  table(apply(z, 1, sum))                   # Capture-frequencies
  yy <- as.matrix(z[ever.observed == 1,]) # Detection histories
  # Augment metisData set
  nz = 500
  CH.ms <- rbind(ch, matrix(0, ncol = dim(ch)[2], nrow = nz))
  
  # Recode CH matrix: a 0 is not allowed in WinBUGS!
  #CH.ms[CH.ms==0] <- 2    
  name=paste("metisData35.",y, sep="")
  assign(name, CH.ms, inherits=T)	
  print(dim(CH.ms))
}

#rm(pup.data35,CH.ms,x)



years=levels(as.factor(unique(bicData35.$year)))
yrs=length(years)

# for bic
for(j in 1:length(years)){
  x=bicData35.[which(bicData35.$year==years[j]),]
  y=as.character(years[j])
  x$myID<-droplevels(x$myID)
  ch=with(x,table(myID,date))
  colnames(ch) <- NULL
  bicData35_l[[j]]$nimbleID <- match(bicData35_l[[j]]$myID, rownames(ch), nomatch = NA_integer_, incomparables = NULL)
  # Add dummy occasion before the first real occasion, augment data set and recode that data
  # to match the codes of the observed states
 # CH.du <- cbind(rep(0, dim(ch)[1]), ch)
  z <- ch                           # Grab counts
  z[z > 1] <- 1                             # Counts to det-nondetections
  ever.observed <- apply(z, 1, max)
  C <- sum(apply(z, 1, max)) ; print(C)     # Number of observed ID
  table(apply(z, 1, sum))                   # Capture-frequencies
  yy <- as.matrix(z[ever.observed == 1,]) # Detection histories
  # Augment bicData set
  nz = 500
  CH.ms <- rbind(ch, matrix(0, ncol = dim(ch)[2], nrow = nz))
  # Recode CH matrix: a 0 is not allowed in WinBUGS!
 # CH.ms[CH.ms==0] <- 2
  name=paste("bicData35.",y, sep="")
  assign(name, CH.ms, inherits=T)
  print(dim(CH.ms))
}

rm(CH.ms,x, pvData03)



# create nimble ID to match with mass and growth curve --------------------
# bicData35_l[["1998"]]$nimbleID <- match(bicData35_l[["1998"]]$myID, rownames(bicData35.1998), nomatch = NA_integer_, incomparables = NULL)
# done in loop 

getwd()
#save(list = ls(), file = "/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Data/Mine/20210210_dataAugBySite.RData") # by site 
#save(list = ls(), file = "Data/Mine/20210608_cmr_pup35.RData") # by site for pup less than 35 kg, males and females 
#save(list = ls(), file = "Data/Mine/20211025_cmr_pup35.RData") # by site for pup less than 35 kg, males and females 



