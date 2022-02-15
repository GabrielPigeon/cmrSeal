# script for estimating preweaning survival in harbour seal using a cjs model
# code adapted from Kéry and Schaub 2012.
library(tidyverse)
library(nimble)
library(coda)
library(boot)

# mydat for all yrs -------------------------------------------------------
load("~/projects/def-pelleti2/renl2702/phoques/20211031_cmr_pup35.RData")
# load(
#     "/Users/LimoilouARenaud/Documents/PostDocI/Phoque/Data/Mine/20211031_cmr_pup35.RData"
# )
