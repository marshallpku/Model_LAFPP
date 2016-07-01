# Run control file of the LAFPP model

rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
# library(xlsx)
library("btools")

source("Functions.R")


# Road map for model files (all with suffix "LAFPP_" and extension ".R")
 
 # Master files:
   # Master_singleTier
   # Master_allTiers

 # Data_RP2000
 # Data_PlanInfo 
 # Data_ImportMemberData

 # Model_decrements
 # Model_InvReturns
 # Model_PrepData
 # Model_Demographics
 # Model_ContingentAnnuity
 # Model_IndivLiab
 # Model_AggLiab
 # Model_Sim


# Notes on service retirement
 # initial service retirees are currently (June27, 2016) modeled as life annuitants. 
 #

# Notes on DROP
  # Major concerns 
   # effect on new entrants
   # effect on EEC
   # DROP participants in initial actives 

   # 1. DROP participants are treated as retirees in the model, they do not affect the determination of new entrants. 
   # 2. Plan to model DROP participants' payroll and EEC, which are not neglectible quantitatively. 
   # 3. When assuming DROP participants are retirees, the fund pays benefits to DROP accounts. 
  

# Notes on benefit for death before retirement


# Notes on disability benefits
# As of June26 2016
  # all disabilities are assumed as service connected (90% in the AV)
  # disability rate not applied to members eligible to DROP
  # Disability are modeled as life annuity. 
  # Plan to apply an adjustment factor to mortality after disability mortality 
  # # of disabled at age min.age(20) must be 0.
# AS of June30 2016
  # Model disability benefit as contingent annuity. 
  # Modeling method is the same as that for contingent annuity for service retirement benefit. 
  # For LAFPP, benefits for QSSs of disability retirees are simplified as a fixed proportion of disability retirees' benefit. 






# 0. Parameters   ####
#*********************************************************************************************************

Global_paramlist <- list(
  
  init.year = 2015,
  nyear     = 30,
  nsim      = 5,
  ncore     = 4,
  
  min.ea    = 20,
  max.ea    = 64, # Retirement rate is 100% at age 65 
  
  min.age   = 20,
  max.age   = 120 
)


paramlist <- list(
  
  runname = "LAFPP",
  #Tier_select = "t76",
  simTiers = "joint",
  useAVamort  = F, 
  useExtFund  = F,
  
  Grouping    = "fillin",
  
  r.min  = 41, # this is not required age of retirement benefit. 
  r.max  = 65, 
  
  #fasyears = 3,
  #cola     = 0.03,
  i = 0.075,
  
  infl = 0.0325,
  prod = 0.01,
  s.year = 7,
  s.lower  = 0.6,  # AVA is adjusted to be within 40% of MVA:
  s.upper  = 1.4,
  
  m = 20,
  
  r.full = 50, # age at which vested terms are assumed to retire(Temp, should use r.vben)
  r.vben = 50, # age at which vested terms are assumed to retire.
  
  #r.yos  = 5,
  #v.yos  = 5, 
  #r.age
  
  startingSal_growth = 0.038,
  w.salgrowth.method =  "simple", # "simple" or "withInit"
  
  actuarial_method = "EAN.CP",
  
  
  wf_growth = 0,
  no_entrance = "F",
  newEnt_byTier = c(t1 = 0, t2 = 0, t3 = 0, t4 = 0, t5 = 0, t6 = 1),
  #entrants_dist = rep(1/length(range_ea), length(range_ea)),
  
  pct.ca.M =  0.8, # proportion of males who opt for ca upon retirement
  pct.ca.F =  0.6,
  
  #factor.ca = 0.25,
  
  # Investment returns
  seed = 1234,
  ir.mean = 0.075,
  ir.sd   = 0.12,
  
  
  init_MA = "AL_pct",
  MA_0_pct = 0.8069,
  init_EAA = "MA",
  
  
  smooth_method = "method1",
  salgrowth_amort = 0,
  amort_method = "cp",
  amort_type = "closed",
  nonNegC = "FALSE",
  EEC_fixed = "TRUE",
  ConPolicy = "ADC",
  EEC_rate = 0.05
)

# Parameters derived from the parameter list above. 
paramlist$range_ea = with(Global_paramlist, min.ea:max.ea)
paramlist$range_age = with(Global_paramlist, min.age:max.age)
paramlist$range_age.r = with(paramlist, r.min:r.max)
# paramlist$m.max = with(paramlist, max(m.UAAL0, m.UAAL1, m.surplus0, m.surplus1))
paramlist$v     = with(paramlist, 1/(1 + i))




## Assign parameters to the global environment
   # assign_parmsList(Global_paramlist, envir = environment())
   # assign_parmsList(paramlist,        envir = environment())  






#  Run all tiers ####
#*********************************************************************************************************

# paramlist$simTiers <- "joint"  # "joint"(defult) or "separate"
# source("LAFPP_0_Master_allTiers.R")

 
 

# Run a single tier ####
#*********************************************************************************************************

# Only useful for the purposes of checking model consistency and looking at liability/benefit dynamics.
# External funding, initial amort payments are not available for single tiers. 


# When running this, initial amortization payments must be set to zero. And results are meaningful only when intial FR = 100, 
# since the inital UAAL will not be amortized.(amort basis of the first year is overriden by the values from AV2015, which is set to zero here.)
# (Already solved.)

 Tier_select <- "t5"
# source("LAFPP_0_Master_singleTier.R")




   
   
# Checking the importance of death benefit

#Tier_select <- "t5"
#source("Test_0_Master_singleTier.R")












