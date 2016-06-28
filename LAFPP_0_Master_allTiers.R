
gc()

#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************

# Plan information
# source("LAFPP_Data_RP2000.R")
# source("LAFPP_Data_PlanInfo.R")
# source("LAFPP_Data_ImportMemberData.R")

load("Data_inputs/LAFPP_PlanInfo.RData")    # for all tiers
load("Data_inputs/LAFPP_MemberData.RData")  # for all tiers



#*********************************************************************************************************
# 1.2 Create decrement tables ####
#*********************************************************************************************************

# Decrement tables
source("LAFPP_Model_Decrements.R")

list.decrements.t1 <- get_decrements("t1")
list.decrements.t2 <- get_decrements("t2")
list.decrements.t3 <- get_decrements("t3")
list.decrements.t4 <- get_decrements("t4")
list.decrements.t5 <- get_decrements("t5")
list.decrements.t6 <- get_decrements("t6")

decrement.model.t1      <- list.decrements.t1$decrement.model
mortality.post.model.t1 <- list.decrements.t1$mortality.post.model

decrement.model.t2      <- list.decrements.t2$decrement.model
mortality.post.model.t2 <- list.decrements.t2$mortality.post.model

decrement.model.t3      <- list.decrements.t3$decrement.model
mortality.post.model.t3 <- list.decrements.t3$mortality.post.model


decrement.model.t4      <- list.decrements.t4$decrement.model
mortality.post.model.t4 <- list.decrements.t4$mortality.post.model

decrement.model.t5      <- list.decrements.t5$decrement.model
mortality.post.model.t5 <- list.decrements.t5$mortality.post.model

decrement.model.t6      <- list.decrements.t6$decrement.model
mortality.post.model.t6 <- list.decrements.t6$mortality.post.model



#*****************************************************
##   Calibration and Modification of initial data ####
#*****************************************************

## Exclude selected type(s) of initial members
 # init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees_all %<>% mutate(nretirees = 0)
 # init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
 # init_terminated_all %<>% mutate(nterm = 0)


## Exclude initial terms with ea < 20: Data_population, line 504
 # init_terminated_all %<>% filter(age.term >= Global_paramlist$min.ea,
 #                                 ea >= Global_paramlist$min.ea)


# ## Exclude the initial amortization basis when testing the program.
#  if(!paramlist$useAVamort)  init_amort_raw %<>% mutate(amount.annual = 0) # CAUTION: For consistency check only; will make initial UAAL not amortized. 
# 
# 
# ## Exclude the external fund. (currently only STIP borrowing)
#  if(!paramlist$useExtFund) extFund %<>% mutate_each(funs(. * 0), -year)


## Matching Segal cash flow

# Matching Segal payroll 
  # Payroll from model:
    # Total: 11040551k
    # t76:   9094102199
    # t13:   1279359295 
    # tm13:   667089761   
  # Goal: 9659652k (from Segal projection and AV 2015) 
  
  # # Method: Applying adjustment factors to initial population and initial salary.
  #   # Adjustment factor for initial workforce, applied to all 3 tiers (NEXT STEP? apply only to t76 tier.)
  #     # Payroll of none-lab seg / Payroll of all segs(unit： $k): 
  #       f1 <- 9659652 / 9927833 # 0.972987  
  #   # Adjustment factor for initial salary
  #     # payroll from vii table / payroll from model (both for all segs, unit $k):
  #       f2 <- 9927833 / 11040551 # 0.8992154
  #   # Total adjustment factor is 
  #     # f1 * f2 = 0.8749248
  # 
  # # Adjusting initial workforce and salary:
  #   init_actives_all %<>% mutate(nactives = nactives * f1,
  #                                salary   = salary   * f2) 
  # 


#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("LAFPP_Model_InvReturns.R")
i.r <- gen_returns()
#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.4 Create plan data ####
#*********************************************************************************************************
source("LAFPP_Model_PrepData.R")

# Create data for each tier

salary.t1  <- get_salary_proc("t1")
salary.t2  <- get_salary_proc("t2")
salary.t3  <- get_salary_proc("t3")
salary.t4  <- get_salary_proc("t4")
salary.t5  <- get_salary_proc("t5")
salary.t6  <- get_salary_proc("t6")

benefit.t1 <- get_benefit_tier("t1")
benefit.t2 <- get_benefit_tier("t2")
benefit.t3 <- get_benefit_tier("t3")
benefit.t4 <- get_benefit_tier("t4")
benefit.t5 <- get_benefit_tier("t5")
benefit.t6 <- get_benefit_tier("t6")

init_pop.t1 <- get_initPop_tier("t1")
init_pop.t2 <- get_initPop_tier("t2")
init_pop.t3 <- get_initPop_tier("t3")
init_pop.t4 <- get_initPop_tier("t4")
init_pop.t5 <- get_initPop_tier("t5")
init_pop.t6 <- get_initPop_tier("t6")


entrants_dist.t1  <- numeric(length(paramlist$range_ea))
entrants_dist.t2  <- numeric(length(paramlist$range_ea))
entrants_dist.t3  <- numeric(length(paramlist$range_ea))
entrants_dist.t4  <- numeric(length(paramlist$range_ea))
entrants_dist.t5  <- numeric(length(paramlist$range_ea))
entrants_dist.t6  <- get_entrantsDist_tier("t6")


get_tier.bfactor <- function(Tier_select_) bfactor %<>% select(yos, matches(Tier_select_)) %>%  rename_("bfactor" = paste0("bf.", Tier_select_))
bfactor.t1 <- get_tier.bfactor("t1")
bfactor.t2 <- get_tier.bfactor("t2")
bfactor.t3 <- get_tier.bfactor("t3")
bfactor.t4 <- get_tier.bfactor("t4")
bfactor.t5 <- get_tier.bfactor("t5")
bfactor.t6 <- get_tier.bfactor("t6")




# # Chnange variable names
# make_tierDec <- function(Tier_select_, df = decrement.ucrp){
#   df %<>% rename_("pxT" = paste0("pxT.", Tier_select_),
#                   "qxr.la"   = paste0("qxr.la.", Tier_select_),
#                   "qxr.ca"   = paste0("qxr.ca.", Tier_select_),
#                   "qxr.LSC"  = paste0("qxr.LSC.", Tier_select_),
#                   "qxr"      = paste0("qxr.", Tier_select_),
#                   "qxt"      = paste0("qxt.", Tier_select_))
#   }
# decrement.ucrp.t76  <- make_tierDec("t76")
# decrement.ucrp.t13  <- make_tierDec("t13")
# decrement.ucrp.tm13 <- make_tierDec("tm13")



#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************
source("LAFPP_Model_Demographics_allTiers.R")
pop <- get_Population_allTiers_LAFPP()
gc()



#*********************************************************************************************************
# 4. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("LAFPP_Model_ContingentAnnuity.R")
liab.ca.t1  <- get_contingentAnnuity("t1", FALSE, decrement.model_ = decrement.model.t1)
liab.ca.t2  <- get_contingentAnnuity("t2", FALSE, decrement.model_ = decrement.model.t2)
liab.ca.t3  <- get_contingentAnnuity("t3", FALSE, decrement.model_ = decrement.model.t3)
liab.ca.t4  <- get_contingentAnnuity("t4", FALSE, decrement.model_ = decrement.model.t4)
liab.ca.t5  <- get_contingentAnnuity("t5", FALSE, decrement.model_ = decrement.model.t5)
liab.ca.t6  <- get_contingentAnnuity("t6", FALSE, decrement.model_ = decrement.model.t6)


#*********************************************************************************************************
# 3. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("LAFPP_Model_IndivLiab.R")
gc()

liab.t1 <- get_indivLab("t1",
                         decrement.model.t1,
                         salary.t1,
                         benefit.t1,
                         bfactor.t1,
                         mortality.post.model.t1,
                         liab.ca.t1)

liab.t2 <- get_indivLab("t2",
                        decrement.model.t2,
                        salary.t2,
                        benefit.t2,
                        bfactor.t2,
                        mortality.post.model.t2,
                        liab.ca.t2)

liab.t3 <- get_indivLab("t3",
                        decrement.model.t3,
                        salary.t3,
                        benefit.t3,
                        bfactor.t3,
                        mortality.post.model.t3,
                        liab.ca.t3)

liab.t4 <- get_indivLab("t4",
                        decrement.model.t4,
                        salary.t4,
                        benefit.t4,
                        bfactor.t4,
                        mortality.post.model.t4,
                        liab.ca.t4)

liab.t5 <- get_indivLab("t5",
                        decrement.model.t5,
                        salary.t5,
                        benefit.t5,
                        bfactor.t5,
                        mortality.post.model.t5,
                        liab.ca.t5)

liab.t6 <- get_indivLab("t6",
                        decrement.model.t6,
                        salary.t6,
                        benefit.t6,
                        bfactor.t6,
                        mortality.post.model.t6,
                        liab.ca.t6)





#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("LAFPP_Model_AggLiab.R")
gc()


AggLiab.t1 <- get_AggLiab("t1",
                          liab.t1,
                          liab.ca.t1,
                          pop$pop.t1,
                          mortality.post.model.t1) 


AggLiab.t2 <- get_AggLiab("t2",
                          liab.t2,
                          liab.ca.t2,
                          pop$pop.t2,
                          mortality.post.model.t2) 


AggLiab.t3 <- get_AggLiab("t3",
                          liab.t3,
                          liab.ca.t3,
                          pop$pop.t3,
                          mortality.post.model.t3) 


AggLiab.t4 <- get_AggLiab("t4",
                          liab.t4,
                          liab.ca.t4,
                          pop$pop.t4,
                          mortality.post.model.t4) 


AggLiab.t5 <- get_AggLiab("t5",
                          liab.t5,
                          liab.ca.t5,
                          pop$pop.t5,
                          mortality.post.model.t5) 


AggLiab.t6 <- get_AggLiab("t6",
                          liab.t6,
                          liab.ca.t6,
                          pop$pop.t6,
                          mortality.post.model.t6) 



AggLiab.sumTiers <- get_AggLiab_sumTiers(AggLiab.t1, AggLiab.t2, AggLiab.t3,
                                         AggLiab.t4, AggLiab.t5, AggLiab.t6)



#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("LAFPP_Model_Sim.R")

if(paramlist$simTiers == "separate"){
  penSim_results.t1  <- run_sim("t1",  AggLiab.t1)
  penSim_results.t2  <- run_sim("t2",  AggLiab.t2)
  penSim_results.t3  <- run_sim("t3",  AggLiab.t3)
  penSim_results.t4  <- run_sim("t4",  AggLiab.t4)
  penSim_results.t5  <- run_sim("t5",  AggLiab.t5)
  penSim_results.t6  <- run_sim("t6",  AggLiab.t6)
}
 
penSim_results.sumTiers <- run_sim("sumTiers", AggLiab.sumTiers)


#*********************************************************************************************************
# 7.1  Showing results: Joint simulation of all tiers ####
#*********************************************************************************************************
# Use full outputs include:
  # penSim_results.sumTiers
  # AggLiab.t76; AggLiab.t13; AggLiab.tm13
# NEXT STEP: extract useful variables from AggLiab.XXX files, so we can still see liability dynamics of each tier
#            when we simulate(do the loop) all tiers jointly. 


var_display <- c("Tier", "sim", "year", "FR", "MA", "AL", 
                 #"AL.act", "AL.act.laca", "AL.act.v", "AL.act.LSC", "AL.la", "AL.ca", "AL.term", 
                 #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
                 "B", "B.la", "B.ca", "B.v", 
                 "nactives", "nterms", "PR", "NC_PR", "NC")


kable(penSim_results.sumTiers %>% filter(sim == -1) %>% select(one_of(var_display)), digits = 2) %>% print 






#*********************************************************************************************************
# 7.1  Showing results: Separate simulations of each tier ####
#*********************************************************************************************************
# Currently for the purpose of checking model consistency. 
# To make sense of separate simulation of each tier, we must allocate initial assets and amortization payments 
# among the tiers. However, we currently lack information for doing this reasonably. 




# var_display <- c("Tier", "sim", "year", "FR", "MA", "AL", 
#                  #"AL.act", "AL.act.laca", "AL.act.v", "AL.act.LSC", "AL.la", "AL.ca", "AL.term", 
#                  #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
#                  "B", "B.la", "B.ca", "B.LSC", "B.v", 
#                  "nactives", "nterms", "PR", "NC_PR")
# 
# penSim_results_byTiers <- bind_rows(penSim_results.t76,
#                                     penSim_results.t13,
#                                     penSim_results.tm13)
# 
# penSim_results_sumTiers <- penSim_results_byTiers %>% 
#   group_by(sim, year) %>% 
#   summarise(MA = sum(MA)/1000,
#             AL = sum(AL)/1000,
#             NC = sum(NC)/1000,
#             PVFB = sum(PVFB)/1000,
#             B = sum(B)/1000,
#             PR = sum(PR)/1000,
#             nactives = sum(nactives)) %>% 
#   mutate(NC_PR = NC/PR * 100,
#          FR = MA/AL * 100)
# 
# 
# penSim_results.t76  %>% filter(sim == -1) %>% select(one_of(var_display)) %>% data.frame
# penSim_results.t13  %>% filter(sim == -1) %>% select(one_of(var_display)) %>% data.frame
# penSim_results.tm13 %>% filter(sim == -1) %>% select(one_of(var_display)) %>% data.frame
# 
# penSim_results_sumTiers %>% filter(sim == -1) 



#write.xlsx2(penSim_results_sumTiers %>% filter(sim == -1), file = "Data/detective_constant_wf.xlsx", sheet = "Total")














