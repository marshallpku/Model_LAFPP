# This master file is for simulating demographic distributions of non-actives in LAFPP.
# The simulated distributions will be used to infer the demographic distributions of initial non-actives

# Simulation period.
  # 80 years

# Distribution of new entrants:
  # Based on actives in Tier 6 with low yos

# All other plan provisions, assumptions are based on tier 2
  # Most current retirees should be from Tier 2(effective in 1967-1980)



gc()

Tier_select <- "t2"

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

list.decrements <- get_decrements(Tier_select)
decrement.model      <- list.decrements$decrement.model
mortality.post.model <- list.decrements$mortality.post.model



#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("LAFPP_Model_InvReturns.R")
i.r <- gen_returns()
#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.2 Create plan data ####
#*********************************************************************************************************

source("LAFPP_Model_PrepData.R")

salary       <- get_salary_proc(Tier_select)
benefit      <- get_benefit_tier(Tier_select)
benefit.disb <- get_benefit.disb_tier(Tier_select)

entrants_dist  <- get_entrantsDist_tier("t6")

bfactor %<>% select(yos, matches(Tier_select)) %>% 
             rename_("bfactor" = paste0("bf.", Tier_select))



## Exclude selected type(s) of initial members
#init_actives_all %<>% mutate(nactives = 0) 
init_retirees_all %<>% mutate(nretirees = 0)
init_beneficiaries_all %<>% mutate(nbeneficiaries = 0)
init_terminated_all %<>% mutate(nterm = 0)
init_disb_all %<>% mutate(ndisb = 0)

init_pop     <- get_initPop_tier(Tier_select)




#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************
source("LAFPP_Model_Demographics.R")
gc()
pop <- get_Population()




#*********************************************************************************************************
# 3. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("LAFPP_Model_ContingentAnnuity.R")
liab.ca <- get_contingentAnnuity(Tier_select, apply_reduction = FALSE)



#*********************************************************************************************************
# 4. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("LAFPP_Model_IndivLiab.R")
gc()


liab <- get_indivLab(Tier_select)







# Examine demographic data

# life annuitants
demo.la <- pop$la %>% filter(year %in% 2015:max(year))

demo.la %>% group_by(year) %>% 
  summarize(avg.age = sum(age * number.la, na.rm = T)/sum(number.la, na.rm = T),
            nret    = 1000 * sum(number.la, na.rm = T))


# disabled 
demo.disb <- pop$disb %>% filter(year %in% 2015:max(year))

demo.disb %>% group_by(year) %>% 
  summarize(avg.age = sum(age * number.disb, na.rm = T)/sum(number.disb, na.rm = T),
            ndisb   = 1000 * sum(number.disb, na.rm = T))






# Target measures

# Retirees 
 # average age of retirees in all tiers 
 (4559*74.5 + 3046 * 63.7 + 227*60.3 + 202*54.3 + 77*86.1 + 11*59.6)/(4559 + 3046 + 227 + 202 + 77 + 11)
 # 69.64
 # average benefit 
 (77*2349 + 4559*5025 + 227*2888 + 202*4745 + 3046*7391 + 11*6397)/(77 + 4559 + 227 + 202 + 3046 + 11)
 # 5822.1

# Disabled
 # average age
(75*82   + 1540*73.2 + 249*56.1 + 45*53.4 + 120*50.9 + 2*50.1)/(75 + 1540 + 249 + 45 + 120 + 2)
 #69.65
 # average benefit
(75*3108 + 1540*4875 + 249*3522 + 45*4525 + 120*4745 + 2*4914)/(75 + 1540 + 249 + 45 + 120 + 2)
 #4628.5

# Beneficiaries
 # average age
(292*84   + 1876*78.6 + 83*53   + 202*35.2 + 185*54.5 )/(292 + 1876 + 83 + 202 + 185)
 #73.37
 # average benefit
(292*2584 + 1876*4288 + 83*3880 + 202*6803 + 185*5504 )/(292 + 1876 + 83 + 202 + 185)
 #4364.4





demo.active <- pop$active %>% filter(year == max(year))

demo.active %>% spread(age, number.a)



demo.active %>% group_by(year, age) %>% 
  summarize(nret = sum(number.a, na.rm = T))

pop$active %>% filter(year == 2085) %>% summarize(n = sum(number.a))


# #*********************************************************************************************************
# # 5. Aggregate actuarial liabilities, normal costs and benenfits ####
# #*********************************************************************************************************
# source("LAFPP_Model_AggLiab.R")
# gc()
# 
# AggLiab <- get_AggLiab(Tier_select,
#                        liab,
#                        liab.ca,
#                        pop) 
# 
# 























# #*********************************************************************************************************
# # 6.  Simulation ####
# #*********************************************************************************************************
# source("LAFPP_Model_Sim.R")
# penSim_results <- run_sim(Tier_select, AggLiab)
# 
# 
# 
# 
# #*********************************************************************************************************
# # 7  Showing results ####
# #*********************************************************************************************************
# 
# 
# var_display <- c("Tier", "sim", "year", "FR", "MA", "AL", "AL.la", "AL.ca", 
#                  #"AL.act", "AL.act.laca", "AL.act.v", "AL.act.LSC", "AL.la", "AL.ca", "AL.term", 
#                  #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
#                  "B", "B.la", "B.ca", "B.death", 
#                  "nactives", "nterms", "PR", "NC_PR", "NC")
# 
# 
# penSim_results %>% filter(sim == -1) %>% select(one_of(var_display)) %>% print
# #penSim_results %>% filter(sim == -1) %>% data.frame
# 












