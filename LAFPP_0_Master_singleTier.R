
gc()

# Tier_select <- "t1"

#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************

# Plan information
# source("LAFPP_Data_RP2000.R")
 source("LAFPP_Data_PlanInfo.R")
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


#**********************************************
##   Modify initial data ####
#**********************************************

## Exclude selected type(s) of initial members
 # init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees_all %<>% mutate(nretirees = 0)
 # init_beneficiaries_all %<>% mutate(nbeneficiaries = 0)
 # init_terminated_all %<>% mutate(nterm = 0)


## Exclude initial terms with ea < 20: Data_population, line 504 
 # init_terminated_all %<>% filter(age.term >= Global_paramlist$min.ea,
 #                                 ea >= Global_paramlist$min.ea)


# ## Exclude the initial amortization basis when testing the program.
# if(!paramlist$useAVamort) init_amort_raw %<>% mutate(amount.annual = 0) 
# 
# ## Exclude the external fund. (currently only STIP borrowing)
# if(!paramlist$useExtFund) extFund %<>% mutate_each(funs(. * 0), -year)


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

salary  <- get_salary_proc(Tier_select)
benefit <- get_benefit_tier(Tier_select)
init_pop <- get_initPop_tier(Tier_select)

if(Tier_select == "t6"){
  entrants_dist  <- get_entrantsDist_tier("t6")} else  
  entrants_dist  <- numeric(length(paramlist$range_ea))


bfactor %<>% select(yos, matches(Tier_select)) %>% 
             rename_("bfactor" = paste0("bf.", Tier_select))



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






#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("LAFPP_Model_AggLiab.R")
gc()

AggLiab <- get_AggLiab(Tier_select,
                       liab,
                       liab.ca,
                       pop) 


#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("LAFPP_Model_Sim.R")
penSim_results <- run_sim(Tier_select, AggLiab)




#*********************************************************************************************************
# 7  Showing results ####
#*********************************************************************************************************


var_display <- c("Tier", "sim", "year", "FR", "MA", "AL", "AL.la", "AL.ca", 
                 #"AL.act", "AL.act.laca", "AL.act.v", "AL.act.LSC", "AL.la", "AL.ca", "AL.term", 
                 #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
                 "B", "B.la", "B.ca", "B.v", 
                 "nactives", "nterms", "PR", "NC_PR", "NC")


penSim_results %>% filter(sim == -1) %>% select(one_of(var_display)) %>% print
#penSim_results %>% filter(sim == -1) %>% data.frame






#*********************************************************************************************************
# Detecitve work: term rates ####
#*********************************************************************************************************
# The AL of actives becomes even higher when higher term rates are used. 

# detective.t13 <- penSim_results
# save(detective.t13, file= "detective.t13.RData")
# 
# load("detective.t13.RData")
# detective.t13 %>% filter(sim == -1) %>% select(Tier,year, FR, MA, AL, AL.act,AL.act.laca, AL.act.v,AL.act.LSC, AL.la, AL.ca, AL.term, AL, PVFB.laca, PVFB.LSC, PVFB.v, PVFB, 
#                         B, B.la, B.ca, B.LSC,B.v, nactives, nterms, PR, NC_PR) %>% data.frame











