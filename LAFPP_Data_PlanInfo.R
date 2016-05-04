# This script imports plan information from PlanInfo-LAFPP.xlsx

# This script performs necessary transformations (eg. truncation, expansion, imputation) to the original data, 
# but do not create new variables (eg. computing weighted average. )

# Data sources:
  # Data_inputs/LAFPP_mortality.RData
  # Data_inputs/LAFPP_PlanInfo.xlsx

# List of outputs
  # mortality_LAFPP, 
  # retRates, 
  # termRates, 
  # disbRates, 
  # bfactors, 
  # salgrowth
  # tier.param

# Output file:
  # Data_inputs/LAFPP_PlanInfo.RData


#### To do list
# 1. More smoothed imputation of decrements



# plan information file:
file_planInfo <- "Data_inputs/LAFPP_PlanInfo.xlsx"


#*********************************************************************************************************
#                      ## Tools ####
#*********************************************************************************************************

# read_planInfoTable <- function(file, sheet, cellStart, cellEnd, ...){
#   require(XLConnect)
#   range <- xlrange(file, sheet, cellStart, cellEnd)
#   readWorksheetFromFile(file, sheet = sheet, header=TRUE, region=range, ...)
# }




#*********************************************************************************************************
#                      ## Mortality tables ####
#*********************************************************************************************************
# Read mortality table created by LAFPP_Data_RP2000.R.
load("Data_inputs/LAFPP_mortality.RData") # data frame "mortality_LAFPP" loaded
mortality_LAFPP %<>%  select(age,
                            qxm.pre.male, qxm.pre.female,   # mortality for active members
                            qxm.post.male, qxm.post.female, # mortality for retirees and beneficiaries 
                            qxm.d.male, qxm.d.female)       # mortality for disabled



#*********************************************************************************************************
#                      ## Retirement rates  ####
#*********************************************************************************************************
retRates <- read_ExcelRange(file_planInfo, sheet="Ret_dec", "B2", "B3", colTypes="numeric")


#*********************************************************************************************************
#                      ## benefit factors  ####
#*********************************************************************************************************
bfactor <- read_ExcelRange(file_planInfo, sheet="Ret_bfactor", "B2", "B3", colTypes="numeric")




#*********************************************************************************************************
#                      ## Termination rates  ####
#*********************************************************************************************************
# Term rates for yos < 5
termRates1 <- read_ExcelRange(file_planInfo, sheet="Term_dec1", "B2", "B3", colTypes="numeric")
# Term rates for yos >=5 (given every 5 years, need to expand to all ages)
termRates2 <- read_ExcelRange(file_planInfo, sheet="Term_dec2", "B2", "B3", colTypes="numeric") %>% 
              rename(age.match = age)

termRates2 <- data.frame(age = 20:64) %>% 
              mutate(age.match = floor(2*age/10)*10/2) %>% 
              left_join(termRates2) %>% 
              select(-age.match)


termRates <- expand.grid(ea = 20:74, age = 20:64) %>% 
  mutate(yos = age - ea) %>% 
  filter(age >= ea) %>% 
  arrange(ea, age) %>% 
  left_join(termRates1) %>% 
  left_join(termRates2) %>% 
  mutate(qxt.fire = ifelse(yos < 5, qxt.fire.yos, qxt.fire.age),
         qxt.plc  = ifelse(yos < 5, qxt.plc.yos,  qxt.plc.age)) %>% 
  select(age, ea, yos, qxt.fire, qxt.plc)

#termRates %>% arrange(ea, age)


#*********************************************************************************************************
#                      ## disability rates  ####
#*********************************************************************************************************
# Assume disability rates are 0 after age 64.
disbRates <- read_ExcelRange(file_planInfo, sheet="Disb_dec", "B2", "B3", colTypes="numeric") %>% 
             rename(age.match = age)

disbRates <- data.frame(age = 20:64) %>% 
  mutate(age.match = floor(2*age/10)*10/2) %>% 
  left_join(disbRates) %>% 
  select(-age.match)


#*********************************************************************************************************
#                      ## Salary growth rates  ####
#*********************************************************************************************************

AV.infl  <- 0.0325 # Assumed inflation used in salary scale in AV2015 
AV.raise <- 0.0075 # Assumed "across the board" salary increases used in salary scale in AV2015 

salgrowth <- read_ExcelRange(file_planInfo, sheet="SalaryGrowth", "B2", "B3", colTypes="numeric") %>% 
             rename(yos.match = yos)

salgrowth <- data.frame(yos = 0:65) %>% 
  mutate(yos.match = ifelse(yos < 11, yos, 11)) %>% 
  left_join(salgrowth) %>% 
  mutate(salgrowth = salgrowth + AV.infl + AV.raise) %>% 
  select(-yos.match)



#*********************************************************************************************************
#                      ## Tier specific parameters ####
#*********************************************************************************************************

tier.param <- read_ExcelRange(file_planInfo, sheet="Tier.param", colTypes="character") %>% 
  mutate_each(funs(as.numeric), -tier)

row.names(tier.param) <- tier.param$tier

save(mortality_LAFPP, retRates, termRates, disbRates, bfactor, salgrowth, tier.param,
     file  = "Data_inputs/LAFPP_PlanInfo.RData")






