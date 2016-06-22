# Checking the importance of death benefit and disability benefit in LAFPP. (Death before retirement)

# Approach
 # Comparing liability, PVFB, and benefit of service retirement and death benefit for 1 unit of actives 


# Death benefit provision
# (a) ALL are assumed service connected;
# (b) IF service connected, 75% of FAS to spouse; 
# (c) all members are assumed married, spouses at the same age. 

# Service retirement provision
# (a) benefit paid in life annuity (no surviving spouses)

# Other assumptions:
# Mortaity is the same for actives and retirees. 






decrement.model_ = decrement.model
salary_          = salary
benefit_         = benefit
bfactor_         = bfactor
init_terms_all_ = init_terms_all # get_tierData(init_terms_all, Tier_select)
Tier_select_     = "t5"
mortality.post.model_ = mortality.post.model
liab.ca_         = liab.ca
paramlist_       =  paramlist
Global_paramlist_ =  Global_paramlist


assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(paramlist_,        envir = environment())

# Choosing tier specific parameters and data
fasyears <- tier.param[Tier_select_, "fasyears"]
r.vben   <- tier.param[Tier_select_, "r.vben"]
r.yos    <- tier.param[Tier_select_, "r.yos"]
r.age    <- tier.param[Tier_select_, "r.age"]
v.yos    <- tier.param[Tier_select_, "v.yos"]
cola     <- tier.param[Tier_select_, "cola"]

init_terminated_ <-  get_tierData(init_terms_all_, Tier_select_)



min.year <- min(init.year - (max.age - (r.max - 1)), init.year - (r.max - 1 - min.ea))
## Track down to the year that is the smaller one of the two below: 
# the year a 120-year-old retiree in year 1 entered the workforce at age r.max - 1 (remeber ea = r.max - 1 is assigned to all inital retirees)
# the year a r.max year old active in year 1 enter the workforce at age min.ea 

# liab.ca %>% filter(age == age.r) %>% select(age, liab.ca.sum.1)


liab.active <- expand.grid(start.year = init.year , 
                           ea = c(20, 30, 40), age = range_age) %>%
  filter(start.year + max.age - ea >= init.year, age >= ea) %>%  # drop redundant combinations of start.year and ea. (delet those who never reach year 1.) 
  mutate(year = start.year + age - ea) %>%  # year index in the simulation)
  arrange(start.year, ea, age) %>% 
  left_join(salary_) %>%
  # left_join(.benefit) %>% # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)
  left_join(decrement.model_) %>% 
  left_join(bfactor_) %>%
  left_join(mortality.post.model_ %>% filter(age == age.r) %>% select(age, ax.r.W)) %>%
  # left_join(liab.ca_ %>% filter(age == age.r) %>% select(age, liab.ca.sum.1)) %>% 
  group_by(start.year, ea) %>%

  # Calculate salary and benefits
  mutate(
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
    yos= age - min(age),                               # years of service
    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    COLA.scale = (1 + cola)^(age - min(age)),     # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    Bx = na2zero(bfactor * fas),                  # accrued benefits, note that only Bx for ages above r.min are necessary under EAN.
    bx = lead(Bx) - Bx,                           # benefit accrual at age x
    
    
    ax = get_tla(pxm.pre, i, COLA.scale),                  # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    # ax.r = get_tla(pxm.r, i, COLA.scale),              # ax calculated with mortality table for retirees. 
    
    
    axR = c(get_tla(pxT[age < r.max], i), rep(0, max.age - r.max + 1)),                        # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < r.max], i, sx[age < r.max]), rep(0, max.age - r.max + 1)),       # ^s_aT..{x:r.max-x-|}
    
    #   axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
    #   axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
    axr = ifelse(ea >= r.vben, 0, c(get_tla(pxT[age < r.vben], i), rep(0, max.age - r.vben + 1))),                 # Similar to axR, but based on r.vben.  For calculation of term benefits when costs are spread up to r.vben.        
    axrs= ifelse(ea >= r.vben, 0, c(get_tla(pxT[age < r.vben], i, sx[age<r.vben]), rep(0, max.age - r.vben + 1))),  # Similar to axRs, but based on r.vben. For calculation of term benefits when costs are spread up to r.vben.
    
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, max.age - r.max)),                     # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, max.age - r.max))   # need to make up the length of the vector up to age max.age
  )


#*************************************************************************************************************
#                        ALs and NCs of life annuity and contingent annuity for actives                #####                  
#*************************************************************************************************************

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.laca = ifelse(yos >= r.yos & age >= r.age, 1, 0),
          # gx.laca = 0,
          Bx.laca  = gx.laca * Bx,  # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
          TCx.la   = lead(Bx.laca) * qxr * lead(ax.r.W) * v,         # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
          TCx.ca   = 0, # Assume no contingent annuitants    lead(Bx.laca) * qxr.ca * lead(liab.ca.sum.1) * v,  # term cost of contingent annuity at the internal retirement age x (start to claim benefit at age x + 1)
          TCx.laca = TCx.la + TCx.ca,
          
          # TCx.r = Bx.r * qxr.a * ax,
          PVFBx.laca  = c(get_PVFB(pxT[age <= r.max], v, TCx.laca[age <= r.max]), rep(0, max.age - r.max)),
          
          ## NC and AL of UC
          # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
          # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          
          # # NC and AL of PUC
          # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
          # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
          # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
          
          # NC and AL of EAN.CD
          NCx.EAN.CD.laca = ifelse(age < r.max, PVFBx.laca[age == min(age)]/ayx[age == r.max], 0),
          ALx.EAN.CD.laca = PVFBx.laca - NCx.EAN.CD.laca * axR,
          
          # NC and AL of EAN.CP
          NCx.EAN.CP.laca   = ifelse(age < r.max, sx * PVFBx.laca[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
          PVFNC.EAN.CP.laca = NCx.EAN.CP.laca * axRs,
          ALx.EAN.CP.laca   = PVFBx.laca - PVFNC.EAN.CP.laca
  ) 


liab.active %>% select(year, age, ea, ALx.EAN.CP.laca, NCx.EAN.CP.laca)



#*************************************************************************************************************
#                        ALs and NCs of death benefit               #####                  
#*************************************************************************************************************

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.death = 1, 
          # gx.death = 0,
          Bx.death  = gx.death * fas * 0.75,  # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
          TCx.death   = lead(Bx.death) * qxm.pre * lead(ax) * v,         # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
          
          # TCx.r = Bx.r * qxr.a * ax,
          PVFBx.death  = c(get_PVFB(pxT[age <= r.max], v, TCx.death[age <= r.max]), rep(0, max.age - r.max)),

                    
          # NC and AL of EAN.CD
          NCx.EAN.CD.death = ifelse(age < r.max, PVFBx.death[age == min(age)]/ayx[age == r.max], 0),
          ALx.EAN.CD.death = PVFBx.death - NCx.EAN.CD.death * axR,
          
          # NC and AL of EAN.CP
          NCx.EAN.CP.death   = ifelse(age < r.max, sx * PVFBx.death[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
          PVFNC.EAN.CP.death = NCx.EAN.CP.death * axRs,
          ALx.EAN.CP.death   = PVFBx.death - PVFNC.EAN.CP.death
  )





#*************************************************************************************************************
#                        ALs and NCs of disability benefit               #####                  
#*************************************************************************************************************

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.disb = 1, 
          # gx.disb = 0,
          Bx.disb  = gx.disb * fas * ifelse(yos < 20, 0.55,
                                     ifelse(yos > 30, 0.75, 0.65)),  # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
          TCx.disb   = lead(Bx.disb) * qxd * lead(ax) * v,         # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
          
          # TCx.r = Bx.r * qxr.a * ax,
          PVFBx.disb  = c(get_PVFB(pxT[age <= r.max], v, TCx.disb[age <= r.max]), rep(0, max.age - r.max)),
          
          
          # NC and AL of EAN.CD
          NCx.EAN.CD.disb = ifelse(age < r.max, PVFBx.disb[age == min(age)]/ayx[age == r.max], 0),
          ALx.EAN.CD.disb = PVFBx.disb - NCx.EAN.CD.disb * axR,
          
          # NC and AL of EAN.CP
          NCx.EAN.CP.disb   = ifelse(age < r.max, sx * PVFBx.disb[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
          PVFNC.EAN.CP.disb = NCx.EAN.CP.disb * axRs,
          ALx.EAN.CP.disb   = PVFBx.disb - PVFNC.EAN.CP.disb
  )



Comparison <- 
liab.active %>% select(year,ea, age, 
                       ALx.EAN.CP.laca, NCx.EAN.CP.laca, PVFBx.laca, 
                       ALx.EAN.CP.death, NCx.EAN.CP.death, PVFBx.death,
                       ALx.EAN.CP.disb, NCx.EAN.CP.disb, PVFBx.disb) %>% 
  mutate(pct.AL_death = 100 * ALx.EAN.CP.death / ALx.EAN.CP.laca,
         pct.NC_death = 100 * NCx.EAN.CP.death / NCx.EAN.CP.laca,
         pct.AL_disb  = 100 * ALx.EAN.CP.disb / ALx.EAN.CP.laca,
         pct.NC_disb  = 100 * NCx.EAN.CP.disb / NCx.EAN.CP.laca) %>% 
  filter(age %in% seq(20,60, 5))


# AL, NC of death benefit and disability benefit as % of service retirement benefit.   
Comparison %>% select(ea, age, starts_with("pct")) %>% print


# Notes
 #1. ALs of death benefit are around 1%~4% of retirement benefit, depending on ea and age. 
 #2. NCs of death benefit are around 6% of retirement benefit for ea less than 30 and 11% for ea of 40. 
 
 #3. ALs of disability benefit are around 5%~10% of retirement benefit for ea less than 30, and 6%~15% for ea of 40. 
 #4. NCs of disability benefit are around 10%~12% of retirement benefit for ea less than 30, and 25% for ea of 40. 

# Conclusion:
 # Death and disability benefits are worth modeling for LAFPP.
 # Adding death and disability benefits are expected to increase normal cost by around 15%~20%, and AL by 6%~12%, which are not negligible. 























