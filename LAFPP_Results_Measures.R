

#### measure of contribution volatility ####
#**********************************************************

# What presures the political system?
# 1. ERC rate rises unexpectedly in a relatively short period of time. This threats budget planning.
# 2. ERC rate rises to a very high level, even through a relatively long period of time, that is unaffordable to the plan sponsor. 
#    This threats the affordability and may cause benefit cuts, tax increase, and crowding out expenditure on other public services.   

# Questions we may want to ask plan sponsor/policymakers:
# How big is the rise in ERC rate in a short period time that can threat budget planning?
# How big is the ERC rate that can threat the affordability?


# General measure of volatility
# - standard devation of year-to-year change in

# Measures of sharp rise of ERC rate in a short time period
# - max 5-year  change in ERC rate in each simulation, median over all simulations
# - max 10-year change in ERC rate in each simulation, median over all simulations
# - max deviation from 5/10 year moving average. For ERC rate and ERC
# - probability(over all simulations) of ERC rate rising by a% in 5/10 years, through year x  


# Measure of high ERC/ERC rate level
# - probability of ERC exceeding 2*NC in any of the years through year x
# - probability of ERC rate exceeding a% in any of the years through year x


# make_lContVol <- function(runname, folderName){
  
  runname <- "sumTiers_RS3"
  folderName <- "Results/"
  fileName <- paste0(folderName,"results_", runname, ".RData") 
  load(fileName)
  
  results.stch <- penSim_results.sumTiers %>% filter(sim > 0)
  
  penSim_results.sumTiers %>% names
  
  
  
  qts <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  # max devation from 5/10 year moving average over 30 years,  
  max.dMA.ERC_PR <- results.stch %>% 
    group_by(sim) %>% 
    mutate(MA5y   = rollapply(ERC_PR, c(1:5, rep(5, length(ERC_PR)-5)),    mean, align = "right", fill = NA),
           MA10y  = rollapply(ERC_PR, c(1:10, rep(10, length(ERC_PR)-10)), mean, align = "right", fill = NA),
           dMA5y  = ERC_PR - MA5y,
           dMA10y = ERC_PR - MA10y
    ) %>% 
    summarise(max.dMA5y  = max(dMA5y, na.rm = T),
              max.dMA10y = max(dMA10y, na.rm = T)) %>% 
    mutate(runname = runname)
  
  
  prob_FR40less.rolling <- results.stch %>% 
    group_by(sim) %>% 
    mutate(FR40less = cumany(FR_MA <= 40)) %>% 
    select(sim, year, FR40less) %>% 
    group_by(year) %>% 
    summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
    mutate(runname = runname)
  prob_FR40less.rolling
  
  
  ## Probability of FR above 95% as of any given year. 
  prob_FR95more.rolling <- results.stch %>% 
    group_by(sim) %>% 
    mutate(FR95more = cumany(FR_MA >= 95)) %>% 
    select(sim, year, FR95more) %>% 
    group_by(year) %>% 
    summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
    mutate(runname = runname)
  prob_FR95more.rolling
  
  
  
  prob_ERCsharpRisePct.rolling <- 
    results.stch %>% 
    group_by(sim) %>% 
    mutate(Chg5y2.pct    = 100 * (ERC_PR / lag(ERC_PR, 4) - 1),
           Chg5y2.pct    = na2zero(Chg5y2.pct),
           Chg50pct5y2   = cumany(Chg5y2.pct >= 50),
           Chg100pct5y2  = cumany(Chg5y2.pct >= 100)) %>% 
    select(sim, year, Chg50pct5y2, Chg100pct5y2) %>% 
    group_by(year) %>% 
    summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
    mutate(runname = runname)
  prob_ERCsharpRisePct.rolling 
  
  
  
  
  prob_ERCsharpRisePts.rolling <- 
    results.stch %>% 
    group_by(sim) %>% 
    mutate(ChgPts5y2 =  ERC_PR - lag(ERC_PR, 4),  # year1-5 change in pct points 
           ChgPts5y2 = na2zero(ChgPts5y2),
           Chg6Pts5y2    = cumany(ChgPts5y2 >= 6), 
           Chg10Pts5y2   = cumany(ChgPts5y2 >= 10),
           Chg15Pts5y2   = cumany(ChgPts5y2 >= 15)) %>% 
    select(sim, year, Chg6Pts5y2, Chg10Pts5y2, Chg15Pts5y2) %>% 
    group_by(year) %>% 
    summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
    mutate(runname = runname)
  prob_ERCsharpRisePts.rolling
  
  
  
  prob_highERC <- results.stch %>% 
    group_by(sim) %>% 
    mutate(ERC_2NC  = cumany(ERC >= 2 * NC),
           ERC_PR30 = cumany(ERC_PR >= 30),
           ADC_3NC  = cumany(ADC >= 3* NC),
           ADC_PR50 = cumany(100*ADC/PR >= 50)) %>% 
    select(sim, year, ERC_2NC, ERC_PR30, ADC_3NC, ADC_PR50) %>%
    group_by(year) %>% 
    summarize_each(funs(100 * sum(., na.rm = T)/n()), -sim) %>% 
    mutate(runname = runname)
  prob_highERC
  
  
  # maxChg.ERC_PR
  # max.dMA.ERC_PR 
  # 
  # prob_chgPct
  # prob_highERC
  
  
  assign(paste0("lContVol_", runname), 
         list( 
           #maxChg.ERC_PR  = maxChg.ERC_PR,   
           max.dMA.ERC_PR = max.dMA.ERC_PR,  
           prob_FR40less.rolling = prob_FR40less.rolling,
           prob_FR95more.rolling = prob_FR95more.rolling,
           prob_ERCsharpRisePct.rolling = prob_ERCsharpRisePct.rolling,
           prob_ERCsharpRisePts.rolling = prob_ERCsharpRisePts.rolling,
           prob_highERC   = prob_highERC)
  )
  
  do.call(save, list(paste0("lContVol_", runname), file=paste0(folderName, "lContVol_", runname, ".RData")))
  
}


runname.list <- runList$runname
folderName <- "FullOverride/Results/"
for(runname in runname.list) make_lContVol(runname, folderName)





lContVol_sumTiers_RS1$prob_FR40less.rolling %>% filter(year <=2044)
lContVol_sumTiers_RS1$prob_ERCsharpRisePts.rolling %>% filter(year <=2044)
lContVol_sumTiers_RS1$prob_highERC %>% filter(year <=2044)

lContVol_sumTiers_RS2$prob_FR40less.rolling
lContVol_sumTiers_RS2$prob_ERCsharpRisePts.rolling
lContVol_sumTiers_RS2$prob_highERC

lContVol_sumTiers_RS3$prob_FR40less.rolling
lContVol_sumTiers_RS3$prob_ERCsharpRisePts.rolling
lContVol_sumTiers_RS3$prob_highERC













