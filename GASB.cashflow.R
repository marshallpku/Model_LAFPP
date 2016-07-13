



file_planInfo <- "Data_inputs/LAFPP_PlanInfo.xlsx"
GASB_cashflow <- read_ExcelRange(file_planInfo, sheet = "GASB_cashflow", "B2", "B3", colTypes="numeric")


load("Results/results_sumTiers_RS1.closed_July12.RData")



AV.values <- data.frame(year = 2015, MA.AV = 17346554076, AL.AV = 18337507075,  C.AV = 396943582 + 181541919, 
                        B.AV = 8122 * 5822*12 + 2031*4628*12 + 2440 * 4166 *12 )  
AV.values

cashflow_comparison <- data.frame(year = 2015:2086) %>%
  left_join(AV.values %>% mutate_each(funs(./1e6), -year)) %>% 
  left_join(GASB_cashflow) %>% 
  left_join(penSim_results.sumTiers %>% filter(sim ==0) %>% 
              select(year, MA.model = MA, AL.model = AL, C.model = C, B.model = B, I.r.Model = I.r) %>% 
              mutate_each(funs(./1e6), -year)) %>%
  mutate(ExF_MA.GASB = 100 * (C.GASB - B.GASB)/MA.GASB,
         ExF_MA.Model= 100 * (C.model - B.model)/MA.model,
         ExF_MA.AV   = 100 * (C.AV - B.AV)/MA.AV )

cashflow_comparison

save(cashflow_comparison, file = "Results/cashflow_comparison.RData")

