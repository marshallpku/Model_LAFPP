# This script produces example graphs for focus group on Feb 28, 2017.


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
library(zoo)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
library(xlsx)
library("btools")
library("scales")

source("Functions.R")


# With results from average plan 
 # Funded ratio percentile graph
 # ERC rate percentile graph 
 # Probability of FR below 40%
 # Probability of ERC rate rising 5% in 5 years
 # Probability of ERC rate rising above 30% 

# With results form LAFPP 
 # ERC as % of General fund percentile graph
 # Probability of ERC as % of general fund rising 5% in 5 years. 



#*****************************************************
##  Defining paths for inputs and outputs         ####
#*****************************************************
IO_folder_average <- "../Model_Main/IO_M1_new/" 
IO_folder_LAFPP   <- "Results/"
Outputs_folder    <- "Results/Graphs_FG/"



#**********************************************************************************************
##  Defining color and theme for publication format of Rockefeller Institute of Government ####
#**********************************************************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
                 RIG.orange,
                 RIG.purple,
                 RIG.green ,
                 RIG.blue,
                 RIG.yellow.dark)


# demo.shape5 <- c(16, 16, 16, 15, 17) # 16-average, 15-mature, 17-immature 


RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0, size = 9))
}

#*****************************************************
##   average plan         ####
#*****************************************************

load(paste0(IO_folder_average, "Outputs_A1F075_O30pA5.RData"))

df_average <- 
  outputs_list$results %>% filter(sim > 0, year <= 30)  %>% 
  select(runname, sim, year, FR_MA, ERC_PR) %>% 
  group_by(runname, sim) %>% 
  mutate(FR40less  = cumany(FR_MA  <= 40),
         ERC_high  = cumany(ERC_PR >= 30), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(year = year + 2015)




# # Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- NULL
fig_FRdist <- df_average %>% 
  select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25"))
  )) + theme_bw() + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(50,140)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 10)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
                     label  = c("75th percentile", "Median", "25th percentile")) + 
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Funded ratio (%)") + 
  #theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme() + 
  theme(axis.text = element_text(face="bold", size=10)) +
  theme(legend.justification=c(0, 1), legend.position=c(0.01, 0.99),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))

fig_FRdist





# # Distribution of funded ratio 
fig.title <- "Distribution of employer contribution rate"
fig.subtitle <- NULL
fig_ERC_PRdist <- df_average %>% 
  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25"))
  )) + theme_bw() + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,20)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL, 
                     label  = c("75th percentile", "Median", "25th percentile")) + 
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Funded ratio (%)") + 
  #theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme() + 
  theme(axis.text = element_text(face="bold", size=10)) +
  theme(legend.justification=c(0, 1), legend.position=c(0.01, 0.99),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1))

fig_ERC_PRdist


fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- NULL
fig_FR40less <- df_average %>% 
  select(year, FR40less) %>% 
  ggplot(aes(x = year, y = FR40less)) + theme_bw() + 
  geom_point(size = 2, color = RIG.blue) + 
  geom_line(size = 1, color = RIG.blue) + 
  coord_cartesian(ylim = c(0,20)) + 
  scale_y_continuous(breaks = seq(0,200, 2)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()+
  theme(axis.text = element_text(face="bold", size=10))
fig_FR40less

ggsave(file = paste0(Outputs_folder, "fig_FRdist.png"), fig_FRdist, height = 7*0.7, width = 9*0.7)
ggsave(file = paste0(Outputs_folder, "fig_FR40less.png"), fig_FR40less, height = 7*0.7, width = 9*0.7)




fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- NULL
fig_ERC_PRhike <- df_average %>% 
  select(year, ERC_hike) %>% 
  ggplot(aes(x = year, y = ERC_hike)) + theme_bw() + 
  geom_point(size = 2, color = RIG.blue) + 
  geom_line(size = 1, color = RIG.blue) + 
  coord_cartesian(ylim = c(0,20)) + 
  scale_y_continuous(breaks = seq(0,200, 2)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_ERC_PRhike



fig.title <- "Probability of ERC above 30% of payroll \nat any time prior to and including the given year \nunder different funding approaches"
fig.subtitle <- NULL
fig_ERC_PRhigh <- df_average %>% 
  select(year, ERC_high) %>% 
  ggplot(aes(x = year, y = ERC_high)) + theme_bw() + 
  geom_point(size = 2, color = RIG.blue) + 
  geom_line(size = 1, color = RIG.blue) + 
  coord_cartesian(ylim = c(0,20)) + 
  scale_y_continuous(breaks = seq(0,200, 2)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_ERC_PRhigh








#*****************************************************
##  LAFPP  ####
#*****************************************************

#*****************************************************
##  Loading simulation data  
#*****************************************************

## Outputs of pension finance  
get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    
    if("results.t7" %in% names(outputs_list)){
      df_out <- bind_rows(outputs_list$results,
                          outputs_list$results.t7,
                          outputs_list$results.xt7)
      return(df_out)
    } else {
      return(outputs_list$results)
    }
  }
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}

results_all <- get_results(IO_folder_LAFPP, "results_sumTiers_RS") %>% select(runname, Tier, sim, year, everything()) %>% 
  filter(run.policyScn != "FR075")



# Runs used in the report 
runs_RS <- paste0("RS", 1:5)
runs_policy <- c("noCap", "cap", "cap.allTiers")

runs_RS_labels <- c("Expected return = 7.5%", 
                    "Scenario 3: \n5 years of low returns", 
                    "Scenario 4: \n15 years of low returns",
                    "Scenario 5: \nHigh volatility \nreflecting market forecasts", 
                    "Expected return = 6.1%")

runs_policy_labels <- c("without ERC cap", 
                        "ERC cap for new hires", 
                        "ERC cap for all tiers")


results_all %<>% mutate(run.policyScn.lab = factor(run.policyScn, levels = runs_policy, labels = runs_policy_labels),
                        run.returnScn.lab = factor(run.returnScn, levels = runs_RS,     labels = runs_RS_labels),
                        run.policyScn = factor(run.policyScn, levels = runs_policy),
                        run.returnScn = factor(run.returnScn, levels = runs_RS)
) 


#**********************************************************************************************
##  Load revenue data and extend the projection into 2044  
#**********************************************************************************************

# source: City of LA Revenue Outlook, FY 2016-17
df_revenue <- read_ExcelRange("Data_inputs/LAFPP_PlanInfo_2016.xlsx", sheet = "Fiscal")

# extend the projection into 2044 using the projected growth rate of 2.9% in 2020 
rev.growth <- 0.03

df_revenue %<>% 
  mutate(GenFund.proj = 1000 * ifelse(year < 2021, GenFund.original, GenFund.original[year == 2020] * (1 + rev.growth)^(year - 2020))) 

df_revenue


#**********************************************************************************************
##  Fiscal analysis:  
#**********************************************************************************************

# Assumption about the non-pension ERC in the LAFPP system (mainly health subsidy):
# Assume non-pension ERC is 25% of total LAFPP system ERC
share_LAFPP.health <- 0.25

# Assumption about the total contributions of LACERS:
# Assume total LACERS contribution is 85% of total LAFPP system ERC (pension + health)
factor_LACERS <- 0.85 

df_estimates <- results_all %>% 
  filter(run.returnScn %in% paste0("RS", 1:5),run.policyScn == "noCap") %>%
  select(run.returnScn, sim, year, ERC) %>% 
  left_join(df_revenue) %>% 
  mutate(ERC.LAFPP.pension = ERC,
         ERC.LAFPP = ERC / (1-share_LAFPP.health),
         ERC.LAFPP.health = ERC.LAFPP - ERC.LAFPP.pension, 
         ERC.LACERS = ERC.LAFPP * factor_LACERS,
         #ERC.tot    = ERC.LAFPP + ERC.LACERS, 
         #ERC.LAFPP.pension_GenFund = 100 * ERC.LAFPP.pension / GenFund.proj,
         #ERC.LAFPP_GenFund = 100 * ERC.LAFPP / GenFund.proj,
         ERC.LACERS_GenFund = 100 * ERC.LACERS / GenFund.proj) %>% 
  #ERC.tot_GenFund = 100 * ERC.tot / GenFund.proj
  select(-ERC, -ERC.LAFPP, -ERC.LAFPP.pension)


results_fiscal <- 
  results_all %>% 
  left_join(df_estimates) %>% 
  mutate(ERC.LAFPP.pension = ERC,
         ERC.LAFPP = ERC.LAFPP.pension + ERC.LAFPP.health,
         ERC.tot    = ERC.LAFPP + ERC.LACERS, 
         ERC.LAFPP.pension_GenFund = 100 * ERC.LAFPP.pension / GenFund.proj,
         ERC.LAFPP_GenFund = 100 * ERC.LAFPP / GenFund.proj,
         ERC.LACERS_GenFund = 100 * ERC.LACERS / GenFund.proj,
         ERC.tot_GenFund = 100 * ERC.tot / GenFund.proj) %>% 
  select(runname,run.policyScn, run.returnScn, run.policyScn.lab, run.returnScn.lab,
         Tier, sim, year, C, EEC, ERC, GenFund.proj, ERC_PR,
         ERC.LAFPP, ERC.LAFPP.health, ERC.LAFPP.pension,
         ERC.LACERS,
         ERC.tot,
         ERC.LAFPP.pension_GenFund,
         ERC.LAFPP_GenFund,
         ERC.LACERS_GenFund,
         ERC.tot_GenFund,
         NC,
         FR_MA) 




#**********************************************************************************************
## Stochastic runs: policies  ####
#**********************************************************************************************
## Stochastic runs


results_fiscal.stch <- 
  results_fiscal %>%  filter(Tier == "sumTiers", sim >0) %>%
  group_by(run.returnScn, run.policyScn, sim) %>% 
  mutate(ERC_GF.hike = cumany(na2zero(ERC.LAFPP.pension_GenFund - lag(ERC.LAFPP.pension_GenFund, 5) >= 5))) %>% 
  group_by(run.returnScn, run.policyScn, year) %>% 
  summarise(
    
    ERC_GF.hike = 100 * sum(ERC_GF.hike, na.rm = T)/n(),
    
    ERC.LAFPP.pension_GenFund.q10   = quantile(ERC.LAFPP.pension_GenFund, 0.1,  na.rm = T),
    ERC.LAFPP.pension_GenFund.q25   = quantile(ERC.LAFPP.pension_GenFund, 0.25, na.rm = T),
    ERC.LAFPP.pension_GenFund.q50   = quantile(ERC.LAFPP.pension_GenFund, 0.50, na.rm = T),
    ERC.LAFPP.pension_GenFund.q75   = quantile(ERC.LAFPP.pension_GenFund, 0.75, na.rm = T),
    ERC.LAFPP.pension_GenFund.q90   = quantile(ERC.LAFPP.pension_GenFund, 0.90, na.rm = T),
    
    ERC.tot_GenFund.q10   = quantile(ERC.tot_GenFund, 0.1,  na.rm = T),
    ERC.tot_GenFund.q25   = quantile(ERC.tot_GenFund, 0.25, na.rm = T),
    ERC.tot_GenFund.q50   = quantile(ERC.tot_GenFund, 0.50, na.rm = T),
    ERC.tot_GenFund.q75   = quantile(ERC.tot_GenFund, 0.75, na.rm = T),
    ERC.tot_GenFund.q90   = quantile(ERC.tot_GenFund, 0.90, na.rm = T)
    )

results_fiscal.stch

# Distribution under current policy and assumption is met


# LAFPP ERC  
fig_ERC_PRdist <- 
  results_fiscal.stch %>% 
  filter(run.returnScn %in% paste0("RS", c(1)), run.policyScn %in% c("noCap")) %>% 
  select(run.returnScn, run.policyScn, year, 
         ERC.LAFPP.pension_GenFund.q25, 
         ERC.LAFPP.pension_GenFund.q50, 
         ERC.LAFPP.pension_GenFund.q75) %>% 
  gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
  mutate(qtile = factor(qtile, levels = c("ERC.LAFPP.pension_GenFund.q75", "ERC.LAFPP.pension_GenFund.q50", "ERC.LAFPP.pension_GenFund.q25"),
                        labels = c("75th percentile", "Median", "25th percentile"))) %>% 
  ggplot(aes(x = year, y = value , color = qtile))  + 
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 100, 2)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green), name = NULL) + 
  coord_cartesian(ylim = c(0,20)) + 
  theme_bw() + 
  RIG.theme() +
  # guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
  labs(title = "Distribution of employer contribution \nas a percentage of General fund",
       #subtitle = "Current policy; expected return = 7.5%",
       x = NULL,
       y = "Percent") + 
  theme(legend.justification=c(0, 1), legend.position=c(0.01, 0.99),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1)) + 
  theme(axis.text = element_text(face="bold", size=10)) 
fig_ERC_PRdist



# LAFPP ERC  
fig_ERC_GFhike <- 
  results_fiscal.stch %>% 
  filter(run.returnScn %in% paste0("RS", c(1)), run.policyScn %in% c("noCap")) %>% 
  select(run.returnScn, run.policyScn, year, ERC_GF.hike) %>% 
  ggplot(aes(x = year, y = ERC_GF.hike))  + 
  geom_point(size = 2, color = RIG.blue) + 
  geom_line(size = 1, color = RIG.blue) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 100, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green), name = NULL) + 
  coord_cartesian(ylim = c(0,60)) + 
  theme_bw() + 
  RIG.theme() +
  # guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
  labs(title = "Probability of employer contribution rising more than 5% of General fund \nin a 5-year period at any time up to the given year",
       #subtitle = "Current policy; expected return = 7.5%",
       x = NULL,
       y = "Percent") + 
  theme(legend.justification=c(0, 1), legend.position=c(0.01, 0.99),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1)) + 
  theme(axis.text = element_text(face="bold", size=10)) 
fig_ERC_GFhike





#**********************************************************************************************
##  LAFPP Deterministic Run ####
#**********************************************************************************************
## Deterministic run
results_fiscal.det <- 
  results_fiscal %>%  filter(sim ==0, Tier == "sumTiers") %>% 
  select(runname, Tier, run.policyScn, run.returnScn, run.policyScn.lab, run.returnScn.lab, sim, year,
         ERC.LAFPP.pension_GenFund, 
         ERC.LAFPP_GenFund,
         ERC.tot_GenFund,
         FR_MA)

# LAFPP pension ERC

fig_det_ERC_GF <- 
  results_fiscal.det %>% 
  filter(run.returnScn %in% paste0("RS", c(1, 5)), run.policyScn %in% c("noCap")) %>% 
  ggplot(aes(x = year, y = ERC.LAFPP.pension_GenFund, color = run.returnScn.lab))  + 
  geom_point(size = 2) + 
  geom_line(size = 1)+ 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 100, 2)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red), name = NULL, label = c("Constant annual return of 7.5%", 
                                                                           "Constant annual return of 6.1%")) + 
  coord_cartesian(ylim = c(0,18)) + 
  theme_bw() + 
  RIG.theme() +
  #guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  theme(legend.justification=c(0, 1), legend.position=c(0.01, 0.99),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1)) + 
  theme(axis.text = element_text(face="bold", size=10)) + 
  labs(title = "Employer contribution as a percentage of General fund \nunder different deterministic return scenarios",
       # subtitle = "Deterministic runs",
       # caption = fig.caption,
       x = NULL,
       y = "Percentage of General fund (%)")
fig_det_ERC_GF 


fig_det_FR <- 
  results_fiscal.det %>% 
  filter(run.returnScn %in% paste0("RS", c(1, 5)), run.policyScn %in% c("noCap")) %>% 
  ggplot(aes(x = year, y = FR_MA, color = run.returnScn.lab))  + 
  geom_point(size = 2) + 
  geom_line(size = 1)+ 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 200, 10)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red), name = NULL, label = c("Constant annual return of 7.5%", 
                                                                           "Constant annual return of 6.1%")) + 
  coord_cartesian(ylim = c(50, 110)) + 
  theme_bw() + 
  RIG.theme() +
  #guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  theme(legend.justification=c(0, 1), legend.position=c(0.01, 0.99),
        legend.background = element_rect(color = "grey",  size=0.5, linetype=1)) + 
  theme(axis.text = element_text(face="bold", size=10)) + 
  labs(title = "Funded ratio \nunder different deterministic return scenarios",
       # subtitle = "Deterministic runs",
       # caption = fig.caption,
       x = NULL,
       y = "Funded ratio (%)")
fig_det_FR







ggsave(file = paste0(Outputs_folder, "fig_FRdist.png"),   fig_FRdist, height = 7*0.7, width = 9*0.7)
ggsave(file = paste0(Outputs_folder, "fig_FR40less.png"), fig_FR40less, height = 7*0.7, width = 9*0.7)

ggsave(file = paste0(Outputs_folder, "fig_ERC_PRdist.png"), fig_ERC_PRdist, height = 7*0.7, width = 9*0.7)
ggsave(file = paste0(Outputs_folder, "fig_ERC_GFhike.png"), fig_ERC_GFhike, height = 7*0.7, width = 9*0.7)

ggsave(file = paste0(Outputs_folder, "fig_det_FR.png"),       fig_det_FR, height = 7*0.7, width = 9*0.7)
ggsave(file = paste0(Outputs_folder, "fig_det_ERC_GF.png"),   fig_det_ERC_GF, height = 7*0.7, width = 9*0.7)





