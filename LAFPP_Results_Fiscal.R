# Fiscal analysis of LAFPP

# Risk measures for LAFPP

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

source("Functions.R")



#*****************************************************
##  Defining paths for inputs and outputs         ####
#*****************************************************
IO_folder       <- "Results/"
Outputs_folder  <- "Results/Graphs_report2/"


#*****************************************************
##  Loading simulation data  ####
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




results_all <- get_results(IO_folder, "results_sumTiers_RS") %>% select(runname, Tier, sim, year, everything()) %>% 
  filter(run.policyScn != "FR075")
  


# Runs used in the report 
runs_RS <- paste0("RS", 1:5)
runs_policy <- c("noCap", "cap", "cap.allTiers")

runs_RS_labels <- c("Scenario 2: \nAssumption achieved", 
                    "Scenario 3: \n5 years of low returns", 
                    "Scenario 4: \n15 years of low returns",
                    "Scenario 5: \nHigh volatility \nreflecting market forecasts", 
                    "Scenario 6: \nLow expected return \nbased on LAFPP target portfolio")

runs_policy_labels <- c("without ERC cap", 
                        "ERC cap for new hires", 
                        "ERC cap for all tiers")


results_all %<>% mutate(run.policyScn.lab = factor(run.policyScn, levels = runs_policy, labels = runs_policy_labels),
                        run.returnScn.lab = factor(run.returnScn, levels = runs_RS,     labels = runs_RS_labels),
                        run.policyScn = factor(run.policyScn, levels = runs_policy),
                        run.returnScn = factor(run.returnScn, levels = runs_RS)
                        ) 


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



#**********************************************************************************************
##  Load revenue data and extend the projection into 2044  ####
#**********************************************************************************************

# source: City of LA Revenue Outlook, FY 2016-17
df_revenue <- read_ExcelRange("Data_inputs/LAFPP_PlanInfo(1).xlsx", sheet = "Fiscal")

# extend the projection into 2044 using the projected growth rate of 2.9% in 2020 
rev.growth <- 0.029

df_revenue %<>% 
  mutate(GenFund.proj = 1000 * ifelse(year < 2021, GenFund.original, GenFund.original[year == 2020] * (1 + rev.growth)^(year - 2020))) 




#**********************************************************************************************
##  Fiscal analysis  ####
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
         ERC.tot_GenFund) 



## Deterministic run
 results_fiscal.det <- 
 results_fiscal %>%  filter(sim ==0, Tier == "sumTiers") %>% 
   select(runname, Tier, run.policyScn, run.returnScn, run.policyScn.lab, run.returnScn.lab, sim, year,
          ERC.LAFPP.pension_GenFund, 
          ERC.LAFPP_GenFund,
          ERC.tot_GenFund)


 # LAFPP ERC  
 results_fiscal.det %>% 
   filter(run.returnScn %in% paste0("RS", c(1, 3, 5)), run.policyScn %in% c("noCap", "cap", "cap.allTiers" )) %>% 
   ggplot(aes(x = year, y = ERC.LAFPP_GenFund, color = run.returnScn.lab))  + 
   facet_grid(.~run.policyScn.lab) + 
   geom_point() + 
   geom_line()+ 
   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
   scale_y_continuous(breaks = seq(0, 100, 2)) + 
   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "Return scenarios") + 
   coord_cartesian(ylim = c(0,20)) + 
   theme_bw() + 
   RIG.theme() +
   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
   labs(title = "ERC for LAFPP (pension and health) as a percentage of General Fund of LA",
        subtitle = "Deterministic runs",
        x = "Year",
        y = "Percent")

 
 # LAFPP + LACERS ERC  
 results_fiscal.det %>% 
   filter(run.returnScn %in% paste0("RS", c(1, 3, 5)), run.policyScn %in% c("noCap", "cap", "cap.allTiers" )) %>% 
   ggplot(aes(x = year, y = ERC.tot_GenFund, color = run.returnScn.lab))  + 
   facet_grid(.~run.policyScn.lab) + 
   geom_point() + 
   geom_line()+ 
   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
   scale_y_continuous(breaks = seq(0, 100, 2)) + 
   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "Return scenarios") + 
   coord_cartesian(ylim = c(0,40)) + 
   theme_bw() + 
   RIG.theme() +
   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
   labs(title = "ERC for LAFPP and LACERS (pension and health) as a percentage of General Fund of LA",
        subtitle = "Deterministic runs",
        x = "Year",
        y = "Percent")
  
 














