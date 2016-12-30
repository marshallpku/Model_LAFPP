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
##  Loading data  ####
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


results_all <- get_results(IO_folder, "results_sumTiers_RS") %>% select(runname, Tier, sim, year, everything())
# save(results_all, file = paste0(IO_folder, "/Analysis_Demo/Demo_results_all.RData"))


## Loading existing data. 
#load("Results/results_sumTiers_RS1.RData")


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
        plot.subtitle=element_text(hjust=0.5))
}


#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************

# Runs used in the report 
runs_RS <- paste0("RS", 1:5)
runs_cap <-  paste0("RS", 1:5, "_cap") 
runs_cap.allTiers <-  paste0("RS", 1:5, "_cap.allTiers")
runs_RS_FR075 <- paste0("RS", 1:5, "_FR075")

runs_RS_labels <- c("Assumption Achieved",
                     "5 years of low returns",
                     "15 years of low returns",
                     "Callan",
                     "RVK")

runs_cap_labels <- c("Assumption Achieved; w/ERC cap for new hires",
                     "5 years of low returns; w/ERC cap for new hires",
                     "15 years of low returns; w/ERC cap for new hires",
                     "Callan; w/ERC cap for new hires",
                     "RVK; w/ERC cap for new hires")


runs_cap.allTiers_labels <- c("Assumption Achieved; w/ERC cap",
                              "5 years of low returns; w/ERC cap",
                              "15 years of low returns; w/ERC cap",
                              "Callan; w/ERC cap",
                              "RVK; w/ERC cap")

runs_RS_FR075_labels <- c("Assumption Achieved; 75% initial FR", 
                    "5 years of low returns; 75% initial FR",
                    "15 years of low returns; 75% initial FR",
                    "Callan; 75% initial FR",
                    "RVK; 75% initial FR")


runs_all <- c(runs_RS, runs_cap, runs_cap.allTiers, runs_RS_FR075)
runs_all_labels <- c(runs_RS_labels, runs_cap_labels, runs_cap.allTiers_labels, runs_RS_FR075_labels )


df_all.stch <- results_all  %>% 
  filter(runname %in% runs_all, sim >= 0, year <= 2044)


df_all.stch %<>%   
  select(runname, Tier, sim, year, AL, MA, EEC, PR, ERC_PR) %>% 
  group_by(runname, sim, Tier) %>% 
  mutate(EEC_PR = 100 * EEC/PR,
         FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
         FR100more  = cumany(FR_MA >= 100),
         FR100more2 = FR_MA >= 100,
         ERC_high  = cumany(ERC_PR >= 50), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10)),
         EEC_high  = cumany(ifelse(is.nan(EEC_PR), 0, EEC_PR) >= 15)) %>% 
  group_by(runname, Tier, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            EEC_high = 100 * sum(EEC_high, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T),
            
            EEC_PR.q10 = quantile(EEC_PR, 0.1, na.rm = T),
            EEC_PR.q25 = quantile(EEC_PR, 0.25, na.rm = T),
            EEC_PR.q50 = quantile(EEC_PR, 0.5, na.rm = T),
            EEC_PR.q75 = quantile(EEC_PR, 0.75, na.rm = T),
            EEC_PR.q90 = quantile(EEC_PR, 0.9, na.rm = T)
  ) %>% 
  ungroup() %>%
  mutate(runname.lab = factor(runname, 
                              levels = runs_all, 
                              labels = runs_all_labels))


df_t7.stch <- results_all  %>% 
  filter(runname %in% runs_all, Tier %in% c("xt7", "t7"), sim >= 0, year <= 2044)


df_t7.stch %<>%   
  select(runname, Tier, sim, year, AL, MA, ERC_PR, EEC_PR) %>% 
  group_by(runname, sim, Tier) %>% 
  mutate(FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
         FR100more  = cumany(FR_MA >= 100),
         FR100more2 = FR_MA >= 100,
         ERC_high  = cumany(ERC_PR >= 50), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10)),
         EEC_high  = cumany(ifelse(is.nan(EEC_PR), 0, EEC_PR) >= 15)) %>% 
  group_by(runname, Tier, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            EEC_high = 100 * sum(EEC_high, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T),
            
            EEC_PR.q10 = quantile(EEC_PR, 0.1, na.rm = T),
            EEC_PR.q25 = quantile(EEC_PR, 0.25, na.rm = T),
            EEC_PR.q50 = quantile(EEC_PR, 0.5, na.rm = T),
            EEC_PR.q75 = quantile(EEC_PR, 0.75, na.rm = T),
            EEC_PR.q90 = quantile(EEC_PR, 0.9, na.rm = T)
  ) %>% 
  ungroup() %>%
  mutate(runname.lab = factor(runname, 
                              levels = runs_all, 
                              labels = runs_all_labels))

df_t7.stch






df_all.stch %>% filter(runname == "RS4")
df_all.stch %>% filter(runname == "RS1_cap")
df_all.stch %>% filter(runname == "RS1_cap.allTiers") %>% select(runname, year, starts_with("EEC"))


df_all.stch %>% filter(runname == "RS1_FR075")
df_all.stch %>% filter(runname == "RS2_FR075")
df_all.stch %>% filter(runname == "RS3_FR075")



(8.546271 - 6.1)/8.54
(37.73475 - 33.07573) / 37.73475

x <- results_all %>% filter(runname %in% c("RS1", "RS1_FR075"), sim == 0) %>% 
  group_by(runname) %>% 
  summarise(sum_I.r = sum(I.r))
x$sum_I.r[1]/x$sum_I.r[2]

# df_all.stch
# 
# x <- results_all %>% filter(runname %in% c("RS1", "RS1_cap"), sim == 0, year<=2025) %>%
#   select(runname, Tier,  sim, year, AL, MA, AA, B, C, nactives, NC, SC, AL.act, AL.act.death, NC.death, ndeathBen, B.death)


#*****************************************************
## 10-year and 30 year compound return  ####
#*****************************************************


results_all %>% filter(runname == "RS1", sim > 0) %>% 
  group_by(sim) %>% 
  summarise(geoReturn30y = get_geoReturn(i.r),
            geoReturn5y = get_geoReturn(i.r[year<=2019])) %>% 
  summarise(negReturn30y = sum(geoReturn30y <= 0)/n(),
            negReturn5y = sum(geoReturn5y <= 0)/n())



#*****************************************************
## Deterministic run  ####
#*****************************************************

df_det <- results_all  %>% 
  filter(runname == "RS1", sim == 0, year <= 2044) %>% 
  select(year, AL, MA, B, C, ERC, EEC, ExF, FR_MA, ERC_PR, NC_PR, ExF_MA, MA_PR) %>% 
  mutate_at(vars(-year, -FR_MA, -ERC_PR, -ExF_MA, -MA_PR, -NC_PR), funs(./1e6)) %>% 
  mutate(MA_PR = MA_PR/100) 


df_det.short <- df_det %>% filter(year %in% c(seq(2015, 2040, 5), 2044))
  
df_det
df_det.short


results_all  %>% 
  filter(runname == "RS1", sim == 0, year <= 2044) %>% 
  select(year, AL, MA, B, C, ERC, EEC, ExF, FR_MA, ERC_PR, NC_PR, ExF_MA, MA_PR, SC, PR) %>% 
  mutate(SC_PR = 100*SC/PR)


#*****************************************************
## Stochastic run: assumption achieved  ####
#*****************************************************

# Distribution of 30-year compound returns

fig_distReturn <- results_all %>% 
  filter(runname == "RS1", sim > 0) %>% 
  group_by(sim) %>% 
  summarize(geoReturn = get_geoReturn(i.r)) %>% 
  ggplot(aes(100*geoReturn)) + theme_bw() + 
  geom_histogram(color = "black", fill = RIG.blue, binwidth = 0.5, boundary = 0) + 
  geom_vline(xintercept = 0.075 * 100, color = RIG.red) + 
  scale_x_continuous(breaks = seq(0,20,1))+
  labs(title = "Distribution of 30-year compound annual return over 2,000 simulations",
       x = "%",
       y = "Simulatoin count") + 

  RIG.theme()

fig_distReturn


med.FR_MA <- (results_all %>% filter(runname == "RS1", sim > 0, year == 2044))$FR_MA %>% median
fig_distFR30 <- results_all %>% 
  filter(runname == "RS1", sim > 0, year == 2044) %>% 
  # group_by(sim) %>% 
  # summarize(geoReturn = get_geoReturn(i.r)) %>% 
  ggplot(aes(FR_MA)) + theme_bw() + 
  geom_histogram(color = "black", fill = RIG.blue, binwidth = 10, boundary = 0) + 
  geom_vline(xintercept = c(100, med.FR_MA), color = c(RIG.red,"blue"), size = 0.8) + 
  coord_cartesian(xlim = c(0, 400)) + 
  scale_x_continuous(breaks = seq(0,400,20))+
  labs(title = "Distribution of funded ratios in year 30 over 2,000 simulations",
       x = "%",
       y = "Simulatoin count") + 
  annotate("text", x = med.FR_MA + 50, y = 175, label = paste0("Median funded ratio in year 30: \n", round(med.FR_MA, 1), "%"),
           color = "blue", size = 3.5) + 
  RIG.theme()
fig_distFR30



fig_stchDet.FR40less <- df_all.stch %>% filter(runname == "RS1") %>% 
  select(year, FR40less) %>% 
  mutate(FR40less.det = 0) %>% 
  gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = value, color = variable)) + theme_bw() + 
  geom_point() + geom_line() + 
  coord_cartesian(ylim = c(0,5)) + 
  scale_y_continuous(breaks = seq(0,200, 1)) +
  scale_color_manual(values = c(RIG.red,"black"),  name = "", 
                     label  = c("Stochastic run", "Deterministic run")) + 
  labs(title = "Probability of funded ratio below 40% \nin a given year",
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_stchDet.FR40less



fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig_stchDet.ERChike <- df_all.stch %>% filter(runname == "RS1") %>% 
  select(year, ERC_hike) %>% 
  mutate(ERChike.det = 0) %>% 
  gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = value, color = variable)) + theme_bw() + 
  geom_point() + geom_line() + 
  coord_cartesian(ylim = c(0,100)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_color_manual(values = c(RIG.red,"black"),  name = "", 
                     label  = c("Stochastic run", "Deterministic run")) + 
  labs(title = fig.title ,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_stchDet.ERChike



fig.title <- "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different funding approaches"
fig_stchDet.ERChigh <- df_all.stch %>% filter(runname == "RS1") %>% 
  select(year, ERC_high) %>% 
  mutate(ERChigh.det = 0) %>% 
  gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = value, color = variable)) + theme_bw() + 
  geom_point() + geom_line() + 
  coord_cartesian(ylim = c(0,100)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_color_manual(values = c(RIG.red,"black"),  name = "", 
                     label  = c("Stochastic run", "Deterministic run")) + 
  labs(title = fig.title ,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_stchDet.ERChigh




fig.levels <- c("FR40less", "ERC_hike", "ERC_high")
fig.lvl.labs <- c("Probability of funded ratio below 40% \nin a given year",
                  "Probability of employer contribution \nrising more than 10% of payroll in a 5-year period \nat any time prior to and including the given year",
                  "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different funding approaches")
fig.title <- 'Measures of risks under scenario "Assumption achieved: deterministic" \n and scenario "Assumption achieved: stochastic" '
fig_stchDet.3measures <- df_all.stch %>% filter(runname == "RS1") %>% 
  select(year, FR40less, ERC_hike, ERC_high) %>% 
  gather(variable, value, -year) %>% 
  # mutate(FR40less.det = 0, 
  #        ERChike.det  = 0,
  #        ERChigh.det  = 0) %>% 
  mutate(value.det = 0) %>%
  gather(Var, value, -year, -variable) %>% 
  mutate(variable = factor(variable, levels = fig.levels, labels = fig.lvl.labs )) %>% 
  ggplot(aes(x = year, y = value, color = Var)) + facet_grid(.~variable) +  theme_bw() + 
  geom_point() + geom_line() + 
  coord_cartesian(ylim = c(0,70)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_x_continuous(breaks = seq(2015,2050, 5)) +
  scale_color_manual(values = c(RIG.red,"black"),  name = "", 
                     label  = c("Stochastic run", "Deterministic run")) + 
  labs(title = fig.title ,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_stchDet.3measures



fig.levels <- c("ERC_hike", "ERC_high")
fig.lvl.labs <- c("Probability of employer contribution \nrising more than 10% of payroll in a 5-year period \nat any time prior to and including the given year",
                  "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different funding approaches")
fig.title <- 'Risk of high employer contribution rates and risk of sharp increases of employer contribution rate \nunder scenario "Assumption achieved: deterministic" and scenario "Assumption achieved: stochastic" '
fig_stchDet.2measures <- df_all.stch %>% filter(runname == "RS1") %>% 
  select(year, ERC_hike, ERC_high) %>% 
  gather(variable, value, -year) %>% 
  # mutate(FR40less.det = 0, 
  #        ERChike.det  = 0,
  #        ERChigh.det  = 0) %>% 
  mutate(value.det = 0) %>%
  gather(Var, value, -year, -variable) %>% 
  mutate(variable = factor(variable, levels = fig.levels, labels = fig.lvl.labs )) %>% 
  ggplot(aes(x = year, y = value, color = Var)) + facet_grid(.~variable) +  theme_bw() + 
  geom_point() + geom_line() + 
  coord_cartesian(ylim = c(0,70)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_x_continuous(breaks = seq(2015,2050, 5)) +
  scale_color_manual(values = c(RIG.red,"black"),  name = "", 
                     label  = c("Stochastic run", "Deterministic run")) + 
  labs(title = fig.title ,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_stchDet.2measures




# Distribution of funded ratio 
fig_stchDet.FRdist <- df_all.stch %>% filter(runname %in% c("RS1"), Tier == "sumTiers") %>% 
  left_join(results_all  %>% 
             filter(runname == "RS1", sim == 0) %>% 
             select(year, FR_det = FR_MA)) %>%  
  select(runname, year, FR.q25, FR.q50, FR.q75, FR_det) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det"))
  )) + theme_bw() + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(50,180)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  labs(title = "Distribution of funded ratios",
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()

fig_stchDet.FRdist



# Distribution of ERC rate
fig_stchDet.ERCdist <- df_all.stch %>% filter(runname %in% c("RS1"), Tier == "sumTiers") %>% 
  left_join(results_all  %>% 
              filter(runname == "RS1", sim == 0) %>% 
              select(year, ERC_det = ERC_PR)) %>% 
  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75, ERC_det) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25", "ERC_det")))) + 
  theme_bw() + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,50)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  labs(title = "Distribution of employer contribution rates",
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_stchDet.ERCdist



#*****************************************************
## Comparing funding policies: proposed ERC cap ####
#*****************************************************

# Distribution of funded ratio 

fig_policy.FRdist <- df_all.stch %>% filter(runname %in% c("RS1", "RS1_cap"), Tier == "sumTiers") %>% 
  mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  select(runname, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25"))
  )) + theme_bw() + facet_grid(.~runname) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(50,180)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  labs(title = "Distribution of funded ratios across simulations under different funding approaches",
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()

fig_policy.FRdist


# Distribution of ERC rate
fig_policy.ERCdist <- df_all.stch %>% filter(runname %in% c("RS1", "RS1_cap", "RS1_cap.allTiers"), Tier == "sumTiers") %>% 
  mutate(runname = factor(runname, levels = c("RS1", "RS1_cap", "RS1_cap.allTiers" ), 
                                   labels = c("without ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) + 
  theme_bw() + facet_grid(.~runname) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,50)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  labs(title = "Distribution of employer contribution rates across simulations under different funding approaches",
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_policy.ERCdist


# Risk of high ERC
fig.title <-  "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different funding approaches"
fig_policy.ERChigh <- df_all.stch %>% filter(runname %in% c("RS1", "RS1_cap", "RS1_cap.allTiers" ), Tier == "sumTiers") %>%
  ggplot(aes(x = year, y = ERC_high, color = factor(runname, levels = c("RS1", "RS1_cap",  "RS1_cap.allTiers" )))) + theme_bw() + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 40)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 5)) +
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red),  name = "Contribution \napproaches", 
                     label  = c("without ERC cap", "ERC cap for new hires", "ERC cap for all tiers")) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_policy.ERChigh




# Risk of sharp ERC increase
fig.title <- "Probability of ERC rising by more than 10% of payroll \nin any 5-year period up to the given year \nunder different funding approaches"
fig_policy.ERChike <- df_all.stch %>% filter(runname %in% c("RS1", "RS1_cap", "RS1_cap.allTiers"), Tier == "sumTiers") %>%
  ggplot(aes(x = year, y = ERC_hike, color = factor(runname, levels = c("RS1", "RS1_cap", "RS1_cap.allTiers")))) + theme_bw() + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 80)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 10)) +
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red),  name = "Contribution \napproaches", 
                     label  = c("without ERC cap", "ERC cap for new hires", "ERC cap for all tiers")) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_policy.ERChike


# Impact on EEC rates of new hires after Jan 2019

# Distribution of EEC rates of t7 members 


# Distribution of EEC rate: ERC cap applied on new hires
fig.title <- "Distribution of employee contribution rates for new hires \nif the proposed ERC cap is applied to new hires"
fig_policy.EECdist.t7 <- df_t7.stch %>% filter(runname %in% c("RS1_cap"), Tier == "t7") %>% 
  #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  select(runname, year, EEC_PR.q10, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75, EEC_PR.q90) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("EEC_PR.q90", "EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25", "EEC_PR.q10")))) + 
  theme_bw() + 
  #facet_grid(.~runname) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,20)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0, 500, 2.5)) + 
  scale_color_manual(values = c(RIG.yellow.dark,RIG.red, RIG.blue, RIG.green, "green"),  name = NULL, 
                     label  = c("90th percentile", "75th percentile", "50th percentile", "25th percentile", "10th percentile")) + 
  labs(title = fig.title,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_policy.EECdist.t7



# Risk of high EEC rate (15%) for t7: ERC cap applied on new hires
fig.title <-  "Probability of employee contribution above 15% of payroll \nat any time prior to and including the given year \nif the proposed ERC cap is applied to new hires"
fig_policy.EEChigh.t7 <- df_t7.stch %>% 
  filter(runname %in% c("RS1_cap"), Tier == "t7") %>%
  ggplot(aes(x = year, y = EEC_high)) + 
  theme_bw() + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 55)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 5)) +
  #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
  #                   label  = c("without ERC cap", "with ERC cap")) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_policy.EEChigh.t7





# Distribution of EEC rate: : ERC cap applied to all tiers
fig.title <- "Distribution of employee contribution rates \nif the proposed ERC cap is applied to all current tiers"
fig_policy.EECdist.allTiers <- df_all.stch %>% filter(runname %in% c("RS1_cap.allTiers")) %>% 
  #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  select(runname, year, EEC_PR.q10, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75, EEC_PR.q90) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("EEC_PR.q90", "EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25", "EEC_PR.q10")))) + 
  theme_bw() + 
  #facet_grid(.~runname) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,70)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0, 500, 10)) + 
  scale_color_manual(values = c(RIG.yellow.dark,RIG.red, RIG.blue, RIG.green, "green"),  name = NULL, 
                     label  = c("90th percentile", "75th percentile", "50th percentile", "25th percentile", "10th percentile")) + 
  labs(title = fig.title,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_policy.EECdist.allTiers



# Risk of high EEC rate (15%): ERC cap applied to all tiers
fig.title <-  "Probability of employee contribution above 15% of payroll \nat any time prior to and including the given year \nif the proposed ERC cap is applied to all current tiers"
fig_policy.EEChigh.allTiers <- df_all.stch %>% 
  filter(runname %in% c("RS1_cap.allTiers")) %>%
  ggplot(aes(x = year, y = EEC_high)) + 
  theme_bw() + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 100)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 5)) +
  #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
  #                   label  = c("without ERC cap", "with ERC cap")) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_policy.EEChigh.allTiers










#*************************************************************
## Comparing return scenarios: low returns in early years ####
#*************************************************************

runs.compareRS1 <- c("RS1", "RS1_cap", "RS1_cap.allTiers",
                     "RS2", "RS2_cap", "RS2_cap.allTiers",
                     "RS3", "RS3_cap", "RS3_cap.allTiers")

fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
                           "Scenario 3: \n5 years of low returns", 
                           "Scenario 4: \n15 years of low returns")

# Risk of high ERC
fig.title <-  "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different return scenarios"

fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
                           "Scenario 3: \n5 years of low returns", 
                           "Scenario 4: \n15 years of low returns")

fig_compareRS1.ERChigh <- df_all.stch %>% filter(runname %in% runs.compareRS1, Tier == "sumTiers") %>% 
  select(runname, year, ERC_high) %>% 
  gather(variable, value, -year, -runname) %>% 
  mutate(value = as.numeric(value),
         RS = str_sub(runname, 1, 3),
         policy = str_sub(runname, 5),
         policy = ifelse(policy == "", "no_cap", policy),
         policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
                         labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
  facet_grid(. ~ policy) + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 55)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 5)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
                     label  = fig.labels.lowReturns) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS1.ERChigh



# Risk of sharp ERC increase
fig.title <- "Probability of ERC rising by more than 10% of payroll \nin any 5-year period up to the given year \nunder different return scenarios"

fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
                           "Scenario 3: \n5 years of low returns", 
                           "Scenario 4: \n15 years of low returns")

fig_compareRS1.ERChike <- df_all.stch %>% filter(runname %in% runs.compareRS1, Tier == "sumTiers") %>% 
  select(runname, year, ERC_hike) %>% 
  gather(variable, value, -year, -runname) %>% 
  mutate(value = as.numeric(value),
         RS = str_sub(runname, 1, 3),
         policy = str_sub(runname, 5),
         policy = ifelse(policy == "", "no_cap", policy),
         policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
                         labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>%  
  ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
  facet_grid(. ~ policy) + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 100)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 10)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
                     label  = fig.labels.lowReturns) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS1.ERChike


# Risk of low FR
fig.title <- "Probability of funded ratio below 40% \nat any time prior to and including the given year \nunder different return scenarios"

fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
                           "Scenario 3: \n5 years of low returns", 
                           "Scenario 4: \n15 years of low returns")

fig_compareRS1.FR40less <- df_all.stch %>% filter(runname %in% runs.compareRS1[c(1,4,7)], Tier == "sumTiers") %>% 
  select(runname, year, FR40less) %>% 
  gather(variable, value, -year, -runname) %>% 
  # mutate(value = as.numeric(value),
  #        RS = str_sub(runname, 1, 3),
  #        policy = str_sub(runname, 5),
  #        policy = ifelse(policy == "cap", policy, "no_cap" ),
  #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(runname, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 10)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 1)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
                     label  = fig.labels.lowReturns) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS1.FR40less




# Median ERC rate
fig.title <- "Median employer contribution rates \nunder different return scenarios and funding approaches"

fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
                           "Scenario 3: \n5 years of low returns", 
                           "Scenario 4: \n15 years of low returns")

fig_compareRS1.MedERC <- df_all.stch %>% filter(runname %in% runs.compareRS1, Tier == "sumTiers") %>% 
  select(runname, year, ERC_PR.q50) %>% 
  gather(variable, value, -year, -runname) %>% 
  mutate(value = as.numeric(value),
         RS = str_sub(runname, 1, 3),
         policy = str_sub(runname, 5),
         policy = ifelse(policy == "", "no_cap", policy),
         policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
                         labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
  facet_grid(. ~ policy) + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 50)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 10)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
                     label  = fig.labels.lowReturns) + 
  labs(title = fig.title,
       x = NULL, y = "Percent of payroll (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS1.MedERC



# Median  FR
fig.title <- "Median funded ratios \nunder different return scenarios"

fig.labels.lowReturns <- c("Scenario 2: \nAssumption achieved \nstochastic", 
                           "Scenario 3: \n5 years of low returns", 
                           "Scenario 4: \n15 years of low returns")

fig_compareRS1.MedFR <- df_all.stch %>% filter(runname %in% runs.compareRS1[c(1,4,7)], Tier == "sumTiers") %>% 
  select(runname, year, FR.q50) %>% 
  gather(variable, value, -year, -runname) %>% 
  # mutate(value = as.numeric(value),
  #        RS = str_sub(runname, 1, 3),
  #        policy = str_sub(runname, 5),
  #        policy = ifelse(policy == "cap", policy, "no_cap" ),
  #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(runname, levels = c("RS1", "RS2", "RS3")))) + theme_bw() + 
  geom_line() + geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) + 
  coord_cartesian(ylim = c(0, 150)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,500, 20)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
                     label  = fig.labels.lowReturns) + 
  labs(title = fig.title,
       x = NULL, y = "Percent of payroll (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS1.MedFR 




# Impact on EEC rates of new hires after 2019

df_t7.stch.RS1 <- df_t7.stch %>% filter(runname %in% c("RS1_cap", "RS2_cap", "RS3_cap"), Tier == "t7") %>% 
  mutate(runname = factor(runname, labels = fig.labels.lowReturns))

# Distribution of EEC rate
fig.title <- "Distribution of employee contribution rates for new hires \nif the proposed ERC cap is applied to new hires"
fig_compareRS1.EECdist.t7 <- df_t7.stch.RS1 %>% 
  #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  select(runname, year, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")))) + 
  theme_bw()  + 
  facet_grid(.~runname) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,20)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0, 500, 2.5)) + 
  scale_color_manual(values = c(RIG.yellow.dark,RIG.red, RIG.blue, RIG.green, "green"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  labs(title = fig.title,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_compareRS1.EECdist.t7



# Risk of high EEC rate (15%) for t7
fig.title <-  "Probability of employee contribution above 15% of payroll for new hires \nat any time prior to and including the given year \nif the proposed ERC cap is applied to new hires"
fig_compareRS1.EEChigh.t7 <- df_t7.stch.RS1 %>% 
  ggplot(aes(x = year, y = EEC_high, color = runname)) + 
  theme_bw() + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 100)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 10)) +
  scale_color_manual(name = "", values = c(RIG.red, RIG.blue, RIG.green)) + 
  #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
  #                   label  = c("without ERC cap", "with ERC cap")) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS1.EEChigh.t7




# Impact on EEC rates: applying ERC cap on all current tiers

df_allTiers.stch.RS1 <- df_all.stch %>% filter(runname %in% c("RS1_cap.allTiers", "RS2_cap.allTiers", "RS3_cap.allTiers")) %>% 
  mutate(runname = factor(runname, labels = fig.labels.lowReturns))

# Distribution of EEC rate

fig.title <- "Distribution of employee contribution rates \nif the proposed ERC cap is applied to all current tiers"
fig_compareRS1.EECdist.allTiers <- df_allTiers.stch.RS1 %>% 
  #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  select(runname, year, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")))) + 
  theme_bw()  + 
  facet_grid(.~runname) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,70)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0, 500, 10)) + 
  scale_color_manual(values = c(RIG.yellow.dark,RIG.red, RIG.blue, RIG.green, "green"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  labs(title = fig.title,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8),
        plot.title=element_text(hjust=0.5)) + 
  RIG.theme()
fig_compareRS1.EECdist.allTiers



# Risk of high EEC rate (15%)
fig.title <-  "Probability of employee contribution above 15% of payroll \nat any time prior to and including the given year \nif the proposed ERC cap is applied to all current tiers"
fig_compareRS1.EEChigh.allTiers <- df_allTiers.stch.RS1 %>% 
  ggplot(aes(x = year, y = EEC_high, color = runname)) + 
  theme_bw() + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 100)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 5)) +
  scale_color_manual(name = "", values = c(RIG.red, RIG.blue, RIG.green)) + 
  #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
  #                   label  = c("without ERC cap", "with ERC cap")) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS1.EEChigh.allTiers










#*************************************************************************
## Comparing return scenarios: alternative capital market assumptions ####
#*************************************************************************
runs.compareRS2 <- c("RS1", "RS1_cap", "RS1_cap.allTiers",
                     "RS4", "RS4_cap", "RS4_cap.allTiers",
                     "RS5", "RS5_cap", "RS5_cap.allTiers")

fig.labels.altAssumptions <- c("Scenario 2: \nAssumption achieved \nstochastic", 
                               "Scenario 5: \nHigh volatility \nreflecting market forecasts", 
                               "Scenario 6: \nLow expected return \nbased on LAFPP target portfolio")

df_all.stch.RS2 <-  df_all.stch %>% filter(runname %in% runs.compareRS2, Tier == "sumTiers")

# Risk of high ERC
fig.title <-  "Probability of ERC above 50% of payroll \nat any time prior to and including the given year \nunder different return scenarios"

fig_compareRS2.ERChigh <- df_all.stch.RS2 %>% 
  select(runname, year, ERC_high) %>% 
  gather(variable, value, -year, -runname) %>% 
  mutate(value = as.numeric(value),
         RS = str_sub(runname, 1, 3),
         policy = str_sub(runname, 5),
         policy = ifelse(policy == "", "no_cap", policy),
         policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
                         labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
  facet_grid(. ~ policy) + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 55)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 5)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
                     label  = fig.labels.altAssumptions) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS2.ERChigh





# Risk of sharp ERC increase
fig.title <- "Probability of ERC rising by more than 10% of payroll \nin any 5-year period up to the given year \nunder different return scenarios"

fig_compareRS2.ERChike <- df_all.stch.RS2 %>% 
  select(runname, year, ERC_hike) %>% 
  gather(variable, value, -year, -runname) %>% 
  mutate(value = as.numeric(value),
         RS = str_sub(runname, 1, 3),
         policy = str_sub(runname, 5),
         policy = ifelse(policy == "", "no_cap", policy),
         policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
                         labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
  facet_grid(. ~ policy) + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 100)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 10)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
                     label  = fig.labels.altAssumptions) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS2.ERChike



# Risk of low FR
fig.title <- "Probability of funded ratio below 40% \nat any time prior to and including the given year \nunder different return scenarios"

fig_compareRS2.FR40less <-  df_all.stch %>% filter(runname %in% runs.compareRS2[c(1,4,7)], Tier == "sumTiers") %>% 
  select(runname, year, FR40less) %>% 
  gather(variable, value, -year, -runname) %>% 
  # mutate(value = as.numeric(value),
  #        RS = str_sub(runname, 1, 3),
  #        policy = str_sub(runname, 5),
  #        policy = ifelse(policy == "cap", policy, "no_cap" ),
  #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(runname, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 20)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 2.5)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
                     label  = fig.labels.altAssumptions) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS2.FR40less




# Median ERC rate
fig.title <- "Median employer contribution rates \nunder different return scenarios and funding approaches"

fig_compareRS2.MedERC <- df_all.stch.RS2 %>% 
  select(runname, year, ERC_PR.q50) %>% 
  gather(variable, value, -year, -runname) %>% 
  mutate(value = as.numeric(value),
         RS = str_sub(runname, 1, 3),
         policy = str_sub(runname, 5),
         policy = ifelse(policy == "", "no_cap", policy),
         policy = factor(policy, levels = c("no_cap", "cap", "cap.allTiers"), 
                         labels = c("No ERC cap", "ERC cap for new hires", "ERC cap for all tiers"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(RS, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
  facet_grid(. ~ policy) + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 50)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 10)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
                     label  = fig.labels.altAssumptions) + 
  labs(title = fig.title,
       x = NULL, y = "Percent of payroll (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS2.MedERC



# Median FR
fig.title <- "Median funded ratios \nunder different return scenarios"

fig_compareRS2.MedFR <-  df_all.stch %>% filter(runname %in% runs.compareRS2[c(1,4,7)], Tier == "sumTiers") %>% 
  select(runname, year, FR.q50) %>% 
  gather(variable, value, -year, -runname) %>% 
  # mutate(value = as.numeric(value),
  #        RS = str_sub(runname, 1, 3),
  #        policy = str_sub(runname, 5),
  #        policy = ifelse(policy == "cap", policy, "no_cap" ),
  #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(runname, levels = c("RS1", "RS4", "RS5")))) + theme_bw() + 
  geom_line() + geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) + 
  coord_cartesian(ylim = c(0, 150)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,500, 20)) +
  scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue),  name = "", 
                     label  = fig.labels.altAssumptions) + 
  labs(title = fig.title,
       x = NULL, y = "Percent of payroll (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS2.MedFR 



# distribuiton FR with current funding policy under different return scenarios
fig.title <- "Distribution of funded ratios \nunder different return scenarios"
fig.subtitle <- "Current funding policy, without ERC cap"
fig.labels.pctile3 <- c("25th percentile",  "50th percentile", "75th percentile")
fig.labels.pctile5 <- c("10th percentile",  "25th percentile", "50th percentile", "75th percentile", "90th percentile")
fig_compareRS2.distFR <-  df_all.stch %>% filter(runname %in% runs.compareRS2[c(1,4,7)], Tier == "sumTiers") %>% 
  select(runname, year, FR.q10, FR.q25, FR.q50, FR.q75, FR.q90) %>% 
  gather(variable, value, -year, -runname) %>% 
  mutate(runname = factor(runname, labels = fig.labels.altAssumptions)) %>% 
  # mutate(value = as.numeric(value),
  #        RS = str_sub(runname, 1, 3),
  #        policy = str_sub(runname, 5),
  #        policy = ifelse(policy == "cap", policy, "no_cap" ),
  #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(variable, levels = c("FR.q90", "FR.q75", "FR.q50", "FR.q25", "FR.q10"),
                                                           labels = rev(fig.labels.pctile5) ))) + 
  theme_bw() + 
  facet_grid(. ~ runname) + 
  geom_line() + geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) + 
  coord_cartesian(ylim = c(0, 200)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,500, 20)) +
  scale_color_manual(values = rev(c("red", RIG.red, RIG.blue, RIG.green, "green")),  name = "") + 
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Funded ratio (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS2.distFR




# distribuiton ERC with current funding policy under different return scenarios
fig.title <- "Distribution of funded ratios \nunder different return scenarios"
fig.subtitle <- "Current funding policy, without ERC cap"
fig.labels.pctile3 <- c("25th percentile",  "50th percentile", "75th percentile")
fig.labels.pctile5 <- c("10th percentile",  "25th percentile", "50th percentile", "75th percentile", "90th percentile")
fig_compareRS2.distERC <-  df_all.stch %>% filter(runname %in% runs.compareRS2[c(1,4,7)], Tier == "sumTiers") %>% 
  select(runname, year, ERC_PR.q10, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75, ERC_PR.q90) %>% 
  gather(variable, value, -year, -runname) %>% 
  mutate(runname = factor(runname, labels = fig.labels.altAssumptions)) %>% 
  # mutate(value = as.numeric(value),
  #        RS = str_sub(runname, 1, 3),
  #        policy = str_sub(runname, 5),
  #        policy = ifelse(policy == "cap", policy, "no_cap" ),
  #        policy = factor(policy, levels = c("no_cap", "cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  ggplot(aes(x = year, y = value, color = factor(variable, levels = c("ERC_PR.q90", "ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25", "ERC_PR.q10"),
                                                 labels = rev(fig.labels.pctile5) ))) + 
  theme_bw() + 
  facet_grid(. ~ runname) + 
  geom_line() + geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) + 
  coord_cartesian(ylim = c(0, 80)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,500, 10)) +
  scale_color_manual(values = c("red", RIG.red, RIG.blue, RIG.green, "green"),  name = "") + 
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of payroll (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS2.distERC









# Impact on EEC rates of new hires after 2019

df_t7.stch.RS2 <- df_t7.stch %>% filter(runname %in% c("RS1_cap", "RS4_cap", "RS5_cap"), Tier == "t7") %>% 
  mutate(runname = factor(runname, labels = fig.labels.altAssumptions))

# Distribution of EEC rate
fig.title <- "Distribution of employee contribution rates for new hires \nif the proposed ERC cap is applied to new hires"
fig_compareRS2.EECdist.t7 <- df_t7.stch.RS2 %>% 
  #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  select(runname, year, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")))) + 
  theme_bw()  + 
  facet_grid(.~runname) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,20)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0, 500, 2.5)) + 
  scale_color_manual(values = c(RIG.yellow.dark,RIG.red, RIG.blue, RIG.green, "green"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  labs(title = fig.title,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_compareRS2.EECdist.t7



# Risk of high EEC rate (15%) for t7
fig.title <-  "Probability of employee contribution above 15% of payroll for new hires \nat any time prior to and including the given year \nif the proposed ERC cap is applied to new hires"
fig_compareRS2.EEChigh.t7 <- df_t7.stch.RS2 %>% 
  ggplot(aes(x = year, y = EEC_high, color = runname)) + 
  theme_bw() + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 100)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 10)) +
  scale_color_manual(name = "", values = c(RIG.red, RIG.blue, RIG.green)) + 
  #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
  #                   label  = c("without ERC cap", "with ERC cap")) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS2.EEChigh.t7



# Impact on EEC rates: applying the proposed ERC cap to all tiers

df_allTiers.stch.RS2 <- df_all.stch %>% filter(runname %in% c("RS1_cap.allTiers", "RS4_cap.allTiers", "RS5_cap.allTiers")) %>% 
  mutate(runname = factor(runname, labels = fig.labels.altAssumptions))

# Distribution of EEC rate
fig.title <- "Distribution of employee contribution rate for new hires \nif the proposed ERC cap is applied to all current tiers"
fig_compareRS2.EECdist.allTiers <- df_allTiers.stch.RS2 %>% 
  #mutate(runname = factor(runname, levels = c("RS1", "RS1_cap"), labels = c("without ERC cap", "with ERC cap"))) %>% 
  select(runname, year, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")))) + 
  theme_bw()  + 
  facet_grid(.~runname) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,70)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0, 500, 10)) + 
  scale_color_manual(values = c(RIG.yellow.dark,RIG.red, RIG.blue, RIG.green, "green"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  labs(title = fig.title,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_compareRS2.EECdist.allTiers



# Risk of high EEC rate (15%) for t7
fig.title <-  "Probability of employee contribution above 15% of payroll \nat any time prior to and including the given year \n\nif the proposed ERC cap is applied to all current tiers "
fig_compareRS2.EEChigh.allTiers <- df_allTiers.stch.RS2 %>% 
  ggplot(aes(x = year, y = EEC_high, color = runname)) + 
  theme_bw() + 
  geom_line() + geom_point() + 
  coord_cartesian(ylim = c(0, 100)) + 
  scale_x_continuous(breaks = seq(2015, 2045, 5)) + 
  scale_y_continuous(breaks = seq(0,100, 10)) +
  scale_color_manual(name = "", values = c(RIG.red, RIG.blue, RIG.green)) + 
  #scale_color_manual(values = c(RIG.blue, RIG.green),  name = "Contribution \napproaches", 
  #                   label  = c("without ERC cap", "with ERC cap")) + 
  labs(title = fig.title,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
  RIG.theme()
fig_compareRS2.EEChigh.allTiers






#*************************************************************************
##                     Summary tables ####
#*************************************************************************


lvl_policies  <- c("", "_cap", "_cap.allTiers")
lvl_scenarios <- paste0(paste0("RS", rep(1:5, 3)), rep(lvl_policies, each = 5))
lvl_measures  <- c("FR40less", "ERC_hike", "ERC_high") 

# Summary tables for the three major risk measures
tab_summary1 <- 
  df_all.stch %>% filter(runname %in% c(runs_all[1:15]), year == 2044, Tier == "sumTiers") %>% 
  select(runname, FR40less, ERC_hike, ERC_high) %>% 
  gather(Measure, value, -runname) %>% 
  mutate(runname = factor(runname, levels = lvl_scenarios),
         Measure = factor(Measure, levels = lvl_measures)) %>% 
  spread(runname, value)


# Summary tables for EEC risks
tab_summary2 <- 
df_all.stch %>% filter(runname %in% c(runs_all[1:15]), year == 2044) %>% 
  mutate(policy = str_sub(runname, 5)) %>% 
  filter(!(policy == "cap" & Tier == "sumTiers"), !(policy == "cap" & Tier == "xt7")) %>% 
  select(runname, EEC_high, EEC_PR.q75) %>% 
  gather(Measure, value, -runname) %>% 
  mutate(runname = factor(runname, levels = lvl_scenarios)) %>% 
  spread(runname, value)




#*************************************************************************
##                        Saving results                              ####
#*************************************************************************

g.height <- 5
g.width <- 12

# g.height.3col <- 
# g.width.3col <- 

ggsave(file = paste0(Outputs_folder, "fig_distReturn.png"), fig_distReturn, height = 7, width = 10)
ggsave(file = paste0(Outputs_folder, "fig_distFR30.png"), fig_distFR30, height = 7, width = 10)
ggsave(file = paste0(Outputs_folder, "fig_stchDet.FR40less.png"), fig_stchDet.FR40less, height = 7, width = 10)
ggsave(file = paste0(Outputs_folder, "fig_stchDet.ERChigh.png"), fig_stchDet.ERChigh, height = 7, width = 10)
ggsave(file = paste0(Outputs_folder, "fig_stchDet.ERChike.png"), fig_stchDet.ERChike, height = 7, width = 10)
ggsave(file = paste0(Outputs_folder, "fig_stchDet.3measures.png"), fig_stchDet.3measures, height = g.height, width = g.width)
ggsave(file = paste0(Outputs_folder, "fig_stchDet.2measures.png"), fig_stchDet.2measures, height = 1.1*g.height, width = g.width)
ggsave(file = paste0(Outputs_folder, "fig_stchDet.ERCdist.png"), fig_stchDet.ERCdist, height = 7*0.8, width = 10*0.8)
ggsave(file = paste0(Outputs_folder, "fig_stchDet.FRdist.png"), fig_stchDet.FRdist, height = 7*0.8, width = 10*0.8)



ggsave(file = paste0(Outputs_folder, "fig_policy.FRdist.png"), fig_policy.FRdist, height = 7, width = 13)
ggsave(file = paste0(Outputs_folder, "fig_policy.ERCdist.png"), fig_policy.ERCdist, height = 0.8*5, width = 0.8*15)
ggsave(file = paste0(Outputs_folder, "fig_policy.ERChigh.png"), fig_policy.ERChigh, height = 7*0.9, width = 10*0.9)
ggsave(file = paste0(Outputs_folder, "fig_policy.ERChike.png"), fig_policy.ERChike, height = 7*0.9, width = 10*0.9)
ggsave(file = paste0(Outputs_folder, "fig_policy.EECdist.t7.png"), fig_policy.EECdist.t7, height = 7*0.9, width = 10*0.9)
ggsave(file = paste0(Outputs_folder, "fig_policy.EEChigh.t7.png"), fig_policy.EEChigh.t7, height = 7*0.9, width = 9*0.9)
ggsave(file = paste0(Outputs_folder, "fig_policy.EECdist.allTiers.png"), fig_policy.EECdist.allTiers, height = 7*0.9, width = 10*0.9)
ggsave(file = paste0(Outputs_folder, "fig_policy.EEChigh.allTiers.png"), fig_policy.EEChigh.allTiers, height = 7*0.9, width = 9*0.9)


ggsave(file = paste0(Outputs_folder, "fig_compareRS1.MedFR.png"), fig_compareRS1.MedFR, height = 7*0.9, width = 10*0.9)
ggsave(file = paste0(Outputs_folder, "fig_compareRS1.FR40less.png"), fig_compareRS1.FR40less, height = 7*0.9, width = 10*0.9)
ggsave(file = paste0(Outputs_folder, "fig_compareRS1.MedERC.png"), fig_compareRS1.MedERC, height = 0.8*5, width = 0.8*15)
ggsave(file = paste0(Outputs_folder, "fig_compareRS1.ERChigh.png"), fig_compareRS1.ERChigh, height = 0.8*5, width = 0.8*15)
ggsave(file = paste0(Outputs_folder, "fig_compareRS1.ERChike.png"), fig_compareRS1.ERChike, height = 0.8*5, width = 0.8*15)
ggsave(file = paste0(Outputs_folder, "fig_compareRS1.EECdist.t7.png"), fig_compareRS1.EECdist.t7, height = 5, width = 15)
ggsave(file = paste0(Outputs_folder, "fig_compareRS1.EEChigh.t7.png"), fig_compareRS1.EEChigh.t7, height = 7, width = 10)
ggsave(file = paste0(Outputs_folder, "fig_compareRS1.EECdist.allTiers.png"), fig_compareRS1.EECdist.allTiers, height = 5, width = 15)
ggsave(file = paste0(Outputs_folder, "fig_compareRS1.EEChigh.allTiers.png"), fig_compareRS1.EEChigh.allTiers, height = 7, width = 10)


ggsave(file = paste0(Outputs_folder, "fig_compareRS2.MedFR.png"), fig_compareRS2.MedFR, height = 7*0.9, width = 10*0.9)
ggsave(file = paste0(Outputs_folder, "fig_compareRS2.FR40less.png"), fig_compareRS2.FR40less, height = 7*0.9, width = 10*0.9)
ggsave(file = paste0(Outputs_folder, "fig_compareRS2.MedERC.png"), fig_compareRS2.MedERC, height = 0.8*5, width = 0.8*15)

ggsave(file = paste0(Outputs_folder, "fig_compareRS2.distFR.png"), fig_compareRS2.distFR, height = 5*0.9, width = 15*0.9)
ggsave(file = paste0(Outputs_folder, "fig_compareRS2.distERC.png"), fig_compareRS2.distERC, height = 0.8*5, width = 0.8*15)

ggsave(file = paste0(Outputs_folder, "fig_compareRS2.ERChigh.png"), fig_compareRS2.ERChigh, height = 0.8*5, width = 0.8*15)
ggsave(file = paste0(Outputs_folder, "fig_compareRS2.ERChike.png"), fig_compareRS2.ERChike, height = 0.8*5, width = 0.8*15)
ggsave(file = paste0(Outputs_folder, "fig_compareRS2.EECdist.t7.png"), fig_compareRS2.EECdist.t7, height = 5, width = 15)
ggsave(file = paste0(Outputs_folder, "fig_compareRS2.EEChigh.t7.png"), fig_compareRS2.EEChigh.t7, height = 7, width = 10)
ggsave(file = paste0(Outputs_folder, "fig_compareRS2.EECdist.allTiers.png"), fig_compareRS2.EECdist.allTiers, height = 5, width = 15)
ggsave(file = paste0(Outputs_folder, "fig_compareRS2.EEChigh.allTiers.png"), fig_compareRS2.EEChigh.allTiers, height = 7, width = 10)

write.xlsx2(df_det, paste0(Outputs_folder, "tables.xlsx"), sheetName = "det_full")
write.xlsx2(df_det.short, paste0(Outputs_folder, "tables.xlsx"), sheetName = "det_short", append = TRUE)
write.xlsx2(tab_summary1, paste0(Outputs_folder, "tables.xlsx"), sheetName = "summary1", append = TRUE)
write.xlsx2(tab_summary2, paste0(Outputs_folder, "tables.xlsx"), sheetName = "summary2", append = TRUE)


#**************************************************************************
## #Explore the impact of modeling Tier 7 separately on funded status  ####
#**************************************************************************

# Runs involved: RS1 and RS1_cap
# Variables involved: 
  # NC, SC, ADC, C, ERC, EEC

# Look for sims under RS1 with a extended period of zero ERC

results_all %>% filter(runname == "RS1", sim == 2, year <=2040) %>% 
  select(runname, sim, year, NC, NC_PR, SC, ADC, B, C, EEC, ERC, AL, MA, AA, UAAL, EUAAL, LG, FR_MA, I.r)
# %>% 
  mutate(ADC_unadj = NC + SC)
  
  
results_all %>% filter(runname == "RS1_cap", sim == 2, Tier == "sumTiers", year <=2040) %>% 
  select(runname, sim, year, NC, SC, ADC, B, C, EEC, ERC, AL, MA, AA, UAAL, EUAAL, LG, FR_MA, I.r)
  # %>% 
  mutate(ADC_unadj = NC + SC)
  

  results_all %>% filter(runname == "RS1_cap", sim == 2, Tier == "t7", year <=2044) %>% 
    select(runname, sim, year, NC, NC_PR, SC, ADC, B, C, EEC, ERC, AL, MA, AA, UAAL, EUAAL, LG, FR_MA, C_PR) %>% 
  # %>% 
  mutate(ADC_unadj = NC + SC)

results_all %>% filter(runname == "RS1_cap", sim == 2) %>% 
  select(runname, sim, Tier, NC, SC, ADC,B, C, year, ERC, ERC_PR, AL, MA, AA) %>% 
  mutate(ADC_unadj = NC + SC) %>% 
  gather(variable, value, -runname, -sim, -Tier, -year) %>% 
  mutate(variable = paste(variable, Tier, sep = ".")) %>% 
  select(-Tier) %>% 
  spread(variable, value) %>% 
  select(runname, sim, year, ends_with("sumTiers"), ends_with(".t7"))




results_all %>% filter(runname == "RS1", sim == 0) %>% 
  select(runname, sim, year, NC, SC, ADC, C, EEC, ERC, ERC_PR, AL, MA, AA, FR_MA, LG, UAAL, Amort_basis) %>% 
  mutate(ADC_unadj = NC + SC)

results_all %>% filter(runname == "RS1_cap", sim == 0) %>% 
  select(runname, sim, Tier, NC, SC, ADC, C, year, ERC, ERC_PR, AL, MA, FR_MA) %>% 
  mutate(ADC_unadj = NC + SC) %>% 
  gather(variable, value, -runname, -sim, -Tier, -year) %>% 
  mutate(variable = paste(variable, Tier, sep = ".")) %>% 
  select(-Tier) %>% 
  spread(variable, value) %>% 
  select(runname, sim, year, ends_with("sumTiers"), ends_with(".t7"))








# #*****************************************************
# ## Exploratory graphs  ####
# #*****************************************************
# 
# 
# df_all.stch %<>% filter(!Tier %in% c("t7", "xt7"))
# 
# g.FR40less <- 
#   df_all.stch %>% 
#   ggplot(aes(x = year, y = FR40less, color = runname)) + theme_bw() + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_continuous(breaks = seq(2015, 2100, 5))
# g.FR40less
# 
# 
# g.ERCsharpRise <- 
#   df_all.stch %>% 
#   ggplot(aes(x = year, y = ERC_hike, color = runname)) + theme_bw() + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_continuous(breaks = seq(2015, 2100, 5))
# g.ERCsharpRise
# 
# 
# g.ERChigh <- 
#   df_all.stch %>%  
#   ggplot(aes(x = year, y = ERC_high, color = runname)) + theme_bw() + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_continuous(breaks = seq(2015, 2100, 5))
# g.ERChigh
# 
# 
# g.FR.pctmore <-
#   df_all.stch %>% 
#   ggplot(aes(x = year, y = FR100more, color = runname)) + theme_bw() + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_continuous(breaks = seq(2015, 2100, 5))
# g.FR.pctmore
# 
# g.FR.pctmore2 <-
#   df_all.stch %>% 
#   ggplot(aes(x = year, y = FR100more2, color = runname)) + theme_bw() + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_continuous(breaks = seq(2015, 2100, 5))
# g.FR.pctmore2
# 
# 
# g.FR.qts <- 
#   df_all.stch %>% 
#   select(runname, year, starts_with("FR.q")) %>% 
#   gather(var, value, -runname, -year) %>% 
#   ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
#   geom_point() + 
#   geom_line() + 
#   geom_hline(yintercept = 100, linetype = 2) + 
#   scale_x_continuous(breaks = seq(2015, 2100, 5))
# g.FR.qts
# 
# 
# g.FR.qts2 <- 
#   df_all.stch %>% 
#   select(runname, year, starts_with("FR.q")) %>% 
#   gather(var, value, -runname, -year) %>% 
#   ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
#   geom_point() + 
#   geom_line() + 
#   geom_hline(yintercept = 100, linetype = 2) + 
#   coord_cartesian(ylim = c(0,200)) + 
#   scale_x_continuous(breaks = seq(2015, 2100, 5))
# g.FR.qts2
# 
# 
# g.ERC_PR.qts <- 
#   df_all.stch %>% 
#   select(runname, year, starts_with("ERC_PR.q")) %>% 
#   gather(var, value, -runname, -year) %>%
#   ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_continuous(breaks = seq(2015, 2100, 5)) +
#   scale_y_continuous(breaks = seq(0, 100, 10))
# g.ERC_PR.qts
# 
# 
# 
# ggsave(g.FR40less,   file = paste0(outputs.folder, "g.FR40less.png"), width  = 10, height = 5)
# ggsave(g.ERCsharpRise, file = paste0(outputs.folder, "g.ERCsharpRise.png"), width  = 10, height = 5)
# ggsave(g.ERChigh,      file = paste0(outputs.folder, "g.ERChigh.png"), width  = 10, height = 5)
# ggsave(g.FR.pctmore,   file = paste0(outputs.folder, "g.FR.pctmore.png"), width  = 10, height = 5)
# ggsave(g.FR.pctmore2,   file = paste0(outputs.folder, "g.FR.pctmore2.png"), width  = 10, height = 5)
# 
# ggsave(g.FR.qts,     file = paste0(outputs.folder, "g.FR.qts.png"), width  = 15, height = 5)
# ggsave(g.FR.qts2,     file = paste0(outputs.folder, "g.FR.qts2.png"), width  = 15, height = 5)
# ggsave(g.ERC_PR.qts, file = paste0(outputs.folder, "g.ERC_PR.qts.png"), width  = 15, height = 5)
# 
# 
# 
# 
# 
# #*****************************************************
# ## Assumption achieved  ####
# #*****************************************************
# 
# 
# 
# 
# 
# 
# #*****************************************************
# ## Low returns in early years  ####
# #*****************************************************
# 
# 
# 
# 
# #********************************************************************
# ## Return scenarios based on capital market assumptions          ####
# #********************************************************************
# 
# 
# 
# 
# 

G2A <- function(G, V){
  ArithMean <- (1 + G)*(0.5 + 0.5*(1 + 4*V/(1 + G)^2 )^0.5 )^0.5 - 1
}

(G2A(0.075, 0.12))



