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
# library(xlsx)
library("btools")

source("Functions.R")



#*****************************************************
##  Defining paths for inputs and outputs         ####
#*****************************************************
IO_folder       <- "Results/"
outputs.folder  <- "Results/Graphs_report/"


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
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
}


#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************

# Runs used in the report 
runs_RS <- paste0("RS", 1:5)
runs_cap <-  paste0("RS", 1:5, "_cap") 


runs_RS_labels <- c("Assumption Achieved",
                     "5 years of low returns",
                     "15 years of low returns",
                     "Callan",
                     "RVK")

runs_cap_labels <- c("Assumption Achieved; w/ERC cap",
                     "5 years of low returns; w/ERC cap",
                     "15 years of low returns; w/ERC cap",
                     "Callan; w/ERC cap",
                     "RVK; w/ERC cap")

runs_all <- c(runs_RS, runs_cap)
runs_all_labels <- c(runs_RS_labels, runs_cap_labels)


df_all.stch <- results_all  %>% 
  filter(runname %in% runs_all, sim >= 0, year <= 2044)


df_all.stch %<>%   
  select(runname, Tier, sim, year, AL, MA, ERC_PR) %>% 
  group_by(runname, sim, Tier) %>% 
  mutate(FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
         FR100more  = cumany(FR_MA >= 100),
         FR100more2 = FR_MA >= 100,
         ERC_high  = cumany(ERC_PR >= 50), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))) %>% 
  group_by(runname, Tier, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
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
  mutate(runname = factor(runname, 
                          levels = runs_all, 
                          labels = runs_all_labels))


df_all.stch





x <- results_all %>% filter(runname %in% c("RS1", "RS1_cap"), sim == 0, year<=2025) %>%
  select(runname, Tier,  sim, year, AL, MA, AA, B, C, nactives, NC, SC, AL.act, AL.act.death, NC.death, ndeathBen, B.death)














#*****************************************************
## Deterministic run  ####
#*****************************************************

df_det <- results_all  %>% 
  filter(runname == "RS1", sim == 0, year <= 2044) %>% 
  select(year, AL, MA, B, C, ERC, EEC, ExF, FR_MA, ERC_PR, ExF_MA, MA_PR) %>% 
  mutate_at(vars(-year, -FR_MA, -ERC_PR, -ExF_MA, -MA_PR), funs(./1e6)) %>% 
  mutate(MA_PR = MA_PR/100) %>% 
  filter(year %in% c(seq(2015, 2040, 5), 2044))
  
df_det


#*****************************************************
## Deterministic run  ####
#*****************************************************





#*****************************************************
## Exploratory graphs  ####
#*****************************************************

df_all.stch %<>% filter(!Tier %in% c("t7", "xt7"))

g.FR40less <- 
  df_all.stch %>% 
  ggplot(aes(x = year, y = FR40less, color = runname)) + theme_bw() + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR40less


g.ERCsharpRise <- 
  df_all.stch %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname)) + theme_bw() + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.ERCsharpRise


g.ERChigh <- 
  df_all.stch %>%  
  ggplot(aes(x = year, y = ERC_high, color = runname)) + theme_bw() + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.ERChigh


g.FR.pctmore <-
  df_all.stch %>% 
  ggplot(aes(x = year, y = FR100more, color = runname)) + theme_bw() + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR.pctmore

g.FR.pctmore2 <-
  df_all.stch %>% 
  ggplot(aes(x = year, y = FR100more2, color = runname)) + theme_bw() + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR.pctmore2


g.FR.qts <- 
  df_all.stch %>% 
  select(runname, year, starts_with("FR.q")) %>% 
  gather(var, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 100, linetype = 2) + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR.qts


g.FR.qts2 <- 
  df_all.stch %>% 
  select(runname, year, starts_with("FR.q")) %>% 
  gather(var, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 100, linetype = 2) + 
  coord_cartesian(ylim = c(0,200)) + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR.qts2


g.ERC_PR.qts <- 
  df_all.stch %>% 
  select(runname, year, starts_with("ERC_PR.q")) %>% 
  gather(var, value, -runname, -year) %>%
  ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5)) +
  scale_y_continuous(breaks = seq(0, 100, 10))
g.ERC_PR.qts



ggsave(g.FR40less,   file = paste0(outputs.folder, "g.FR40less.png"), width  = 10, height = 5)
ggsave(g.ERCsharpRise, file = paste0(outputs.folder, "g.ERCsharpRise.png"), width  = 10, height = 5)
ggsave(g.ERChigh,      file = paste0(outputs.folder, "g.ERChigh.png"), width  = 10, height = 5)
ggsave(g.FR.pctmore,   file = paste0(outputs.folder, "g.FR.pctmore.png"), width  = 10, height = 5)
ggsave(g.FR.pctmore2,   file = paste0(outputs.folder, "g.FR.pctmore2.png"), width  = 10, height = 5)

ggsave(g.FR.qts,     file = paste0(outputs.folder, "g.FR.qts.png"), width  = 15, height = 5)
ggsave(g.FR.qts2,     file = paste0(outputs.folder, "g.FR.qts2.png"), width  = 15, height = 5)
ggsave(g.ERC_PR.qts, file = paste0(outputs.folder, "g.ERC_PR.qts.png"), width  = 15, height = 5)





#*****************************************************
## Assumption achieved  ####
#*****************************************************






#*****************************************************
## Low returns in early years  ####
#*****************************************************




#********************************************************************
## Return scenarios based on capital market assumptions          ####
#********************************************************************









