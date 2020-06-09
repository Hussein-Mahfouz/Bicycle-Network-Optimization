library(tidyverse)
library(stplanr)

# read in the data
flow <- readr::read_csv("../data/alt_city/flows_dist_elev_for_potential_flow.csv")

###############
# OPTION 1: Use a distance decay function from stplanr https://itsleeds.github.io/pct/reference/uptake_pct_godutch.html
###############

# create a copy of the df 
demand_decay <- flow

# get % increase in cycling as distance decay 
demand_decay$uptake_dutch = pct::uptake_pct_godutch(distance = demand_decay$dist, gradient = demand_decay$slope)

# get potential demand: non-active flow*uptake + active flow
demand_decay <- demand_decay %>% 
  # get current active travel
  mutate(active_travel = Bicycle + `On foot`) %>% 
  # get potential active travel: non-active modes * distance decay parameter
  mutate(potential_demand = round((`All categories: Method of travel to work` - active_travel) * uptake_dutch) +
           active_travel) 

# save csv to use in '4_aggregating_flows'
demand_decay %>% 
  subset(select = c(`Area of residence`, `Area of workplace`, `potential_demand`)) %>%
  write_csv(path = "../data/alt_city/flows_for_aggregated_routing_opt_1.csv")

# % increase in active travel
demand_decay$perc_increase = (((demand_decay$potential_demand - demand_decay$active_travel) / 
                                 demand_decay$active_travel) *100)





