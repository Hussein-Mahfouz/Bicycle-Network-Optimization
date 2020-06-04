library(tidyverse)
library(stplanr)

# read in the data
flow <- readr::read_csv("../data/flows_dist_for_potential_flow.csv")

# get potential demand

###############
# OPTION 1: Use a distance decay function from stplanr https://itsleeds.github.io/pct/reference/uptake_pct_godutch.html
###############

# create a copy of the df 
demand_decay <- flow

# get % increase in cycling as distance decay 
demand_decay$uptake_dutch = pct::uptake_pct_godutch(distance = demand_decay$dist, gradient = 0)

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
  write_csv(path = "../data/flows_for_aggregated_routing.csv")

# % increase in active travel
demand_decay$perc_increase = (((demand_decay$potential_demand - demand_decay$active_travel) / 
                                 demand_decay$active_travel) *100)

###############
# OPTION 2: without distance decay
###############

# function that takes the following arguments
# 1. dataframe to be used
# 2. max_dist: cutoff distance in meters. All OD pairs above this distance are assumed to have 0 potential demand
# 3. public: fraction of public transport flow that should be considered potential cycling demand
# 4. private: fraction of private vehicle flow that should be considered potential cycling demand
# 3 & 4 are 0 if distance between MSOA pair > max_dist

# potential_demand <- function(data, max_dist = 6000, public = 0.5, private = 1) {
#   name <- data %>% 
#     mutate(active = Bicycle + `On foot`) %>%
#     mutate(sustainable = (`Underground, metro, light rail, tram` + Train + `Bus, minibus or coach`)) %>%
#     mutate(motor = (`All categories: Method of travel to work` - (sustainable + active))) %>%
#     mutate(potential_demand = if_else(dist <= max_dist, round(active + sustainable*public + motor*private), active))
#     return(name)
# }
# 
# # use function to get potential demand
# demand_dist <- potential_demand(data=flow)


###############
# OPTION 3: my own distance decay function
###############



#plot distance vs flow


ggplot(data = flow) +
  #geom_point(mapping = aes(x = dist, y = Bicycle)) +
  geom_smooth(mapping = aes(x = dist, y = Bicycle))



  

