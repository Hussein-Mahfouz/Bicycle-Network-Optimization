library(tidyverse)
library(pct)
library(sf)

#Download the data using the pct package and keep only the necessary columns
flow_sub <- pct::get_pct(region = "london", layer = "rf") %>%
  st_drop_geometry %>%
  select(geo_code1, geo_code2, all, bicycle, rf_dist_km) %>%
  rename(dist = rf_dist_km)

# get % of cyclists 
flow_sub$perc_cycle <- flow_sub$bicycle / flow_sub$all

# group rows based on distance column. Change 'by' to edit number of groups
flow_sub$distance_groups <- cut(flow_sub$dist, breaks = seq(from = 0, to = 50, by = 1))
#show
flow_sub
# group by distance categories created above and get summary stats
flow_grouped <- flow_sub %>%
  group_by(distance_group = as.character(distance_groups)) %>%
  summarise(distance = mean(dist),
            perc_cycle = sum(bicycle)/ sum(all))
#show
flow_grouped
# show probabilty of cycling vs distance
ggplot(flow_grouped) +
  geom_point(aes(distance, perc_cycle)) + 
  labs( x="Commuting Distance (km)", y = "Probability of Trip Being Cycled")

# model to predict the distance group based on the % of commuters who cycle
glm1 <- glm(perc_cycle ~ distance_group, data = flow_grouped, family = "quasibinomial")

# predict cycling probability on all OD pairs
flow_sub$prob_cycle <- predict(glm1, data.frame(distance_group = flow_sub$distance_groups), type = "response")
#show
flow_sub


# to distribute flows
flow_norm <- flow_sub %>% add_count(distance_groups) %>%
  group_by(distance_groups) %>%
  summarize(prob_cycle = mean(prob_cycle),
            count = mean(n))

# the idea is that prob_cycle(1)*count(1) *X + prob_cycle(2) count(2)*X ..... = Target Cycling Trips
flow_norm$weighted_count <- flow_norm$prob_cycle * flow_norm$count

# what is the current proportion of cyclists
sum(flow_sub$bicycle) / sum(flow_sub$all)

# Let's assume we want cycling mode share to increase to 20%
target_cycle <- 0.2
# no. of additional cycling trips needed to acheive target
target <- round((target_cycle* sum(flow_sub$all)) - sum(flow_sub$bicycle))

# Here we solve for X
multiply_factor <- target / sum(flow_norm$weighted_count)

# Get the additional number of trips for each OD pair using multiplication factor found above
flow_sub$cycling_increase_20 <- flow_sub$prob_cycle * multiply_factor
# Add additional trips to current trips to get future demand
flow_sub$potential_demand_20 <- round(flow_sub$cycling_increase_20) + flow_sub$bicycle

# group to plot
flow_grouped2 <- flow_sub %>% group_by(distance_groups) %>%
  summarise(distance = mean(dist), 
            perc_cycle = sum(bicycle)/ sum(all),
            perc_cycle_20 = sum(potential_demand_20)/ sum(all))

# plot to see difference betwwen current demand and future demand
ggplot(flow_grouped2) +
  geom_point(aes(distance, perc_cycle), color = 'darkred') + 
  geom_point(aes(distance, perc_cycle_20), color = 'darkgreen') +
  labs( x="Commuting Distance (km)", y = "Probability of Trip Being Cycled")
