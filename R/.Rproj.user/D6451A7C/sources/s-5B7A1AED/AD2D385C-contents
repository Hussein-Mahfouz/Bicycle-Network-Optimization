library(sf)
library(tidyverse)
library(tmap)

graph_sf <- readRDS(paste0("../data/", chosen_city, "/graph_with_flows_default_communities.RDS"))


graph_sf <- graph_sf %>%
  mutate(perc_dist = (d/sum(d))  *100,
         perc_flow = (flow/sum(flow)) * 100) %>% 
  # get the distance covered and flow satisfied for each community as a % of the community totals
  group_by(Community) %>%
  mutate(perc_dist_comm = (d/sum(d))  *100,               
         perc_flow_comm = (flow/sum(flow)) * 100) %>%
  ungroup()
  




solution <- growth_community_2(graph = graph_sf, km = 200, col_name = "flow")


cumulative_df <- solution %>% 
  ungroup %>%   # not sure why it is a grouped df. This only has an effect on the select argument
  st_drop_geometry() %>% 
  dplyr::select(Community, d, flow, cycle_infra, sequen, perc_dist, perc_flow, perc_dist_comm, perc_flow_comm)


cumulative_df <- cumulative_df %>%
  mutate(cum_dist = cumsum(perc_dist),
         cum_flow = cumsum(perc_flow)) %>%
  # groupby so that you can apply cumsum by community 
  group_by(Community) %>% 
  mutate(cum_dist_comm = cumsum(perc_dist_comm),
         cum_flow_comm = cumsum(perc_flow_comm))
  
ggplot(data=cumulative_df , aes(x=cum_dist, y=cum_flow)) +
  geom_line()

ggplot(data=cumulative_df, aes(x=cum_dist_comm, y=cum_flow_comm, group=Community, color = Community)) +
  geom_line()



