library(sf)
library(tidyverse)
library(ggtext)
library(tmap)

graph_sf <- readRDS(paste0("../data/", chosen_city, "/graph_with_flows_default_communities.RDS"))

# we weigh the flow on each edge by its distance. We can then get how much of the commuter km are satisfied
graph_sf$person_km <- graph_sf$flow * graph_sf$d

########## GGPLOTS SHOWING FLOW/PERSON_KM SATISFIED AT THE NETWORK LEVEL AND AT THE COMMUNITY LEVEL #############

# get percentage contibution of each edge to the network (distance, flow, person_km)
graph_sf <- graph_sf %>%
  mutate(perc_dist = (d/sum(d))  *100,      # edge length as % of total network length
         perc_flow = (flow/sum(flow))  *100, # flow on edge as % of total
         perc_person_km = (person_km/sum(person_km))  *100) %>% # % of person_km satisfied
  # get the same % for each community as a % of the community totals
  group_by(Community) %>%
  mutate(perc_dist_comm = (d/sum(d)) * 100,  
         perc_flow_comm = (flow/sum(flow))  *100,
         perc_person_km_comm = (person_km/sum(person_km))  *100) %>%
  ungroup()
  
# let's grow the network based on the flow column
grow_flow <- growth_community_2(graph = graph_sf, km = 500, col_name = "flow")

# prepare a dataframe for ggplot
grow_flow_c <- grow_flow %>% 
  ungroup %>%   # not sure why it is a grouped df. This only has an effect on the select argument
  #st_drop_geometry() %>% 
  dplyr::select(Community, d, flow, cycle_infra, sequen, perc_dist, perc_flow,
                perc_person_km, perc_dist_comm, perc_flow_comm, perc_person_km_comm)

# cumsum is cumulative sum. We see how much of person_km has been satisfied after each iteration 
grow_flow_c <- grow_flow_c %>%
  mutate(dist_c = cumsum(d/1000),
         perc_dist_c = cumsum(perc_dist),
         perc_person_km_c = cumsum(perc_person_km)) %>%
  # groupby so that you can apply cumsum by community 
  group_by(Community) %>% 
  mutate(dist_c_comm = cumsum(d/1000),
         perc_dist_comm_c = cumsum(perc_dist_comm),
         perc_person_km_comm_c = cumsum(perc_person_km_comm))
  
# network level plot
ggplot(data=grow_flow_c , aes(x=dist_c, y=perc_person_km_c)) +
  geom_line() +
  ggtitle("Connected growth (investment distributed equally between communities)") +
  labs(x = "Length of Investment (km)", y = "% of person km satisfied",
       subtitle=expression("Segments Prioritized Based On **Flow**")) +
       theme_minimal() +
       theme(plot.subtitle = element_markdown())

# community level plots
ggplot(data=grow_flow_c, 
       aes(x=dist_c_comm, y=perc_person_km_comm_c, group=Community, color = Community)) +
  geom_line() + 
  ggtitle("Connected growth (investment distributed equally between communities)") +
  labs(x = "Length of Investment (km)", y = "% of person km satisfied",
       subtitle="Segments Prioritized Based On **Flow**") +
  theme_minimal() +
  theme(plot.subtitle = element_markdown())



# grow the network based on the person_km column. 
grow_person_km <- growth_community_2(graph = graph_sf, km = 500, col_name = "person_km")

# prepare a dataframe for ggplot
grow_person_km_c <- grow_person_km %>% 
  ungroup %>%   # not sure why it is a grouped df. This only has an effect on the select argument
  st_drop_geometry() %>% 
  dplyr::select(Community, d, flow, cycle_infra, sequen, perc_dist, perc_flow,
                perc_person_km, perc_dist_comm, perc_flow_comm, perc_person_km_comm) %>%
  mutate(dist_c = cumsum(d/1000),
         perc_dist_c = cumsum(perc_dist),
         perc_person_km_c = cumsum(perc_person_km)) %>%
  # groupby so that you can apply cumsum by community 
  group_by(Community) %>% 
  mutate(dist_c_comm = cumsum(d/1000),
         perc_dist_comm_c = cumsum(perc_dist_comm),
         perc_person_km_comm_c = cumsum(perc_person_km_comm))


ggplot(data=grow_person_km_c , aes(x=dist_c, y=perc_person_km_c)) +
  geom_line() +
  ggtitle("Connected growth (investment distributed equally between communities)") +
  labs(x = "Length of Investment (km)", y = "% of person km satisfied",
       subtitle="Segments Prioritized Based On **Person km** Satisfied") +
  theme_minimal() +
  theme(plot.subtitle = element_markdown())


ggplot(data=grow_person_km_c, 
       aes(x=dist_c_comm, y=perc_person_km_comm_c, group=Community, color = Community)) +
  geom_line() + 
  ggtitle("Connected growth (investment distributed equally between communities)") +
  labs(x = "Length of Investment (km)", y = "% of person km satisfied",
       subtitle="Segments Prioritized Based On **Person km** Satisfied") +
  theme_minimal() +
  theme(plot.subtitle = element_markdown())



########## MAPS SHOWING SEQUENCE OF GROWTH AT THE NETWORK LEVEL AND AT THE COMMUNITY LEVEL #############

# we want edges added first to have a thicker lineweight. I add a column to inverse the sequence because tmap
#will automatically give higher numbers a thicker weight when passing a variable to 'lwd'
grow_flow_c$sequen_inv <- max(grow_flow_c$sequen) - grow_flow_c$sequen

### Plot color proportional to sequence at which edge is selected
tm_shape(graph_sf) +
  tm_lines(col = 'gray95') +
tm_shape(grow_flow) +
  tm_lines(title.col = "Sequence",
           col = 'sequen', 
           lwd = 'sequen_inv',
           scale = 2,     #multiply line widths by X
           palette = "-Blues",
           style = "cont",   # to get a continuous gradient and not breaks
           legend.lwd.show = FALSE) +
  tm_layout(title = "Growing A Network",        
            title.size = 1.2,
            title.color = "azure4",
            title.position = c("left", "top"),
            inner.margins = c(0.1, 0.25, 0.1, 0.1),    # bottom, left, top, and right margin
            fontfamily = 'Georgia',
            legend.position = c("right", "bottom"),
            frame = FALSE)

# same but facet by community
tm_shape(graph_sf) +
  tm_lines(col = 'gray95') +
  tm_shape(grow_flow) +
  tm_lines(title.col = "Sequence",
           col = 'sequen', 
           lwd = 'sequen_inv',
           scale = 1,     #multiply line widths by X
           palette = "-Blues",
           style = "cont",   # to get a continuous gradient and not breaks
           legend.lwd.show = FALSE) +
  tm_facets(by="Community",
            nrow = 1,
            free.coords=FALSE)  +  # so that the maps aren't different sizes
  tm_layout(main.title = "Growing A Network",        
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            legend.outside.position = c("right", "bottom"),
            frame = FALSE) 



# tmap_save(tm = p, filename = paste0("../data/", chosen_city,"/Plots/testtt.png"), 
#           width=10, height=4)

#### Plot with colour proportional to km (from cumulative distance column) instead of sequence  ####
tm_shape(graph_sf) +
  tm_lines(col = 'gray95') +
  tm_shape(grow_flow_c) +
  tm_lines(title.col = "Priority (km)",
           col = 'dist_c', 
           lwd = 'sequen_inv',
           scale = 1.5,     #multiply line widths by X
           palette = "-Blues",
           #style = "cont",   # to get a continuous gradient and not breaks
           legend.lwd.show = FALSE) +
  tm_layout(title = "Growing A Network",        
            title.size = 1.2,
            title.color = "azure4",
            title.position = c("left", "top"),
            inner.margins = c(0.1, 0.25, 0.1, 0.1),    # bottom, left, top, and right margin
            fontfamily = 'Georgia',
            legend.position = c("right", "bottom"),
            frame = FALSE)
# same but facet by community
tm_shape(graph_sf) +
  tm_lines(col = 'gray95') +
  tm_shape(grow_flow_c) +
  tm_lines(title.col = "Priority (km)",
           col = 'dist_c', 
           lwd = 'sequen_inv',
           scale = 1,     #multiply line widths by X
           palette = "-Blues",
           style = "cont",   # to get a continuous gradient and not breaks
           legend.lwd.show = FALSE) +
  tm_facets(by="Community",
            nrow = 1,
            free.coords=FALSE)  +  # so that the maps aren't different sizes
  tm_layout(main.title = "Growing A Network",        
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            legend.outside.position = c("right", "bottom"),
            frame = FALSE) 





# No of components growth_community_4. Just get results then filter out all cycle infra and readjust dist_c column


# let's grow the network based on the flow column
grow_flow <- growth_community_4(graph = graph_sf, km = 200, col_name = "flow")

# prepare a dataframe for ggplot
grow_flow_c <- grow_flow %>% 
  ungroup %>%   # not sure why it is a grouped df. This only has an effect on the select argument
  filter(cycle_infra == 0) %>% # all edges with cycle infrastructure were added at the beginning
  #st_drop_geometry() %>% 
  dplyr::select(Community, d, flow, cycle_infra, sequen, perc_dist, perc_flow,
                perc_person_km, perc_dist_comm, perc_flow_comm, perc_person_km_comm, no_components)

# cumsum is cumulative sum. We see how much of person_km has been satisfied after each iteration 
grow_flow_c <- grow_flow_c %>%
  mutate(dist_c = cumsum(d/1000),
         perc_dist_c = cumsum(perc_dist),
         perc_person_km_c = cumsum(perc_person_km)) %>%
  # groupby so that you can apply cumsum by community 
  group_by(Community) %>% 
  mutate(dist_c_comm = cumsum(d/1000),
         perc_dist_comm_c = cumsum(perc_dist_comm),
         perc_person_km_comm_c = cumsum(perc_person_km_comm))

# network level plot showing no of components (decreasing?) as network grows
ggplot(data=grow_flow_c , aes(x=dist_c, y=no_components)) +
  geom_line() +
  ggtitle("No of Components Making Up Network") +
  labs(x = "Length of Investment (km)", y = "No. of Components",
       subtitle=expression("Segments Prioritized Based On **Flow**")) +
  theme_minimal() +
  theme(plot.subtitle = element_markdown())
