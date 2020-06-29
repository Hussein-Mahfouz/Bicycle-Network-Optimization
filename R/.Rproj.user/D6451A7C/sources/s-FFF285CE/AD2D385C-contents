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

# prepare a dataframe for plotting the results
grow_flow_c <- grow_flow %>% 
  ungroup %>%   # not sure why it is a grouped df. This only has an effect on the select argument
  #st_drop_geometry() %>% 
  dplyr::select(Community, d, flow, cycle_infra, sequen, perc_dist, perc_flow,
                perc_person_km, perc_dist_comm, perc_flow_comm, perc_person_km_comm, highway)

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


# add a categorical column to show which investment brack the segment is in (0:100, 100:200 etc)
grow_flow_c$distance_groups <- cut(grow_flow_c$dist_c, breaks = seq(from = 0, to = max(grow_flow_c$dist_c) + 100, by = 100))

# network level plot
ggplot(data=grow_flow_c , aes(x=dist_c, y=perc_person_km_c)) +
  geom_line() +
  ggtitle("Connected growth - investment distributed \nequally between communities") +
  labs(x = "Length of Investment (km)", y = "% of person km satisfied",
       subtitle=expression("Segments Prioritized Based On **Flow**")) +
       theme_minimal() +
       theme(plot.title = element_text(size = 14)) +
       theme(plot.subtitle = element_markdown(size = 10))

ggsave(paste0("../data/", chosen_city,"/Plots/satisfied_km_all_flow_column.png"))

# community level plots
ggplot(data=grow_flow_c, 
       aes(x=dist_c_comm, y=perc_person_km_comm_c, group=Community, color = Community)) +
  geom_line() + 
  ggtitle("Connected growth - investment distributed \nequally between communities") +
  labs(x = "Length of Investment in Community (km)", y = "% of person km satisfied",
       subtitle="Segments Prioritized Based On **Flow**") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.subtitle = element_markdown(size = 10))

ggsave(paste0("../data/", chosen_city,"/Plots/satisfied_km_community_flow_column.png"))


# Plot how many kms of investment are needed on each highway type. Use the distance_group categorical variable 
# to show how the investment on each highway type is split. e.g. (10km in the first 100km overall, 
# 7km in the 100:200km bracket, 50km in the 200:330km bracket...)

grow_flow_c %>% st_drop_geometry() %>% 
  dplyr::filter(!(highway %in% c('trunk_link', 'track', 'tertiary_link', 'steps', 
                                 'secondary_link', 'primary_link', 'living_street', 'motorway_link', 'cycleway'))) %>%
  group_by(highway, distance_groups) %>%
  summarize(dist = sum(d) /1000) %>%
  mutate(dist_perc = dist / sum(dist) * 100) -> p

# plot
ggplot(data=p , aes(x=highway, y=dist, fill = distance_groups)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = "Blues", direction=-1) +
  ggtitle("Investment on Different Highway Types") +
  labs(x = "Highway Type", y = "Length (km)", fill = "Investment Priority \n(km groups)", 
       subtitle="Segments Prioritized Based On **Flow**") +
  theme_minimal() +
  # edit angle of text, hjust argument so that text stays below plot AND center plot title
  theme(axis.text.x = element_text(angle=50, hjust=1)) +
  theme(plot.subtitle = element_markdown()) +
  coord_flip()

ggsave(paste0("../data/", chosen_city,"/Plots/investment_highways_flow_growth_2.png"))

# grow the network based on the person_km column. 
grow_person_km <- growth_community_2(graph = graph_sf, km = 500, col_name = "person_km")

# prepare a dataframe for ggplot
grow_person_km_c <- grow_person_km %>% 
  ungroup %>%   # not sure why it is a grouped df. This only has an effect on the select argument
  dplyr::select(Community, d, flow, cycle_infra, sequen, perc_dist, perc_flow,
                perc_person_km, perc_dist_comm, perc_flow_comm, perc_person_km_comm, highway) %>%
  mutate(dist_c = cumsum(d/1000),
         perc_dist_c = cumsum(perc_dist),
         perc_person_km_c = cumsum(perc_person_km)) %>%
  # groupby so that you can apply cumsum by community 
  group_by(Community) %>% 
  mutate(dist_c_comm = cumsum(d/1000),
         perc_dist_comm_c = cumsum(perc_dist_comm),
         perc_person_km_comm_c = cumsum(perc_person_km_comm))


# add a categorical column to show which investment brack the segment is in (0:100, 100:200 etc)
grow_person_km_c$distance_groups <- cut(grow_person_km_c$dist_c, breaks = seq(from = 0, to = max(grow_person_km_c$dist_c) + 100, by = 100))


ggplot(data=grow_person_km_c , aes(x=dist_c, y=perc_person_km_c)) +
  geom_line() +
  ggtitle("Connected growth - investment distributed \nequally between communities") +
  labs(x = "Length of Investment (km)", y = "% of person km satisfied",
       subtitle="Segments Prioritized Based On **Person km** Satisfied") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.subtitle = element_markdown(size = 10))

ggsave(paste0("../data/", chosen_city,"/Plots/satisfied_km_all_person_km_column.png"))


ggplot(data=grow_person_km_c, 
       aes(x=dist_c_comm, y=perc_person_km_comm_c, group=Community, color = Community)) +
  geom_line() + 
  ggtitle("Connected growth - investment distributed \nequally between communities ") +
  labs(x = "Length of Investment in Community (km)", y = "% of Person km Satisfied",
       subtitle="Segments Prioritized Based On **Person km** Satisfied") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14)) +
  theme(plot.subtitle = element_markdown(size = 10))

ggsave(paste0("../data/", chosen_city,"/Plots/satisfied_km_community_person_km_column.png"))


# Plot how many kms of investment are needed on each highway type. Use the distance_group categorical variable 
# to show how the investment on each highway type is split.

grow_person_km_c %>% st_drop_geometry() %>% 
  dplyr::filter(!(highway %in% c('trunk_link', 'track', 'tertiary_link', 'steps', 
                                 'secondary_link', 'primary_link', 'living_street', 'motorway_link', 'cycleway'))) %>%
  group_by(highway, distance_groups) %>%
  summarize(dist = sum(d) /1000) %>%
  mutate(dist_perc = dist / sum(dist) * 100) -> p

# plot
ggplot(data=p , aes(x=highway, y=dist, fill = distance_groups)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = "Blues", direction=-1) +
  ggtitle("Investment on Different Highway Types") +
  labs(x = "Highway Type", y = "Length (km)", fill = "Investment Priority \n(km groups)", 
       subtitle="Segments Prioritized Based On **Person km** Satisfied") +
  theme_minimal() +
  # edit angle of text, hjust argument so that text stays below plot AND center plot title
  theme(axis.text.x = element_text(angle=50, hjust=1)) +
  theme(plot.subtitle = element_markdown()) +
  coord_flip()

ggsave(paste0("../data/", chosen_city,"/Plots/investment_highways_person_km_growth_2.png"))


########## MAPS SHOWING SEQUENCE OF GROWTH AT THE NETWORK LEVEL AND AT THE COMMUNITY LEVEL #############

# we want edges added first to have a thicker lineweight. I add a column to inverse the sequence because tmap
#will automatically give higher numbers a thicker weight when passing a variable to 'lwd'
grow_flow_c$sequen_inv <- max(grow_flow_c$sequen) - grow_flow_c$sequen
grow_person_km_c$sequen_inv <- max(grow_person_km_c$sequen) - grow_person_km_c$sequen

###### RESULT FROM GROWTH BASED ON FLOW COLUMN   ######

#### Plot with colour proportional to km (from cumulative distance column) instead of sequence  ####
tm_shape(graph_sf) +
  tm_lines(col = 'gray95') +
  tm_shape(grow_flow_c) +
  tm_lines(title.col = "Priority (km)",
           col = 'dist_c',    # could do col='sequen' to 
           lwd = 'sequen_inv',
           scale = 1.8,     #multiply line widths by X
           palette = "-Blues",
           #style = "cont",   # to get a continuous gradient and not breaks
           legend.lwd.show = FALSE) +
  tm_layout(title = "Growing A Network Based on Flow",        
            title.size = 1.2,
            title.color = "azure4",
            title.position = c("left", "top"),
            inner.margins = c(0.1, 0.1, 0.1, 0.1),    # bottom, left, top, and right margin
            fontfamily = 'Georgia',
            legend.position = c("right", "bottom"),
            frame = FALSE) -> p

tmap_save(tm = p, filename = paste0("../data/", chosen_city,"/Plots/priority_all_growth_community_2_FLOW.png"))

# same but facet by community
tm_shape(graph_sf) +
  tm_lines(col = 'gray95') +
  tm_shape(grow_flow_c) +
  tm_lines(title.col = "Priority (km)",
           col = 'dist_c', 
           lwd = 'sequen_inv',
           scale = 1.2,     #multiply line widths by X
           palette = "-Blues",
           #style = "cont",   # to get a continuous gradient and not breaks
           legend.lwd.show = FALSE) +
  tm_facets(by="Community",
            nrow = 1,
            free.coords=FALSE)  +  # so that the maps aren't different sizes
  tm_layout(main.title = "Priority of Road Segments in Different Communities",        
            main.title.size = 1.2,
            main.title.color = "azure4",
            main.title.position = c("left", "top"),
            fontfamily = 'Georgia',
            legend.outside.position = c("right", "bottom"),
            frame = FALSE) -> p

tmap_save(tm = p, filename = paste0("../data/", chosen_city,"/Plots/priority_community_growth_community_2_FLOW.png"), 
          width=10, height=4)

###### RESULT FROM GROWTH BASED ON PERSON_KM COLUMN   ######
tm_shape(graph_sf) +
  tm_lines(col = 'gray95') +
  tm_shape(grow_person_km_c) +
  tm_lines(title.col = "Priority (km)",
           col = 'dist_c',    # could do col='sequen' to 
           lwd = 'sequen_inv',
           scale = 1.8,     #multiply line widths by X
           palette = "-Blues",
           #style = "cont",   # to get a continuous gradient and not breaks
           legend.lwd.show = FALSE) +
  tm_layout(title = "Growing A Network Based on Person Km Satisfied",        
            title.size = 1.2,
            title.color = "azure4",
            title.position = c("left", "top"),
            inner.margins = c(0.1, 0.1, 0.1, 0.1),    # bottom, left, top, and right margin
            fontfamily = 'Georgia',
            legend.position = c("right", "bottom"),
            frame = FALSE) -> p

tmap_save(tm = p, filename = paste0("../data/", chosen_city,"/Plots/priority_all_growth_community_2_PERSON_KM.png"))




                              #########################################
# growth_community_4 function starts with all edges that already have cycling infrastructure. It may be more 
# practical in real life. It also shows the no of components and the size of the gcc at each iteration
                              #########################################
# No of components growth_community_4. Just get results then filter out all cycle infra and readjust dist_c column

# let's grow the network based on the flow column
grow_flow_infra <- growth_community_4(graph = graph_sf, km = 500, col_name = "flow")
# get % of edges in gcc
grow_flow_infra$gcc_size_perc <- (grow_flow_infra$gcc_size / nrow(graph_sf)) * 100
# prepare a dataframe for ggplot
grow_flow_infra_c <- grow_flow_infra %>% 
  ungroup %>%   # not sure why it is a grouped df. This only has an effect on the select argument
  filter(cycle_infra == 0) %>% # all edges with cycle infrastructure were added at the beginning
  #st_drop_geometry() %>% 
  dplyr::select(Community, d, flow, highway, cycle_infra, sequen, perc_dist, perc_flow,
                perc_person_km, perc_dist_comm, perc_flow_comm, perc_person_km_comm, no_components,
                gcc_size, gcc_size_perc)

# cumsum is cumulative sum. We see how much of person_km has been satisfied after each iteration 
grow_flow_infra_c <- grow_flow_infra_c %>%
  mutate(dist_c = cumsum(d/1000),
         perc_dist_c = cumsum(perc_dist),
         perc_person_km_c = cumsum(perc_person_km)) %>%
  # groupby so that you can apply cumsum by community 
  group_by(Community) %>% 
  mutate(dist_c_comm = cumsum(d/1000),
         perc_dist_comm_c = cumsum(perc_dist_comm),
         perc_person_km_comm_c = cumsum(perc_person_km_comm))

# add a categorical column to show which investment brack the segment is in (0:100, 100:200 etc)
grow_flow_infra_c$distance_groups <- cut(grow_flow_infra_c$dist_c, breaks = seq(from = 0, to = max(grow_flow_infra_c$dist_c) + 100, by = 100))

# network level plot showing no of components (decreasing?) as network grows
ggplot(data=grow_flow_infra_c , aes(x=dist_c, y=no_components)) +
  geom_line() +
  ggtitle("Number of Components Making Up Network") +
  labs(x = "Length of Investment (km)", y = "No. of Components",
       subtitle=expression("Segments Prioritized Based On **Flow**")) +
  theme_minimal() +
  theme(plot.subtitle = element_markdown())

ggsave(paste0("../data/", chosen_city,"/Plots/components_number_flow_growth_4.png"))


# network level plot showing size of gcc 
ggplot(data=grow_flow_infra_c , aes(x=dist_c, y=gcc_size_perc)) +
  geom_line() +
  ggtitle("Edges in Largest Connected Component") +
  labs(x = "Length of Investment (km)", y = "% of Edges in LCC",
       subtitle=expression("Segments Prioritized Based On **Flow**")) +
  theme_minimal() +
  theme(plot.subtitle = element_markdown())

ggsave(paste0("../data/", chosen_city,"/Plots/components_gcc_flow_growth_4.png"))


# Plot how many kms of investment are needed on each highway type. Use the distance_group categorical variable 
# to show how the investment on each highway type is split


grow_flow_infra_c %>% st_drop_geometry() %>% 
  group_by(highway, distance_groups) %>%
  summarize(dist = sum(d) /1000) %>%
  mutate(dist_perc = dist / sum(dist) * 100) %>%  
  dplyr::filter(!(highway %in% c('trunk_link', 'track', 'tertiary_link', 'steps', 
                                 'secondary_link', 'primary_link', 'living_street', 'motorway_link', 'cycleway'))) -> p

# plot of length of investment on each highway type
ggplot(data=p , aes(x=highway, y=dist, fill = distance_groups)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = "Blues", direction=-1) +
  ggtitle("Investment on Different Highway Types") +
  labs(x = "Highway Type", y = "Length (km)", fill = "Investment Priority \n(km groups)") +
  theme_minimal() +
  # edit angle of text, hjust argument so that text stays below plot AND center plot title
  theme(axis.text.x = element_text(angle=50, hjust=1), plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave(paste0("../data/", chosen_city,"/Plots/investment_highways_flow_growth_4.png"))

###### MAP #####

# inverse sequence so that thickest edges are the ones selected first
grow_flow_infra_c$sequen_inv <- max(grow_flow_infra_c$sequen) - grow_flow_infra_c$sequen

# segments that had dedicated cycling infrastructure
initial_infra <- grow_flow_infra %>% filter(cycle_infra == 1)

#### Plot with colour proportional to km (from cumulative distance column) instead of sequence  ####
tm_shape(graph_sf) +
  tm_lines(col = 'gray95') +
tm_shape(initial_infra) +
  tm_lines(col = 'firebrick2',
           lwd = 2) +
tm_shape(grow_flow_infra_c) +
  tm_lines(title.col = "Priority (km)",
           col = 'dist_c',    # could do col='sequen' to 
           lwd = 'sequen_inv',
           scale = 1.8,     #multiply line widths by X
           palette = "-Blues",
           #style = "cont",   # to get a continuous gradient and not breaks
           legend.lwd.show = FALSE) +
  tm_layout(title = "Growing A Network Around Existing Cycling Infrastructure",        
            title.size = 1.2,
            title.color = "azure4",
            title.position = c("left", "top"),
            inner.margins = c(0.1, 0.1, 0.1, 0.1),    # bottom, left, top, and right margin
            fontfamily = 'Georgia',
            legend.position = c("right", "bottom"),
            frame = FALSE) +
  # add legend for the existing cycling infrastructure
  tm_add_legend(type = "line", labels = 'Existing Cycling Infrastructure', col = 'firebrick2', lwd = 2) -> p


tmap_save(tm = p, filename = paste0("../data/", chosen_city,"/Plots/priority_all_growth_community_4_FLOW.png"))





