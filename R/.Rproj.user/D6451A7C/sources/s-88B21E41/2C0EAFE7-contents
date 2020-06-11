library(tidyverse)
library(sf)
library(dodgr)


# read in MSOA centroids
lon_lat <- st_read('../data/alt_city/msoa_lon_lat.shp') 
# flow data: there are 3 files (check script 3 and choose 1)
od_flow <- read_csv('../data/alt_city/flows_for_aggregated_routing_opt_3.csv') 

# remove flows where ORIGIN = DESTINATION
od_flow <- od_flow %>% filter(`Area of residence` != `Area of workplace`)

# tranform into distance matrix for routing. 
# column_to_rownames added so that 1st column is not the MSOA names. It assigns the MSOA names to the row ID
od_flow_matrix <- 
  od_flow %>% 
  pivot_wider(names_from = `Area of workplace`, values_from = potential_demand) %>%
  tibble::column_to_rownames(var = "Area of residence") %>%  
  replace(is.na(.), 0) %>% as.matrix()    
# last line gives 0 values to intra flows, as NAs would cause problems
# This doesn't matter in calculations as intra flows aren't aggregated on road segments


# 'from' is the 1st Column - get the MSOA names using rownames and join them with the coordinates in lon_lat
from <- rownames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "msoa11cd")) %>%
  dplyr::select(lon, lat) %>% as.matrix()

# same as above but 'to is the column names(i.e. index row)
to <- colnames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "msoa11cd")) %>%
  dplyr::select(lon, lat) %>% as.matrix()

# # subsets for testing (THIS WORKS)
# from2 <- rownames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "MSOA11CD")) %>%
#   dplyr::select(lon, lat) %>% slice(1:10) %>% as.matrix()
# to2 <- colnames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "MSOA11CD")) %>%
#   dplyr::select(lon, lat) %>% slice(1:10) %>% as.matrix()
#   # select first 10 rows then 1st 10 columns
# od_flow_matrix_2 <- od_flow_matrix %>% as.data.frame() %>% slice(1:10) %>% select(1:10) %>% as.matrix()

# ROUTE
# load in graph save in script 2
graph <- readRDS('../data/alt_city/city_graph.Rds')

# add flows to road segments
graph_flows <- dodgr_flows_aggregate(
                  graph = graph,
                  from = from,
                  to = to,
                  flows = od_flow_matrix,
                  contract = TRUE,
                  quiet = FALSE)

# Turn into undirected graph
graph_undir <- dodgr::merge_directed_flows(graph_flows)

# convert to sf for prioritixing segments. 
# Notice that the segments are merged (much less than graph_flows)
graph_sf <- graph_undir %>% dodgr_to_sf()

# save as RDS to load in next script (geojson, shp etc cause problems)
saveRDS(graph_sf, file = "../data/alt_city/graph_with_flows.Rds")

rm(from, graph, graph_flows, graph_sf, graph_undir, lon_lat, od_flow, od_flow_matrix, to)


