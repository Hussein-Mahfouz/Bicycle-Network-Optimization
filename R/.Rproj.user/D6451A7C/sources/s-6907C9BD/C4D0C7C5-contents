library(tidyverse)
library(sf)
library(dodgr)

# read in MSOA centroids
lon_lat <- read_csv('../data/msoa_lon_lat.csv') 
# flow data
od_flow <- read_csv('../data/flows_for_aggregated_routing.csv') 

# remove flows where ORIGIN = DESTINATION
od_flow <- od_flow %>% filter(`Area of residence` != `Area of workplace`)

# tranform into distance matrix for routing. 
# column_to_rownames added so that 1st column is not the MSOA names. It assigns the MSOA names to the row ID
od_flow_matrix <- 
  od_flow %>% 
  pivot_wider(names_from = `Area of workplace`, values_from = potential_demand) %>%
  tibble::column_to_rownames(var = "Area of residence") %>%  
  replace(is.na(.), 0) %>% as.matrix()    # EDIT THIS!!!!!



# 'from' is the 1st Column - get the MSOA names using rownames and join them with the coordinates in lon_lat
from <- rownames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "MSOA11CD")) %>%
  subset(select = c(lon, lat)) %>% as.matrix()

# same as above but 'to is the column names(i.e. index row)
to <- colnames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "MSOA11CD")) %>%
  subset(select = c(lon, lat)) %>% as.matrix()


# # subsets for testing (THIS WORKS)
# from2 <- rownames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "MSOA11CD")) %>%
#   subset(select = c(lon, lat)) %>% slice(1:10) %>% as.matrix()
# to2 <- colnames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "MSOA11CD")) %>%
#   subset(select = c(lon, lat)) %>% slice(1:10) %>% as.matrix()
#   # select first 10 rows then 1st 10 columns
# od_flow_matrix_2 <- od_flow_matrix %>% as.data.frame() %>% slice(1:10) %>% select(1:10) %>% as.matrix()

# ROUTE

# get road network

# bounding box function attempt (Fails)
# bb_ldn <- bb_buffer(city = "london uk", buffer = 1000)
# streetnet <- dodgr_streetnet(bbox = bb_ldn)    # `Error: vector memory exhausted (limit reached?)`

streetnet <- dodgr_streetnet("london uk", expand = 0.05) # 217101 rows    # 1794.96 mb
graph <- weight_streetnet(streetnet, wt_profile = "bicycle")
#OR IF IT IS SAVED
#graph <- readRDS("../data/london_roads.Rds")

# free up some space
rm(streetnet)
# I have it stored locally, but perhaps it is better to generate it from the distance_matrix script,
# due to differences in classes
#graph2 <- read_csv('../data/weighted_graph.csv')



res <- dodgr_flows_aggregate(
          graph = graph,
          from = from,
          to = to,
          flows = od_flow_matrix,
          contract = TRUE,
          quiet = FALSE)






