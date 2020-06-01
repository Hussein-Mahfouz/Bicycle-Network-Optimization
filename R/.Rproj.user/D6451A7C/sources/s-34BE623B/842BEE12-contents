library(tidyverse)
library(dodgr)
library(sf)
library(osmdata)
library(lwgeom)

# this is a lookup table matching MSOAs to major towns and cities
city_names <- read_csv('../data-raw/Middle_Layer_Super_Output_Area__2011__to_Major_Towns_and_Cities__December_2015__Lookup_in_England_and_Wales.csv') 
# change column name
city_names <- city_names %>% rename(city = TCITY15NM)
# flow data from the 2011 census https://www.nomisweb.co.uk/census/2011/bulk/rOD1
flows <- read_csv('../data-raw/flow_data.csv')

###############
# MERGING NAMES WITH FLOW DATA (TO GET INTERNAL FLOWS IN ANY CITY)
###############

# add a column with the city name corresponding to each Residence MSOA
flows <- flows %>% left_join(city_names[,c("MSOA11CD", "city")],
                             by = c("Area of residence" = "MSOA11CD")) %>%
  rename(city_origin = city) # rename column so that we know it is referring to the 'Area of residence'

# add a column with the city name corresponding to each Workplace MSOA
flows <- flows %>% left_join(city_names[,c("MSOA11CD", "city")],
                             by = c("Area of workplace" = "MSOA11CD")) %>%
  rename(city_dest = city) # rename column so that we know it is referring to the 'Area of workplace'

# Subset flows to keep only those that are within a specific city
flows_internal <- function(name) {
  x <- flows %>% filter(city_origin == name, city_dest == name)
  return(x)
}

# use function to get flows between all MSOAs in Oxford
flows_ox <- flows_internal("Oxford") %>% select(1:3) %>% 
  rename(potential_demand = `All categories: Method of travel to work`)

# download spatial data locally and read it
msoa_boundaries <- st_read('../data-raw/MSOA_2011_Boundaries/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp') %>%
  st_transform(4326)   # transform to EPSG 4326 for dodgr routing later

# function to filter MSOAs that are within a certain city
msoas_city <- function(name) {
  x <- city_names %>% filter(city == name)
  return(x)
}

# get msoas in Oxford
msoas_ox <- msoas_city("Oxford")

# Add spatial data to oxford msoas
spatial_ox <- 
  msoas_ox[, c("MSOA11CD", "MSOA11NM")] %>% 
  left_join(msoa_boundaries[ , c("msoa11cd", "geometry")], by = c("MSOA11CD" = "msoa11cd")) %>% 
  st_as_sf() %>% lwgeom::st_make_valid() %>%
  mutate(centroid = st_centroid(geometry))  # we will need centroids to calculate distance matrix

rm(msoa_boundaries)
# Remove border geometry and set centroids as geometry. This does not overwrite 'spatial_ox' sf
msoa_centroids <- 
  st_drop_geometry(spatial_ox) %>% 
  st_set_geometry('centroid') 

# function to split c(lat, lon) to two seperate columns  FROM JM London (https://github.com/r-spatial/sf/issues/231)
# lat = Y lon = X
split_lon_lat <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


# get lon and lat as seperate columns
lon_lat <-  msoa_centroids %>% split_lon_lat() %>% select(-c(MSOA11NM)) 

############
# ROUTING USING DODGR
############

# this downloads all the road data from OSM (equivalent to : key = 'highway')
streetnet <- dodgr_streetnet("oxford uk", expand = 0.05)
graph <- weight_streetnet(streetnet, wt_profile = "bicycle")

# remove flows where ORIGIN = DESTINATION
od_flow <- flows_ox %>% filter(`Area of residence` != `Area of workplace`)

od_flow_matrix <- 
  od_flow %>% 
  pivot_wider(names_from = `Area of workplace`, values_from = potential_demand) %>%
  tibble::column_to_rownames(var = "Area of residence") %>%  
  replace(is.na(.), 0) %>% as.matrix()


# 'from' is the 1st Column - get the MSOA names using rownames and join them with the coordinates in lon_lat
from <- rownames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "MSOA11CD")) %>%
  subset(select = c(lon, lat)) %>% as.matrix()

# same as above but 'to is the column names(i.e. index row)
to <- colnames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "MSOA11CD")) %>%
  subset(select = c(lon, lat)) %>% as.matrix()


# smaller subset 
# from_sub <- rownames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "MSOA11CD")) %>%
#   subset(select = c(lon, lat)) %>% slice(1:10) %>% as.matrix()
# to_sub <- colnames(od_flow_matrix) %>% as.data.frame() %>% left_join(lon_lat, by = c("." = "MSOA11CD")) %>%
#   subset(select = c(lon, lat)) %>% slice(1:10) %>% as.matrix()
# 
# od_flow_matrix_sub <- od_flow_matrix %>% as.data.frame() %>% slice(1:10) %>% select(1:10) %>% as.matrix()
# 

# Routing Function

graph_f <- dodgr_flows_aggregate(
  graph = graph,
  from = from, to = to, flows = od_flow_matrix,
  contract = TRUE,
  quiet = FALSE)

# Plotting to Check
graph_undir <- dodgr::merge_directed_flows(graph_f)

graph <- graph [graph_undir$edge_id, ]
graph$flow <- graph_undir$flow

graph_f <- graph_f [graph_f$flow > 0, ]
dodgr_flowmap (graph_f, linescale = 5)



