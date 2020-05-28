library(tidyverse)
library(dodgr)
library(sf)
library(osmdata)
library(lwgeom)

###############
# IMPORTING SPATIAL DATA FOR ROUTING
###############

#import spatial data using API: https://geoportal.statistics.gov.uk/datasets/826dc85fb600440889480f4d9dbb1a24_0
#msoa_boundaries <- st_read('https://opendata.arcgis.com/datasets/826dc85fb600440889480f4d9dbb1a24_0.geojson')
# or download it locally and read it (faster)
msoa_boundaries <- st_read('../data-raw/MSOA_2011_Boundaries/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp') %>%
  st_transform(4326)   # transform to EPSG 4326 for dodgr routing later

# function to filter MSOAs that are within a certain city
msoas_city <- function(name) {
  x <- city_names %>% filter(city == name)
  return(x)
}

# get msoas in London
msoas_london <- msoas_city("London")

# Add spatial data to london msoas
spatial_london <- 
    msoas_london[, c("MSOA11CD", "MSOA11NM")] %>% 
    left_join(msoa_boundaries[ , c("msoa11cd", "geometry")], by = c("MSOA11CD" = "msoa11cd")) %>% 
    st_as_sf() %>% lwgeom::st_make_valid() %>%
    mutate(centroid = st_centroid(geometry))  # we will need centroids to calculate distance matrix

# Remove border geometry and set centroids as geometry. This does not overwrite 'spatial_london' sf
msoa_centroids <- 
  st_drop_geometry(spatial_london) %>% 
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

# add lon and lat columns to dataframe using sfc_as_cols function
msoa_lon_lat <- msoa_centroids %>% split_lon_lat() %>% st_drop_geometry() %>% 
                    select(c(lon, lat))



############
# DISTANCE MATRIX USING DODGR
############

# this downloads all the road data from OSM (equivalent to : key = 'highway')
streetnet <- dodgr_streetnet("london uk", expand = 0.05)
#silicate format: penalizes intersections and hilliness
#streetnet_sc <- dodgr_streetnet_sc("london uk", expand = 0.05)

# attempt at subsetting the data from bounding box --> london border (FAILS TO CONVERT TO GRAPH)
#bb <- osmdata::getbb ("london uk", format_out = "sf_polygon")
#streetnet2 <- sf::st_intersection(streetnet, bb)

graph <- weight_streetnet(streetnet, wt_profile = "bicycle")
#graph_sc <- weight_streetnet(streetnet_sc, wt_profile = "bicycle")

#save the data so that you don't have to do this again
readr::write_csv(graph, path = "../data/weighted_graph.csv")
#write_csv(graph_sc, path = "../data/weighted_graph_sc.csv")
# read it back in and use it in dodgr::dodgr_dists
x <- read_csv('../data/weighted_graph.csv') 

# contract graph for faster routing
graph_contracted <- dodgr_contract_graph(graph)
# get distance matrix
dist_mat <- dodgr_dists(graph_contracted, from = msoa_lon_lat, to = msoa_lon_lat) %>% as.data.frame()

# Change column names for pivoting
colnames(dist_mat) <- msoa_centroids$MSOA11CD
# column bind to add MSOA IDs to df. If this is done using rownames then then it is added as a column index (cannot pivot)
dist_mat <- cbind(msoa_centroids$MSOA11CD, dist_mat) %>% rename(from = `msoa_centroids$MSOA11CD`)

# Change to long format to merge with flows
dist_mat <- dist_mat %>%
  pivot_longer(-from, names_to = "to", values_to = "dist")

# save to use in calculation of potential demand (next script)
flows_london %>% subset(select = -c(geom_orig, cent_orig, geom_dest, cent_dest)) %>%
   left_join(dist_mat, by = c("Area of residence" = "from" , "Area of workplace" = "to")) %>%
   write_csv(path = "../data/flows_dist_for_potential_flow.csv")
  

##########
# GETTING SF STRAIGHT LINE DISTANCES TO COMPARE WITH RESULTS
##########

flows_london <- read_csv("../data/flows_london.csv") %>% 
      subset(select = -c(city_origin, city_dest))

# add geometry of origin
flows_london <- flows_london %>% left_join(spatial_london[ , c("MSOA11CD", "geometry", "centroid")], 
                                           by = c("Area of residence" = "MSOA11CD")) %>%
  rename(geom_orig = geometry, cent_orig = centroid)
# add geometry of destination
flows_london <- flows_london %>% left_join(spatial_london[ , c("MSOA11CD", "geometry", "centroid")], 
                                           by = c("Area of workplace" = "MSOA11CD")) %>%
  rename(geom_dest = geometry, cent_dest = centroid)

# get straight line distances - this takes TIMMEEEE
flows_london <- flows_london %>% 
  mutate(dist =st_distance(cent_orig, cent_dest, by_element = T))


# Create dataframe with both straight line distances and dodgr distance
distances <- flows_london %>% 
  subset(select = -c(geom_orig, geom_dest, cent_orig, cent_dest)) %>% 
  subset(select = c(`Area of residence`, `Area of workplace`, dist)) %>%
  rename(dist_straight = dist) %>% 
  left_join(dist_mat, by = c("Area of residence" = "from", "Area of workplace" = "to")) %>%
  rename(dist_dodgr = dist)

# save for reference
write_csv(distances, path = "../data/dist_straight_vs_dodgr.csv")

# remove variables from global environment
rm(city_names, dist_mat, distances, msoa_boundaries, spatial_london, msoas_london)


