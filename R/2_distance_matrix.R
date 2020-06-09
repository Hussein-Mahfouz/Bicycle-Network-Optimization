library(tidyverse)
library(dodgr)
library(sf)
library(osmdata)
library(lwgeom)
library(pct)

###############
# IMPORTING SPATIAL DATA FOR ROUTING
###############

# get population weighted centroids from pct
msoa_centroids <- pct::get_pct_centroids( region = "london", geography = "msoa") %>%
  select(c(geo_code, geo_name, geometry)) %>% rename(MSOA11CD = geo_code)
# rename geo_code as downstream scripts relay on 'MSOA11CD' column name

#######################
# SNAPPING CENTROIDS TO MAIN OSM ROADS. Do this to prevent centroids snapping to innaccessible road segments

#1. Get stree network and filter main road types (filter argument can be changed)
# get boundary to query OSM roads from
pts <- st_coordinates (msoa_centroids)
#query OSM
roads <- dodgr_streetnet(pts = pts, expand = 0.05) %>% 
  filter(highway %in% c('primary', 'secondary', 'tertiary'))

bb <- osmdata::getbb ("london uk", format_out = "sf_polygon")

roads <- opq(bbox = bb) %>%
  add_osm_feature(key = 'highway', value = c('primary', 'secondary', 'tertiary')) %>%
  osmdata_sf () %>%
  trim_osmdata (bb)

#keep only the lines. They are in EPSG 4326
roads <- roads$osm_lines
#2. snap centroids to lines

# function from https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf
st_snap_points = function(x, y, max_dist = 1000) {
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}

msoa_centroids_snapped <- 
  st_snap_points(msoa_centroids, roads, max_dist = 1000) %>%   # if dist to nearest road > max dist, point is unchanged
  st_as_sf() %>%   # convert to get it in a dataframe
  bind_cols(msoa_centroids) %>%  # bind with msoa centroids df to ge MSOA IDs
  select(-c(geometry)) %>% # drop old geometry
  rename(centroid = x)

# check how the points were shifted
plot(st_geometry(roads))
plot(st_geometry(msoa_centroids), add = TRUE, col = 'red')
plot(st_geometry(msoa_centroids_snapped), add = TRUE, col = 'green')

# check that bind_cols() was correct. Plot individual points:
plot(st_geometry(roads))
plot(st_geometry(msoa_centroids$geometry[50]), add = TRUE, col = 'red')
plot(st_geometry(msoa_centroids_snapped$centroid[50]), add = TRUE, col = 'green')

#clear environment
rm(roads)
########################

# save this version for routing in `4_aggregating_flows`
msoa_centroids_snapped %>% 
  cbind(st_coordinates(.)) %>%  #split geometry into X and Y columns
  rename(lon = X, lat = Y) %>%  # rename x and y
  select(-c(geo_name)) %>% 
  write_csv(path = "../data/msoa_lon_lat.csv")
  #st_write( "../data/msoa_lon_lat.shp")
# SAVE AS .shp YOU IDIOT

# read it in for dodgr_dist calculations (below)
msoa_lon_lat <- read_csv("../data/msoa_lon_lat.csv") %>% as.data.frame() %>%
  dplyr::select(c(lon, lat))        

############
# DISTANCE MATRIX USING DODGR
############

# this downloads all the road data from OSM (equivalent to : key = 'highway')
pts <- st_coordinates (msoa_centroids_snapped)
streetnet <- dodgr_streetnet(pts = pts, expand = 0.05)
#silicate format: penalizes intersections and hilliness
#streetnet_sc <- dodgr_streetnet_sc("london uk", expand = 0.05)

# attempt at subsetting the data from bounding box --> london border (FAILS TO CONVERT TO GRAPH)
#bb <- osmdata::getbb ("london uk", format_out = "sf_polygon")
#streetnet2 <- sf::st_intersection(streetnet, bb)

graph <- weight_streetnet(streetnet, wt_profile = "bicycle")
#graph_sc <- weight_streetnet(streetnet_sc, wt_profile = "bicycle")

# SAVE TO LOAD IN NEXT TIME!
saveRDS (graph, file = "../data/london_roads.Rds")

#save the data so that you don't have to do this again
#readr::write_csv(graph, path = "../data/weighted_graph.csv")
#readr::write_csv(graph_sc, path = "../data/weighted_graph_sc.csv")


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
flows_london <- readr::read_csv("../data/flows_london.csv")

flows_london %>% subset(select = -c(city_origin, city_dest)) %>%
   left_join(dist_mat, by = c("Area of residence" = "from" , "Area of workplace" = "to")) %>%
   write_csv(path = "../data/flows_dist_for_potential_flow.csv")

flows_slope <- flows_london %>% subset(select = -c(city_origin, city_dest)) %>%
  left_join(dist_mat, by = c("Area of residence" = "from" , "Area of workplace" = "to"))


##### ADD SLOPE - START
# flows_slope <- flows_slope %>% left_join(msoa_centroids_snapped[,c('MSOA11CD' ,'centroid')],
#                                     by = c("Area of residence" = "MSOA11CD")) %>%
#   rename(cent_orig = centroid) 
# 
# flows_slope <- flows_slope %>% left_join(msoa_centroids_snapped[,c('MSOA11CD' ,'centroid')],
#                                      by = c("Area of workplace" = "MSOA11CD")) %>%
#   rename(cent_dest = centroid)
# 
# # coordinates for bb
# pts <- st_coordinates(msoa_centroids_snapped)
# # road network for routing
# net <- dodgr::dodgr_streetnet(pts = pts, expand = 0.1)
# # elevation data
# uk_elev <- raster::raster('../data/uk_elev.tif')
# 
# # 1. get geometries
# route <- function(df, net){
#   nrows <- nrow(df)
#   i = 1:nrows
#   df$route = st_sfc(lapply(1:nrows, function(x) st_geometrycollection()))
#   st_crs(df$route) <- 4326
#   df$route[i] <- st_geometry(stplanr::route_dodgr(from = df$cent_orig[i], to = df$cent_dest[i], net = net))
#   return(df)
# }
# 
# # 1. get slopes
# slope <- function(df, elev){
#   nrows <- nrow(df)
#   i = 1:nrows
#   df$slope[i] <- slopes::slope_raster(df$route[i], elev) 
#   return(df)
# }
# 
# # get routes column then slope column
# flows_slope <- flows_slope %>% route(elev = uk_elev, net = net) %>% slope(elev = uk_elev)
# 
# flows_slope %>% write_csv(path = "../data/flows_dist_elev_for_potential_flow.csv")

##### ADD SLOPE - END



##########
# GETTING SF STRAIGHT LINE DISTANCES TO COMPARE WITH RESULTS
##########

# flows_london <- read_csv("../data/flows_london.csv") %>% 
#       subset(select = -c(city_origin, city_dest))
# 
# # add geometry of origin
# flows_london <- flows_london %>% left_join(spatial_london[ , c("MSOA11CD", "geometry", "centroid")], 
#                                            by = c("Area of residence" = "MSOA11CD")) %>%
#   rename(geom_orig = geometry, cent_orig = centroid)
# # add geometry of destination
# flows_london <- flows_london %>% left_join(spatial_london[ , c("MSOA11CD", "geometry", "centroid")], 
#                                            by = c("Area of workplace" = "MSOA11CD")) %>%
#   rename(geom_dest = geometry, cent_dest = centroid)
# 
# # get straight line distances - this takes TIMMEEEE
# flows_london <- flows_london %>% 
#   mutate(dist =st_distance(cent_orig, cent_dest, by_element = T))
# 
# 
# # Create dataframe with both straight line distances and dodgr distance
# distances <- flows_london %>% 
#   subset(select = -c(geom_orig, geom_dest, cent_orig, cent_dest)) %>% 
#   subset(select = c(`Area of residence`, `Area of workplace`, dist)) %>%
#   rename(dist_straight = dist) %>% 
#   left_join(dist_mat, by = c("Area of residence" = "from", "Area of workplace" = "to")) %>%
#   rename(dist_dodgr = dist)
# 
# # save for reference
# write_csv(distances, path = "../data/dist_straight_vs_dodgr.csv")

# remove variables from global environment
rm(city_names, dist_mat, distances, msoa_boundaries, spatial_london, msoas_london)


