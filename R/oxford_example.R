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
  rename(potential_demand = `All categories: Method of travel to work`)   # TEMPORARY!!

#clear env
rm(flows)

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

#clear env
rm(msoa_boundaries)

# Remove border geometry and set centroids as geometry. This does not overwrite 'spatial_ox' sf
msoa_centroids <- 
  st_drop_geometry(spatial_ox) %>% 
  st_set_geometry('centroid') 


#######################
# SNAPPING CENTROIDS TO MAIN OSM ROADS. Do this to prevent centroids snapping to innaccessible road segments

#1. Get stree network and filter main road types (filter argument can be changed)
pts <- st_coordinates (msoa_centroids)
roads <- dodgr_streetnet(pts = pts, expand = 0.05) %>% 
      filter(highway %in% c('primary', 'secondary', 'tertiary'))

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
  select(-c(centroid)) %>% # drop old geometry
  rename(centroid = x)

# check how the points were shifted
plot(st_geometry(roads))
plot(st_geometry(msoa_centroids), add = TRUE, col = 'red')
plot(st_geometry(msoa_centroids_snapped), add = TRUE, col = 'green')

# check that bind_cols() was correct. Plot individual points:
plot(st_geometry(roads))
plot(st_geometry(msoa_centroids$centroid[1]), add = TRUE, col = 'red')
plot(st_geometry(msoa_centroids_snapped$centroid[1]), add = TRUE, col = 'green')

#clear environment
rm(roads)
########################

# get lon and lat as seperate columns
lon_lat <- msoa_centroids_snapped %>% 
  cbind(st_coordinates(msoa_centroids_snapped)) %>%
  rename(lon = X, lat = Y) %>%  
  select(-c(MSOA11NM)) 

############
# ROUTING USING DODGR
############

# USING OSM

##########
# bb <- osmdata::getbb ("oxford uk", format_out = "polygon")
# Result of above query can be passed directly to streetnet, but we want to add a buffer. 
# Buffers can be added using expand in `dodgr_streetnet("oxford uk", expand = 0.05)`. the problem with this 
# is that it gets a rectangular bb. I want something more compact (i.e buffer around city boundary). This is 
# done to deal with RAM limitations

# get compact bounding box with buffer

# check functions.R for documentation. 
#bb_ox <- bb_buffer(city= "oxford uk", buffer= 1000)
#streetnet <- dodgr_streetnet (bbox = bb_ox) 
##########
pts <- st_coordinates (lon_lat)
streetnet <- dodgr_streetnet(pts = pts, expand = 0.05)

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






