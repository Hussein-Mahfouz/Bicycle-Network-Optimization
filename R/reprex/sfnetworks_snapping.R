library(osmdata)
library(sfnetworks)
library(tidygraph)
library(pct)
library(sf)
library(dplyr)


# get road data from osm
#bounding box
bb <- getbb ("oxford uk", format_out = "sf_polygon")
# download all road data
roads <- opq ("oxford uk") %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf() %>%
  trim_osmdata(bb) # trim to bounding box

# extract linestrig geometry
roads <- roads$osm_lines %>% select(highway) # remove all unnecessary columns

# plot to check
plot(bb)
plot(st_geometry(roads), add=T, col="grey")

# get population weighted centroids from pct and change crs (default is northing)
msoa_centroids <- pct::get_centroids_ew() %>% st_transform(4326)
# kepp only centroids inside Oxford bb
msoa_centroids <- st_filter(msoa_centroids, bb)

#convert roads to sf network to calculate shortest paths
roads <- as_sfnetwork(roads, directed=F)

# sample points
p1 <- msoa_centroids %>% slice(1) %>% st_geometry()
p2 <- msoa_centroids %>% slice(5) %>% st_geometry()

#get shortest path
path <- roads %>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  convert(to_spatial_shortest_paths, p1, p2)

plot(roads, col="grey")
plot(path, add=T)
plot(p1, add=T, col="red")
plot(p2, add=T, col="red")


# Get node degree centrality 
roads <- roads %>%  
  activate("nodes") %>%  
  mutate(degree = centrality_degree())  


# get buffer (project first)
buffer1 <- p1 %>% st_transform(27700) %>% st_buffer(200) %>% st_transform(4326)
buffer2 <- p2 %>% st_transform(27700) %>% st_buffer(200) %>% st_transform(4326)

plot(roads, col="grey")
plot(p1, add=T, col="red")
plot(buffer1, add=T, border="darkgreen")
plot(p2, add=T, col="red")
plot(buffer2, add=T, border="darkgreen")


# determine node with highest degree centrality inside the buffer
p1_new <- st_filter(roads, buffer1) %>% 
  activate("nodes") %>% as_tibble() %>%
  slice(which.max(degree)) %>%
  st_geometry()

p2_new <- st_filter(roads, buffer2) %>% 
  activate("nodes") %>% as_tibble() %>%
  slice(which.max(degree)) %>%
  st_geometry()

# check how these points differ from original ones
plot(roads, col="grey")
plot(p1, add=T, col="red")
plot(p2, add=T, col="red")
plot(p1_new, add=T, col="green")
plot(p2_new, add=T, col="green")

# new path with adjusted nodes (based on buffer and degree centrality)
path_new <- roads %>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  convert(to_spatial_shortest_paths, p1_new, p2_new)

# check the difference in the paths
plot(roads, col="grey")
plot(path, add=T)
plot(path_new, add=T, col="green")
plot(p1_new, add=T, col="red")
plot(p2_new, add=T, col="red")


# check the path lengths
path %>% activate("edges") %>% st_as_sf() %>% 
  st_drop_geometry() %>%
  summarize(length = sum(weight))

path_new %>% activate("edges") %>% st_as_sf() %>% 
  st_drop_geometry() %>%
  summarize(length = sum(weight))
