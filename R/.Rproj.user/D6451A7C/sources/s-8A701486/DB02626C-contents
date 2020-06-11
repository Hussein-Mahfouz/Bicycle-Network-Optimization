library(sf)
library(dodgr)
library(tidyverse)

# centroids for bounding box
msoa_centroids <- st_read("../data/alt_city/msoa_lon_lat.shp")

# bounding box
pts <- st_coordinates (msoa_centroids)
#this downloads all the road data from OSM (equivalent to : key = 'highway')
streetnet <- dodgr_streetnet(pts = pts, expand = 0.05)

# filter out useful columns
streetnet2 <- streetnet %>% 
  dplyr::select(osm_id, bicycle, bicycle_road, cycleway, highway,
                lanes, maxspeed, segregated)
# add length column
streetnet2 <- streetnet2 %>% dplyr::mutate(length_m = st_length(.))

# check different columns

bicycle <- streetnet2 %>% 
  st_drop_geometry() %>%
  group_by(bicycle) %>% 
  summarize(segments=n(), `length (m)` = sum(length_m))

# useless
bicycle_road <- streetnet2 %>% 
  st_drop_geometry() %>%
  group_by(bicycle_road) %>% 
  summarize(segments=n(), `length (m)` = sum(length_m))

cycleway <- streetnet2 %>% 
  st_drop_geometry() %>%
  group_by(cycleway) %>% 
  summarize(segments=n(), `length (m)` = sum(length_m))

highway <- streetnet2 %>% 
  st_drop_geometry() %>%
  group_by(highway) %>% 
  summarize(segments=n(), `length (m)` = sum(length_m))

# MOST ARE NA
lanes <- streetnet2 %>% 
  st_drop_geometry() %>%
  group_by(lanes) %>% 
  summarize(segments=n(), `length (m)` = sum(length_m))

# MOST ARE NA
maxspeed <- streetnet2 %>% 
  st_drop_geometry() %>%
  group_by(maxspeed) %>% 
  summarize(segments=n(), `length (m)` = sum(length_m))

# MOST ARE NA
segregated <- streetnet2 %>% 
  st_drop_geometry() %>%
  group_by(segregated) %>% 
  summarize(segments=n(), `length (m)` = sum(length_m))

bicycle_designated <- streetnet2 %>% filter(bicycle == 'designated')
cycleways <- streetnet2 %>% filter(highway == 'cycleway')

# redo this chunk: I want the segments in bicycle_designated that aren't in cycleways...
x <- st_within(bicycle_designated, cycleways)
logical = lengths(x) > 0
x2 = bicycle_designated[logical, ]

plot(st_geometry(cycleways))
plot(st_geometry(bicycle_designated))
plot(st_geometry(bicycle_designated), add = TRUE, col = 'red')

# filter streetnet to get all segments with cycling infrastructure
