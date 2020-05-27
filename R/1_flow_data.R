library(readr)
library(dplyr)
library(tidyr)
library(sf)
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
# function to return rows where origin and destination match the specified city name 

flows_internal <- function(name) {
  x <- flows %>% filter(city_origin == name, city_dest == name)
  return(x)
}

# use function to get flows between all MSOAs in London
flows_london <- flows_internal("London")


###############
# IMPORTING SPATIAL DATA TO MERGE WITH DATAFRAME
###############

#import spatial data using API: https://geoportal.statistics.gov.uk/datasets/826dc85fb600440889480f4d9dbb1a24_0
#msoa_boundaries <- st_read('https://opendata.arcgis.com/datasets/826dc85fb600440889480f4d9dbb1a24_0.geojson')
# or download it locally and read it (faster)
msoa_boundaries <- st_read('../data-raw/MSOA_2011_Boundaries/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp')

# function to filter MSOAs that are within a certain city
msoas_city <- function(name) {
  x <- city_names %>% filter(city == name)
  return(x)
}
# get msoas in London
msoas_london <- msoas_city("London")

# Add spatial data to london msoas
spatial_london <- msoas_london[, c("MSOA11CD", "MSOA11NM")] %>% left_join(msoa_boundaries[ , c("msoa11cd", "geometry")],
                              by = c("MSOA11CD" = "msoa11cd")) %>% st_as_sf() %>% st_make_valid() %>%
                  mutate(centroid = st_centroid(geometry))  # we will need centroids to calculate distance matrix


# transform back to sf and handle invalid geometries
#spatial_london <- spatial_london %>% st_as_sf() %>% st_make_valid()
# plot to check
#plot(st_geometry(spatial_london))


###############
# DISTANCE MATRIX
###############

# add geometry of origin
flows_london <- flows_london %>% left_join(spatial_london[ , c("MSOA11CD", "geometry", "centroid")], 
                                            by = c("Area of residence" = "MSOA11CD")) %>%
                                 rename(geom_orig = geometry, cent_orig = centroid)
# add geometry of destination
flows_london <- flows_london %>% left_join(spatial_london[ , c("MSOA11CD", "geometry", "centroid")], 
                                            by = c("Area of workplace" = "MSOA11CD")) %>%
                                 rename(geom_dest = geometry, cent_dest = centroid)


# remove variables from global environment that are no longer needed
rm(msoa_boundaries, flows, city_names)

# get diastances - this takes TIMMEEEE
flows_london <- flows_london %>% 
  mutate(dist =st_distance(cent_orig, cent_dest, by_element = T))


# SAVE OUTPUT AS CSV. 

# Drop spatial data columns. geojson only takes one
msoa_distances <- subset(flows_london, select = -c(geom_orig, geom_dest, cent_orig, cent_dest))
write_csv(msoa_distances, path = "../data/msoa_distances.csv")
