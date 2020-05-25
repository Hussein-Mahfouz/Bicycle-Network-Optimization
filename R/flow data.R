library(readr)
library(dplyr)
library(sf)


# this is a lookup table matching MSOAs to major towns and cities
city_names <- read_csv('../data-raw/Middle_Layer_Super_Output_Area__2011__to_Major_Towns_and_Cities__December_2015__Lookup_in_England_and_Wales.csv') 
# change column name
city_names <- city_names %>% rename(city = TCITY15NM)

# flow data from the 2011 census https://www.nomisweb.co.uk/census/2011/bulk/rOD1
flows <- read_csv('../data-raw/flow_data.csv')


#import spatial data using API: https://geoportal.statistics.gov.uk/datasets/826dc85fb600440889480f4d9dbb1a24_0
msoa_boundaries <- st_read('https://opendata.arcgis.com/datasets/826dc85fb600440889480f4d9dbb1a24_0.geojson')
# or download it locally and read it (faster)
#spatial <- st_read('../data-raw/MSOA_2011_Boundaries/Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp')

# MERGING NAMES WITH FLOW DATA

# add a column with the city name corresponding to each Residence MSOA
flows <- merge(flows, city_names[,c("MSOA11CD", "city")], by.x = "Area of residence", by.y = "MSOA11CD") %>% 
               rename(city_origin = city) # rename column so that we know it is referring to the 'Area of residence'

# add a column with the city name corresponding to each Workplace MSOA
flows <- merge(flows, city_names[,c("MSOA11CD", "city")], by.x = "Area of workplace", by.y = "MSOA11CD") %>% 
               rename(city_dest = city) # rename column so that we know it is referring to the 'Area of workplace'


# Subset flows to keep only those that are within a specific city
# function to return rows where origin and destination match the specified city name 

internal_flows <- function(name) {
  x <- flows %>% filter(city_origin == name, city_dest == name)
  return(x)
}

# use function to get flows between all MSOAs in London
London <- internal_flows("London")

