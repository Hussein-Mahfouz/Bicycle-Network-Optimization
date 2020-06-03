library(pct)
library(tidyverse)

#download all flows
flows <- pct::get_od()

# this is a lookup table matching MSOAs to major towns and cities. From: http://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-area-2011-to-major-towns-and-cities-december-2015-lookup-in-england-and-wales
city_names <- readr::read_csv('../data-raw/Middle_Layer_Super_Output_Area__2011__to_Major_Towns_and_Cities__December_2015__Lookup_in_England_and_Wales.csv') 
# change column name
city_names <- city_names %>% rename(city = TCITY15NM)

# add a column with the city name corresponding to each Residence MSOA
flows <- flows %>% dplyr::left_join(city_names[,c("MSOA11CD", "city")],
                             by = c("geo_code1" = "MSOA11CD")) %>%
  rename(city_origin = city) # rename column so that we know it is referring to the 'Area of residence'

# add a column with the city name corresponding to each Workplace MSOA
flows <- flows %>% dplyr::left_join(city_names[,c("MSOA11CD", "city")],
                             by = c("geo_code2" = "MSOA11CD")) %>%
  rename(city_dest = city) # rename column so that we know it is referring to the 'Area of workplace'

# Subset flows to keep only those that are within a specific city
# function to return rows where origin and destination match the specified city name 

flows_internal <- function(name) {
  x <- flows %>% filter(city_origin == name, city_dest == name)
  return(x)
}

# use function to get flows between all MSOAs in Liverpool
flows_liv <- flows_internal("Liverpool")
