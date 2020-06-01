library(geofabrik)
library(tidyverse)
library(sf)
library(lwgeom)

# check the extents of the geofabrik query using 'gf_find'
london_gf_region = geofabrik::gf_find("greater london") %>% lwgeom::st_make_valid()
plot(st_geometry(london_gf_region))

