
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sloperouting

<!-- badges: start -->

<!-- badges: end -->

The goal of sloperouting is to provide a place to share code/data for
slope sensitive routing.

The starting point is having the latest version of `sfnetworks`
installed:

``` r
remotes::install_github("luukvdmeer/sfnetworks")
remotes::install_github("itsleeds/slopes")
remotes::install_github("itsleeds/od")
```

``` r
library(sfnetworks)
library(tidygraph)
#> 
#> Attaching package: 'tidygraph'
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 7.0.0
```

# Example with package data

Test the package is working:

``` r
net = as_sfnetwork(roxel, directed = FALSE)
class(net)
#> [1] "sfnetwork" "tbl_graph" "igraph"
sf::st_crs(net)
#> Coordinate Reference System:
#>   User input: EPSG:4326 
#>   wkt:
#> GEOGCRS["WGS 84",
#>     DATUM["World Geodetic System 1984",
#>         ELLIPSOID["WGS 84",6378137,298.257223563,
#>             LENGTHUNIT["metre",1]]],
#>     PRIMEM["Greenwich",0,
#>         ANGLEUNIT["degree",0.0174532925199433]],
#>     CS[ellipsoidal,2],
#>         AXIS["geodetic latitude (Lat)",north,
#>             ORDER[1],
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>         AXIS["geodetic longitude (Lon)",east,
#>             ORDER[2],
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>     USAGE[
#>         SCOPE["unknown"],
#>         AREA["World"],
#>         BBOX[-90,-180,90,180]],
#>     ID["EPSG",4326]]
net_proj = sf::st_transform(net, 3035)
p1 = net_proj %>%  
  activate(nodes) %>%  
  st_as_sf() %>%  
  slice(1)  
p2 = net_proj %>%  
  activate(nodes) %>%  
  st_as_sf() %>%  
  slice(9)  
p3 = sf::st_sfc(  
  sf::st_geometry(p1)[[1]] + sf::st_point(c(500, 500)),  
  crs = sf::st_crs(p1)  
)  
p4 = sf::st_sfc(  
  sf::st_geometry(p2)[[1]] + sf::st_point(c(-500, -500)),  
  crs = sf::st_crs(p2)  
)  
net_proj %>%  
  activate("edges") %>%  
  mutate(weight = edge_length()) %>%  
  convert(to_spatial_shortest_paths, p3, p4) ->  
  net_sp  
par(mar = c(1,1,1,1), bg = NA)  
plot(net_proj)  
plot(net_sp,
     col = "Orange", lwd = 1.5, cex = 1.5,
     add = T)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Example with data from Lisbon

``` r
library(sfnetworks)
library(tidygraph)
library(dplyr)
library(sf)
r = slopes::lisbon_road_segments
sf::st_is_longlat(r)
#> [1] FALSE
class(r)
#> [1] "sf"         "tbl_df"     "tbl"        "data.frame"
names(r)
#>  [1] "OBJECTID"   "fid_1"      "gradient_s" "Shape_Leng" "Z_Min"     
#>  [6] "Z_Max"      "Z_Mean"     "SLength"    "Min_Slope"  "Max_Slope" 
#> [11] "Avg_Slope"  "z0"         "z1"         "gradverifi" "query"     
#> [16] "lat"        "lon"        "lat_min"    "lat_max"    "lon_min"   
#> [21] "lon_max"    "bbox"       "geom"
plot(r["Avg_Slope"])
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
net = as_sfnetwork(r)
p1 = net %>%  
  activate(nodes) %>%  
  st_as_sf() %>%  
  slice(1)  
p2 = net %>%  
  activate(nodes) %>%  
  st_as_sf() %>%  
  slice(9)
mapview::mapview(p1) + mapview::mapview(p2)
```

![](README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
path1 = net %>%  
  activate("edges") %>%  
  mutate(weight = edge_length()) %>%  
  convert(to_spatial_shortest_paths, p1, p2)
plot(path1)
```

![](README_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
mapview::mapview(st_as_sf(path1)) +
  mapview::mapview(p1) + mapview::mapview(p2)
```

![](README_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

## Shortest path in Lisbon with sfnetworks
