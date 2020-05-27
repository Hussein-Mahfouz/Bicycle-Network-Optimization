library(dodgr)
library(sf)
library(osmdata)

# this downloads all the road data from OSM (equivalent to : key = 'highway')
#streetnet <- dodgr_streetnet("london uk", expand = 0.05)
#silicate format: penalizes intersections and hilliness
streetnet_sc <- dodgr_streetnet_sc("london uk", expand = 0.05)


# attempt at subsetting the data from bounding box --> london border (FAILS TO CONVERT TO GRAPH)
#bb <- osmdata::getbb ("london uk", format_out = "sf_polygon")
#streetnet2 <- sf::st_intersection(streetnet, bb)

#graph <- weight_streetnet(streetnet, wt_profile = "bicycle")
graph_sc <- weight_streetnet(streetnet_sc, wt_profile = "bicycle")

#save the data so that you don't have to do this again
#readr::write_csv(graph, path = "../data/weighted_graph.csv")
write_csv(graph_sc, path = "../data/weighted_graph_sc.csv")

