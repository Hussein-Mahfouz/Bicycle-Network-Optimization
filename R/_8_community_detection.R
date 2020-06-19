library(sf)
library(sfnetworks)



graph_t <- readRDS(paste0("../data/", chosen_city,"/graph_with_flows_default.RDS"))
graph_t <- graph_t %>% dplyr::select(flow)


graph_tt <- as_sfnetwork(graph_t, directed = F)

graph_tt <- graph_tt %>% activate("nodes") %>%
  mutate(group = group_louvain(weights = flow))

tm_shape(graph_tt %>% activate("nodes") %>% st_as_sf()) +
  tm_dots(
    size = 0.2,
    col = "group",
    #palette = "viridis",
    title = ""
  )
