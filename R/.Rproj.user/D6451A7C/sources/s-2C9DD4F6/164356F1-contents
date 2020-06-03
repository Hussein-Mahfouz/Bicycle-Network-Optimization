# convert to sf
graph_sf <- graph_undir %>% dodgr_to_sf()
graph_sf$flow_normalized <- graph_sf$flow / graph_sf$d_weighted


x <- graph_sf
#get edge_id of edge with highest flow
edge_sel <- x$edge_id[which.max(x$flow_normalized)]
# prepare row for adding to new df
x <- x %>% filter(edge_id == edge_sel) %>% 
  mutate(sequence= 0)

i <- 1
while (i < 300){

  # remove rows that have already been selected
  remaining <- graph_sf %>% filter(!(edge_id %in% x$edge_id))
  
  # identify road segments that neighbour existing selection
  neighb_id <- graph_sf$edge_id[which(graph_sf$from_id %in% x$from_id | 
                                      graph_sf$from_id %in% x$to_id |
                                      graph_sf$to_id %in% x$from_id | 
                                      graph_sf$to_id %in% x$to_id)]

  # get neighbouring edges
  neighb <- remaining %>% filter(edge_id %in% neighb_id)
  # get id of best neighboring edge
  edge_sel <- neighb$edge_id[which.max(neighb$flow_normalized)]
  # get nest neighboring edge as df row
  edge_next <- graph_sf %>% filter(edge_id == edge_sel) %>% 
    mutate(sequence= i)
  # append it to the solution
  x <- dplyr::bind_rows(x, edge_next) 
  
  i = i+1
}

# FIGURE THIS OUT. I WANT TO PLOT BY COL = SEQUENCE, so need to put sequence into graph_sf
x_plot <- x %>% subset(select = c(edge_id, sequence)) %>%
            right_join(graph_sf, by = c("edge_id" = "edge_id")) 

x_plot <- x %>% st_as_sf() %>% st_drop_geometry()

plot <- graph_sf %>% filter(edge_id %in% x$edge_id)
plot(st_geometry(plot))
plot(plot$sequence)
