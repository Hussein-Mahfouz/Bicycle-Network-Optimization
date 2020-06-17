library(tidyverse)
library(sf)
library(tmap)


graph_sf_default <- readRDS(paste0("../data/", chosen_city,"/graph_with_flows_default.RDS"))
graph_sf_unweight <- readRDS(paste0("../data/", chosen_city,"/graph_with_flows_unweighted.RDS"))
graph_sf_trunk <- readRDS(paste0("../data/", chosen_city,"/graph_with_flows_trunk.RDS"))


plot(graph_sf_default['flow'])

# we want to know the total person-kilometres traveled on each road type
# 1. multiply flow by distance to get person-km on each edge
# 2. group by highway type
# 3. get total and % of person-km on each highway type

dist_def <- graph_sf_default %>% st_drop_geometry() %>%
  # we add the weighted-distance column before grouping because we can't get it after grouping
  mutate(sum_weighted_dist = sum(d*flow)) %>%   
  group_by(highway) %>%
  summarize(dist = sum(d*flow) /1000,         # this is the total person-km per highway type
            dist_perc = ((sum(d*flow)) / mean(sum_weighted_dist)) * 100) %>% #same as above but as %
  mutate(weighting = 'default')


dist_tr <- graph_sf_trunk %>% st_drop_geometry() %>%
  mutate(sum_weighted_dist = sum(d*flow)) %>%   
  group_by(highway) %>%
  summarize(dist = sum(d*flow) /1000,         
            dist_perc = ((sum(d*flow)) / mean(sum_weighted_dist)) * 100) %>% 
  mutate(weighting = 'trunk')


dist_unw <- graph_sf_unweight %>% st_drop_geometry() %>%
  mutate(sum_weighted_dist = sum(d*flow)) %>%   
  group_by(highway) %>%
  summarize(dist = sum(d*flow) /1000,         
            dist_perc = ((sum(d*flow)) / mean(sum_weighted_dist)) * 100) %>% 
  mutate(weighting = 'unweighted')

# remove some highway types

person_km <- rbind(dist_def, dist_tr, dist_unw)
 
# exploratory plot
ggplot(data=dist_def, aes(x=highway, y=dist_perc))+
  geom_col() + coord_flip()

#remove highway types that have very little share (for plotting purposes)
person_km <- person_km %>% 
  dplyr::filter(!(highway %in% c('trunk_link', 'track', 'tertiary_link', 'steps', 
                               'secondary_link', 'primary_link', 'living_street',
                               'motorway_link')))

# plot % of person-km on each highway type
ggplot(data=person_km , aes(x=highway, y=dist_perc, group=factor(weighting), fill=factor(weighting))) +
    geom_col(position=position_dodge(0.7), colour="black") +
    ggtitle("Percentage of Total Flow Traversing \nDifferent Highway Types") +
    labs(x = "Highway Type", y = "% of Total Flow", fill = "weighting") +
    scale_y_continuous(labels = scales::comma_format()) +                         # add comma to y labels
    scale_fill_brewer(palette = "Greys", name="Weighting Profile") +                                        # for nice color schemes
    # edit angle of text, hjust argument so that text stays below plot AND center plot title
    theme(axis.text.x = element_text(angle=50, hjust=1), plot.title = element_text(hjust = 0.5)) + 
    coord_flip() -> p

p
ggsave(path = paste0("../data/", chosen_city,"/Plots"), file="perc_person-km-per-highway-type.png", p, width = 10, height = 6)

# plot person-km on each highway type
ggplot(data=person_km , aes(x=highway, y=dist, group=factor(weighting), fill=factor(weighting))) +
  geom_col(position=position_dodge(0.7), colour="black") +
  ggtitle("Percentage of Total Flow Traversing \nDifferent Highway Types") +
  labs(x = "Highway Type", y = "% of Total Flow", fill = "weighting") +
  scale_y_continuous(labels = scales::comma_format()) +                         # add comma to y labels
  scale_fill_brewer(palette = "Greys", name="Weighting Profile") +                                        # for nice color schemes
  # edit angle of text, hjust argument so that text stays below plot AND center plot title
  theme(axis.text.x = element_text(angle=50, hjust=1), plot.title = element_text(hjust = 0.5)) + 
  coord_flip() -> p

p
ggsave(path = paste0("../data/", chosen_city,"/Plots"), file="person-km-per-highway-type.png", p, width = 10, height = 6)




#remove highway types that have very little share (for plotting purposes)
x <- graph_sf_default %>% 
  dplyr::filter(!(highway %in% c('trunk_link', 'track', 'tertiary_link', 'steps', 
                                 'secondary_link', 'primary_link', 'living_street',
                                 'motorway_link', 'path', 'service', 'unclassified')))

tm_shape(x) +
  tm_lines(col = 'gray95') +
tm_shape(x) +
  tm_lines(col = 'highway', 
           scale = 1.5,     #multiply line widths by 3
           palette = "Set2") +
  tm_layout(title = "OSM Road Types \nOnly Roads with Routed Flow",        
            title.size = 1.2,
            title.color = "azure4",
            title.position = c("left", "top"),
            inner.margins = c(0.1, 0.25, 0.1, 0.1),    # bottom, left, top, and right margin
            fontfamily = 'Georgia',
            legend.position = c("right", "bottom"),
            frame = FALSE) -> p

tmap_save(tm = p, filename = paste0("../data/", chosen_city,"/Plots/osm_road_types.png"))

# facet plot of flows. works but v slow (2 mins)
x <- graph_sf_default %>% 
  dplyr::filter(!(highway %in% c('trunk_link', 'track', 'tertiary_link', 'steps', 
                                 'secondary_link', 'primary_link', 'living_street',
                                 'motorway_link', 'path', 'service', 'unclassified'))) 
tm_shape(x) +
    tm_lines(col = 'gray92') +
tm_shape(x) +
    tm_lines(lwd = "flow",
             scale = 8,  #multiply line widths by scale
             col = "darkgreen") +    
    tm_facets(by="highway",
              nrow = 2,
              free.coords=FALSE)  +  # so that the maps aren't different sizes
    tm_layout(fontfamily = 'Georgia',
              main.title = "Flows on Different Road Types", # this works if you need it
              main.title.size = 1.3,
              main.title.color = "azure4",
              main.title.position = "left",
              legend.outside.position = "bottom" , 
              legend.outside.size = .1,
              #inner.margins = c(0.01, 0.01, 0.01, 0.01),
              frame = FALSE)  -> p

tmap_save(tm = p, filename = paste0("../data/", chosen_city,"/Plots/flows_facet_default.png"), 
          width=10, height=6)


