#===========================================================
# Name: f3_plot_go_node_map.R
# Author: D. Broman, PNNL
# Last Modified: 2025-02-25
# Description: [D3] plot GO-WEST node map
#===========================================================
options(scipen = 999)

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(sf)
library(extrafont)
library(tools)
extrafont::loadfonts()
library(ggfx)
library(usmap)
library(ggrepel)

wd = '/Users/brom374/Library/CloudStorage/OneDrive-PNNL/Documents/Projects/git/broman-etal_2025_wrr'
setwd(wd)

# GO-WEST nodes representative locations for plotting
go_nodes_100_tbl = read_csv('data/input_data/go-west_topology/WECC_nodes_100_wcoordsandstates_adj.csv')
go_nodes_10k_tbl = read_csv('data/input_data/go-west_topology/WECC_nodes_10k.csv')
go_lines_100_tbl = read_csv('data/input_data/go-west_topology/WECC_lines_100.csv')

# build connection sf between buses for 100 node model
edges_with_coords = go_lines_100_tbl %>%
  left_join(select(go_nodes_100_tbl, Number, lon_adj, lat_adj), by = c("fbus" = "Number")) %>%
  rename(fbus_lon = lon_adj, fbus_lat = lat_adj) %>%
  left_join(select(go_nodes_100_tbl, Number, lon_adj, lat_adj), by = c("tbus" = "Number")) %>%
  rename(tbus_lon = lon_adj, tbus_lat = lat_adj)

edges_sf = edges_with_coords %>%
  rowwise() %>%
  mutate(geometry = st_sfc(st_linestring(matrix(c(fbus_lon, tbus_lon, fbus_lat, tbus_lat), ncol = 2)))) %>%
  ungroup() %>%
  st_sf(crs = 4326)

#-----------------
# map of median revenue difference by Bus
states_sf = st_read('data/spatial/wecc_boundary_states.gpkg')
wecc_sf = st_read('data/spatial/wecc_boundary.gpkg')
transmission_sf = st_read('data/spatial/wecc_transmission.gpkg')

# GO-WEST 100 node map
gg1 = ggplot() +
  with_shadow(geom_sf(data = wecc_shp, fill = '#ece6d9', color = NA), colour = '#cccccc') +
  geom_sf(data = edges_sf, color = '#D0A5E9', size = 0.5, alpha = 0.7) +
  geom_sf(data = states_sf, fill = NA, color = '#f4f4f2', linewidth = 0.2) +
  geom_point(data = go_nodes_100_tbl, aes(x = lon_adj, y = lat_adj), shape = 15, color = '#EAA74A') +
  geom_text_repel(data = go_nodes_100_tbl, aes(x = lon_adj, y = lat_adj, label = str_to_title(Name)),
                  size = 2.5, family = 'Lato', max.overlaps = 20, box.padding = 0.25) +
  theme_bw() +
  xlab('') +
  ylab('') +
  theme(text = element_text(family = 'Lato', size = 12),
  ) +
  ggtitle('GO-WEST 100 Nodes')

# GO-WEST 10,000 node map
gg2 = ggplot() +
  with_shadow(geom_sf(data = wecc_sf, fill = '#ece6d9', color = NA), colour = '#cccccc') +
  geom_sf(data = transmission_sf, color = '#D0A5E9', size = 0.5, alpha = 0.7) +
  geom_sf(data = states_sf, fill = NA, color = '#f4f4f2', linewidth = 0.2) +
  geom_point(data = go_nodes_10k_tbl, aes(x = lon, y = lat), shape = 15, size = 0.5, color = '#EAA74A') +
  theme_bw() +
  xlab('') +
  ylab('') +
  theme(text = element_text(family = 'Lato', size = 12),
  ) +
  ggtitle('GO-WEST 10,000 Nodes')

# combine two maps together
ggcomb = gridExtra::grid.arrange(gg2, gg1, ncol = 2)

ggsave('figures/figure3_gowestmap.png', ggcomb, height = 7, width = 12)
