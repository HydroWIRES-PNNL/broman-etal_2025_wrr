#===========================================================
# Name: f2_plot_hydropower_facility_map.R
# Author: D. Broman, PNNL
# Last Modified: 2022-10-07
# Description: [D3] Figure 2: hydropower facility map
#===========================================================

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(extrafont)
library(usmap)
library(sf)
extrafont::loadfonts()

# read in EHA
eha2021_tbl = read_csv('data/input_data/hydrosource/HydroSource_HYC.csv')

eha2021_jn = eha2021_tbl %>%
  dplyr::select(EIA_ID, plant, CH_MW, lon, lat)

# table of facilities used in experiment
target_tbl = read_csv('data/input_data/starfit_reservoirtargets/2000_2019_weekly_GOWEST_hydro_inputs_with_release_mult_and_storage_tuewk_updateror.csv')

target_meta = target_tbl %>%
  dplyr::select(EIA_ID, type) %>%
  distinct()

# hydropower facilities used in experiment
facility_set = eha2021_jn %>%
  left_join(target_meta) %>%
  dplyr::filter(!is.na(type))

# hydropower facilities not used in experiment
facility_set_other = eha2021_tbl %>%
  dplyr::filter(state %in% c('WA', 'OR', 'CA', 'AZ', 'NV', 'ID', 'MT', 'WY',
                             'UT', 'NM', 'CO')) %>%
  dplyr::select(EIA_ID, plant, CH_MW, lon, lat) %>%
  dplyr::filter(!EIA_ID %in% target_meta$EIA_ID)

# type labels
type_tbl = tibble(type = c('STARFIT', 'RoR/small storage'), 
                      Type = c('Storage', 'Run-of-River'))

facility_set = facility_set %>%
  left_join(type_tbl) %>%
  dplyr::rename(`Nameplate Capacity (MW)` = CH_MW)

# spatial layers
states_sf = st_read('data/spatial/wecc_boundary_states.gpkg')
wecc_sf = st_read('data/spatial/wecc_boundary.gpkg')

# hydropower facility map
ggplot() +
  with_shadow(geom_sf(data = wecc_sf, fill = '#ece6d9', color = NA), colour = '#cccccc') +
  geom_sf(data = states_sf, fill = NA, color = '#f4f4f2', linewidth = 0.2) +
  geom_point(data = facility_set_other, aes(x = lon, y = lat), shape = 16, size = 0.5, color = 'gray30', alpha = 0.5) +
  geom_point(data = facility_set, aes(x = lon, y = lat, size = `Nameplate Capacity (MW)`, fill = Type), 
             shape = 21, alpha = 0.6) +
  scale_fill_manual(values = c('#24833c', '#815417')) +
  theme_bw() +
  xlab('') +
  ylab('') +
  theme(legend.position = 'bottom',
        text = element_text(family = 'Lato', size = 12),
        legend.box = 'horizontal',
  ) +
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5),
         size = guide_legend(title.position = 'top', title.hjust = 0.5)
  ) # +
  # ggtitle('Western Interconnection Conventional Hydropower Facilities')

ggsave('figures/figure2_hydropowerfacilitymap.png', height = 8, width = 6)

