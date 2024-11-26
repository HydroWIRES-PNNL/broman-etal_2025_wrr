#===========================================================
# Name: f3_plot_hydropower_facility_map.R
# Author: D. Broman, PNNL
# Last Modified: 2022-10-07
# Description: [D3] Figure 3: hydropower facility map
#===========================================================

library(tidyverse)
library(lubridate)
library(furrr)
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
id_tbl = read_csv('data/input_data/mosartwmpy_streamflow/GO_WEST_MOSART_tbl.csv')

target_tbl = read_csv('data/input_data/starfit_reservoirtargets/2000_2019_weekly_GOWEST_hydro_inputs_with_release_mult_and_storage_tuewk_updateror.csv')

# hydropower facilities used in experiment
facility_set = eha2021_jn %>%
  dplyr::filter(EIA_ID %in% id_tbl$ID) %>%
  left_join(target_tbl)

# hydropower facilities not used in experiment
facility_set_other = eha2021_tbl %>%
  dplyr::filter(state %in% c('WA', 'OR', 'CA', 'AZ', 'NV', 'ID', 'MT', 'WY',
                             'UT', 'NM', 'CO')) %>%
  dplyr::select(EIA_ID, plant, CH_MW, lon, lat) %>%
  dplyr::filter(!EIA_ID %in% id_tbl$ID)

# map of facilities
states_wus = map_data('state') %>%
  dplyr::filter(region %in% c('washington', 'oregon', 'california', 'arizona', 'nevada',
                              'idaho', 'montana', 'wyoming', 'utah',
                              'new mexico', 'colorado'))

# type labels
type_tbl = tibble(type = c('STARFIT', 'RoR/small storage'), 
                      Type = c('Storage', 'Run-of-River'))

facility_set = facility_set %>%
  left_join(type_tbl) %>%
  dplyr::rename(`Nameplate Capacity (MW)` = nameplate)

ggplot() +
  geom_polygon(data = states_wus, aes(x = long, y = lat, group = group), 
               fill = '#dbdbdb', color = 'white', linewidth = 0.5) +
  geom_point(data = facility_set_other, aes(x = lon, y = lat), shape = 21, size = 0.5, fill = 'gray30', alpha = 0.5) +
  geom_point(data = facility_set, aes(x = lon, y = lat, size = `Nameplate Capacity (MW)`, fill = Type), 
             shape = 21, alpha = 0.6) +
  scale_fill_manual(values = c('#24833c', '#815417')) +
  theme_bw() +
  coord_map('conic', lat0 = 40) +
  xlab('') +
  ylab('') +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.position = 'bottom', 
    legend.box = 'horizontal',
    text = element_text(family = 'Segoe UI', size = 10)) +
  guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5),
         size = guide_legend(title.position = 'top', title.hjust = 0.5))

ggsave('figures/figure3_hydropowerfacilitymap.png/', height = 6, width = 8)
