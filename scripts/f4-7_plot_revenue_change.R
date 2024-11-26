#===========================================================
# Name: f4-7_plot_revenue_change.R
# Author: D. Broman, PNNL
# Last Modified: 2023-09-21
# Description: [D3] revenue change figures from FIScH simulations
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

water_year = function(x, numeric = FALSE){
  x = as.POSIXlt(x)
  yr = x$year + 1900L
  mn = x$mon + 1L
  ## adjust for water year
  yr = yr + ifelse(mn < 10L, 0L, 1L)
  if(numeric)
    return(yr)
  return(yr)
}

# GO-WEST nodes representative locations for plotting
go_nodes_100_tbl = read_csv('data/input_data/go-west_topology/WECC_nodes_100_wcoordsandstates_adj.csv')

# correspondance between GO-WEST nodes and hydropower faciliites 
hydro_node_tbl = read_csv('data/input_data/go-west_topology/EIA_bus_match100.csv')

hydro_node_tbl = hydro_node_tbl %>%
  mutate(Number = as.numeric(substr(`new bus`, 5, nchar(`new bus`))))

go_nodes_100_hydro_tbl = go_nodes_100_tbl %>%
  left_join(hydro_node_tbl) %>%
  dplyr::filter(!is.na(plant)) %>%
  dplyr::rename(Bus = `new bus`) %>%
  group_by(Bus, Number, Name, lat, lon, lat_adj, lon_adj, state_bus) %>%
  dplyr::summarise(capacity = sum(capacity))

bus_label_tbl = go_nodes_100_hydro_tbl %>%
  ungroup() %>%
  dplyr::select(Bus, Name, state_bus) %>%
  mutate(Name = str_to_title(Name))
  
states_wus = map_data('state') %>%
  dplyr::filter(region %in% c('washington', 'oregon', 'california', 'arizona', 'nevada',
                              'idaho', 'montana', 'wyoming', 'utah',
                              'new mexico', 'colorado'))

# ----------------------

hydro_plant_tbl = read_csv('/Users/brom374/Library/CloudStorage/OneDrive-PNNL/Documents/Projects/FY2022/HydroWIRES_D3/calc/run_go/lib/EIA_bus_match100.csv')

hydro_plant_tbl_fl = hydro_plant_tbl %>%
  dplyr::rename(eia_plant_id = EIA_ID,
                balancing_authority = bal_auth,
                Bus = `new bus`) %>%
  mutate(Generator = paste0(Bus, '_HYDRO')) %>%
  dplyr::select(Bus, Generator, balancing_authority, eia_plant_id)


# build static plant data
boundary_conditions = read_csv('../run_startfit/data/output/2000_2019_weekly_GOWEST_hydro_inputs_with_release_mult_and_storage_tuewk_updateror.csv')
boundary_conditions_ror = read_csv('../run_startfit/data/output/2000_2019_weekly_GOWEST_hydro_inputs_rorstoragetarget.csv')
  
plant_tbl = boundary_conditions %>%
  dplyr::select(EIA_ID, MWh_per_MCM_release, nameplate) %>%
  distinct()

# -----------------------------------
# read in fisch data (storage facilities)

file_list = list.files('data/input_data/fisch', pattern = 'dayahead_storage*')

dp_dat_stat = tibble()
for(i in 1:length(file_list)){
  file_sel = file_list[i]
  file_meta = unlist(str_split(file_path_sans_ext(file_sel), pattern = '_'))
  type_sel = file_meta[2]
  eia_id_sel = as.numeric(file_meta[3])
    
  dp_dat_temp = read_csv(paste0('data/output/wsynthetic/dayahead_', type_sel, '_', eia_id_sel, '.csv'))
  dp_dat_temp = dp_dat_temp %>%
    mutate(EIA_ID = eia_id_sel, type = type_sel)

  dp_dat_stat_temp = dp_dat_temp %>%
    dplyr::filter(!is.na(week_commencing), time != 169) %>%
    mutate(generation = benefit_revenue / price_price,
           MWh_per_MCM = generation / release_turbine) %>%
    group_by(EIA_ID, type, forecast, week_commencing) %>%
    dplyr::summarise(benefit_revenue = sum(benefit_revenue), 
                     release_turbine = sum(release_turbine),
                     release_spill = sum(release_spill),
                     price_price = mean(price_price),
                     generation = sum(generation),
                     MWh_per_MCM_calc = mean(MWh_per_MCM),
                     storage_sim = mean(storage_sim))

  dp_dat_stat = bind_rows(dp_dat_stat_temp, dp_dat_stat)
}

# ---- currently only keeping storage facilities in analysis

# compute total revenue by forecast type
dp_dat_total = dp_dat_stat %>%
  group_by(forecast) %>%
  dplyr::summarise(benefit_revenue = sum(benefit_revenue))

dp_dat_total_stat = dp_dat_total %>%
  spread(forecast, benefit_revenue) %>%
  mutate(pers_diff = (persistence - perfect) / perfect * 100,
         synt_diff = (synthetic - perfect) / perfect * 100)

# id list of facilities in fisch sims
dp_eia_id_list = unique(dp_dat_stat$EIA_ID)

dp_dat_stat_bus = dp_dat_stat %>%
  dplyr::rename(eia_plant_id = EIA_ID) %>%
  left_join(hydro_plant_tbl_fl) %>%
  group_by(forecast, week_commencing, Bus) %>%
  dplyr::summarise(revenue_dp = sum(benefit_revenue),
                   price_dp = mean(price_price),
                   generation_dp = sum(generation))


# compute differences in revenue by bus and year
dp_dat_rev_bus_yr = dp_dat_stat_bus %>%
  dplyr::select(Bus, forecast, week_commencing, revenue_dp) %>%
  mutate(wyear = water_year(week_commencing)) %>%
  group_by(Bus, forecast, wyear) %>%
  dplyr::summarise(revenue_dp = sum(revenue_dp)) %>%
  spread(forecast, revenue_dp) %>%
  mutate(pers_diff_pct = (persistence - perfect) / perfect,
         synt_diff_pct = (synthetic - perfect) / perfect) %>%
  mutate(fcst_diff_pct = synt_diff_pct - pers_diff_pct)

# arrange data and format for plotting
dp_dat_rev_bus_yr_plot = dp_dat_rev_bus_yr %>%
  mutate(ylab = paste(Bus, wyear, sep = '_'),
         col_flag = ifelse(synt_diff_pct > pers_diff_pct, 'inc', 'dec')) %>% 
  arrange(pers_diff_pct)

dp_dat_rev_bus_yr_plot$ylab = factor(dp_dat_rev_bus_yr_plot$ylab, levels = unique(dp_dat_rev_bus_yr_plot$ylab))
dp_dat_rev_bus_yr_plot$yind = 1:nrow(dp_dat_rev_bus_yr_plot)

# plot distribution of revenue difference and change across forecast type (Figure 7)
ggplot(data = dp_dat_rev_bus_yr_plot) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 1, ymax = 183, 
            fill = 'gray90', alpha = 0.1) + # '#fef0e6'
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 183, ymax = 548, 
            fill = 'gray80', alpha = 0.1) + # '#eef2f8'
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 548, ymax = 609, 
            fill = 'gray70', alpha = 0.1) +
  geom_vline(xintercept = 0, alpha = 0.8) +
  geom_point(aes(y = yind, x = pers_diff_pct), shape = '●') +
  geom_point(aes(y = yind, x = synt_diff_pct), shape = '|') +
  geom_segment(aes(x = pers_diff_pct, xend = synt_diff_pct, y = yind, yend = yind,
                   color = col_flag), 
               arrow = arrow(length = unit(2, 'points')), linewidth = 0.2) +
  scale_x_continuous(limits = c(-0.03, 0.03), breaks = seq(from = -0.03, to = 0.03, by = 0.01),
                     labels = paste0(seq(from = -3, to = 3, by = 1), '%')) +
  scale_y_continuous(limits = c(1, nrow(test)), breaks = round(as.numeric(quantile(1:nrow(test), c(0.1, 0.3, 0.5, 0.7, 0.9)))),
                     labels = paste0(c(10, 30, 50, 70, 90), '%'), expand = c(0, 0)) +
  scale_color_manual(values = c('#fe6100', '#648fff'), guide = 'none') +
  xlab('') +
  ylab('') +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#808080"))

ggsave('figures/figure7_annualrevdist.png', height = 8, width = 12, units = 'in')


# ----
source_tbl = tibble(source = c('pers_diff_pct', 'synt_diff_pct'),
                    source_name = c('Persistence Optimized - Perfect Optimized',
                                    'Synthetic Optimized - Perfect Optimized'))

state_order_tbl = tibble(state_bus = c('WA', 'OR', 'ID', 'CA', 'NV', 'AZ', 'TX', 'CO', 'WY', 'MT'),
                         state_bus_ord = 1:10)


dp_dat_rev_bus_box = dp_dat_rev_bus_yr %>%
  dplyr::select(Bus, wyear, pers_diff_pct, synt_diff_pct) %>%
  gather(source, revenue_pct, -Bus, -wyear) %>%
  left_join(source_tbl) %>%
  left_join(bus_label_tbl) %>%
  left_join(state_order_tbl) %>%
  arrange(state_bus_ord)

dp_dat_rev_bus_box$Name = factor(dp_dat_rev_bus_box$Name, levels = unique(dp_dat_rev_bus_box$Name))

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, 
             colour = 'gray20',
             alpha = 1,
             linewidth = 0.3) +
  geom_boxplot(data = dp_dat_rev_bus_box, aes(x = Name, y = revenue_pct * 100, color = state_bus)) +
  scale_y_continuous(limits = c(-3, 2)) +
  scale_color_manual(values = brewer.pal(10, 'Paired')) +
  facet_wrap(~source_name, scales = 'free', ncol = 1) +
  theme_bw() +
  xlab('') +
  ylab('Revenue Difference (%)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(family = 'Lato', size = 12),
        strip.background = element_rect(fill = NA),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  ggtitle('Total Revenue Difference')

ggsave('figures/figure6_annrevboxplot.png', height = 8, width = 8)

#-----------------
# map of median revenue difference by Bus
conus_shp = st_read('/Users/brom374/Library/CloudStorage/OneDrive-PNNL/Documents/Projects/FY2022/9505/PM/presentationprep/boundary_conus.shp')
states_shp = st_read('/Users/brom374/Library/CloudStorage/OneDrive-PNNL/Documents/Datasets/EIA/WECC_Boundary_States.shp')
wecc_shp = st_read('/Users/brom374/Library/CloudStorage/OneDrive-PNNL/Documents/Datasets/EIA/WECC_Boundary.shp')

# ---
dp_dat_pp = dp_dat_tot_wy_ud %>%
  group_by(Bus, source, source_name) %>%
  dplyr::summarise(revenue_pct = median(revenue_pct))

dp_dat_pp_map = go_nodes_100_hydro_tbl %>%
  left_join(dp_dat_pp) %>%
  mutate(cap_bin = cut(capacity, breaks=c(0, 50, 200, 1000, 5000, 10000))) %>%
  dplyr::filter(!is.na(revenue_pct))

ggplot() +
  with_shadow(geom_sf(data = wecc_shp, fill = '#ece6d9', color = NA), colour = '#cccccc') +
  geom_sf(data = states_shp, fill = NA, color = '#f4f4f2', linewidth = 0.2) +
  # geom_sf(data = huc4_shp_skill, aes(fill = NSE), color = 'white', linewidth = 0.1) +
  # geom_sf(data = lakes_shp, color = '#a6cee3') +
  # geom_sf(data = rivers_shp, fill = NA, color = '#a6cee3', linewidth = 0.1, alpha = 0.7) +
  # geom_sf_text(data = huc4_shp_skill, aes(label = round(NSE, 2)), size = 2.2) +
  geom_point(data = dp_dat_pp_map, aes(x = lon_adj, y = lat_adj, size = cap_bin, color = revenue_pct * 100)) +
  geom_text(data = dp_dat_pp_map, aes(x = lon_adj, y = lat_adj, label = paste0(round(revenue_pct * 100, 2), '%')), 
            size = 4, nudge_x = 0.5, nudge_y = 0.5, hjust = 0) +
  facet_wrap(~source_name, ncol = 2) +
  scale_color_gradientn(colors = rev(brewer.pal(9, 'YlOrRd')), na.value = NA, name = 'Revenue Difference',
                        limits = c(-1, 0.05), breaks = seq(from = -1, to = 0, by = 0.5),
                        labels = paste0(seq(from = -1, to = 0, by = 0.5), '%')) +
  # coord_sf(datum = st_crs(3857)) +
  theme_bw() +
  xlab('') +
  ylab('') +
  # scale_y_continuous(limits = c(23, 54)) +
  # scale_x_continuous(limits = c(-126, -65)) +
  theme(legend.position = 'bottom',
        # legend.title = element_blank(),
        text = element_text(family = 'Lato', size = 12),
        strip.background = element_rect(fill = NA)
  ) +
  guides(size = 'none',
         linetype = 'none',
         color = guide_colorbar(barwidth = 20, barheight = 1, title.position = 'bottom',
                                title.hjust = 0.5))

ggsave('data/output/bus_map_revenuediff_sp.png', height = 8, width = 10)

# ---
#- forecast perfect vs persistence

dp_dat_pp = dp_dat_tot_wy %>%
  dplyr::filter(source == 'revenue_pct_pers') %>%
  group_by(Bus) %>%
  dplyr::summarise(revenue_pct = median(revenue_pct))

dp_dat_pp_map = go_nodes_100_hydro_tbl %>%
  left_join(dp_dat_pp)

dp_dat_pp_map = dp_dat_pp_map %>%
  mutate(cap_bin = cut(capacity, breaks=c(0, 50, 200, 1000, 5000, 10000)),
         revenue_pct_bin = cut(revenue_pct, breaks = c(-1, 0, 0.5, 1, 1.5, 2, 5))) %>%
  dplyr::filter(!is.na(revenue_pct))

dp_dat_pp_map$set = 'Persistence Forecast vs Perfect'

ggplot() +
  with_shadow(geom_sf(data = wecc_shp, fill = '#ece6d9', color = NA), colour = '#cccccc') +
  geom_sf(data = states_shp, fill = NA, color = '#f4f4f2', linewidth = 0.2) +
  # geom_sf(data = huc4_shp_skill, aes(fill = NSE), color = 'white', linewidth = 0.1) +
  # geom_sf(data = lakes_shp, color = '#a6cee3') +
  # geom_sf(data = rivers_shp, fill = NA, color = '#a6cee3', linewidth = 0.1, alpha = 0.7) +
  # geom_sf_text(data = huc4_shp_skill, aes(label = round(NSE, 2)), size = 2.2) +
  geom_point(data = dp_dat_pp_map, aes(x = lon, y = lat, size = cap_bin, color = revenue_pct * -100)) +
  geom_text(data = dp_dat_pp_map, aes(x = lon, y = lat, label = paste0(round(revenue_pct * -100, 2), '%')), 
            size = 2, nudge_x = 0.5, nudge_y = 0, hjust = 0) +
  scale_color_gradientn(colors = rev(brewer.pal(9, 'YlOrRd')), na.value = NA, name = 'Persistence Forecast Revenue Loss (%)',
                        limits = c(-2.75, 0), breaks = seq(from = -2.5, to = 0, by = 0.5)) +
  # coord_sf(datum = st_crs(3857)) +
  theme_bw() +
  xlab('') +
  ylab('') +
  # scale_y_continuous(limits = c(23, 54)) +
  # scale_x_continuous(limits = c(-126, -65)) +
  theme(legend.position = 'bottom',
        # legend.title = element_blank(),
        text = element_text(family = 'Lato', size = 12, hjust = 2),
  ) +
  guides(size = 'none',
         linetype = 'none',
         color = guide_colorbar(barwidth = 20, barheight = 1, title.position = 'bottom',
                                title.hjust = 0.5))

ggsave('data/output/bus_map_revenuediff_pers.png', height = 6, width = 4)

#- forecast perfect vs synthetic

dp_dat_ps = dp_dat_tot_wy %>%
  dplyr::filter(source == 'revenue_pct_synt') %>%
  group_by(Bus) %>%
  dplyr::summarise(revenue_pct = median(revenue_pct))

dp_dat_ps_map = go_nodes_100_hydro_tbl %>%
  left_join(dp_dat_ps)

dp_dat_ps_map = dp_dat_ps_map %>%
  mutate(cap_bin = cut(capacity, breaks=c(0, 50, 200, 1000, 5000, 10000)),
         revenue_pct_bin = cut(revenue_pct, breaks = c(-1, 0, 0.5, 1, 1.5, 2, 5))) %>%
  dplyr::filter(!is.na(revenue_pct))

dp_dat_ps_map$set = 'Synthetic Forecast vs Perfect'

ggplot() +
  with_shadow(geom_sf(data = wecc_shp, fill = '#ece6d9', color = NA), colour = '#cccccc') +
  # geom_sf(data = states_shp, fill = NA, color = '#f4f4f2', linewidth = 0.2) +
  # geom_sf(data = huc4_shp_skill, aes(fill = NSE), color = 'white', linewidth = 0.1) +
  # geom_sf(data = lakes_shp, color = '#a6cee3') +
  # geom_sf(data = rivers_shp, fill = NA, color = '#a6cee3', linewidth = 0.1, alpha = 0.7) +
  # geom_sf_text(data = huc4_shp_skill, aes(label = round(NSE, 2)), size = 2.2) +
  geom_point(data = dp_dat_pp_map, aes(x = lon, y = lat, size = capacity, color = revenue_pct * -100)) +
  geom_text(data = dp_dat_pp_map, aes(x = lon, y = lat, label = paste0(round(revenue_pct * -100, 2), '%')), 
            size = 2, nudge_x = 0.5, nudge_y = 0, hjust = 0) +
  scale_color_gradientn(colors = rev(brewer.pal(9, 'YlOrRd')), na.value = NA, name = 'Synthetic Forecast Revenue Loss (%)',
                        limits = c(-2.75, 0)) +
  # coord_sf(datum = st_crs(3857)) +
  theme_bw() +
  xlab('') +
  ylab('') +
  # scale_y_continuous(limits = c(23, 54)) +
  # scale_x_continuous(limits = c(-126, -65)) +
  theme(legend.position = 'bottom',
        # legend.title = element_blank(),
        text = element_text(family = 'Lato', size = 12),
  ) +
  guides(size = 'none',
         linetype = 'none',
         color = guide_colorbar(barwidth = 20, barheight = 1, title.position = 'bottom',
                                title.hjust = 0.5))

ggsave('data/output/bus_map_revenuediff_synt.png', height = 6, width = 4)

dp_dat_map_pd = bind_rows(dp_dat_pp_map, dp_dat_ps_map)

ggplot() +
  with_shadow(geom_sf(data = wecc_shp, fill = '#ece6d9', color = NA), colour = '#cccccc') +
  # geom_sf(data = states_shp, fill = NA, color = '#f4f4f2', linewidth = 0.2) +
  # geom_sf(data = huc4_shp_skill, aes(fill = NSE), color = 'white', linewidth = 0.1) +
  # geom_sf(data = lakes_shp, color = '#a6cee3') +
  # geom_sf(data = rivers_shp, fill = NA, color = '#a6cee3', linewidth = 0.1, alpha = 0.7) +
  # geom_sf_text(data = huc4_shp_skill, aes(label = round(NSE, 2)), size = 2.2) +
  geom_point(data = dp_dat_map_pd, aes(x = lon, y = lat, size = capacity, color = revenue_pct * -100)) +
  geom_text(data = dp_dat_map_pd, aes(x = lon, y = lat, label = paste0(round(revenue_pct * -100, 2), '%')), 
            size = 2, nudge_x = 0.5, nudge_y = 0, hjust = 0) +
  facet_wrap(~set) +
  scale_color_gradientn(colors = rev(brewer.pal(9, 'YlOrRd')), na.value = NA, name = 'Revenue Difference',
                        limits = c(-3, 1), breaks = seq(from = -3, to = 1, by = 0.5), 
                        labels = paste0(seq(from = -3, to = 1, by = 0.5), '%')) +
  # coord_sf(datum = st_crs(3857)) +
  theme_bw() +
  xlab('') +
  ylab('') +
  # scale_y_continuous(limits = c(23, 54)) +
  # scale_x_continuous(limits = c(-126, -65)) +
  theme(legend.position = 'bottom',
        # legend.title = element_blank(),
        text = element_text(family = 'Lato', size = 12),
  ) +
  guides(size = 'none',
         linetype = 'none',
         color = guide_colorbar(barwidth = 20, barheight = 1, title.position = 'bottom',
                                title.hjust = 0.5))

ggsave('data/output/bus_map_revenuediff_sp.png', height = 6, width = 8)


#-------------------------------------------------------------------------------
#- examine when differences are large for buses with large differences
dp_dat_tot = dp_dat_stat_comp %>%
  left_join(dp_dat_pers) %>%
  left_join(dp_dat_synt) %>%
  mutate(wyear = water_year(week_commencing)) %>%
  group_by(Bus, forecast, week_commencing) %>%
  dplyr::summarise(revenue_dp = sum(revenue_dp),
                   revenue_dp_pers = sum(revenue_dp_pers),
                   revenue_dp_synt = sum(revenue_dp_synt)) %>%
  mutate(revenue_pct_pers = (revenue_dp_pers - revenue_dp) / revenue_dp) %>%
  mutate(revenue_pct_synt = (revenue_dp_synt - revenue_dp) / revenue_dp)


Bus_list = paste0('bus_', c(10228,  10677, 13000, 13366, 20136, 23054, 23060,
                            10570, 10677, 20007, 25289))
test = dp_dat_tot %>%
  dplyr::filter(forecast == 'perfect', Bus %in% Bus_list) %>%
  arrange(revenue_pct_pers)

bus_sel = 'bus_23060' # 'bus_10677'
week_sel = '2017-01-03' # '2005-12-20'
eia_id_list = hydro_node_tbl %>%
  dplyr::filter(`new bus` == bus_sel) %>%
  pull(EIA_ID)

test2 = dp_dat_stat %>%
  dplyr::filter(week_commencing == week_sel, EIA_ID %in% eia_id_list)

ggplot() +
  geom_point(data = test2, aes(x = week_commencing, y = benefit_revenue, color = forecast,
                               shape = forecast, group = EIA_ID), size = 4) + 
  scale_y_log10() +
  theme_bw()

hydro_node_tbl %>% filter(EIA_ID == 3867)

ggplot() +
  stat_ecdf(data = test, aes(x = revenue_pct_synt, group = Bus)) +
  theme_bw() +
  xlab('Revenue Difference (%)') +
  ylab('') +
  ggtitle('Synthetic')

#-------------------------------------------------------------------------------
#- forecast go vs dp

dp_dat_pp = dp_dat_tot_wy %>%
  dplyr::filter(source == 'revenue_pct_uo') %>%
  group_by(Bus) %>%
  dplyr::summarise(revenue_pct = median(revenue_pct))

dp_dat_pp_map = go_nodes_100_hydro_tbl %>%
  left_join(dp_dat_pp)

ggplot() +
  with_shadow(geom_sf(data = wecc_shp, fill = '#ece6d9', color = NA), colour = '#cccccc') +
  # geom_sf(data = states_shp, fill = NA, color = '#f4f4f2', linewidth = 0.2) +
  # geom_sf(data = huc4_shp_skill, aes(fill = NSE), color = 'white', linewidth = 0.1) +
  # geom_sf(data = lakes_shp, color = '#a6cee3') +
  # geom_sf(data = rivers_shp, fill = NA, color = '#a6cee3', linewidth = 0.1, alpha = 0.7) +
  # geom_sf_text(data = huc4_shp_skill, aes(label = round(NSE, 2)), size = 2.2) +
  geom_point(data = dp_dat_pp_map, aes(x = lon, y = lat, size = capacity, color = revenue_pct * -100), alpha = 0.5) +
  scale_color_gradientn(colors = c('#fec44f', brewer.pal(9, 'GnBu')), na.value = NA, name = 'DP Optimization Revenue Gain (%)',
                        limits = c(-2.5, 35)) +
  # coord_sf(datum = st_crs(3857)) +
  theme_bw() +
  xlab('') +
  ylab('') +
  # scale_y_continuous(limits = c(23, 54)) +
  # scale_x_continuous(limits = c(-126, -65)) +
  theme(legend.position = 'bottom',
        # legend.title = element_blank(),
        text = element_text(family = 'Lato', size = 12),
  ) +
  guides(size = 'none',
         linetype = 'none',
         color = guide_colorbar(barwidth = 20, barheight = 1, title.position = 'bottom',
                                title.hjust = 0.5))




# ------------------------------------------------------------------------------
# compare release
dat_starfit_release = dat_starfit_release %>%
  dplyr::rename(release_starfit = release, generation_starfit = MWh,
                spill_starfit = spill, storage_starfit = storage)

# use merged data w/ ror storage target data
dp_dat_release = dp_dat_stat_ror %>%
  ungroup() %>%
  dplyr::filter(forecast == 'perfect') %>%
  dplyr::rename(release_dp = release_turbine,
                spill_dp = release_spill) %>%
  left_join(dat_starfit_release) %>%
  dplyr::rename(generation_dp = generation,
                storage_dp = storage_sim,
                revenue_dp = benefit_revenue) %>%
  # mutate(generation_dp = release_dp * MWh_per_MCM_release) %>%
  mutate(revenue_starfit = generation_starfit * price_price,
         totalrelease_starfit = release_starfit + spill_starfit,
         totalrelease_dp = release_dp + spill_dp) %>%
  mutate(release_diff = release_dp - release_starfit,
         totalrelease_diff = totalrelease_dp - totalrelease_starfit,
         generation_diff = generation_dp - generation_starfit,
         revenue_diff = revenue_dp - revenue_starfit)


dp_dat_release_td = dp_dat_release %>%
  dplyr::select(type, EIA_ID, week_commencing, 
                release_dp, release_starfit, release_diff, 
                totalrelease_dp, totalrelease_starfit, totalrelease_diff,
                generation_diff, generation_dp, generation_starfit, 
                spill_dp, spill_starfit,
                storage_dp, storage_starfit,
                revenue_diff, revenue_dp, revenue_starfit) %>%
  gather(variable, value, -EIA_ID, -week_commencing, -type)

set_tbl = tibble(variable = c('release_dp', 'release_starfit', 'spill_dp', 'spill_starfit', 
                              'totalrelease_dp', 'totalrelease_starfit', 'totalrelease_diff',
                              'release_diff', 'generation_diff', 'revenue_diff',
                              'generation_dp', 'generation_starfit',
                              'storage_dp', 'storage_starfit',
                              'revenue_dp', 'revenue_starfit'),
                 set = c(rep('release', 4), 
                         rep('total release', 2),
                         'total release difference',
                         'release difference', 
                         'generation difference', 
                         'revenue difference',
                         rep('generation', 2),
                         rep('storage', 2),
                         rep('revenue', 2)))

dp_dat_release_td = dp_dat_release_td %>%
  left_join(set_tbl)


dp_dat_release_td$variable = factor(dp_dat_release_td$variable, levels = c('generation_dp', 'generation_starfit', 
             'generation_diff',
             'release_dp', 'release_starfit', 'spill_dp', 'spill_starfit',
             'release_diff',
             'totalrelease_dp', 'totalrelease_starfit',
             'totalrelease_diff',
             'revenue_dp', 'revenue_starfit',
             'revenue_diff',
             'storage_dp', 'storage_starfit'))

dp_dat_release_td$set = factor(dp_dat_release_td$set, levels = c('generation', 
                             'generation difference', 'release', 'release difference',
                             'total release', 'storage', 'total release difference', 'revenue', 'revenue difference'))

eia_id_list = unique(dp_dat_release$EIA_ID)

diff_tbl = tibble()
for(i in 1:length(eia_id_list)){
  eia_id = eia_id_list[i]
  test = dp_dat_release_td %>%
    dplyr::filter(EIA_ID == eia_id)
  
  type_sel = test$type[1]
  
  test2 = test %>%
    group_by(variable, set) %>%
    dplyr::summarise(value = sum(value))
  
  gen_diff_pct = ((dplyr::filter(test2, variable == 'generation_dp') %>% pull(value)) -
                    (dplyr::filter(test2, variable == 'generation_starfit') %>% pull(value))) /
    (dplyr::filter(test2, variable == 'generation_starfit') %>% pull(value))
  
  release_diff_pct = ((dplyr::filter(test2, variable == 'release_dp') %>% pull(value)) -
                        (dplyr::filter(test2, variable == 'release_starfit') %>% pull(value))) /
    (dplyr::filter(test2, variable == 'release_starfit') %>% pull(value))
  
  totalrelease_diff_pct = ((dplyr::filter(test2, variable == 'totalrelease_dp') %>% pull(value)) -
                        (dplyr::filter(test2, variable == 'totalrelease_starfit') %>% pull(value))) /
    (dplyr::filter(test2, variable == 'totalrelease_starfit') %>% pull(value))
  
  rev_diff_pct = ((dplyr::filter(test2, variable == 'revenue_dp') %>% pull(value)) -
                        (dplyr::filter(test2, variable == 'revenue_starfit') %>% pull(value))) /
    (dplyr::filter(test2, variable == 'revenue_starfit') %>% pull(value))
  
  label_tbl = tibble(label = c(paste(round(gen_diff_pct * 100, 2), '%'),
                               paste(round(release_diff_pct * 100, 2), '%'),
                               paste(round(rev_diff_pct * 100, 2), '%'),
                               paste(round(totalrelease_diff_pct * 100, 2), '%')),
                     week_commencing = as.Date('2000-01-04'),
                     value = 0, set = c('generation', 'release', 'revenue', 'total release'))
  diff_tbl_temp = tibble(gen_diff_pct, release_diff_pct, rev_diff_pct, totalrelease_diff_pct)
  diff_tbl_temp$EIA_ID = eia_id
  diff_tbl = bind_rows(diff_tbl, diff_tbl_temp)
  
 ggplot() +
    geom_hline(yintercept = 0, color = 'black', linetype = 2, alpha = 0.8) +
    geom_line(data = test, aes(x = week_commencing, y = value, color = variable)) +
    geom_text(data = label_tbl, aes(x = week_commencing, y = value, label = label), vjust = 0) +
    facet_wrap(~set, ncol = 1, scales = 'free_y') +
    scale_color_manual(values = c('#648FFF', '#FFB000', # generation
                                  '#DC2660',            # generation difference
                                  '#648FFF', '#FFB000', '#785EF0', '#FE6100', # release (and spill)
                                  '#DC2660',            # release difference
                                  '#648FFF', '#FFB000', # total release
                                  '#DC2660',            # total release difference
                                  '#648FFF', '#FFB000', # revenue
                                  '#DC2660',            # revenue difference
                                  '#648FFF', '#FFB000')) + # storage
    theme_bw() +
    xlab('') +
    ylab('Release (MCM)                       Generation (MWh)') +
    theme( #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = 'Lato', size = 12),
      strip.background = element_rect(fill = NA),
      legend.position = 'bottom',
      legend.title = element_blank()) +
    ggtitle(paste(eia_id, type_sel))

  ggsave(paste0('data/output/gen_release_timeseries_', eia_id, '.png'), height = 8, width = 10)
}

dp_dat_release_stat = dp_dat_release_td %>%
  group_by(type, variable) %>%
  dplyr::summarise(value = sum(value)) %>%
  spread(variable, value) %>%
  mutate(release_diff_pct = (release_dp - release_starfit) / release_starfit,
         rev_diff_pct = (revenue_dp - revenue_starfit) / revenue_starfit,
         gen_diff_pct = (generation_dp - generation_starfit) / generation_starfit,
         totalrelease_diff_pct = (totalrelease_dp - totalrelease_starfit) / totalrelease_starfit) %>%
  dplyr::select(type, release_diff_pct, rev_diff_pct, gen_diff_pct, totalrelease_diff_pct) %>%
  gather(variable, value, -type)

diff_tbl_td = diff_tbl %>%
  gather(variable, value, -EIA_ID)

lab_tbl = tibble(variable = c('gen_diff_pct', 'release_diff_pct', 
                              'rev_diff_pct', 'totalrelease_diff_pct'))
ggplot() +
  geom_boxplot(data = diff_tbl_td, aes(x = variable, y = value)) +
  geom_point(data = dp_dat_release_stat, aes(x = variable, y = value, color = type)) +
  theme_bw() +
  theme( #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    text = element_text(family = 'Lato', size = 12),
    strip.background = element_rect(fill = NA),
    legend.position = 'bottom',
    legend.title = element_blank()) 


# ------------------------------------------------------------------------------

# -----
ggplot() +
  geom_point(data = dp_dat_tot, aes(x = revenue_uo, y = revenue_dp, color = factor(Bus))) +
  geom_abline(linetype = 2) +
  theme_bw() +
  ylab('DP Revenue') +
  xlab('GO1 Revenue') +
  theme(text = element_text(family = 'Lato', size = 12),
    strip.background = element_rect(fill = NA)) +
  ggtitle('')

dp_dat_tot_yr = dp_dat_tot %>%
  mutate(year = year(week_commencing)) %>%
  group_by(year, Bus) %>%
  dplyr::summarise(revenue_uo = sum(revenue_uo), revenue_dp = sum(revenue_dp)) %>%
  mutate(revenue_pct_uo = (revenue_dp - revenue_uo) / revenue_uo)


bus_cap_tbl = hydro_plant_tbl %>% 
  left_join(type_tbl) %>%
  dplyr::filter(!is.na(type)) %>%
  dplyr::rename(Bus = `new bus`) %>%
  group_by(type, Bus) %>%
  dplyr::summarise(capacity = sum(capacity)) %>%
  spread(type, capacity) %>%
  mutate(storage = ifelse(is.na(storage), 0, storage),
         ror = ifelse(is.na(ror), 0, ror)) %>%
  mutate(pct_storage = storage / (storage + ror))

dp_dat_tot_yr = dp_dat_tot_yr %>%
  left_join(bus_cap_tbl)

pp = ggplot() +
  geom_line(data = dp_dat_tot_yr, aes(x = year, y = revenue_pct_uo * 100, 
                                      color = pct_storage, group = Bus)) +
  theme_bw()

ggplotly(pp)

# 30001, 74235, 70063, 10033

hydro_plant_tbl %>% 
  left_join(type_tbl) %>%
  filter(`new bus` == 'bus_50316')

dp_val_mw = dp_dat_stat_comp %>%
  ungroup() %>%
  dplyr::select(forecast, week_commencing, Bus, value_mw_uo, value_mw_dp) %>%
  dplyr::filter(forecast == 'perfect') %>%
  gather(set, value, -week_commencing, -Bus, -forecast)

ggplot() +
  geom_boxplot(data = dp_val_mw, aes(x = Bus, fill = set, y = value), position = 'dodge') +
  theme_bw() +
  xlab('') +
  ylab('Dollars / MWh') +
  theme(text = element_text(family = 'Lato', size = 12),
        axis.text.x = element_text(family = 'Lato', angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_rect(fill = NA),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  ggtitle('')


test = dp_dat_stat_comp %>%
  ungroup() %>%
  filter(forecast == 'perfect') %>%
  dplyr::select(week_commencing, Bus, generation_dp, generation_uo, generation_sf) %>%
  dplyr::rename(DP = generation_dp, GO1 = generation_uo, STARFIT = generation_sf) %>%
  gather(source, generation_MWh, -Bus, -week_commencing)

ggplot() +
  stat_ecdf(data = test, aes(x = generation_MWh, color = source)) +
  theme_bw() +
  xlab('') +
  ylab('') +
  theme(text = element_text(family = 'Lato', size = 12),
        strip.background = element_rect(fill = NA),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  ggtitle('')


test2 = test %>%
  group_by(Bus, source) %>%
  dplyr::summarise(generation_MWh = sum(generation_MWh))


ggplot(test2, aes(Date2, total, fill=event)) +
  geom_bar(data = test2, aes(x = Bus, fill = source, y = generation_MWh), stat = "identity", position = 'dodge') +
  theme_bw() +
  xlab('') +
  ylab('') +
  scale_y_continuous(labels = comma) +
  theme(text = element_text(family = 'Lato', size = 12),
        strip.background = element_rect(fill = NA),
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle('')


# revenue difference by year
dp_dat_tot_yr = dp_dat_stat_comp %>%
  dplyr::rename(revenue_dp = benefit_revenue) %>%
  left_join(dp_dat_pers) %>%
  left_join(dp_dat_synt) %>%
  mutate(year = year(week_commencing)) %>%
  group_by(year, EIA_ID, forecast, type) %>%
  dplyr::summarise(revenue_dp = sum(revenue_dp),
                   revenue_uo = sum(revenue_uo),
                   revenue_dp_pers = sum(revenue_dp_pers),
                   revenue_dp_synt = sum(revenue_dp_synt)) %>%
  mutate(revenue_pct_uo = (revenue_dp - revenue_uo) / revenue_uo) %>%
  mutate(revenue_pct_pers = (revenue_dp - revenue_dp_pers) / revenue_dp_pers) %>%
  mutate(revenue_pct_synt = (revenue_dp - revenue_dp_synt) / revenue_dp_synt)

source_tbl = tibble(source = c('revenue_pct_uo', 'revenue_pct_pers', 'revenue_pct_synt'),
                    source_name = c('Perfect Unoptimzed - Perfect Optimized', 
                                    'Persistence Optimized - Perfect Optimized',
                                    'Synthetic Optimized - Perfect Optimized'))

dp_dat_tot_bm_yr = dp_dat_tot_yr %>%
  ungroup() %>%
  dplyr::filter(forecast == 'perfect') %>%
  dplyr::select(year, type, EIA_ID, revenue_pct_uo, revenue_pct_pers, revenue_pct_synt) %>%
  gather(source, revenue_pct, -year, -type, -EIA_ID) %>%
  left_join(source_tbl)

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, 
             colour = 'gray20',
             alpha = 1,
             size = 0.3) +
  geom_boxplot(data = dp_dat_tot_bm_yr, aes(x = source_name, y = revenue_pct * 100)) +
  facet_wrap(~type, ncol = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('Revenue Difference (%)') +
  theme( #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    text = element_text(family = 'Lato', size = 12),
    strip.background = element_rect(fill = NA)) +
  ggtitle('Annual Revenue Difference')

dp_dat_tot_bm %>% group_by(source_name, type) %>% summarise(q50 = quantile(revenue_pct, 0.5)*100, 
                                                            q75 = quantile(revenue_pct, 0.75)*100, 
                                                            q25 = quantile(revenue_pct, 0.25)*100)


dp_dat_tot_bm_yr %>% 
  dplyr::filter(!is.na(revenue_pct)) %>%
  group_by(source_name, type) %>% summarise(q50 = quantile(revenue_pct, 0.5)*100, 
                                                            q75 = quantile(revenue_pct, 0.75)*100, 
                                                            q25 = quantile(revenue_pct, 0.25)*100)
