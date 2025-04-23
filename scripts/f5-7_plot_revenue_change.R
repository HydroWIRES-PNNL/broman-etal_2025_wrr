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
library(ggrepel)
library(patchwork)

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

# correspondence between GO-WEST nodes and hydropower faciliites 
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

hydro_plant_tbl = read_csv('data/input_data/go-west_topology/EIA_bus_match100.csv')

hydro_plant_tbl_fl = hydro_plant_tbl %>%
  dplyr::rename(eia_plant_id = EIA_ID,
                balancing_authority = bal_auth,
                Bus = `new bus`) %>%
  mutate(Generator = paste0(Bus, '_HYDRO')) %>%
  dplyr::select(Bus, Generator, balancing_authority, eia_plant_id)


# build static plant data
boundary_conditions = read_csv('data/input_data/starfit_reservoirtargets/2000_2019_weekly_GOWEST_hydro_inputs_with_release_mult_and_storage_tuewk_updateror.csv')
boundary_conditions_ror = read_csv('data/input_data/starfit_reservoirtargets/2000_2019_weekly_GOWEST_hydro_inputs_rorstoragetarget.csv')
  
plant_tbl = boundary_conditions %>%
  dplyr::select(EIA_ID, MWh_per_MCM_release, nameplate) %>%
  distinct()

# -----------------------------------
# read in fisch data (storage facilities)

file_list = list.files('data/input_data/d3_fisch_simulations', pattern = 'dayahead_storage*')

dp_dat_stat = tibble()
for(i in 1:length(file_list)){
  file_sel = file_list[i]
  file_meta = unlist(str_split(file_path_sans_ext(file_sel), pattern = '_'))
  type_sel = file_meta[2]
  eia_id_sel = as.numeric(file_meta[3])
    
  dp_dat_temp = read_csv(paste0('data/input_data/d3_fisch_simulations/', file_sel))
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
         synt_diff = (synthetic - perfect) / perfect * 100,
         chg = (synthetic - perfect) / (persistence - perfect),
         synthetic - persistence) %>%
  mutate(syntpers_diff = pers_diff - synt_diff)



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
  geom_hline(yintercept = c(183, 548, 609), linetype = 3) +
  geom_vline(xintercept = 0, alpha = 0.8) +
  geom_point(aes(y = yind, x = pers_diff_pct), shape = '●') +
  geom_point(aes(y = yind, x = synt_diff_pct), shape = '|') +
  geom_segment(aes(x = pers_diff_pct, xend = synt_diff_pct, y = yind, yend = yind, color = col_flag), 
               arrow = arrow(length = unit(2, 'points')), linewidth = 0.2) +
  scale_x_continuous(limits = c(-0.03, 0.03), breaks = seq(from = -0.03, to = 0.03, by = 0.01), 
                     labels = paste0(seq(from = -3, to = 3, by = 1), '%')) +
  scale_y_continuous(limits = c(1, nrow(dp_dat_rev_bus_yr_plot)), breaks = round(as.numeric(quantile(1:nrow(dp_dat_rev_bus_yr_plot), c(0.1, 0.3, 0.5, 0.7, 0.9)))),
                     labels = paste0(c(10, 30, 50, 70, 90), '%')) +
  scale_color_manual(values = c('#fe6100', '#648fff'), guide = 'none') +
  xlab('Revenue Difference (%)') +
  ylab('') +
  theme_bw() +
  theme(text = element_text(family = 'Lato', size = 12), plot.title = element_text(hjust = 0.5)) +
  

  annotate('text', x = 0.015, y = 578.5, 
           label = 'more revenue than perfect', hjust = 0.5, vjust = 0.5, size = 4,
           family = 'Lato') +

  annotate('text', x = -0.015, y = 91.5, 
           label = 'less revenue than perfect', hjust = 0.5, vjust = 0.5, size = 4,
           family = 'Lato') +
    
  annotate('segment', x = 0.02, xend = 0.01, y = 400, yend = 400, 
           color = '#fe6100', arrow = arrow(length = unit(0.2, 'cm'))) +
  
  annotate('text', x = 0.015, y = 380, 
           label = 'decrease from persistence to synthetic', hjust = 0.5, vjust = 0.5, size = 3,
           family = 'Lato') +
    
  annotate('segment', x = 0.01, xend = 0.02, y = 300, yend = 300, 
           color = '#648fff', arrow = arrow(length = unit(0.2, 'cm'))) +
  
  annotate('text', x = 0.015, y = 280, 
           label = 'increase from persistence to synthetic', hjust = 0.5, vjust = 0.5, size = 3,
           family = 'Lato')

ggsave('figures/figure7_annualrevdist.png', height = 6, width = 8, units = 'in')

# quantile statistics for paper
dp_dat_rev_bus_yr_quantile = dp_dat_rev_bus_yr_plot %>%
  ungroup() %>%
  mutate(quantile = 1:nrow(dp_dat_rev_bus_yr_plot) / nrow(dp_dat_rev_bus_yr_plot))

dp_dat_rev_bus_30to90 = dp_dat_rev_bus_yr_quantile %>% filter(quantile >= 0.3, quantile < 0.9)

range(dp_dat_rev_bus_30to90$pers_diff_pct) * 100

dp_dat_rev_bus_90to100 = dp_dat_rev_bus_yr_quantile %>% 
  dplyr::filter(quantile >= 0.9)

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
dp_dat_rev_bus_box$revenue_pct_label = dp_dat_rev_bus_box$revenue_pct * 100

ggplot() +
  geom_hline(yintercept = 0, linetype = 2, 
             colour = 'gray20',
             alpha = 1,
             linewidth = 0.3) +
  geom_boxplot(data = dp_dat_rev_bus_box, aes(x = Name, y = revenue_pct_label, color = state_bus)) +
  scale_y_continuous(limits = c(-3, 3), breaks = round(seq(from = -3, to = 3, by = 0.5), 2), 
                     labels = paste0(round(seq(from = -3, to = 3, by = 0.5), 2), '%')) +
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

ggsave('figures/figure6_annrevboxplot-t.png', height = 8, width = 8)

#-----------------
# map of median revenue difference by Bus
states_sf = st_read('data/spatial/wecc_boundary_states.gpkg')
wecc_sf = st_read('data/spatial/wecc_boundary.gpkg')

# ---
dp_dat_pp = dp_dat_rev_bus_box %>%
  group_by(Bus, source, source_name) %>%
  dplyr::summarise(revenue_pct = as.numeric(quantile(revenue_pct, 0.5)))

dp_dat_pp_map = go_nodes_100_hydro_tbl %>%
  left_join(dp_dat_pp) %>%
  mutate(cap_bin = cut(capacity, breaks=c(0, 50, 200, 1000, 5000, 10000))) %>%
  dplyr::filter(!is.na(revenue_pct))

# outline two buses described in text
bus_highlight = dp_dat_pp_map %>%
  dplyr::filter(Name %in% c('PATTERSON 1 1', 'KLAMATH FALLS 1 1'))

ggplot() +
  with_shadow(geom_sf(data = wecc_sf, fill = '#ece6d9', color = NA), colour = '#cccccc') +
  geom_sf(data = states_sf, fill = NA, color = '#f4f4f2', linewidth = 0.2) +
  geom_point(data = dp_dat_pp_map, aes(x = lon_adj, y = lat_adj, size = cap_bin, fill = revenue_pct * 100), shape = 21) +
  geom_point(data = bus_highlight, aes(x = lon_adj, y = lat_adj, size = cap_bin), color = '#000000', fill = NA, shape = 21, stroke = 1.5) +
  geom_text_repel(data = dp_dat_pp_map, aes(x = lon_adj, y = lat_adj, label = paste0(round(revenue_pct * 100, 2), '%')),
                  size = 4, family = 'Lato', max.overlaps = 20, box.padding = 0.25) +
  facet_wrap(~source_name, ncol = 2) +
  scale_fill_gradientn(colors = rev(brewer.pal(9, 'YlOrRd')), na.value = NA, name = 'Revenue Difference',
                        limits = c(-1, 0.05), breaks = seq(from = -1, to = 0, by = 0.5),
                        labels = paste0(seq(from = -1, to = 0, by = 0.5), '%')) +
  theme_bw() +
  xlab('') +
  ylab('') +
  theme(legend.position = 'bottom',
        text = element_text(family = 'Lato', size = 12),
        strip.background = element_rect(fill = NA)
  ) +
  guides(size = 'none',
         linetype = 'none',
         fill = guide_colorbar(barwidth = 20, barheight = 1, title.position = 'bottom',
                                title.hjust = 0.5))

ggsave('figures/figure5_medianannrevmap.png', height = 8, width = 10)



#-----------------
# supplemental material revenue difference example figure

#- compute monthly revenue and look for months with large revenue losses +/-

dp_dat_stat_plant_mon = dp_dat_stat %>%
  mutate(wyear = water_year(week_commencing), month = month(week_commencing)) %>%
  dplyr::rename(eia_plant_id = EIA_ID) %>%
  left_join(hydro_plant_tbl_fl)
  group_by(Bus, eia_plant_id, forecast, wyear, month) %>%
  dplyr::summarise(benefit_revenue = sum(benefit_revenue))

hydro_node_tbl_jn = hydro_node_tbl %>%
  dplyr::select(EIA_ID, plant, state, capacity) %>%
  dplyr::rename(eia_plant_id = EIA_ID) %>%
  mutate(plant_label = paste0(plant, ' [', capacity, 'MW]'))

ex_rev = dp_dat_stat_plant_mon %>%
  dplyr::filter(Bus == 'bus_23060', wyear == 2006) %>% #, month == 9) %>%
  left_join(hydro_node_tbl_jn)

ex_rev_tot = ex_rev %>%
  group_by(week_commencing, forecast) %>%
  dplyr::summarise(benefit_revenue = sum(benefit_revenue))

p1 = ggplot() +
  geom_line(data = ex_rev, aes(x = week_commencing, y = benefit_revenue, color = forecast)) +
  facet_wrap(~plant_label, scales = 'free_x', ncol = 4) +
  scale_color_manual(values = c('#648FFF', '#FFB000', '#DC2660'), name = '') +
  theme_bw() +
  xlab('') +
  ylab('Revenue (dollars)') +
  theme(legend.position = 'bottom',
        text = element_text(family = 'Lato', size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_rect(fill = NA)) +
  guides(color = 'none') +
  ggtitle('Hayfork 1 1 Bus')

p2 = ggplot() +
  geom_line(data = ex_rev_tot, aes(x = week_commencing, y = benefit_revenue, color = forecast)) +
  scale_color_manual(values = c('#648FFF', '#FFB000', '#DC2660'), name = '') +
  theme_bw() +
  xlab('') +
  ylab('Revenue (dollars)') +
  theme(legend.position = 'bottom',
        text = element_text(family = 'Lato', size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.background = element_rect(fill = NA))

combined_plot = (p1 / p2) + 
  plot_layout(heights = c(4, 1))  # Adjust the height of the second plot

print(combined_plot)

ggsave('figures/figureS1_wyrevenueex.png', height = 10, width = 10)


