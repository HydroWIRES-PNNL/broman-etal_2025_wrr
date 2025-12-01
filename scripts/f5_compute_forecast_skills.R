#===========================================================
# Name: compute_forecast_skills.R
# Author: D. Broman, PNNL
# Last Modified: 2025-10-01
# Description: [D3] compute inflow forecast skill metrics
#===========================================================

library(tidyverse)
library(arrow)
library(readxl)
library(hydroGOF)
library(aweek)
library(RColorBrewer)

wd = ''
setwd(wd)

fcst_day_tbl = tibble(fcst_day_abb = c('Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun', 'Mon'),
                      validtime_ind = c(0:6))

# read in plant table

boundary_conditions = read_csv('data/input_data/starfit_reservoirtargets/2000_2019_weekly_GOWEST_hydro_inputs_with_release_mult_and_storage_tuewk_updateror.csv')

# create list of storage facility IDs
storage_list = boundary_conditions %>%
  dplyr::filter(type == 'STARFIT') %>%
  pull(EIA_ID) %>% 
  unique()

# read in bus table
hydro_node_tbl = read_csv('data/input_data/go-west_topology/EIA_bus_match100.csv')

# GO-WEST nodes representative locations for plotting
go_nodes_100_tbl = read_csv('data/input_data/go-west_topology/WECC_nodes_100_wcoordsandstates_adj.csv')

hydro_node_tbl = hydro_node_tbl %>%
  mutate(Number = as.numeric(substr(`new bus`, 5, nchar(`new bus`)))) %>%
  dplyr::rename(Bus = `new bus`)

go_nodes_100_hydro_tbl = go_nodes_100_tbl %>%
  left_join(hydro_node_tbl) %>%
  dplyr::filter(!is.na(plant)) %>%
  group_by(Bus, Number, Name, lat, lon, lat_adj, lon_adj, state_bus) %>%
  dplyr::summarise(capacity = sum(capacity))

bus_label_tbl = go_nodes_100_hydro_tbl %>%
  ungroup() %>%
  dplyr::select(Bus, Name, state_bus) %>%
  mutate(Name = str_to_title(Name))

# read in forecast data

# - format perfect forecast data; subset to keep only storage facilities; 
# - format synthetic forecast data
# - create and format persistence forecast data

#- perfect forecasts
fcst_perfect = read_csv('data/input_data/perfect_forecasts/forecast_perfect.csv') %>%
  mutate(valid_date = ymd(paste0(year, '-', mon, '-', day))) %>%
  dplyr::select(-year, -mon, -day) %>%
  gather(EIA_ID, inflow_actual, -valid_date) %>%
  mutate(EIA_ID = as.numeric(EIA_ID)) %>%
  dplyr::filter(EIA_ID %in% storage_list) %>%
  mutate(epiweek = as.numeric(substr(date2week(valid_date, week_start = 'Tuesday'), 7,8)), # set week definition (start day)
         year = year(valid_date),
         month = month(valid_date)) %>%
  mutate(year = ifelse(epiweek > 50 & month == 1, year - 1, year)) %>%
  mutate(year = ifelse(epiweek == 1 & month == 12, year + 1, year)) %>%
  group_by(EIA_ID, epiweek, year) %>%
  mutate(week_commencing = min(valid_date)) %>%
  mutate(week_ending = max(valid_date) + days(1),
         nday = n()) %>%
  dplyr::filter(nday == 7) %>%
  ungroup() %>%
  dplyr::select(EIA_ID, week_commencing, valid_date, inflow_actual)

# test persistence
fcst_persistence = read_csv('data/input_data/perfect_forecasts/forecast_perfect.csv') %>%
  mutate(valid_date = ymd(paste0(year, '-', mon, '-', day)) + days(1)) %>%
  dplyr::select(-year, -mon, -day) %>%
  gather(EIA_ID, inflow_actual, -valid_date) %>%
  mutate(EIA_ID = as.numeric(EIA_ID)) %>%
  dplyr::filter(EIA_ID %in% storage_list) %>%
  mutate(epiweek = as.numeric(substr(date2week(valid_date, week_start = 'Tuesday'), 7,8)), # set week definition (start day)
         year = year(valid_date),
         month = month(valid_date)) %>%
  mutate(year = ifelse(epiweek > 50 & month == 1, year - 1, year)) %>%
  mutate(year = ifelse(epiweek == 1 & month == 12, year + 1, year)) %>%
  group_by(EIA_ID, epiweek, year) %>%
  mutate(week_commencing = min(valid_date)) %>%
  mutate(week_ending = max(valid_date) + days(1),
         nday = n()) %>%
  dplyr::filter(nday == 7) %>%
  ungroup() %>%
  dplyr::select(EIA_ID, week_commencing, valid_date, inflow_actual) %>%
  dplyr::rename(inflow = inflow_actual) %>%
  mutate(initialization_date = week_commencing,
         lead = as.numeric(valid_date - initialization_date)) %>%
  group_by(EIA_ID, week_commencing) %>% 
  mutate(inflow = inflow[lead == 0]) %>%
  ungroup() %>%
  dplyr::select(EIA_ID, week_commencing, initialization_date, valid_date, lead, inflow)

#- synthetic forecasts
# EIA_ID = storage_list[1]
fcst_synthetic = tibble()
for(EIA_ID in storage_list){
  print(EIA_ID)
  
  fcst_synthetic_raw = read_excel(path = paste0('data/input_data/synthetic_forecasts/forecast_synthetic_', EIA_ID, '.xls'))
  
  fcst_synthetic_loc = fcst_synthetic_raw %>%
    mutate(initialization_date = as.Date(paste(Year, Month, Day, sep = '-'))) %>%
    dplyr::select(initialization_date, Tue, Wed, Thu, Fri, Sat, Sun, Mon) %>%
    gather(fcst_day_abb, inflow, -initialization_date) %>%
    mutate(epiweek = as.numeric(substr(date2week(initialization_date, week_start = 'Tuesday'), 7,8)), # set week definition (start day)
           year = year(initialization_date),
           month = month(initialization_date)) %>%
    mutate(year = ifelse(epiweek > 50 & month == 1, year - 1, year)) %>%
    mutate(year = ifelse(epiweek == 1 & month == 12, year + 1, year)) %>%
    group_by(epiweek, year) %>%
    mutate(week_commencing = min(initialization_date)) %>%
    ungroup() %>%
    left_join(fcst_day_tbl) %>%
    mutate(inittime_ind = as.numeric(difftime(initialization_date, week_commencing, units = 'days'))) %>%
    mutate(lead = validtime_ind - inittime_ind) %>%
    dplyr::filter(lead >= 0) %>%
    mutate(valid_date = initialization_date + days(lead), EIA_ID) %>%
    dplyr::select(EIA_ID, week_commencing, initialization_date, valid_date, lead, inflow)

  fcst_synthetic = bind_rows(fcst_synthetic, fcst_synthetic_loc)
}

ss_synthetic_dat = fcst_synthetic %>%
  left_join(fcst_perfect)

# compute skill statistics

ss_synthetic = ss_synthetic_dat %>%
  group_by(EIA_ID, lead) %>%
  dplyr::summarise(NSE = NSE(inflow, inflow_actual),
                   KGE = KGE(inflow, inflow_actual),
                   r2 = cor(inflow, inflow_actual),
                   RMSE = rmse(inflow, inflow_actual),
                   PBIAS = pbias(inflow, inflow_actual)) %>%
  mutate(type = 'Synthetic')


write_csv(ss_synthetic, 'data/output_data/forecast_skill/synthetic_skills.csv')

# persistence forecasts
ss_persistence_dat = fcst_persistence %>%
  left_join(fcst_perfect)

# compute skill statistics

ss_persistence = ss_persistence_dat %>%
  group_by(EIA_ID, lead) %>%
  dplyr::summarise(NSE = NSE(inflow, inflow_actual),
                   KGE = KGE(inflow, inflow_actual),
                   r2 = cor(inflow, inflow_actual),
                   RMSE = rmse(inflow, inflow_actual),
                   PBIAS = pbias(inflow, inflow_actual)) %>%
  mutate(type = 'Persistence')

write_csv(ss_persistence, 'data/output_data/forecast_skill/persistence_skills.csv')

# skill summaries

ss_synthetic_plant_stats = ss_synthetic %>%
  gather(stat, value, -EIA_ID, -lead) %>%
  group_by(stat, lead) %>%
  dplyr::summarise(max = max(value),
                   min = min(value),
                   median = median(value))


ss_merge = bind_rows(ss_synthetic, ss_persistence)

state_order_tbl = tibble(state_bus = c('WA', 'OR', 'ID', 'CA', 'NV', 'AZ', 'TX', 'CO', 'WY', 'MT'),
                         state_bus_ord = 1:10)

ss_bus = ss_merge %>%
  left_join(hydro_node_tbl) %>%
  left_join(bus_label_tbl) %>%
  dplyr::filter(!is.na(Bus)) %>%
  mutate(lead = lead + 1) %>%
  left_join(state_order_tbl) %>%
  arrange(state_bus_ord)

ss_bus$Name = factor(ss_bus$Name, levels = unique(ss_bus$Name))

ggplot() +
  geom_boxplot(data = ss_bus, aes(x = Name, y = KGE, color = state_bus)) +
  facet_grid(lead~type) +
  scale_color_manual(values = brewer.pal(10, 'Paired')) +
  xlab('Bus') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = 'Lato', size = 12),
      strip.background = element_rect(fill = NA),
      legend.position = 'bottom',
      legend.title = element_blank())

ggsave('figures/figure5_forecast_kge_boxplots_by_bus.png', height = 10, width = 8)

ggplot() +
  geom_boxplot(data = ss_bus, aes(x = Name, y = RMSE, color = state_bus)) +
  facet_grid(lead~type) +
  xlab('Bus') +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggsave('figures/figuresi3_forecast_rmse_boxplots_by_bus.png', height = 10, width = 8)

ggplot() +
  geom_boxplot(data = ss_bus, aes(x = Name, y = r2, color = state_bus)) +
  facet_grid(lead~type) +
  xlab('Bus') +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggsave('figures/figuresi4_forecast_r2_boxplots_by_bus.png', height = 10, width = 8)

ggplot() +
  geom_boxplot(data = ss_bus, aes(x = Name, y = NSE, color = state_bus)) +
  facet_grid(lead~type) +
  xlab('Bus') +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggsave('figures/figuresi5_forecast_nse_boxplots_by_bus.png', height = 10, width = 8)

ggplot() +
  geom_boxplot(data = ss_bus, aes(x = Name, y = PBIAS, color = state_bus)) +
  facet_grid(lead~type) +
  xlab('Bus') +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggsave('figures/figuresi6_forecast_pbias_boxplots_by_bus.png', height = 10, width = 8)
