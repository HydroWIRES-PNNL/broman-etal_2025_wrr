# ----
# Name: 3_map_lmps_to_facilities.R
# Author: S. Turner, PNNL
# Modified: D. Broman, PNNL
# Last Modified: 2023-08-21
# Description: [D3] maps GO-WEST LMPs to each hydropower facility
# ----

library(tidyverse)
library(lubridate)

# PERFECT FORECASTS
read_csv('data/input_data/starfit_reservoirtargets/2000_2019_weekly_GOWEST_hydro_inputs_with_release_mult_and_storage_tuewk_updateror.csv') ->
  plant_data_weekly

plant_data_weekly %>%
  select(week_commencing) %>% unique() %>% mutate(year = year(week_commencing)) %>%
  group_by(year) %>%
  mutate(real_week = 1:n()) %>% ungroup() %>%
  mutate(GO_week = case_when(
    real_week %in% 1:52L ~ real_week,
    real_week == 53L ~ 52L)
    ) %>%
  mutate(GO_period = paste0(year, "-wk", GO_week)) %>%
  select(week_commencing, GO_period) ->
  real_week_to_GO_period_mapping


# numbers refer to number of nodes in GO
for(node in c(75, 100, 125, 150)){
  message(paste('node', node, 'starting!'))
  read_csv(paste0('data/input_data/go-west_lmps/duals_', node, '.csv')) %>%
    spread(Bus, Value) %>%
    mutate(year = 2000) -> LMPs
  
  LMPs %>% #select(year, Time) %>%
    mutate(GO_week = rep(rep(1:52, each = 168), 1),
           GO_period = paste0(year, "-wk", GO_week)) %>%
    left_join(real_week_to_GO_period_mapping) -> LMPs_with_week_commencing_mapped
  
  read_csv(paste0('data/input_data/go-west_lmps/EIA_bus_match', node, '.csv')) %>% arrange(EIA_ID) ->
    GO_bus_to_EIA_ID_mapping
  
  plant_data_weekly %>%
    select(EIA_ID) %>% unique() %>%
    left_join(GO_bus_to_EIA_ID_mapping) %>%
    filter(!is.na(plant)) %>%
    pull(EIA_ID) -> EIA_IDs
  
  EIA_IDs %>%
    map(
      function(x){
  
        GO_bus_to_EIA_ID_mapping %>% filter(EIA_ID == x) %>%
          .[["new bus"]] -> GO_bus
  
        LMPs_with_week_commencing_mapped %>%
          select(week_commencing, Time, year, price = one_of(GO_bus)) %>%
          filter(!is.na(week_commencing)) %>%
          group_by(week_commencing) %>% mutate(hour_of_week = 1:n()) %>%
          ungroup() %>% select(-Time) %>%

        write_csv(paste0('data/input_data/go-west_lmps/processed/EIA_', x, '_', node, '_LMPs.csv'))
      }
    )
}
