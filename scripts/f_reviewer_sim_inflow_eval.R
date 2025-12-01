#===========================================================
# Name: evaluate_simulated_inflows.R
# Author: D. Broman, PNNL
# Last Modified: 2025-10-01
# Description: [D3] evaluate simulated inflows
#===========================================================

library(tidyverse)
library(arrow)
library(readxl)
library(hydroGOF)
library(aweek)

wd = '' # repo top directory
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

eia_bus_tbl = read_csv('data/input_data/go-west_topology/EIA_bus_match100.csv')

grand_eia = read_csv('/Users/brom374/Downloads/GRanD_EHA_crosswalk.csv')

grand_eia = grand_eia %>%
  dplyr::select(GRAND_ID, EIA_PID) %>%
  dplyr::rename(EIA_ID = EIA_PID)

# read in forecast data

# - format perfect forecast data; subset to keep only storage facilities; 
# - format synthetic forecast data
# - create and format persistence forecast data

#- perfect forecasts
inflow_sim = read_csv('data/input_data/perfect_forecasts/forecast_perfect.csv') %>%
  mutate(date = ymd(paste0(year, '-', mon, '-', day))) %>%
  dplyr::select(-year, -mon, -day) %>%
  gather(EIA_ID, simulated, -date) %>%
  mutate(EIA_ID = as.numeric(EIA_ID)) %>%
  dplyr::filter(EIA_ID %in% storage_list) %>%
  left_join(grand_eia)


resops_tbl = read_parquet('/Users/brom374/Library/CloudStorage/OneDrive-PNNL/Documents/Projects/git/mosartwmpy-lstm-reservoirmethod/data/resopsus.parquet')

grand_tbl_d3 = grand_eia %>%
  dplyr::filter(EIA_ID %in% storage_list)

resops_tbl_fl = resops_tbl %>%
  dplyr::filter(GRAND_ID %in% grand_tbl_d3$GRAND_ID)

# compute stats to identify locations where data are available

resops_tbl_stat = resops_tbl_fl %>%
  dplyr::filter(date >= '2000-01-01') %>%
  dplyr::select(GRAND_ID, date, inflow) %>%
  group_by(GRAND_ID) %>%
  dplyr::summarise(inflow_ct = sum(ifelse(!is.na(inflow), 1, 0)), ct = n()) %>%
  mutate(inflow_pct = inflow_ct / ct) %>%
  dplyr::filter(inflow_pct >= 0.5) # keep locations with at least 50% (10 years) of data

resops_in_sta = resops_tbl_stat %>% pull(GRAND_ID)


resops_tbl_comp = resops_tbl %>%
  dplyr::filter(GRAND_ID %in% resops_in_sta, 
                date >= '2000-01-01') %>%
dplyr::select(GRAND_ID, date, inflow) %>%
dplyr::rename(observed = inflow)


comp_tbl = resops_tbl_comp %>%
  left_join(inflow_sim) 

stat_yr_tbl = comp_tbl %>%
  dplyr::filter(!is.na(observed),
                !is.na(EIA_ID),
                !is.na(simulated)) %>%
  group_by(EIA_ID) %>%
  dplyr::summarise(KGE = KGE(simulated, observed),
                   PBIAS = pbias(simulated, observed))

stat_mon_tbl = comp_tbl %>%
  dplyr::filter(!is.na(observed),
                !is.na(EIA_ID),
                !is.na(simulated)) %>%
  mutate(month = month(date)) %>%
  group_by(EIA_ID, month) %>%
  dplyr::summarise(KGE = KGE(simulated, observed),
                   PBIAS = pbias(simulated, observed))

ggplot() +
  stat_ecdf(data = stat_yr_tbl, aes(y = KGE)) +
  theme_bw()

ggplot() +
  stat_ecdf(data = stat_yr_tbl, aes(y = PBIAS)) +
  theme_bw()


ggplot() +
  stat_ecdf(data = stat_mon_tbl, aes(y = PBIAS)) +
  facet_wrap(~month) +
  theme_bw()

ggplot(comp_tbl, aes(x = factor(EIA_ID), y = value, fill = type)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6) +
  labs(title = "Paired Boxplots by Type (X-axis: EIA_ID)",
       x = "EIA_ID",
       y = "Value",
       fill = "Type") +
  scale_fill_manual(values = c("simulated" = "blue", "another_type" = "red")) + # Adjust colors by `type`
  theme_minimal()
