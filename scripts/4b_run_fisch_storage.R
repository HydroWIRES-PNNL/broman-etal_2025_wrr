# ----
# Name: run_fisch_storage.R
# Author: S. Turner, PNNL
# Modified: D. Broman, PNNL
# Last Modified: 2022-11-07
# Description: [D3] runs FIScH for storage facilities
# mode set to 'day_ahead'; includes synthetic forecasts
#   script called from bash script to iterate over facilities
# ----

args = commandArgs(trailingOnly=TRUE)
case <- as.numeric(args[1])

start_time = Sys.time()

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(readxl)
library(future)
library(furrr)
library(aweek)
plan(cluster)

fcst_day_tbl = tibble(fcst_day_abb = c('Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun', 'Mon'),
                          validtime_ind = c(0:6))

year_start = 2000

read_csv('data/input_data/starfit_reservoirtargets/2000_2019_weekly_GOWEST_hydro_inputs_with_release_mult_and_storage_tuewk_updateror.csv') ->
  boundary_conditions

boundary_conditions %>%
  mutate(year = year(week_commencing)) %>%
  filter(type != 'RoR/small storage', year >= year_start) ->
  bcs_storage

read_csv('data/input_data/hydrosource/HydroSource_HYC.csv') %>%
  select(PLANT = plant, EIA_ID, eha_ptid = EHA_ID, CH_MW) %>%
 filter(EIA_ID %in% (bcs_storage[['EIA_ID']] %>% unique())) %>%
  left_join(read_csv('data/input_data/hydrosource/HILARRI_v1_1_Public.csv') %>%
              select(eha_ptid, nidid, GRanD_ID = grand_id)) %>%
  left_join(
    read_csv('data/input_data/istarf/ISTARF-CONUS.csv') %>%
      select(GRanD_ID, GRanD_CAP_MCM)
  ) %>% unique() -> storage_plant_data

# forecasts
read_csv('data/input_data/perfect_forecasts/forecast_perfect.csv') %>%
  mutate(date = ymd(paste0(year, '-', mon, '-', day))) %>%
  mutate(epiweek = as.numeric(substr(date2week(date, week_start = 'Tuesday'), 7,8)), # set week definition (start day)
         year = year(date),
         month = month(date)) %>%
  mutate(year = ifelse(epiweek > 50 & month == 1, year - 1, year)) %>%
  mutate(year = ifelse(epiweek == 1 & month == 12, year + 1, year)) %>%
  group_by(epiweek, year) %>%
  mutate(week_commencing = min(date)) %>%
  mutate(week_ending = max(date) + days(1),
         nday = n()) %>%
  filter(nday == 7) ->
  flow_forecasts_daily_perfect

## SNAP TO DISC ##
snap_to_disc <- function(x, disc){
  discs <- seq(0, x + disc, disc)
  discs[which.min(abs(discs - x))]
}

## PULL PERSISTENCE DAY AHEAD ##
pull_persistence_forecasts <- function(x, fday){
  x %>%
    filter(week_commencing == week[['week_commencing']], date == week[['week_commencing']] + fday) %>%
    mutate(inflow_MCM_per_hr = inflow_cumecs * 60 * 60 * 1e-6) %>%
    pull(inflow_MCM_per_hr) %>% .[1] %>%
    sapply(snap_to_disc, disc = disc_step) %>%
    rep(., 168)
}

# check data against available LMPs
lmp_file_list = list.files('data/input_data/go-west_lmps/processed/')

lmp_tbl = tibble(file = lmp_file_list) %>%
  separate(file, sep = '_', into = c('V1', 'EIA_ID', 'V3'))

bcs_storage = bcs_storage %>%
  dplyr::filter(EIA_ID %in% lmp_tbl$EIA_ID) %>%
  distinct()

bcs_storage %>% split(.$EIA_ID) %>% .[[case]] -> bc

bc[['EIA_ID']] %>% unique() -> EIA_ID

message(EIA_ID)
message(start_time)

# start of run loop ------------------------------------------------------------
if(paste0('dayahead_storage_', EIA_ID, '.csv') %in% list.files('data/output_data/fisch/')){

  message('done already')

}else{

  storage_plant_data %>%
    filter(EIA_ID == !!EIA_ID) ->
    plant_specs

  # read in LMPs
  read_csv(paste0('data/input_data/go-west_lmps/processed/EIA_', EIA_ID, '_LMPs.csv')) ->
  LMPs

  # read in synthetic plant-specific forecasts
  read_excel(path = paste0('data/input_data/synthetic_forecasts/forecast_synthetic_', EIA_ID, '.xls')) ->
    fcst_synthetic_raw
  
  fcst_synthetic_proc = fcst_synthetic_raw %>%
    mutate(initialization_time = as.Date(paste(Year, Month, Day, sep = '-'))) %>%
    dplyr::select(initialization_time, Tue, Wed, Thu, Fri, Sat, Sun, Mon) %>%
    gather(fcst_day_abb, inflow, -initialization_time) %>%
    mutate(epiweek = as.numeric(substr(date2week(initialization_time, week_start = 'Tuesday'), 7,8)), # set week definition (start day)
           year = year(initialization_time),
           month = month(initialization_time)) %>%
    mutate(year = ifelse(epiweek > 50 & month == 1, year - 1, year)) %>%
    mutate(year = ifelse(epiweek == 1 & month == 12, year + 1, year)) %>%
    group_by(epiweek, year) %>%
    mutate(week_commencing = min(initialization_time)) %>%
    ungroup() %>%
    left_join(fcst_day_tbl) %>%
    mutate(inittime_ind = as.numeric(difftime(initialization_time, week_commencing, units = 'days'))) %>%
    mutate(lead_time_day = validtime_ind - inittime_ind) %>%
    dplyr::filter(lead_time_day >= 0) %>%
    mutate(valid_time_day = initialization_time + days(lead_time_day)) %>%
    dplyr::select(week_commencing, initialization_time, valid_time_day, inflow)
  
  ntimes = length(unique(fcst_synthetic_proc$initialization_time))
  fcst_synthetic_disag_tp = tibble(initialization_time = rep(sort(unique(fcst_synthetic_proc$initialization_time)), each = 168),
                                       lead_time = rep(0:167, ntimes)) %>%
    mutate(valid_time = initialization_time + hours(lead_time)) %>%
    mutate(valid_time_day = as.Date(valid_time))
  
  fcst_synthetic_disag = fcst_synthetic_disag_tp %>%
    left_join(fcst_synthetic_proc) %>%
    ungroup() %>%
    dplyr::select(week_commencing, initialization_time, lead_time, valid_time, inflow) %>%
    dplyr::filter(!is.na(inflow))
  
  possible_disc <- c(0.0001, 0.0002, 0.0005,
                   0.001, 0.002, 0.005,
                   0.01, 0.02, 0.05,
                   0.1, 0.2, 0.5,
                   1, 2, 5,
                   10, 20, 50)

  # get flow forecasts for plant
  flow_forecasts_daily_perfect %>%
    ungroup() %>%
    select(date, week_commencing, inflow_cumecs = one_of(as.character(EIA_ID))) ->
    flow_forecasts_perfect

  bc %>%
    mutate(max_r = p_max / MWh_per_MCM_release,
           min_r = p_min / MWh_per_MCM_release,
           range_r = max_r - min_r) %>%
    pull(range_r) %>% min() / 20 -> qdisc

  possible_disc[which.min(abs(qdisc - possible_disc))] -> disc_step

  disc_grid_storage <- seq(0,
    snap_to_disc(plant_specs[['GRanD_CAP_MCM']], disc_step), disc_step)


  res_cap <- last(disc_grid_storage)

  initial_storage <- snap_to_disc(bc[['storage']][1], disc_step)

tibble(
  time = 0,
  storage_sim = initial_storage, # this is initial storage for RoR
  release_spill = NA_real_,
  release_turbine = NA_real_, 
  benefit_revenue = NA_real_,
  price_price = NA_real_, 
  inflow_actual = NA_real_,
  target_storage =  NA_real_,
  inflow_forecast_day1 = NA_real_,
  inflow_forecast_day2 = NA_real_,
  inflow_forecast_day3 = NA_real_,
  inflow_forecast_day4 = NA_real_,
  inflow_forecast_day5 = NA_real_,
  inflow_forecast_day6 = NA_real_,
  inflow_forecast_day7 = NA_real_,
  week_commencing = NA_Date_
) ->
  simulation_collect_perfect_dayahead ->
  simulation_collect_persistence_dayahead ->
  simulation_collect_synthetic_dayahead

for(wk in 1:(nrow(bc) - 1)){
  message(bc[wk,] %>% .[['week_commencing']])

  week <- bc[wk,] %>% mutate(max_release = snap_to_disc(p_max / MWh_per_MCM_release, disc_step),
                             min_release = snap_to_disc(p_min / MWh_per_MCM_release, disc_step)) %>%
    mutate(max_release = ifelse(max_release == 0, disc_step, max_release)) # if max_release is 0, it breaks optimization; set it to smallest disc_step
  
  LMPs %>% filter(week_commencing %in% week[['week_commencing']]) %>%
    arrange(hour_of_week) %>% pull(price) %>% round(2) -> wk_prices

  flow_forecasts_perfect %>%
    filter(week_commencing == week[['week_commencing']]) %>%
    mutate(inflow_MCM_per_hr = inflow_cumecs * 60 * 60 * 1e-6) %>%
    arrange(date) %>% pull(inflow_MCM_per_hr) %>% rep(each = 24) %>%
    sapply(snap_to_disc, disc = disc_step) -> wk_inflow

  wk_inflow_forecast_perfect <-
    rbind(
      wk_inflow, wk_inflow, wk_inflow, wk_inflow, wk_inflow, wk_inflow, wk_inflow
    )
  
  wk_inflow_forecast_persistence <-
    rbind(
      flow_forecasts_perfect %>% pull_persistence_forecasts(0),
      flow_forecasts_perfect %>% pull_persistence_forecasts(1),
      flow_forecasts_perfect %>% pull_persistence_forecasts(2),
      flow_forecasts_perfect %>% pull_persistence_forecasts(3),
      flow_forecasts_perfect %>% pull_persistence_forecasts(4),
      flow_forecasts_perfect %>% pull_persistence_forecasts(5),
      flow_forecasts_perfect %>% pull_persistence_forecasts(6)
    ) 
  
  wk_inflow_forecast_synthetic = fcst_synthetic_disag %>%
    filter(week_commencing == week[['week_commencing']]) %>%
    mutate(inflow_MCM_per_hr = inflow * 60 * 60 * 1e-6) %>%
    group_by(initialization_time, lead_time) %>%
    mutate(inflow_snap = seq(0, inflow_MCM_per_hr + disc_step, disc_step)[which.min(abs(seq(0, inflow_MCM_per_hr + disc_step, disc_step) - inflow_MCM_per_hr))]) %>%
    ungroup() %>%
    dplyr::select(initialization_time, valid_time, inflow_snap) %>%
    spread(valid_time, inflow_snap) %>%
    dplyr::select(-initialization_time) %>%
    as.matrix()
  
  colnames(wk_inflow_forecast_synthetic) = NULL
  
  bc[wk + 1,] %>% .[['storage']] %>%
    snap_to_disc(disc_step)-> target_storage

  c('perfect', 'persistence', 'synthetic') %>%
    future_map(function(x){

      if(x == 'perfect'){
        s1 <- c(simulation_collect_perfect_dayahead %>% pull(storage_sim) %>% last())

        forecast_ <- wk_inflow_forecast_perfect
      }

      if(x == 'persistence'){
        s1 <- c(simulation_collect_persistence_dayahead %>% pull(storage_sim) %>% last())

        forecast_ <- wk_inflow_forecast_persistence
      }
      
      if(x == 'synthetic'){
        s1 = c(simulation_collect_synthetic_dayahead %>% pull(storage_sim) %>% last())
        
        forecast_ <- wk_inflow_forecast_synthetic
      }

      # dp model call ----------------------------------
      schedule_release(mode = 'day_ahead',
                       inflow = wk_inflow,
                       inflow_forecast = forecast_,
                       price_forecast = wk_prices,
                       MWh_per_MCM = week[['MWh_per_MCM_release']],
                       initial_storage = s1,
                       reservoir_capacity = res_cap,
                       target_end_of_week_storage = target_storage,
                       max_release = week[['max_release']],
                       min_release = week[['min_release']],
                       discretization_step = disc_step) -> x_out

      return(
        x_out %>%
          mutate(week_commencing = bc[wk,] %>% .[['week_commencing']],
                 target_storage = target_storage, 
                 inflow_forecast_day1 = c(forecast_[1,], NA),
                 inflow_forecast_day2 = c(forecast_[2,], NA),
                 inflow_forecast_day3 = c(forecast_[3,], NA),
                 inflow_forecast_day4 = c(forecast_[4,], NA),
                 inflow_forecast_day5 = c(forecast_[5,], NA),
                 inflow_forecast_day6 = c(forecast_[6,], NA),
                 inflow_forecast_day7 = c(forecast_[7,], NA),
                 forecast = x)
        )

    }) -> both_out

  both_out[[1]] -> perfect
  both_out[[2]] -> persistence
  both_out[[3]] -> synthetic

  simulation_collect_perfect_dayahead <- bind_rows(
    simulation_collect_perfect_dayahead,
    perfect
  )

  simulation_collect_persistence_dayahead <- bind_rows(
    simulation_collect_persistence_dayahead,
    persistence
  )
  
  simulation_collect_synthetic_dayahead <- bind_rows(
    simulation_collect_synthetic_dayahead,
    synthetic
  )

}

bind_rows(
  simulation_collect_perfect_dayahead,
  simulation_collect_persistence_dayahead,
  simulation_collect_synthetic_dayahead
) %>%
  mutate_if(is.double, function(x) round(x, 5)) %>%
  write_csv(paste0('data/output_data/fisch/dayahead_storage_', EIA_ID, '.csv'))

}

end_time = Sys.time()
message(end_time)
message('run complete in:')
message(end_time - start_time)
