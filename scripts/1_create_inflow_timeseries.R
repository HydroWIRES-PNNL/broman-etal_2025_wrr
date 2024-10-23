# ----
# Name: 1_create_inflow_timeseries.R
# Author: D. Broman, PNNL
# Last Modified: 2022-02-24
# Description: Developes initial inflow timeseries from mosartwmpy data for
# HydroWIRES D3 project (used as perfect forecasts)
# ----

library(tidyverse)
library(ncdf4)
library(tools)

# mosartwmpy netCDF files available at: https://doi.org/10.5281/zenodo.13923721
dir_nc = 'data/control_Daymet_VIC4/'
altvar_tbl = read_csv('data/input_data/mosartwmpy_streamflow/altvar_tbl.csv')

id_tbl = read_csv('data/input_data/mosartwmpy_streamflow/GO_WEST_MOSART_tbl.csv')

id_tbl = id_tbl %>%
  left_join(dplyr::select(altvar_tbl, ID, Variable)) %>%
  mutate(Variable = ifelse(is.na(Variable), 'channel_inflow', Variable))
id_tbl_inflow = id_tbl %>%
  dplyr::filter(Variable == 'channel_inflow')
id_tbl_outflow = id_tbl %>%
  dplyr::filter(Variable == 'channel_outflow')

file_list = list.files(dir_nc)

inflow_dt = tibble()
for(i in 1:length(file_list)){
  file_sel = file_list[i]
  meta_temp = unlist(str_split(file_path_sans_ext(file_sel), '_'))
  date_start = as.Date(paste(meta_temp[4], meta_temp[5], '01', sep = '-'))
  nc_temp = nc_open(paste0(dir_nc, file_sel))
  lon_vec = ncvar_get(nc_temp, 'lon')
  lat_vec = ncvar_get(nc_temp, 'lat')
  time_vec = ncvar_get(nc_temp, 'time')

  nlon = length(lon_vec)
  nlat = length(lat_vec)
  ntime = length(time_vec)
  date_vec = seq(from = date_start, length = ntime, by = 'days')
  inflow_raw = ncvar_get(nc_temp, 'channel_inflow')
  inflow_dt_tmp = tibble(lon_1_8_ = lon_vec, lat_1_8_ = rep(lat_vec, each = nlon), date = rep(date_vec, each = nlon * nlat), var = as.numeric(inflow_raw))
  inflow_dt_tmp_fl = inflow_dt_tmp %>%
    left_join(id_tbl_inflow) %>%
    dplyr::filter(!is.na(ID))
  inflow_dt_sp = inflow_dt_tmp_fl %>%
    dplyr::select(date, ID, var) %>%
    spread(ID, var)
  outflow_raw = ncvar_get(nc_temp, 'channel_outflow')
  outflow_dt_tmp = tibble(lon_1_8_ = lon_vec, lat_1_8_ = rep(lat_vec, each = nlon), date = rep(date_vec, each = nlon * nlat), var = as.numeric(outflow_raw))
  outflow_dt_tmp_fl = outflow_dt_tmp %>%
    left_join(id_tbl_outflow) %>%
    dplyr::filter(!is.na(ID)) %>%
    mutate(var = abs(var)) # make outflow positive where it is used to represent inflow
  outflow_dt_sp = outflow_dt_tmp_fl %>%
    dplyr::select(date, ID, var) %>%
    spread(ID, var)

  inflow_comb_dt = left_join(inflow_dt_sp, outflow_dt_sp)
  inflow_dt = bind_rows(inflow_dt, inflow_comb_dt)

  nc_close(nc_temp)
}

inflow_dt = inflow_dt %>%
  arrange(date)

write_csv(inflow_dt, 'data/input_data/perfect_forecasts/forecast_perfect.csv', row.names = F, quote = F)
