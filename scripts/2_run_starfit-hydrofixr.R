# ----
# Name: run_starfit-hydrofixr.R
# Author: S. Turner, PNNL
# Modified: D. Broman, PNNL
# Last Modified: 2023-08-21
# Description: [D3] gets weekly storage targets from starfit and 
#   uses hydrofixr methods to convert to weekly power targets (pmin, pmax, pavg)
#   for use in GO-WEST
# ----

# - code to install starfit and hydrofixr dependencies
# devtools::install_github("IMMM-SFA/starfit")
# devtools::install_github("pnnl/hydrofixr")

# ----

library(tidyverse)
library(lubridate)
library(starfit)
library(hydrofixr)
library(aweek)

all_inflows_raw = read_csv('data/input_data/perfect_forecasts/forecast_perfect.csv')

# create one year of spin-up (fake 1999 by repeating 2000)
all_inflows_inityear = all_inflows_raw %>%
  dplyr::filter(year == 2000) %>%
  mutate(year = 1999) %>%
  dplyr::filter(!(mon == 2 & day == 29)) # remove leap day in 2000

# merge spin-up
all_inflows = bind_rows(all_inflows_inityear, all_inflows_raw) %>%
  mutate(date = ymd(paste0(year, '-', mon, '-', day))) %>%
  dplyr::select(-year, -mon, -day)
  
names(all_inflows) %>% .[-(length(.))] %>% as.integer() -> all_plants

read_csv('data/input_data/hydrosource/HydroSource_HYC.csv') %>%
  select(PLANT = plant, EIA_ID, eha_ptid = EHA_ID, CH_MW) %>%
  filter(EIA_ID %in% all_plants) -> EHA

read_csv('data/input_data/hydrosource/HILARRI_v1_1_Public.csv') %>%
  select(GRAND_ID = grand_id, pt_name, eha_ptid, nididfull, nidid) %>%
  filter(!is.na(eha_ptid)) %>%
  left_join(EHA) %>% unique() -> GRAND_TO_EIA

hydrofixr::read_EIA_capabilities('data/input_data/hydrofixr/') %>%
  select(EIA_ID, nameplate) %>% unique() %>%
  filter(EIA_ID %in% all_plants) -> nameplate

read_csv('data/input_data/mosartwmpy_streamflow/GO_WEST_MOSART_tbl.csv') %>%
  left_join(nameplate, by = c('ID' = 'EIA_ID')) %>%
  left_join(GRAND_TO_EIA, by = c('ID' = 'EIA_ID')) %>%
  select(EIA_ID = ID, plant, state, bal_auth, GRAND_ID, nididfull, nidid, nameplate) %>%
  filter(EIA_ID %in% all_plants) ->
  plants_with_GRAND

# starfit simulation
plants_with_GRAND %>%
  filter(!is.na(GRAND_ID)) ->
  sim_with_starfit

# other mode of simulation
plants_with_GRAND %>%
  filter(is.na(GRAND_ID)) ->
  sim_with_other_mode

hydrofixr:::read_EIA_netgen('data/input_data/hydrofixr/') %>%
  filter(EIA_ID %in% all_plants) %>%
  group_by(EIA_ID, year) %>%
  summarise(netgen_MWh = sum(netgen_MWh), .groups = 'drop') ->
  annual_netgen_all_plants

read_csv('data/input_data/istarf/ISTARF-CONUS.csv') ->
  ISTARF_DATA

# ----
# simulations

sim_with_starfit %>% select(EIA_ID, GRAND_ID, nameplate, plant) %>%
  pmap(function(EIA_ID, GRAND_ID, nameplate, plant){

    all_inflows %>% select(date, inflow_cumecs = one_of(as.character(EIA_ID))) ->
      daily_inflow

    daily_inflow %>%
      mutate(epiweek = as.numeric(substr(date2week(date, week_start = 'Tuesday'), 7,8)), # set week definition (start day)
             year = year(date),
             month = month(date),
             dow = weekdays(date)) %>%
      mutate(year = ifelse(epiweek > 50 & month == 1, year - 1, year)) %>%
      mutate(year = ifelse(epiweek == 1 & month == 12, year + 1, year)) %>%
      group_by(epiweek, year) %>%
      mutate(week_commencing = min(date)) %>%
      mutate(week_ending = max(date) + days(1),
             nday = n()) %>%
      filter(nday == 7) %>% # only keep complete weeks
      filter(year(week_commencing) >1998,
             year(week_ending) < 2020) %>%
      group_by(week_commencing, epiweek) %>%
      summarise(inflow_cumecs = mean(inflow_cumecs), .groups = 'drop') %>%
      mutate(inflow_MCM = inflow_cumecs * 60 * 60 * 24 * 7 * 1e-6) ->
      weekly_inflow


    ISTARF_DATA %>% filter(GRanD_ID == !!GRAND_ID) ->
      params

    # Compute Normal Operating Range ('NOR') using `starfit` ...
    # ... convert_parameters_to_targets() function.
    # NOTE: parameters argument requires order: mu, alpha, beta, max, min

    # upper bound of NOR
    convert_parameters_to_targets(
      parameters = c(params[['NORhi_mu']],
                     params[['NORhi_alpha']],
                     params[['NORhi_beta']],
                     params[['NORhi_max']],
                     params[['NORhi_min']]),
      # give the harmonic a name...
      target_name = 'NORhi'
    ) -> NOR_upper_bound

    # lower bound of NOR
    convert_parameters_to_targets(
      parameters = c(params[['NORlo_mu']],
                     params[['NORlo_alpha']],
                     params[['NORlo_beta']],
                     params[['NORlo_max']],
                     params[['NORlo_min']]),

      # give the harmonic a name...
      target_name = 'NORlo'
    ) -> NOR_lower_bound

    NOR_upper_bound %>%
      left_join(NOR_lower_bound,
                by = 'epiweek') ->
      NOR

    NOR %>%
      mutate(capacity_MCM = params[['GRanD_CAP_MCM']],
             NORhi_actual = capacity_MCM * NORhi / 100,
             NORlo_actual = capacity_MCM * NORlo / 100) ->
      NOR_MCM

    inflow_mean <- weekly_inflow[['inflow_MCM']] %>%
      mean()

    convert_parameters_to_release_harmonic(c(
      params$Release_alpha1,
      params$Release_beta1,
      params$Release_alpha2,
      params$Release_beta2)
    ) -> release_harmonic

    i <- weekly_inflow[['inflow_MCM']]
    s_cap <- params[['GRanD_CAP_MCM']]
    r_max <- (inflow_mean * (1 + params$Release_max))
    r_min <- (inflow_mean * (1 + params$Release_min))
    a <- rep(NA, length(i))
    r <- a
    spill <- rep(0, length(i))
    s <- c(((NOR_MCM$NORhi_actual + NOR_MCM$NORlo_actual) / 2)[1], a)

    weekly_inflow %>%
      mutate(epiweek = if_else(epiweek == 53, 52, epiweek)) %>%
      left_join(NOR_MCM, by = 'epiweek') %>%
      left_join(release_harmonic, by = 'epiweek') -> sim_data

    cons_curve <- sim_data$NORlo
    flood_curve <- sim_data$NORhi
    standarized_release_signal <- sim_data$release_harmonic
    rr <- c(
      params$Release_p1,
      params$Release_p2,
      params$Release_c
    )


    for (t in 1:length(i)){

      # week-ahead persistence forecast
      i_fcast <- i[t]
      (i_fcast / inflow_mean) - 1 -> i_st


      # online simulation
      a[t] <- (100 * (s[t] / s_cap) - cons_curve[t]) / (flood_curve[t] - cons_curve[t])
      if(a[t] < 0){
        r[t] <- (max((i_fcast - ((s_cap * (cons_curve[t] / 100) - s[t]))), r_min) + r[t-1]) / 2
      }else{
        if(a[t] > 1){
          r[t] <- ((s[t] - (s_cap * (flood_curve[t] / 100)) + i_fcast) + r[t-1]) / 2
        }else{

          r[t] <- (inflow_mean *
                     (1 + (
                       standarized_release_signal[t] + rr[1] + (a[t] * rr[2]) + (i_st * rr[3])
                     )))
        }
      }

      if(r[t] < 0) r[t] <- r_min
      if(r[t] > r_max) r[t] <- r_max
      if(r[t] > i[t] + s[t]) r[t] <- i[t] + s[t]

      if((s[t] - r[t] + i[t]) > s_cap){
        s[t + 1] <- s_cap
        spill[t] <- s[t] + i[t] - s_cap - r[t]

      }else{
        s[t + 1] <- s[t] + i[t] - r[t]
      }
    }

     tibble(
       EIA_ID = EIA_ID,
       plant = plant,
       week_commencing = weekly_inflow$week_commencing,
       inflow = i,
       release = r,
       spill = spill,
       storage = s[1:length(i)],
       upper = s_cap * flood_curve / 100,
       lower = s_cap * cons_curve / 100
      ) -> operations
  
     return(operations)

  }) -> grand_sims

# for each EIA ID determine the conversion that leads to reasonable annual output!

optimize_energy_factor <-
  function(sim_table){

    EIA_ID <- sim_table[['EIA_ID']] %>% unique()

    nameplate %>% filter(EIA_ID == !!EIA_ID) %>%
      .[['nameplate']] -> plant_nameplate

    annual_netgen_all_plants %>%
      filter(EIA_ID == !!EIA_ID) %>% select(-EIA_ID) -> netgen

    sim_table %>% select(week_commencing, release) %>%
      mutate(year = year(week_commencing)) %>%
      group_by(year) %>%
      summarise(release = sum(release)) %>%
      left_join(netgen, by = 'year') %>%
      filter(!is.na(netgen_MWh)) -> cols_to_optimize

    rel <- cols_to_optimize[['release']]
    gen <- cols_to_optimize[['netgen_MWh']]

    lm(data = cols_to_optimize,
       netgen_MWh ~ release + 0) -> model_
    model_ %>% coefficients() %>% as.numeric() -> multiplier

    if (!(any((sim_table[['release']] * multiplier / 168) > plant_nameplate))){
      return(
        sim_table %>% mutate(
          MWh_per_MCM_release = multiplier,
          MWh = release * MWh_per_MCM_release,
          MW = MWh / 168,
          nameplate = plant_nameplate)
      )
    }

    get_multiplier <- function(multiplier_x){

      sim_table %>% select(week_commencing, release) %>%
        mutate(year = year(week_commencing)) %>%
        mutate(comp_gen = release * multiplier_x) %>%
        mutate(comp_gen = if_else(comp_gen > plant_nameplate * 168,
                             plant_nameplate * 168, comp_gen)) %>%
        group_by(year) %>%
        summarise(comp_gen = sum(comp_gen)) %>%
        left_join(netgen, by = 'year') %>%
        filter(!is.na(netgen_MWh)) -> cols_to_optimize

      hydroGOF::rmse(cols_to_optimize[['comp_gen']], gen)

    }

    nloptr::nloptr(eval_f = get_multiplier,
                   x0 = 0, lb = 0, ub = Inf,
                   opts = list('algorithm' = 'NLOPT_LN_COBYLA',
                               'xtol_rel'=1.0e-4)) -> opt

    opt$solution -> multiplier_

    return(
      sim_table %>% mutate(
        MWh_per_MCM_release = multiplier_,
        MWh = release * MWh_per_MCM_release,
        MWh = if_else(MWh > plant_nameplate * 168, plant_nameplate * 168, MWh),
        MW = MWh / 168,
        nameplate = plant_nameplate)
    )
  }


grand_sims %>% map_dfr(optimize_energy_factor) ->
  weekly_energy_STARFIT_cases

weekly_energy_STARFIT_cases = weekly_energy_STARFIT_cases %>%
  mutate(p_avg = MW)

## non-STARFIT cases.
## optimize both spill limit (between 0.75 and 1.0 of flows) AND release to energy factor.

EIA_IDs_non_STARFIT <- all_plants[which(!all_plants %in% unique(weekly_energy_STARFIT_cases$EIA_ID))]

EIA_IDs_non_STARFIT %>% 
map_dfr(function(EIA_ID){

  message(EIA_ID)

  all_inflows %>% select(date, inflow_cumecs = one_of(as.character(EIA_ID))) ->
    daily_inflow

  nameplate %>% filter(EIA_ID == !!EIA_ID) %>%
    .[['nameplate']] -> plant_nameplate


  get_multiplier_and_spill_factor <- function(multiplier_and_spill_quantile){

    daily_inflow %>%
      mutate(spill_cumecs = inflow_cumecs - quantile(inflow_cumecs, multiplier_and_spill_quantile[2]),
             spill_cumecs = if_else(spill_cumecs < 0, 0, spill_cumecs),
             release_cumecs = inflow_cumecs - spill_cumecs) %>%
      mutate(epiweek = as.numeric(substr(date2week(date, week_start = 'Tuesday'), 7,8)), # set week definition (start day)
             year = year(date),
             month = month(date),
             dow = weekdays(date)) %>%
      mutate(year = ifelse(epiweek > 50 & month == 1, year - 1, year)) %>%
      mutate(year = ifelse(epiweek == 1 & month == 12, year + 1, year)) %>%
      group_by(epiweek, year) %>%
      mutate(week_commencing = min(date)) %>%
      mutate(week_ending = max(date) + days(1),
             nday = n()) %>%
      filter(nday == 7) %>% # only keep complete weeks
      filter(year(week_commencing) >1998,
             year(week_ending) < 2020) %>%
      mutate(inflow_MCM = inflow_cumecs * 60 * 60 * 24 * 1e-6,
             release_MCM = release_cumecs * 60 * 60 * 24 * 1e-6,
             spill_MCM = spill_cumecs * 60 * 60 * 24 * 1e-6) %>%
      group_by(week_commencing, epiweek) %>%
      summarise(inflow = sum(inflow_MCM),
                release = sum(release_MCM),
                spill = sum(spill_MCM),
                .groups = 'drop') -> weekly_inflow_and_release

    annual_netgen_all_plants %>%
      filter(EIA_ID == !!EIA_ID) %>% select(-EIA_ID) -> netgen

    weekly_inflow_and_release %>%
      mutate(year = year(week_commencing),
             gen = multiplier_and_spill_quantile[1] * release) %>%
      mutate(gen = if_else(gen > (plant_nameplate * 168), plant_nameplate * 168, gen)) %>%
      group_by(year) %>% summarise(gen = sum(gen)) %>%
      left_join(netgen, by = 'year') %>%
      filter(!is.na(netgen_MWh)) -> cols_to_optimize

    computed_gen <- cols_to_optimize[['gen']]
    gen <- cols_to_optimize[['netgen_MWh']]

    hydroGOF::rmse(computed_gen, gen)


  }

  nloptr::nloptr(x0 = c(0, 0.8),
                 eval_f = get_multiplier_and_spill_factor,
                 lb = c(0, 0.8),
                 ub = c(Inf, 1),
                 opts = list('algorithm' = 'NLOPT_LN_BOBYQA',
                             'xtol_rel'=1.0e-8)) -> opt_spill_and_mult


  opt_spill_and_mult$solution[1] -> multiplier
  opt_spill_and_mult$solution[2] -> spill_quantile

  return(

    daily_inflow %>%
      mutate(spill_cumecs = inflow_cumecs - quantile(inflow_cumecs, spill_quantile),
             spill_cumecs = if_else(spill_cumecs < 0, 0, spill_cumecs),
             release_cumecs = inflow_cumecs - spill_cumecs) %>%
      
      mutate(epiweek = as.numeric(substr(date2week(date, week_start = 'Tuesday'), 7,8)), # set week definition (start day)
             year = year(date),
             month = month(date),
             dow = weekdays(date)) %>%
      mutate(year = ifelse(epiweek > 50 & month == 1, year - 1, year)) %>%
      mutate(year = ifelse(epiweek == 1 & month == 12, year + 1, year)) %>%
      group_by(epiweek, year) %>%
      mutate(week_commencing = min(date)) %>%
      mutate(week_ending = max(date) + days(1),
             nday = n()) %>%
      filter(nday == 7) %>% # only keep complete weeks
      filter(year(week_commencing) >1998,
             year(week_ending) < 2020) %>%
      mutate(inflow_MCM = inflow_cumecs * 60 * 60 * 24 * 1e-6,
             release_MCM = release_cumecs * 60 * 60 * 24 * 1e-6,
             spill_MCM = spill_cumecs * 60 * 60 * 24 * 1e-6) %>%
      group_by(week_commencing) %>%
      summarise(inflow = sum(inflow_MCM),
                release = sum(release_MCM),
                spill = sum(spill_MCM),
                .groups = 'drop') %>%
      mutate(MWh = release * multiplier,
             MWh = if_else(MWh > plant_nameplate * 168, plant_nameplate * 168, MWh),
             MW = MWh / 168,
             nameplate = plant_nameplate,
             EIA_ID = !!EIA_ID) %>%
      mutate(MWh_per_MCM_release = multiplier,
             spill_quantile = !!spill_quantile) %>%
      select(EIA_ID, week_commencing, inflow, release, spill, MWh_per_MCM_release, spill_quantile,MWh, p_avg = MW, nameplate)

    )
    }
  ) -> weekly_energy_RoR_cases

# merge STARFIT and non-STARFIT tables
bind_rows(
  weekly_energy_STARFIT_cases %>%
    mutate(type = 'STARFIT') %>% select(-plant),
  weekly_energy_RoR_cases %>% mutate(type = 'RoR/small storage')
) ->
  all_targets


# write out data
all_targets = all_targets %>%
  mutate(p_max = if_else(
    type == 'STARFIT',
    p_avg + 0.631 * (nameplate - p_avg),
    p_avg + 0.474 * (nameplate - p_avg)
  )) %>%
  mutate(p_min = 0.39 * p_avg) %>%
  mutate(p_avg = if_else(p_avg > nameplate, nameplate, p_avg)) %>%
  mutate(p_max = if_else(p_max > nameplate, nameplate, p_max)) %>%
  mutate(EIA_ID = as.integer(EIA_ID)) %>%
  mutate_if(is.double, function(x) round(x, 4)) 

write_csv(all_targets, 'data/input_data/starfit_reservoirtargets/2000_2019_weekly_GOWEST_hydro_inputs_with_release_mult_and_storage_tuewk_updateror.csv')

# extract out water tp energy conversion factor and write out table
ewf_tbl = all_targets %>%
  dplyr::select(EIA_ID, MWh_per_MCM_release) %>%
  distinct()

write_csv(ewf_tbl, 'data/input_data/starfit_reservoirtargets/plant_energy-water-fact_tbl.csv')

# data with storage targets for ror - 
all_targets %>%
  mutate(year = year(week_commencing)) %>%
  filter(type == 'RoR/small storage', year >= 2000) ->
  bcs_ror

read_csv('data/input_data/hydrosource/HESC_v1.csv') %>%
  select(nidid = DamID, MaxVol, MinVol, MaxVolSrc) -> HESC_vols

read_csv('data/input_data/hydrosource/HydroSource_HYC.csv') %>%
  select(PLANT = plant, EIA_ID, eha_ptid = EHA_ID, CH_MW) %>%
  filter(EIA_ID %in% (bcs_ror[['EIA_ID']] %>% unique())) %>%
  left_join(read_csv('data/input_data/hydrosource/HILARRI_v1_1_Public.csv') %>%
              select(eha_ptid, nidid)) %>%
  distinct() %>%
  left_join(HESC_vols) -> ror_plant_data

ror_maxvol = ror_plant_data %>%
  dplyr::select(EIA_ID, MaxVol)

# compute storage targets for ROR - assume full storage at start of simulation
boundary_conditions_ror = all_targets %>%
  dplyr::filter(type == 'RoR/small storage') %>%
  left_join(ror_maxvol) %>%
  dplyr::filter(!is.na(MaxVol))

eia_id_list = unique(boundary_conditions_ror$EIA_ID)
week_list = sort(unique(boundary_conditions_ror$week_commencing))
boundary_conditions_ror_s = tibble()
for(f in 1:length(eia_id_list)){
  eia_id_sel = eia_id_list[f]
  boundary_conditions_ror_fl = boundary_conditions_ror %>%
    dplyr::filter(EIA_ID == eia_id_sel)
  
  for(w in 1:length(week_list)){
    week_sel = week_list[w]
    
    if(week_sel == week_list[1]){
      storage_p = boundary_conditions_ror_fl %>%
        filter(week_commencing == week_sel) %>% pull(MaxVol) * 0.75
    } else {
      storage_p = boundary_conditions_ror_fl %>%
        filter(week_commencing == week_sel) %>% pull(storage)
    }
    storage_max = pull(dplyr::filter(boundary_conditions_ror_fl, week_commencing == week_sel), MaxVol)
    storage_delta = pull(dplyr::filter(boundary_conditions_ror_fl, week_commencing == week_sel), MWh) /
      pull(dplyr::filter(boundary_conditions_ror_fl, week_commencing == week_sel), MWh_per_MCM_release)
    storage_t = storage_p - storage_delta + pull(dplyr::filter(boundary_conditions_ror_fl, week_commencing == week_sel), inflow)
    boundary_conditions_ror_fl$storage[w] = min(storage_t, storage_max)
  }
  boundary_conditions_ror_s = bind_rows(boundary_conditions_ror_fl, boundary_conditions_ror_s)
}

write_csv(boundary_conditions_ror_s, 'data/input_data/starfit_reservoirtargets/2000_2019_weekly_GOWEST_hydro_inputs_rorstoragetarget.csv')
