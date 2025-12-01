#===========================================================
# Name: fX_plot_schedule_evolution.R
# Author: D. Broman, PNNL
# Last Modified: 2025-10-01
# Description: [D3] compute inflow forecast skill metrics
#===========================================================

library(tidyverse)
library(arrow)
library(patchwork)
extrafont::loadfonts()

options(scipen=999)

wd = ''
setwd(wd)

# read in example dayahead-full output for one location
dp_tbl = read_csv('data/output_data/fisch/dayahead-full_storage_840.csv')

# format data
dp_tbl = dp_tbl %>%
  mutate(datetime = week_commencing + hours(time)) %>%
  dplyr::filter(!is.na(datetime))

# read in final schedule data - run the same as above, but not saving intermediate schedules throughout the week
dp_act_tbl = read_csv('data/output_data/fisch/dayahead_storage_840.csv')

# format data
dp_act_tbl = dp_act_tbl %>%
  mutate(datetime = week_commencing + hours(time)) %>%
  dplyr::filter(!is.na(datetime))

# look for case study weeks to present
dp_rev_wk = dp_act_tbl %>%
  dplyr::filter(!is.na(benefit_revenue)) %>%
  group_by(week_commencing, forecast) %>%
  dplyr::summarise(benefit_revenue = sum(benefit_revenue, na.rm = T)) %>%
  ungroup() %>%
  spread(forecast, benefit_revenue) %>%
  mutate(pct_diff_synth = (synthetic - perfect) / perfect * 100,
         pct_diff_pers= (persistence - perfect) / perfect * 100)

dp_rev_wk %>%
  filter(month(week_commencing) == 4) %>% 
  arrange(pct_diff_pers)

week_sel = '2019-04-02'

# create storage target table
eow_storage = dp_tbl %>%
  filter(time == 168) %>%
  dplyr::select(datetime, target_storage, week_commencing) %>%
  distinct() %>%
  mutate(variable_name = 'Storage')

# create starting storage table
sow_storage = dp_tbl %>%
  filter(time == 169, day == 7) %>%
  mutate(datetime = week_commencing + hours(time - 1),
         week_commencing = week_commencing + days(7)) %>%
  dplyr::select(forecast, type, datetime, storage_sim, week_commencing) %>%
  distinct() %>%
  mutate(variable_name = 'Storage')

# plot day 1 and final schedules (Figure 6)

var_name_tbl = tibble(variable = c('inflow_actual', 'release_turbine', 'storage_sim', 'benefit_revenue', 'price_price'),
                      variable_name = c('Inflow', 'Turbine Release', 'Storage', 'Revenue', 'LMP'))

dp_tbl_day1 = dp_tbl %>% 
  dplyr::filter(week_commencing == week_sel, day == 1) %>%
  dplyr::select(datetime, forecast, type, storage_sim, inflow_actual, release_turbine) %>%
  gather(variable, value, -datetime, -forecast, -type) %>%
  left_join(var_name_tbl)
  
pt_day1 = ggplot() +
  geom_line(data = dp_tbl_day1, 
            aes(x = datetime, y = value, color = type, linetype = forecast, linewidth = forecast)) +
  geom_point(data = filter(eow_storage, week_commencing == week_sel),
             aes(x = datetime, y = target_storage), shape = 5) +
  geom_point(data = filter(sow_storage, type == 'actual', week_commencing == week_sel),
             aes(x = datetime, y = storage_sim), shape = 0) +
  facet_wrap(~variable_name, ncol = 1, scales = 'free_y') +
  theme_bw() +
  xlab('') +
  ylab('') +
  scale_x_datetime(date_breaks = '1 day', date_labels = '%b %d') +
  scale_color_manual(values = c('#574331', '#349beb')) +
  scale_linetype_manual(values = c('solid', 'dotted', 'dashed')) +
  scale_linewidth_manual(values = c(0.4, 0.5, 0.75)) +
  theme(legend.position = 'bottom', legend.title = element_blank(),
        strip.background = element_rect(fill = NA)) +
  # guides(color = 'none') +
  ggtitle('Day 1 Schedule')

# plot final schedule vars for one week
dp_act_tbl_ex = dp_act_tbl %>%
  dplyr::filter(week_commencing == week_sel,
                !is.na(release_turbine)) %>%
  dplyr::select(datetime, forecast, storage_sim, release_turbine,
                benefit_revenue) %>%
  gather(variable, value, -datetime, -forecast) %>%
  left_join(var_name_tbl)

dp_act_tbl_ex$variable_name = factor(dp_act_tbl_ex$variable_name, 
  levels = c('Revenue', 'Storage', 'Turbine Release'))

pt_act = ggplot() +
  geom_line(data = dp_act_tbl_ex, 
            aes(x = datetime, y = value, linetype = forecast, linewidth = forecast), color = '#574331') +
  geom_point(data = filter(eow_storage, week_commencing == week_sel),
             aes(x = datetime, y = target_storage), shape = 5) +
  geom_point(data = filter(sow_storage, type == 'actual', week_commencing == week_sel),
             aes(x = datetime, y = storage_sim), shape = 0) +
  theme_bw() +
  xlab('') +
  ylab('') +
  scale_x_datetime(date_breaks = '1 day', date_labels = '%b %d') +
  scale_linetype_manual(values = c('solid', 'dotted', 'dashed')) +
  scale_linewidth_manual(values = c(0.4, 0.5, 0.75)) +
  facet_wrap(~variable_name, ncol = 1, scales = 'free_y') + 
  theme(legend.position = 'bottom', legend.title = element_blank(),
        strip.background = element_rect(fill = NA)) +
  # guides(color = 'none') +
  ggtitle('Final Week Schedule')

# combine day1 and actual plots together
pt_comb = pt_day1 | pt_act

ggsave(plot = pt_comb, 'figures/figure6_schedule_evol_comb.png', height = 8, width = 10)

# calculate revenue totals by forecast - statistics shown in text

dp_act_tbl_ex %>%
  dplyr::filter(variable == 'benefit_revenue') %>%
  group_by(forecast) %>%
  dplyr::summarise(value = sum(value)) %>% spread(forecast, value) %>%
  mutate(pct_diff_synth = (synthetic - perfect) / perfect * 100,
         pct_diff_pers= (persistence - perfect) / perfect * 100)
