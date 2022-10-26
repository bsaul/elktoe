#-----------------------------------------------------------------------------#
# Title: Elktoe River Parameter Figure
# Author: B Saul
# Date: 11/20/15
# Purpose: Create graphic of river parameters over time.
#-----------------------------------------------------------------------------#


vrs <- "v003"

#### Prepare Data ####

###  USGS data
usgs2 <- usgsdt %>%
  filter(date > min(as.Date(mdy(river$date))) - 2,
         date < max(as.Date(mdy(river$date))),
         site_no != '03503000') %>%
  select(-temp, -site_no) %>%
  mutate(
    discharge = (discharge + lag(discharge) + lag(discharge, 2) ) / 3 )

# Button logger data
button2 <- button %>%
  mutate(date = as.Date(ymd_hms(date_time)),
         river = ifelse(river == 'Little Tennessee',
                        'LiTN',
                        'Tuck')) %>%
  filter(site != '', !is.na(date)) %>%
  group_by(river, date) %>%
  dplyr::summarise(button_temp = mean(temp)) %>%
  ungroup()

# Combine all water quality data
wqdt <- river %>%
  mutate(river = ifelse(river == 'Little Tennessee',
                        'LiTN',
                        'Tuck'),
         date = as.Date(mdy(date)) ) %>%
  group_by(river, date) %>%
  dplyr::summarise(temp = mean(temp, na.rm = T),
                   do   = mean(do, na.rm = T),
                   turb = mean(turb, na.rm = T),
                   ph   = mean(ph, na.rm = T)) %>%

  # Combine with USGS and button logger temperature data
  right_join(usgs2, by = c('river', 'date')) %>%
  right_join(button2, by = c('river', 'date')) %>%

  # Compute ratio of LiTN/Tuck for each day
  melt(id.vars = c('river', 'date')) %>%
  dcast(date ~ river + variable) %>%
  mutate(temp = LiTN_temp/Tuck_temp,
         do   = LiTN_do/Tuck_do,
         turb = LiTN_turb/Tuck_turb,
         ph   = LiTN_ph/Tuck_ph,
         button_temp = LiTN_button_temp/Tuck_button_temp,
         discharge   = LiTN_discharge/Tuck_discharge) %>%
  select(date, temp, do, ph, turb, button_temp, discharge) %>%
  melt(id.vars = 'date') %>%
  filter(!is.na(value)) %>%

  # Keep only observations during time A. rav in river
  filter(date < '2013-10-15') %>%

  mutate(facet1 = ifelse(variable %in% c('button_temp', 'do'), 'Temperature & DO',
                         ifelse(variable == 'discharge', 'Discharge', 'pH')))


#### Create plot ####

p <- ggplot(wqdt %>% filter(!(variable %in% c('turb', 'temp') ) ),
       aes(x = date, y = value, group = variable, color = variable)) +
  geom_hline(yintercept = 1, color = 'gray50') +
  geom_vline(xintercept = as.numeric(as.Date(c('2013-04-02', '2013-09-19')) ),
             linetype = c(3, 3),
             size = c(.5, .5)) +
  #  geom_point(size = 1.5) +
  geom_line() +


  # Scales
  # label start/stop times and ticks for 1st of month
  scale_x_date(breaks = as.Date(c('2013-04-02', '2013-05-01', '2013-06-01',
                                  '2013-07-01', '2013-08-01', '2013-09-01',
                                  '2013-09-19')),
               labels  = c('2013/4/2', '', '', '7/1', '', '', '9/19')) +
  scale_y_continuous(breaks = c(0.75, 1, 1.25),
                     labels = c('0.75', '1', '1.25')) +
  scale_color_brewer(guide = F, palette = 'Set2') +
  xlab('') +
  ylab('Ratio of \n LiTN/Tuck') +
  coord_cartesian(ylim = c(.5, 1.5)) +

  # Theme
  theme_bw() +
  theme(text = element_text(family = 'Times'),
        panel.border = element_blank(),
        panel.grid   = element_blank(),
        axis.ticks   = element_line(color = 'gray50'),
        axis.ticks.y   = element_line(size = .25),
#         axis.ticks.x   = element_line(color = 'gray50'),
        #   plot.background = element_rect(color = 'gray50'),
        axis.title.y = element_text(angle = 0, vjust = 1,
                                    size = 10,
                                    color = 'gray50'),
        axis.text    = element_text(color = 'gray50'),
        axis.text.y  = element_text(vjust = -0.2, size = 7),
        axis.text.x  = element_text(size = 8),
        axis.line    = element_line(color = 'gray50')) +

  # Annotations
#   annotate(geom = 'text', x = as.Date('2013-04-23'), y = .6,
#            label = 'Experiment began', color = 'black', family = 'Times',
#            size = 2.5) +
#   geom_segment(aes( x = as.Date('2013-04-03'), xend = as.Date('2013-04-06'),
#                                y = .59, yend = .6), color = 'gray50', size = .1 ) +
#
#   annotate(geom = 'text', x = as.Date('2013-08-25'), y = .6,
#            label = 'A. raveneliana removed', color = 'black', family = 'Times',
#            size = 2.5) +
#   geom_segment(aes( x = as.Date('2013-09-16'), xend = as.Date('2013-09-19'),
#                     y = .6, yend = .59), color = 'gray50', size = .1 ) +
#
# #   annotate(geom = 'text', x = as.Date('2013-07-15'), y = .6,
# #            label = 'Discharge', color = '#e78ac3', family = 'Times', size = 3) +
# #   annotate(geom = 'text', x = as.Date('2013-07-10'), y = 1.27,
# #            label = 'Temperature', color = '#8da0cb', family = 'Times', size = 3) +
# #   annotate(geom = 'text', x = as.Date('2013-10-18'), y = .95,
# #            label = 'DO', color = '#66c2a5', family = 'Times', size = 3) +
# #   annotate(geom = 'text', x = as.Date('2013-10-01'), y = 1.1,
# #            label = 'pH', color = '#fc8d62', family = 'Times', size = 3) +

  facet_grid(~facet1)




#### Save plot ####

ggsave(filename = paste0('analysis/manuscripts/paper0/figures/river_parameters_', vrs, '.pdf'),
       plot = p, height = 2.5, width = 6, units ='in')

