library(elktoe)
river_plot('temp')
ggplot(button, aes(x = date_time, y = temp, color = river)) + geom_line()
river$date_d <- as.Date(river$date_)
boxplot(mussels_wide$buoyant_weight_g_d ~ mussels_wide$river)
river_plot('temp')
#### Descriptive Plots ####

ggplot(mussels_wide, aes(x = volume_d)) +
  geom_histogram() + facet_grid(species ~ .)

subset(mussels_wide, volume_d < -25000 | volume_d > 25000)
temp <- subset(mussels_wide, volume_d > -25000 & volume_d < 25000)

ggplot(temp, aes(x = volume_d)) +
  geom_histogram() + facet_grid(species ~ .)
#### Descriptive Summaries ####

ddply(mussels_wide, .(river, species), summarize,
      mean_volume_d = mean(volume_d, na.rm = T),
      prop_dead     = mean(dead, na.rm = T))

ddply(mussels_wide, .(river, species, site), summarize,
      mean_volume_d = mean(volume_d, na.rm = T),
      prop_dead     = mean(dead, na.rm = T))

ddply(mussels_wide, .(river, species), summarize,
      mean_volume_d = mean(volume_d, na.rm = T))

ddply(mussels_wide, .(river, species, site), summarize,
      mean_volume_d = mean(volume_d, na.rm = T))






