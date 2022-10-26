#-----------------------------------------------------------------------------#
# Title: Feeding Trial Analysis
# Author: B Saul
# Date: 9/18/15
# Purpose:
#-----------------------------------------------------------------------------#
library(ggplot2)
# Run import_feeding_trial_data.R

pre_post <- trial %>%
  filter(time %in% c(0, 4)) %>%
  mutate(control = (genus == 'control') * 1,
         genus_ordered = factor(genus, levels = c('elktoe', 'corbicula', 'lampsilis', 'control'),
                                labels = c('Alasmidonta', 'Corbicula', 'Lampsilis', 'Control'),
                                ordered = T),
         genus_trial_ordered = factor(genus_trial, levels = c('elktoe', 'corbicula', 'lampsilis'),
                                      labels = c('Alasmidonta', 'Corbicula', 'Lampsilis'),
                                      ordered = T),
         time = factor(time, levels = c(0,4), labels = c('Baseline', '4 hours'), ordered = T))

ggplot(pre_post, aes(y = log(diameter), x = genus_ordered, fill = factor(control))) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Organism', 'Control', 'Organism', 'Organism')) +
  scale_fill_brewer(type = 'div', guide = F, palette = 4) +
  coord_flip() +
  facet_grid(time ~ genus_trial_ordered, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('log(Particle Diameter) in microns') +
  ggtitle('Pre and Post Experiment Particle Size Distributions\nin Control and Organism Tanks') +
  theme(text = element_text(family = 'Times'),
        strip.text = element_text(size = 12))

ggsave('dev/particle_size_distribution.png', width = 6, height = 3.75)
