## Feeding Trial scratchpad
boxplot(diameter ~ species, data = trial[trial$time == 0, ])
boxplot(diameter ~ species, data = trial[trial$time == 4, ])
pre_post <- subset(trial, time %in% c(0, 4))

pre_post <- within(pre_post, {
    label1 <- factor(ifelse(time == 0, 'pre', 'post'), c('pre', 'post'))
    label2 <- paste(genus, label1)
    label3 <- factor(genus, c('control', 'corbicula', 'elktoe', 'lampsilis'))
    label4 <- paste(tank_c, label1)})

ggplot(pre_post, aes(x = label1, y = diameter, group = time)) +
  geom_boxplot() + theme_classic() +
  theme(axis.title.y = element_text(angle = 0),
        axis.line.y = element_blank()) +
  xlab("") +
  facet_grid(. ~ label3)


ggplot(subset(pre_post, diameter > 10), aes(x = label1, y = diameter, group = time)) +
  geom_boxplot() + theme_classic() +
  theme(axis.title.y = element_text(angle = 0),
        axis.line.y = element_blank()) +
  xlab("") +
  facet_grid(. ~ label3)

ggplot(pre_post, aes(x = label1, y = diameter, group = time)) +
  geom_boxplot() + theme_classic() +
  theme(axis.title.y = element_text(angle = 0),
        axis.line.y = element_blank()) +
  xlab("") +
  facet_grid(. ~ tank_c)


#### Cut diameters ####

library(dplyr)
xx <- within(trial,{
             dia_c4 <- cut(diameter, c(0, 3, 5, 9, 25))
             dia_c5 <- cut(diameter, c(0, 3, 5, 7, 9, 25))
             dia_c10 <- cut(diameter, c(0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 25))
             dia_c6.11 <- cut(diameter, c(0, 6, 7, 8, 9, 10, 11, 25))
             dia_c2.9  <- cut(diameter, c(0, 2, 2.5, 3, 3.5, 4, 4.5, 5, 6, 7, 8, 9, 25))
             })

xx2 <- subset(xx, xx$diameter > 5)



nn <- xx %>%
  group_by(genus_trial, genus, tank_c, id, time) %>%
  summarise( n = n())

nn2 <- xx2 %>%
    group_by(genus_trial, genus, tank_c, id, time) %>%
    summarise( n = n())

counts_c4 <- xx %>%
    group_by(genus_trial, genus, tank_c, id, time, dia_c4) %>%
    summarise(y = n())

counts_c10 <- xx2 %>%
  group_by(genus_trial, genus, tank_c, id, time, dia_c10) %>%
  summarise(y = n())

counts_c6.11 <- as.data.frame(table(xx$time, xx$id, xx$dia_c6.11,
                                    dnn = c('time', 'id', 'dia_c6.11')))

counts_c2.9 <- as.data.frame(table(xx$time, xx$id, xx$dia_c2.9,
                                    dnn = c('time', 'id', 'dia_c2.9')))



yy_c4 <- merge(counts_c4, nn, by = c('genus_trial', 'genus', 'tank_c', 'id', 'time'))
yy_c10 <- merge(counts_c10, nn.2, by = c('genus_trial', 'genus', 'tank_c', 'id', 'time'))

yy_c6.11 <- merge(counts_c6.11, nn, by = c('id', 'time'))
yy_c2.9 <- merge(counts_c2.9, nn, by = c('id', 'time'))

yy_c4 <- within(yy_c4, pp <- y/n)
yy_c10 <- within(yy_c10, pp <- y/n)
yy_c6.11 <- within(yy_c6.11, pp <- Freq/n)
yy_c2.9 <- within(yy_c2.9, pp <- Freq/n)

cc <- c('corbicula', 'corbicula control')
ee <- c('elktoe', 'elktoe control')

yy_c6.11_mean <- yy_c6.11 %>%
  group_by(genus, tank_c, time, dia_c6.11) %>%
  summarise(pp_m = mean(pp))


ggplot(subset(yy_c6.11_mean, dia_c6.11 != "(0,6]"),
       aes(x = time, y = pp_m, group = tank_c, color = (genus == 'control'))) +
  geom_line() +
  facet_grid(dia_c6.11 ~ .)

#### Ranking ####

yy_c10_r <- yy_c10 %>%
  group_by(genus_trial, time, dia_c10) %>%
  mutate(r = rank(pp))

yy_c10_r2 <- yy_c10_r %>%
  group_by(tank_c, time, dia_c10) %>%
  summarise(rank_sum = sum(r))


yy_c6.11_r <- yy_c6.11 %>%
  group_by(genus_trial, time, dia_c6.11) %>%
  mutate(r = rank(pp, ties.method = 'random'))

yy_c6.11_r2 <- yy_c6.11_r %>%
  group_by(tank_c, time, dia_c6.11) %>%
  summarise(rank_sum = sum(r))


yy_c2.9_r <- yy_c2.9 %>%
  group_by(genus_trial, time, dia_c2.9) %>%
  mutate(r = rank(pp, ties.method = 'random'))

yy_c2.9_r2 <- yy_c2.9_r %>%
  group_by(tank_c, time, dia_c2.9) %>%
  summarise(rank_sum = sum(r))

ggplot(subset(yy_c10_r2, grepl('control$', tank_c)), aes(x = time, y = rank_sum,
                                                    group = dia_c10, color = dia_c10,
                                                    size = dia_c10) ) +
         geom_line() +
    facet_grid(tank_c  ~ .) +
  scale_color_brewer(type = "seq", palette = 4) +
  scale_size_manual(values = c(.5,.5,.5,.5,1.5,.5)) +
  theme_classic()


tc <- unique(yy_c6.11_r2$tank_c)
d611 <- unique(yy_c6.11_r2$tdia_c6.11)

ggplot(subset(yy_c6.11_r2, grepl('control$', tank_c)),
       aes(x = time, y = rank_sum, group = dia_c6.11, color = dia_c6.11, size = dia_c6.11) ) +
  geom_line() +
  facet_grid(tank_c  ~ .) +
  scale_color_brewer(type = "seq", palette = 4) +
  guides(color = guide_legend(title = 'Particle Diameter'),
         size = guide_legend(title = 'Particle Diameter')) +
  scale_size_manual(values = c(.5,.5,.5,.5,1.5,1.5,.5)) +
  theme_classic()  +
  theme(axis.title.y = element_text(angle = 0),
        axis.line.y = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank()) +
  ylab("Sum of\nControl\nRanks")


ggplot(subset(yy_c2.9_r2, grepl('control$', tank_c) &
                dia_c2.9 %in% c('(0,2]','(2,2.5]', '(2.5,3]','(3,3.5]', '(3.5,4]','(4,4.5]', '(4.5,5]')),
       aes(x = time, y = rank_sum, group = dia_c2.9, color = dia_c2.9) ) +
  geom_line() +
  facet_grid(tank_c  ~ .) +
  scale_color_brewer(type = "seq", palette = 4) +
  guides(color = guide_legend(title = 'Particle Diameter'))+
 # scale_size_manual(values = c(.5,.5,.5,.5,1.5,1.5,.5)) +
  theme_classic()  +
  theme(axis.title.y = element_text(angle = 0),
        axis.line.y = element_blank(),
    #    strip.text.y = element_blank(),
        strip.background = element_blank()) +
  ylab("Sum of\nControl\nRanks")


ggplot(subset(yy_c2.9_r2, grepl('control$', tank_c) &
                !dia_c2.9 %in% c('(0,2]','(2,2.5]', '(2.5,3]','(3,3.5]', '(3.5,4]', '(4,4.5]', '(4.5,5]')),
       aes(x = time, y = rank_sum, group = dia_c2.9, color = dia_c2.9) ) +
  geom_line() +
  facet_grid(tank_c  ~ .) +
  scale_color_brewer(type = "seq", palette = 4) +
  guides(color = guide_legend(title = 'Particle Diameter'))+
  # scale_size_manual(values = c(.5,.5,.5,.5,1.5,1.5,.5)) +
  theme_classic()  +
  theme(axis.title.y = element_text(angle = 0),
        axis.line.y = element_blank(),
 #       strip.text.y = element_blank(),
        strip.background = element_blank()) +
  ylab("Sum of\nControl\nRanks")


round(subset(yy_c2.9_r, genus_trial == 'elktoe' & time == 1 & dia_c2.9 == '(8,9]')$r, 4)
round(subset(yy_c2.9_r, genus_trial == 'elktoe' & time == 4 & dia_c2.9 == '(8,9]')$pp, 4)

round(subset(yy_c6.11_r, genus_trial == 'corbicula' & time == 2 & dia_c6.11 == '(9,10]')$pp, 4)


#### MATCHING ####

orgs <- subset(trial, genus != "control")
org.ids <- as.character(unique(orgs$id))
cnts <- subset(trial, genus == "control")
cnts1 <- split(cnts, cnts$genus_trial)

random.controls <- lapply(cnts1, function(x) sample(as.character(unique(x$id)), 10, replace = TRUE))
# Redo this to get controls sampled at least twice?

matches <- cbind(org.ids, unlist(random.controls, use.names = FALSE))

p.by.cut <- function(breaks){

  xx <- within(trial,{cuts <- cut(diameter, breaks)})
  nn <- xx %>%
    group_by(genus_trial, genus, tank_c, id, time) %>%
    summarise( n = n())
  grouped <- as.data.frame(table(xx$time, xx$id, xx$cuts,
                                      dnn = c('time', 'id', 'cuts')))

  yy <- merge(grouped, nn, by = c('id', 'time'))
  yy <- within(yy, pp <- Freq/n)
  return(yy)
}

counts_c6.11 <- p.by.cut(c(0, 6, 7, 8, 9, 10, 11, 25))

counts.orgs <- subset(counts_c6.11, genus != 'control')
counts.cnts <- subset(counts_c6.11, genus == 'control')

names(counts.cnts)[1] <- "control.id"
counts.orgs2 <- merge(counts.orgs, matches, by.x = "id", by.y = 'org.ids')
names(counts.orgs2)[10] <- "control.id"

temp <- merge(counts.orgs2, counts.cnts[, c("control.id", "time", 'cuts', 'pp')],
              by = c('time', 'cuts', 'control.id'))
# TODO: What happened to 7 observations?

temp$pdiff <- temp$pp.x - temp$pp.y

temp %>%
  group_by(genus, cuts, time) %>%
  summarise( x = mean(pdiff))


temp <- temp %>%
  group_by(genus_trial, time, cuts) %>%
  mutate(r = rank(pdiff, ties.method = 'random'),
         s = sign(pdiff),
         sr = r * s)

summ <- temp %>%
  group_by(genus, time, cuts) %>%
  summarise( x = sum(sr),
             m = mean(pdiff),
             mm = median(pdiff))


ggplot(summ,
       aes(x = time, y = mm, group = cuts, color = cuts) ) +
  geom_line() +
  facet_grid(genus  ~ .) +
  theme_classic()  +
  guides(color = guide_legend(title = 'Particle\nDiameter')) +
  theme(axis.title.y = element_text(angle = 0),
        axis.line.y = element_blank()) +
  ylab("Mean Difference in\nProportion of Particles\nControl vs. Organism Tanks")
