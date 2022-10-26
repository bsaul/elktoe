arav <- mussels_wide[mussels_wide$species == 'A. raveneliana', ]
fit <- lm(buoyant_weight_g_d ~ corb_density_0, data = arav)
summary(fit)


plot(arav$corb_density_0, arav$buoyant_weight_g_d)
lm()
# fit <- lmer(buoyant_weight_g_d ~ corb_density_0, data = arav)
# summary(fit)
#
# fit <- lm(volume_d ~ river, data = arav)

library(lme4)
fit <- lmer(volume_d ~ river + (1|site), data = arav)
summary(fit)

temp <- mussels[mussels$species == 'A. raveneliana' & mussels$post == 1, ]

fit <- glmer(dead ~ corb_density + (1|river), data = temp, family = binomial)

fit <- glmer(dead ~ river + (1|site), data = temp, family = binomial)
summary(fit)
tapply(temp$dead, temp$site, sum, na.rm = T)
tapply(temp$dead, temp$site, length)
mean(temp$dead, na.rm = T)
prop.test(c(2, 4, 6, 0, 3, 5), rep(12, 6))
##############

with(mussels_wide, (buoyant_weight_g_0 - buoyant_weight_g_1)/buoyant_weight_g_0)
mussels_wide$lost_or_dead <- with(mussels_wide,
                                  ifelse(dead_1 == 1 |
                                           (buoyant_weight_g_0 - buoyant_weight_g_1)/buoyant_weight_g_0 < -.1, 1,
                                         0))

arav <- mussels_wide[mussels_wide$species == 'A. raveneliana', ]

ddply(arav, .(site), summarize,
      y = sum(lost_or_dead, na.rm = T),
      y1 = sum(dead_1, na.rm = T),
      n = length(dead_1),
      x = mean(corb_density_0))

prop.trend.test(c(2,5,7), rep(12,3))
prop.trend.test(c(0,3,5), rep(12,3))

library(lme4)
fit <- glm(lost_wt_or_dead ~ river, data = elktoe, family = binomial)
summary(fit)

library(lme4)
fit <- glmer(lost_wt_or_dead ~ river + (1|site), data = elktoe, family = binomial)
summary(fit)


fit <- glm(lost_or_dead ~ site, data = arav, family = binomial)
summary(fit)

plot(arav$)

fit <- glm(lost_wt_or_dead ~ site, data = elktoe, family = binomial)
summary(fit)

fit <- glm(lost_10_or_dead ~ river, data = elktoe, family = binomial)
summary(fit)
