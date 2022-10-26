
library(elktoe)
library(tidyr)
library(fastICA)
library(coloredICA)
library(wavelets)


W <- dwt(as.matrix(temp[, 11]))
plot(W)

str(W)





x <- prcomp(t(temp[, 11:ncol(temp)]), center = TRUE, scale = FALSE)
z <- fastICA(x$x, n.comp = 15)
z2 <- coloredICA::cICA(Xin =  log(t(temp[, 11:ncol(temp)]) + 1), M = 15, tol = .005)



str(z2)

z2$A

pc <- cbind(mussel_id = rownames(x$x), as.data.frame(x$x)) %>%
  left_join(mussels_wide, by = c("mussel_id" =  "id"))


ggplot(pc, aes(x = PC1, y = PC2, shape = species, color = river)) +
  geom_point()


ic <- cbind(mussel_id = rownames(z$S), as.data.frame(z$S)) %>%
  left_join(mussels_wide, by = c("mussel_id" =  "id"))

ic2 <- cbind(mussel_id = rownames(z2$X), as.data.frame(z2$A)) %>%
  left_join(mussels_wide, by = c("mussel_id" =  "id")) %>%
  mutate(tuck3 = if_else(site == "Tuck 3", 'yes', 'no'))



lm(V10 ~ s_buoyant_d + length_mm_d + river, data = ic2) %>% summary()


ggplot(ic2, aes(x = site, y = V15, shape = species, color = site)) +
  geom_point()
