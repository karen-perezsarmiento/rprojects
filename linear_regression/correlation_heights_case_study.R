library(tidyverse)
library(HistData)
data("GaltonFamilies")

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father,childHeight) %>%
  rename(son = childHeight)

#means and standard deviations
galton_heights %>%
  summarize(mean(father),sd(father),mean(son),sd(son))

#scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father,son)) +
  geom_point(alpha = 0.5)

#correlation coeff

#rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r=cor(father,son)) %>% pull(r)