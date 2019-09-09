library(ggplot2)
library(tidyverse)
library(HistData)
library(dplyr)

#compute RSS for any pair of beta0 and beta1 in Galton's data
data("GaltonFamilies")

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father,childHeight) %>%
  rename(son=childHeight)


#plot prediction and confidence intervals
galton_heights %>% ggplot(aes(son,father)) +
  geom_point() +
  geom_smooth(method="lm")

#predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .)
Y_hat <- predict (fit,se.fit=TRUE)
names(Y_hat)

#plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son~father,data=.))) %>%
  ggplot(aes(father,Y_hat)) +
  geom_line()
  