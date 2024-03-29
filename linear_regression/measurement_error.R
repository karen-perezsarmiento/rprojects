library(dslabs)
library(tidyverse)
library(ggplot2)

falling_object <- rfalling_object()

#code to draw trajectory of ball
falling_object %>%
  ggplot(aes(time,observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")



#code to use the lm() function to estimate the coefficients
fit <- falling_object %>%
  mutate(time_sq=time^2) %>%
  lm(observed_distance~time+time_sq,data=.)

tidy(fit)

#to check if the estimated parabola fits the data
augment(fit) %>%
  ggplot() +
  geom_point(aes(time,observed_distance)) +
  geom_line(aes(time,.fitted),col="blue")

tidy(fit,conf.int=TRUE)


