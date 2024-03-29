library(dslabs)
library(tidyverse)
library(HistData)

#compute sample correlation
R <- sample_n(galton_heights,25,replace=TRUE) %>%
  summarize(r=cor(father,son))
R

#Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B,{
  sample_n(galton_heights,N,replace=TRUE) %>%
  summarize(r=cor(father,son)) %>%
  pull(r)
})
qplot(R,geom="histogram",binwidth=0.05,colo=I("black"))

###NOTE : if we increase the sample size N from 25 to 50 we can expect the standard deviation of the correlation
### to shrink (central limit theorem)

#expecte value and standard error
mean(R)
sd(R)

#QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample=R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))