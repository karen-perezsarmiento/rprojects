library(HistData)
library(dplyr)
library(tidyverse)

data("GaltonFamilies")

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father,childHeight) %>%
  rename(son=childHeight)

#Monte Carlo simulation

B <- 1000
N <- 50
lse <- replicate(B,{
  sample_n(galton_heights,N,replace=TRUE) %>%
    lm(son ~father,data=.) %>%
    .$coef
}) #returns a matrix. reshape to make useful

lse <- data.frame(beta_0=lse[1,],beta_1=lse[2,])

#plot the distribution of beta_0 and beta_1
library(gridExtra)

p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1,p2,ncol=2)


#summary stats
sample_n(galton_heights,N,replace=TRUE) %>%
  lm(son ~ father, data = .) %>%
  summary %>%
  .$coef

lse %>% summarize(se_0=sd(beta_0),se_1=sd(beta_1))


##it is useful to know that the LSE can be storngly correlated, as seen below

lse %>% summarize(cor(beta_0,beta_1))


##however, the correlation depends on how the predictors are defined or transformed
##here we standardize the father heights

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights,N,replace=TRUE) %>%
  mutate(father = father -mean(father)) %>%
  lm(son ~ father, data = .) %>% .$coef
})

#observe what happens to the correlation
cor(lse[1,],lse[2,])
