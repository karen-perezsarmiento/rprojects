library(tidyverse)
data(co2)

 
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
   setNames(1:12) %>%
   mutate(year = as.character(1959:1997))

co2_tidy <- gather(co2_wide,month,co2,-year)

co2_tidy %>% ggplot(aes(as.numeric(month),co2,color=year))+geom_line()

library(dslabs)
data(admissions)
dat<-admissions %>%select(-applicants)

dat_tidy <- spread(dat,gender,admitted)

tmp <- gather(admissions,key,value,admitted:applicants)
tmp2 <- unite(tmp,column_name,c(key,gender))
admissions_tidy <- spread(tmp2,column_name,value)