library(tidyverse)
path <- system.file("extdata",package="dslabs")
filename <- file.path(path,"fertility-two-countries-example.csv")


#gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
  gather(year,fertility, -country)

#gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

#convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility,-country,convert=TRUE)
class(new_tidy_data$year)

new_tidy_data %>%
  ggplot(aes(year,fertility,color=country)) +
  geom_point()

#spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data,country,`1960`:`1967`)