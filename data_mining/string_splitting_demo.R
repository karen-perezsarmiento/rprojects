library(tidyverse)

filename <- system.file("extdata/murders.csv",package="dslabs")
lines <- readLines(filename)
lines %>% head()

#split at commas with str_split function, rmeove row of column names
x <- str_split(lines,",")
x %>% head()
col_names <- x[[1]]
x <- x[-1]

#extract first element of each list entry
library(purrr)
map(x,function(y) y[1]) %>% head()
map(x,1) %>% head()

# extract columns 1-5 as characters, then convert to proper format
dat <- data.frame(map_chr(x, 1),  
                  map_chr(x, 2),
                  map_chr(x, 3),
                  map_chr(x, 4),
                  map_chr(x, 5)) %>%
  mutate_all(parse_guess) %>%
  setNames(col_names)
dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 
# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)