library(dslabs)
library(pdftools)


#Downloading and importing PDF document into R
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)


#if we examine the object text we notice that it is a character vector with an entry for each page.
#The page we want is page 2

raw_data_research_funding_rates <- txt[2]

#that object above is a long string. Each line on the page is separated by symbol \n
#we can then create a list with the lines as elements

tab <- str_split(raw_data_research_funding_rates,"\n")
tab <- tab[[1]]

#examining tab, we realize that column names are in the third and fourth lists

the_names_1 <- tab[3]
the_names_2 <- tab[4]

#we want to remove the leading space and everything following the comma. The we can obtain the elements
#by splitting. We watn to split only when there are 2 or more spaces to avoid splitting success rate


the_names_1 <- the_names_1 %>%
  str_trim() %>% #recall that trim removes whitespace from start and end of string
  str_replace_all(",\\s.","") %>%
  str_split("\\s{2,}",simplify=TRUE)
the_names_1


#for the second line, we want to trim the leading space and then split by space

the_names_2 <- the_names_2 %>%
  str_trim()  %>%
  str_split("\\s+",simplify=TRUE)
the_names_2

#now we can join these to generate one name for each column
tmp_names <- str_c(rep(the_names_1,each=3),the_names_2[-1],sep="_") #make names with all possible combinations
the_names <- c(the_names_2[1],tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s","_")

the_names

#for the actual data, information is in tab from lines 6 through 14

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()
We can see that the objects are identical:
  
identical(research_funding_rates, new_research_funding_rates)