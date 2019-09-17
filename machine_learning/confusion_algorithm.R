library(tidyverse)
library(caret)
library(dslabs)
data("heights")

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

#compute accuracy
mean(y_hat==test_set$sex)
heights%>%group_by(sex) %>% summarize(mean(height),sd(height))
y_hat <- ifelse(x>62,"Male","Female") %>% factor(levels=levels(test_set$sex))
mean(y==y_hat)

#examine the accuracy of 10 cutoffs
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff,function(x){
  y_hat<-ifelse(train_set$height>x,"Male","Female") %>%
    factor(levels=levels(test_set$sex))
  mean(y_hat==train_set$sex)
})

max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff,"Male","Female") %>%
  factor(levels=levels(test_set$sex))

#tabulate each combination of prediction and actual value

table(predicted = y_hat,actual=test_set$sex)
test_set %>%
  mutate(y_hat=y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy=mean(y_hat==sex))
prev <- mean(y=="Male")

confusionMatrix(data=y_hat,reference=test_set$sex)