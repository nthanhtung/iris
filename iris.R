library(caret)
library(dplyr)
library(dslabs)
library(ggplot2)
library(titanic)
library(dummies)
library(corrplot)
library(reshape)
library(pastecs)

#load data
data("iris")
str(iris)
head(iris)

#data exploration
summary(iris)
boxplot(iris)

iris_norm <- data.frame("Species" = iris[,5] ,apply(iris[, 1:4], 2, function(x) (x - min(x))/(max(x)-min(x))))
stat.desc(iris_norm)

iris_norm %>% 
  melt(id = "Species") %>% 
  ggplot(aes(x = variable, y = value, color = Species)) +
  geom_jitter(width = 0.05)

iris_norm %>% 
  melt(id = "Species") %>% 
  ggplot(aes(x = variable, y = value, color = Species)) +
  geom_boxplot()

iris_exclude_species <- iris_norm %>%
  subset(select = -c(1))
corrplot(cor(iris_exclude_species))

#Normal model approach with all variable
#create train set & test set
index <- createDataPartition(iris$Species, times = 1, p = 0.8, list = FALSE)
train_set <- iris[index,]
test_set <- iris[-index,]
str(train_set)
str(test_set)

#fit model
#naive bayes method for top 2 variable with highest variability and have low correlation
fit <- train(Species ~ Petal.Width + Sepal.Width, data = train_set, method = "nb")
fit
varImp(fit)
y_hat <- predict(fit, newdata = test_set)

ac1 <- mean(y_hat == test_set$Species)
result <- data.frame("method" = "naive bayes top 3 variable", "accuracy" = ac1)

#naive bayes method for all variable
fit <- train(Species ~ ., data = train_set, method = "nb")
fit
varImp(fit)
y_hat <- predict(fit, newdata = test_set)
ac2 <- mean(y_hat == test_set$Species)
result <- rbind(result,
                data.frame("method" = "naive bayes all variable", "accuracy" = ac2))
result

