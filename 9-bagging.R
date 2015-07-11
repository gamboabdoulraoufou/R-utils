# Installation des packages
install.packages('randomForest')
install.packages('ipred')
install.packages('ROCR')

# Chargement des packages
library(randomForest) 
library(ipred) 
library(rpart)
library(ROCR)


set.seed(235)
mtry=length(credit[vars,])-1

# 1- Construction du modèle
bag1 <- bagging(Cible ~ ., data=train[,vars], nbagg=200, coob=TRUE, control=rpart.control(minbucket=5))
bag1

# 2- Application du modèle sur l'echantillon test
# 2-1- Aggregation des modèle par moyenne
test$bag1 <- predict(bag1, test, type="prob", aggregation="average")
head(test$bag1)
pred <- prediction(test$bag1[,2], test$Cible, label.ordering=c(0,1))
performance(pred, "auc")@y.values[[1]]
# 2-2- Aggregation des modèle par vote
test$bag1 <- predict(bag1, test, type="prob", aggregation="majority")
head(test$bag1)
pred <- prediction(test$bag1[,2], test$Cible, label.ordering=c(0,1))
performance(pred, "auc")@y.values[[1]]
