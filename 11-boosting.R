# Installation des packages
install.packages('ada')

# Chargement des packages
library(ada)
library(rpart)


# Construction du modèle avec l'agorithme ada boost
set.seed(235)
boost <- ada(Cible ~ ., data=train[,vars], type="discrete", loss="exponential", control=rpart.control(maxdepth=1, cp=-1, minsplit=0), iter=5000, nu=1, test.y=test[,"Cible"], test.x=test[,1:19])

test$boost <- predict(boost, test, type='prob')[,2]
pred <- prediction(test$boost, test$Cible, label.ordering=c(0,1))
performance(pred,"auc")@y.values[[1]]
