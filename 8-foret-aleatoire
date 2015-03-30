# Installation des packages
install.package('RandomForest')

# Chargement des packages
library(RandomForest)

# Choix du nombre q de predicteur à inclure dans le modèle
## Par défaut q = racine carrée du nombre des préducteur
## Pour une base de 16 prédicteurs dont 6 sont discriminants, q sertai égal à 4. 

## Calculons la probabilité d'avoir 1 prédicteur discriminant dans les 4 arbres
dhyper(1,6,10,4)

## Calculons la probabilité d'avoir au moins 1 prédicteur discriminant dans les 4 arbres
dhyper((1,6,10,4)+(2,6,10,4)+(3,6,10,4)+(4,6,10,4)+(5,6,10,4)+(6,6,10,4)

## Pour que q soit bon, il faudra que la probabilité d'avoir au moins 1 prédicteur discriminant dans chaque arbre soit > 0.5

# Initialiser la valeur du seed
set.seed(235)

# Modèle le seuil des votes est de 50% pour basculer dans une classe
rf <- randomForest(cible ~., data = data_[id, vars], importance=TRUE, ntree = 500, mtry =3, replace = T, 
                  keep.forest = T, nodesize = 5, ytest = test[,cible], xtest = test[, var_quanti])


# Affichage du modèle
rf

# Modèle en baissant le seuil de la deuxème classe de 50% des votes à 25% des votes
rf <- randomForest(cible ~., data = data_[id, vars], importance=TRUE, ntree = 500, mtry =3, replace = T, 
                  keep.forest = T, nodesize = 5, ytest = test[,cible], xtest = test[, var_quanti], cutoff=c(0.5, 0.25))
                  
# Affichage du modèle
rf

# Afficher les votes par individus
head(rf$votes)

# Nombre d'arbre dont un individu x est out of bag.
head(rf$oob.times)

rf$pred <- ifelse(rf$votes[,2] >= 0.5, 1, 0)
table (rf$pred)

err_oob <- sum(rf$pred != data_[id,cible]/nrow(data_[id,])
err_oob

# Le vecteur err.rate contient ntree lignes et trois colonnes
# la 1ère colonne contient le taux d'erreur global (OOB), la séconde celui de la première classe de la variable à expliquer...
tail(rf$err.rate)

tail(rf$test$err.rate)

#Affichage du tausx d'erreur
plot (rf$err.rate[,1], type='l', ylim=c(.2,.4), xlab='nombre d'itérations', ylab='erreur')
lines(rf$test$err.rate[,1], type='1', lwd=2, col='red')

# Détermination du taux d'erreur minimum en apprentissage OOB
(min.err <- min(rf$err.rate[,'OOB']))

# Determination du nombre d'arbre ayant le taux d'erreur minimum
(min.err.idx <- wich(rf$err.rate[,'OOB']==min.err))

(min.err <- min(rf$err.rate[,'OOB']))

# mesure de l'importance des varibales: taux d'erreur
importance(rf, type=1)[order(importance(rf, type=1), decreasing=TRUE),]

# mesure de l'importance des varibales: indice de Gini
importance(rf, type=1)[order(importance(rf, type=2), decreasing=TRUE),]

# Affichage d'importance des varibles
varImpPlot(rf)

# Importance moyenne des variables
nvar <- ncol(data_[,vars])-1
vimp <- rep(0, nvar)
nsimul <- 30

for (i in 1:nsimul) {
    rf <- randomForest(cible ~., data=data_[id, vars], importance = TRUE, 
                                 ntree = 500, mtry = 3, replace = T, keep.forest=T, nodesize=5)
    vip <- importance(rf, type=1)
    vimp <- vimp + vip[order(rownames(vip))]/nsimul
    rm(rf)
}

a1 <- sort(rownames(vip))
a2 <- order(vimp, decreasing = TRUE)
par(mar = c(8, 4, 4, 0)

barplot(vimp[a2], col = gray(0:nvar/nvar), names.arg = a1[a2], ylab='Importance', ylim=c(0.30), cex.names = 0.8, las=3)
title(main=list('randomForest: Importance moyenne des variablrd', font = 1, cex=1.2))

# Utilisation de la fonction tuneRF pour trouver le nombre mtry de varible qui est optimal 
# du point de vue du taux d'erreur OOB
mtry <- tuneRF(data_[id, -c(1,21)], mtryStart = 1, ntreeTry = 500, stepFactor = 2, improve = 0.001)
mtry[,2]
mtry[wich.min(mtry[2]),1] # Nombre de variable qui minimise le taux d'erreur OOB

# Utilisation de la fonction tuneRF pour trouver le nombre mtry de varible qui est optimal 
# du point de l'aire sous la courbe de ROC

set.seed(235)
nsimul <- 100
nvarmin <- 1
nvarmax <- 6
auc <- matrix(NA, nvarmax-nvarmin+1, 2)

for (nvar in nvarmin:nvarmax) {
    auc[nvar-nvarmin+1, 1] <- nvar
    rft <- matrix(NA, nrow(test), nsimul+1)
    
    for (i in 1:nsimul) {
        rf <- randomForest(cible ~., data=data_[id,vars], importance=F, ntree=500, mtry=nvar, replace=T, keep.forest=T, nodesize=5)
        rtf[,i] <- predict(rf, test, type='prob')[,2]
    }
    
    rtf[,nsimul+1] <- apply(rtf[,1:nsimul], 1, mean)
    pred <- prediction(rft[,nsimul+1], test[,cible], label.ordering=c(0,1))
    auc[nvar-nvarmin +1, 2] <- performance(pred, 'auc')@y.values[[1]]
}

colnames(auc) <- c('Nb variables', 'AUC test')

auc


    
    
