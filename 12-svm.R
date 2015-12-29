# Installation des packages
install.packages('e1071')
install.packages('kernlab')

# Chargement des packages
library(e1071) # Package permettant de mettre en oeuvre les svm en regression ou en classement
library(kernlab)

# 1- SVM à noyau linéaire

## Recherche de la valeur du paramètre qui minimise l'erreur
set.seed(123)
swmlin <- tune.svm(Cible ~., data = credit[id, vars]), kernel = 'linear', cost = 10^(-3:1))
summary(svmlin)
plot(svmlin)

## 1-1- Modèle
svmlin <- svm(Cible~., data = credit[id,vars], kernel = 'linear', probability = TRUE, cost = 1)

## SVM à noyau linéaire avec cross validation
svmlin <- svm(Cible~., data = credit[id,vars], kernel = 'linear', probability = TRUE, 
              cost = 1, cross = 10)

## 1-2- Resumé du modèle
summary(svm)

## 1-3- Coefficient: ils correspondent aux deux forntières de la marge (+/-)
head(svmlin$coefs)

## 1-4- Les prédictions du modèle sont dans svmlin$fitted
table(svmlin$fitted, credit[id, 'Cible'])

## 1-5- Taux d'erreur
mean(svmlin$fitted != credit[id, 'Cible'])

## 1-6- Aire sous la courbe de ROC
pred <- prediction(as.numeric(svmlin$fitted), credit[id, 'Cible'], label.ordering=c(0,1))
performance(pred, "auc")@y.values[[1]]

## 1-7- Prédiction
test$svmlin <- predict(svmlin, test,probability = TRUE, decision.value = TRUE)
test$svmlin
attr('probabilities')

## 1-8- Calcul des coefficients des predicteurs
x <- model.matrix(~ . -1, data = credit[id, 1:19])
y <- credit[id, 1:19]
svmlin = svm(x, y, kernel = 'linear', probability = TRUE, cost = 1, scale = T)
w <- t(svmlin$coefs) %*% x[svmlin$index, ]
w

### 1-8-1 Application des prédicteurs à un vecteurs x pour calculer le score: cas des prédicteurs non centrés reduits
p2 <- x %*% t(w) - svmlin$rho # rho = constante du modèle 

### 1-8-2- Application des prédicteurs à un vecteurs x pour calculer le score: cas des prédicteurs centrés reduits
# recuperation de la moyenne et ecart type utilisé à la construction du modèle
x_centre_reduit <- scale(x, apply(x, 2, mean), apply(x, 2, sd)) 
p2 <- x_centre_reduit %*% t(w) - svmlin$rho

### 1-8-3- Vérification du calcul des prédicteurs avec la fonction predict
p1 <- predict(svmlin, newdata = x, decision.value = T)
p1 <- attr(p1, "decision.values")
max(abs(p1-p2))

## 1-9- Obtenir les coefficients non centrée reduits
w / apply(x, 2, sd)


# 2- SVM à à noyau radial gaussien
## 2-1- choix des paramètres gamma (0.001, 0.01, 0.1, 1) et cout (0.1 à 10 avec un pas de 0.1)
swmrad <- tune.svm(cible, ~ . data = data_[id, vars], kernel = 'radial', cost = seq(0.1, 10, by = 0.1), 
                   gamma = c(0.001, 0.01, 0.1, 1), validation.x = test[,-20], validation.y = test[, 20], 
                   tunecontrol = tune.control(sampling = 'fix', fix = 1))

## 2-2- Resumé du modèle
summary(svmrad)

## 2-2- test$svmrad <- prediction(attr(test$svmrad, "probabilities")[,1], test$cible, label.ordering=c(0,1))
performance(pred, 'auc')@y.values[[1]] 

# 3- SVM à noyau radial laplacien
ksvmlap <- ksvm(cible ~., data = data_[id,vars], kernel="laplacedot", type="C-svc", prob.model = TRUE)

## 3-1- Affichage du modèle
ksvmlap

## 3-2- Prédiction
predksvm <- predict(ksvmlap, type='prob', test)
pred <- prediction(predklap[,2], test$cible, label.ordering=c(0,1))
performance(pred, 'auc')@y.values[[1]]

## 3-3- Simulation de la fonction tune pour le choix automatique de gamma et cout
maxauc <- fuction (x) {
    ksvmlap <- ksvm(cible ~., data = data_[id,vars], kernel='laplacedot', type = 'C-svc', C=x=[1], prob.model = TRUE, kpar=list(sigma=x[2]))
    pred <- prediction(predklap[,2], test$cible, label.ordering=c(0,1))
    performance(pred, 'auc')@y.values[[1]]
}

algo <- "Nelder-Mead"
est <- optim(c(1,0.05), maxauc, method=algo)
est$convergence
est$par
est$value

# 4- SVM à noyau sigmoide

# 5- SVM à noyau polynomial

# 6- SVM après réduction de dimension
## 6-1-ACM
### 6-1-1 Calcul du nombre de modalités des variables qualitatives
modal <- apply(data_[,-1], 2, function(x) nlevels(as.factor(x)))
modal

nb_facteur <- sum(modal) - ncol(data_[,-1])

### 6-1-2 ACM
ACM <- MCA(data_, ncp=nb_facteur, axes=c(1,2), graph=TRUE, quali.sup=14)

### 6-1-3 Inertie total (somme des valeurs propres au carré)
ACM$eig[,1]
(sum(modal)/ncol(data_[,-1])) - 1

### 6-2 SVM a noyau lineaire sur les axes factoriels
### 6-2-1 Modele
nb_axes <- 2
x <- ACM$ind$coord[churneur, 1:nb_axes]
y <- data_[churneur, "Cible"]

ksvmlin <- ksvm(x, y, type="C-svc", kernel="vanilladot", C=1, prob.model=TRUE)

### 6-2-1 Application du modèle à l'echantillon test
x_test <- ACM$ind$coord[-churneur, 1:nb_axes]
y_test < data_[churneur, "Cible"]

pred_ksvm <- predict(ksvmlin, type="prob", x_test)

### 6-2-3 performance du modèle
performance(pred, "auc")@y.values[[1]]

### 6-2-4 Recherche du nombre d'axe factoriel et coût optimal (boucle sur le bariable * cout)
f <- function(i) {
  ksvm <- svm(x, y, kernel="linear", cost=i, probaility=TRUE)
  predsvm <- prediction(attr(predksvm, "probability")[,1], y_test, label.ordering=c(0,1))
  performance(pred, "auc")@y.values[[1]]
}

for (n in seq(5, 20, by=1) {
  i <- seq(0.01, 2, by=0.01)
  x <- ACM$ind$coord[churneur, 1:n]
  y <- data_[churneur, "Cible"]
  x_test <- ACM$ind$coord[-churneur, 1:n]
  k <- vectorize(f)(i)
  cout <- i[which(k==max(k), arr.ind=TRUE)[1]]
  cat("\n", "nb facteurs = ", n, " AUC test max = ", max(k), " coût = ", cout)
}

# Radial gaussien
f <- function(i,j) {
  ksvm <- svm(x, y, kernel="radial", cost=i, gamma=j, probaility=TRUE)
  predksvm <- prediction(attr(predksvm, "probability")[,1], y_test, label.ordering=c(0,1))
  performance(pred, "auc")@y.values[[1]]
}

for (n in seq(5, 30, by=1) {
  i <- seq(0.05, 5, by=0.05)
  j <- seq(0.01, 0.5, by=0.01)
  x <- ACM$ind$coord[churneur, 1:n]
  y <- data_[churneur, "Cible"]
  x_test <- ACM$ind$coord[-churneur, 1:n]
  k <- outer(i, j, vectorize(f))
  cout <- i[which(k==max(k), arr.ind=TRUE)[1, 1]]
  gamma <- j[which(k==max(k), arr.ind=TRUE)[1, 2]]
  cat("\n", "nb facteurs = ", n, " AUC test max = ", max(k), " coût = ", cout, " gamma = ", gamma)
}

### 6-1-2
### 6-1-2
