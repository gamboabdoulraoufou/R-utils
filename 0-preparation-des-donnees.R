# Nettoyer la console R
rm(list = ls())
# Ctrl + L 

# Liste des variables
myVariablesNames <- c("Comptes", "Duree_credit", "Historique_credit", "Objet_credit", "Montant_credit", "Epargne", 
                      "Anciennete_emploi", "Taux_effort", "Situation_familiale", "Garanties", "Anciennete_domicile", "Biens", "Age", 
                      "Autres_credits", "Statut_domicile", "Nb_credits", "Type_emploi", "Nb_pers_charge", "Telephone", "Etranger", "Cible")

# Liste des variables qualitatives
varquali <- c("Comptes", "Epargne", "Historique_credit", "Objet_credit", "Situation_familiale", "Garanties",
              "Biens", "Autres_credits", "Statut_domicile", "Type_emploi", "Anciennete_emploi", "Telephone",
              "Nb_pers_charge")

# Liste des variables quantitavives
varquanti <- c("Duree_credit", "Montant_credit", "Taux_effort", "Anciennete_domicile", "Age", "Nb_credits")

# Chargement des données
credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data", h=FALSE, 
                    col.names=myVariablesNames)

# Ajout d'un ID
credit$Cle <- seq(1, nrow(credit))

# Recodage des variables
credit$Comptes <- as.factor(substr(credit$Compte,3,3))
credit$Anciennete_emploi <- as.factor(substr(credit$Anciennete_emploi,3,3))
credit$Epargne <- as.factor(substr(credit$Epargne,3,3))
credit$Etranger <- NULL
credit$Cible[credit$Cible == 1] <- 0
credit$Cible[credit$Cible == 2] <- 1
credit$Cible <- factor(credit$Cible)

# Conversion des variables qualitatives en facteur
indices <- names(credit) %in% varquali
for (i in (1:ncol(credit))) {
  if (indices[i] == 1) {
    credit[,i] <- factor(credit[,i])
  }
}

# Creation de la liste des variables
vars <- -grep('Cle|Cible', names(credit))

# Echantillonnage
set.seed(235)
s=sort(sample(nrow(credit), nrow(credit)*2/3, replace=F))

train <- credit[s,]
test <- credit[-s,]
