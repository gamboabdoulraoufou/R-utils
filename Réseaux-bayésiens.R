### TEST de NAIVE-BAYES
### tiré de la video Strata 2013
### Just the Basics: Core Data Science Skills with Kaggle's Top Competitors .
### William Cukierski (Kaggle), Ben Hamner (Kaggle)
### http://my.safaribooksonline.com/9781449365394/oreillyvideos1562197
 
library("ggplot2")
library("assertive")
 
myBudget <- 3000;
 
data <- diamonds;
 
assert_all_are_not_na(data$price);
assert_all_are_finite(c(data$price, data$carat));
 
data <- subset(data, select=c("price", "carat"));
data$affordable <- data$price < myBudget;
 
g <-function(df,col,val) {
subset(df, eval(parse(text=col)) == val)
};
 
#getPrior(data, "carat",1)
getPrior <- function(df, class, val){
nrow(g(df,class,val)) / nrow(data)
};
 
# P(feat=featval|class=val)
# getLikelihood(data,"affordable",TRUE,"carat",1)
# = %age de diam de prix abordable de 1 carat
getLikelihood <- function(df, class, val, feat, featval){
nrow(g(g(df,class,val),feat,featval)) / nrow(g(df,class,val))
}
 
#P(B|A)P(A)/P(B)
getPosterior <- function(car){
getPrior(data,"affordable",TRUE) * getLikelihood(data,"affordable",TRUE,"carat",car) / getPrior(data, "carat", car)
};
 
uniqCarats <- sort(unique(data$carat));
prediction <- sapply(uniqCarats,getPosterior);
 
qplot(uniqCarats,prediction) 
