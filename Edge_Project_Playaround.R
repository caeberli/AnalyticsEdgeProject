library(modelr) 
library(tidyverse)
library(corrplot)
library(dplyr)
library(leaps)
library(glmnet)
library(tibble)
library(softImpute)

#reading in the data
rest_accepts = read.csv("chefmozaccepts.csv")
rest_cuisine = read.csv("chefmozcuisine.csv")
rest_hours = read.csv("chefmozhours4.csv")
rest_parking = read.csv("chefmozparking.csv")
geo_places = read.csv("geoplaces2.csv")
ratings = read.csv("rating_final.csv")
user_cuisine = read.csv("usercuisine.csv")
user_payment = read.csv("userpayment.csv")
user_profile = read.csv("userprofile.csv")


set.seed(101)
n=200
p=100
J=50
np=n*p
missfrac=0.3
x=matrix(rnorm(n*J),n,J)%*%matrix(rnorm(J*p),J,p)+matrix(rnorm(np),n,p)/5
ix=seq(np)
imiss=sample(ix,np*missfrac,replace=FALSE)
xna=x
xna[imiss]=NA
xnaC=as(xna,"Incomplete")
### here we do it a different way to demonstrate Incomplete
### In practise the observed values are stored in this market-matrix format.
i = row(xna)[-imiss]
j = col(xna)[-imiss]
xnaC=Incomplete(i,j,x=x[-imiss])







simple_rating = ratings[,1:3]

rows = sample(nrow(simple_rating))
rand_simple_rating = simple_rating[rows,]

cutoff = as.integer(0.7 * nrow(rand_simple_rating))

for (i in (1:nrow(rand_simple_rating))) {
  rand_simple_rating[i,1] = substring(rand_simple_rating[i,1],2)
}
rand_simple_rating$userID = as.integer(rand_simple_rating$userID)

traindata = rand_simple_rating[1:cutoff,]
validdata = rand_simple_rating[(cutoff+1):nrow(rand_simple_rating),]

nusers = length(unique(rand_simple_rating[,1]))
nrest = length(unique(rand_simple_rating[,2]))

uniqueusers = unique(rand_simple_rating[,1])
uniquerest = unique(rand_simple_rating[,2])

m = matrix(NA, nusers, nrest)
for (i in 1:(nrow(traindata))) {
  user = traindata[i,1]
  rest = traindata[i,2]
  rating = traindata[i,3]
  m[which(uniqueusers %in% c(user)), which(uniquerest %in% c(rest))] = rating
}


fit = softImpute(m, rank.max=3, lambda=0, maxit=1000)
fullm = complete(m,fit)

userarche = fit$u
archemovie = fit$v * fit$d

errors = c()
for (i in 1:(nrow(validdata))) {
  user = validdata[i,1]
  rest = validdata[i,2]
  predictedrating = fullm[which(uniqueusers %in% c(user)), which(uniquerest %in% c(rest))]
  
  if (predictedrating < 0 || predictedrating < 0.5) {
    predictedrating = 0
  }
  else if (predictedrating > 2 || predictedrating >= 1.5) {
    predictedrating = 2
  }
  else {
    predictedrating = 1
  }
  
  truerating = validdata[i,3]
  errors = append(errors, abs(predictedrating-truerating))
  cat("\n pred: ", predictedrating)
  cat("\n true: ", truerating)
}

print(mean(errors))
errors







