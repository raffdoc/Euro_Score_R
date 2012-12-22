# EuroScore is a mortality estimation model used in cardiovascular medicine
# filter the age scores by cut-off 
EuroScoreAdd <- function(x) {
  x$phi.age <- NULL
  x$phi.sex <- NULL
  x$phi.cpd <- NULL
  x$phi.eca <- NULL
  x$phi.nd <- NULL
  x$phi.pcs <- NULL
  x$phi.creat <- NULL
  x$phi.ae <- NULL
  x$phi.cps <- NULL
  x$phi.ua <- NULL
  x$phi.lv.mo <- NULL
  x$phi.rmi <- NULL
  x$phi.ph <- NULL
  x$phi.em <- NULL
  x$phi.otcabg <- NULL
  x$phi.sta <- NULL
  x$phi.pisr <- NULL
  x$a.es <- NULL
  # age variable scoring
 for(i in seq(along=x$age)) { 
     if (x$age[i]>94.99) { x$phi.age[i] <- 8 } else { 
       if (x$age[i]>89.99) { x$phi.age[i] <- 7 } else { 
         if (x$age[i]>84.99) { x$phi.age[i] <- 6 } else {
           if (x$age[i]>79.99) { x$phi.age[i] <- 5 } else {
             if (x$age[i]>74.99) { x$phi.age[i] <- 4 } else {
               if (x$age[i]>69.99) { x$phi.age[i] <- 3 } else {
                 if (x$age[i]>64.99) { x$phi.age[i] <- 2 } else {
                   if (x$age[i]>59.99) { x$phi.age[i] <- 1 } else {
                     x$phi.age[i] <- 0}}}}}}}}
   }
 #sex variable
  for (i in seq(along=x$sex)){
  if(x$sex[i]==1){x$phi.sex[i]<-1} else {x$phi.sex[i]<- 0}
  }
 # addative score output
  for (i in seq(along=x$a.es)){
  x$a.es[i] <- sum(x$phi.age[i]+x$phi.sex[i])
  }
  x<- data.frame(x,x$a.es)
  return(x)
}