# EuroScore is a mortality estimation model used in cardiovascular medicine
# filter the age scores by cut-off 
EuroScore <- function(x) {
  x$phi.age <- NULL
  x$a.es <- NULL
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
 return(x$phi.age)
}
# used the function done in excell and transformed into the R programming langage
# =IF(A2>94.9,8,SE(A2>89.9,7,SE(A2>84.9,6,SE(A2>79.9,5,SE(A2>74.9,4,SE(A2>69.9,3,SE(A2>64.9,2,SE(A2>59.9,1,0))))))))

# this is the basic logic behid the complex formula of EuroScore
# if (x$age[i]>59.99) { x$phi.age[i] <- 1 } else {x$phi.age[i] <- 0}


