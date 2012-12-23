# EuroScore is a mortality estimation model used in cardiovascular medicine
# filter the age scores by cut-off 
EuroScoreAdd.1 <- function(x,...) {
  if (is.null(x)) 
    stop("Dataframe must be specified", call. = FALSE)
  if (!is.data.frame(x)) {
    stop("Data must be a dataframe", call. = FALSE)
  }
 
 
  
  x$phi.em <- NULL
  x$phi.otcabg <- NULL
  x$phi.sta <- NULL
  x$phi.pisr <- NULL
  x$a.es <- NULL
  # age variable scoring by age groups
  x$phi.age <- NULL
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
  x$phi.sex <- NULL
  for (i in seq(along=x$sex)) {
  if(x$sex[i]==1){x$phi.sex[i]<-1} else {x$phi.sex[i]<- 0}
  }

  #Chronic Pulmonary disease variable
  x$phi.cpd <- NULL
  for (i in seq(along=x$cpd)) {
    if(x$cpd[i]==1){x$phi.cpd[i]<-1} else {x$phi.cpd[i]<- 0}
  }
 # Extracardiac arteriopathy
  x$phi.eca <- NULL
  for (i in seq(along=x$eca)) {
    if(x$eca[i]==1){x$phi.eca[i]<-1} else {x$phi.eca[i]<- 0}
  }

 # Neurological disfunction
  x$phi.nd <- NULL
    for (i in seq(along=x$nd)) {
    if(x$nd[i]==1){x$phi.nd[i]<-1} else {x$phi.nd[i]<- 0}
  }
 # Previous Cardiac Surgery
  x$phi.pcs <- NULL
  for (i in seq(along=x$pcs)) {
    if(x$pcs[i]==1){x$phi.pcs[i]<-1} else {x$phi.pcs[i]<- 0}
  }
 # Creatinin level > 200 µmol/ L
  x$phi.creat <- NULL
  for (i in seq(along=x$creat)) {
    if(x$creat[i]==1){x$phi.creat[i]<-1} else {x$phi.creat[i]<- 0}
  }
 # Active endocardities
  x$phi.ae <- NULL
  for (i in seq(along=x$ae)) {
    if(x$ae[i]==1){x$phi.ae[i]<-1} else {x$phi.ae[i]<- 0}
  }
 # Critical perioperative state
  x$phi.cps <- NULL
  for (i in seq(along=x$cps)) {
    if(x$cps[i]==1){x$phi.cps[i]<-1} else {x$phi.cps[i]<- 0}
  }
 # Anstable angina
  x$phi.ua <- NULL
  for (i in seq(along=x$ua)) {
    if(x$ua[i]==1){x$phi.ua[i]<-1} else {x$phi.ua[i]<- 0}
  }
 # LV function espresed as EF
  x$phi.lv.ef <- NULL
  for(i in seq(along=x$lv.ef)) {
  if (x$lv.ef[i]>50) { x$phi.lv.ef[i] <- 0 } else { 
    if (x$lv.ef[i]>30) { x$phi.lv.ef[i] <- 1 } else { x$phi.lv.ef[i] <- 3}
  }}
 # Recent myocardial infarction
  x$phi.rmi <- NULL
  for (i in seq(along=x$rmi)) {
    if(x$rmi[i]==1){x$phi.rmi[i]<-1} else {x$phi.rmi[i]<- 0}
  }
 # Pulmonary hypertension
  x$phi.ph <- NULL
  for (i in seq(along=x$ph)) {
    if(x$ph[i]==1){x$phi.ph[i]<-1} else {x$phi.ph[i]<- 0}
  }
 # 
  
  
  
  
  
  
  
  
  
  # addative score output
  for (i in seq(along=x$phi.age)){
    x$a.es[i] <- x$phi.age[i]+x$phi.sex[i]+x$phi.cpd[i]+x$phi.eca[i]+x$phi.nd[i]+x$phi.pcs[i]+ x$phi.creat[i]+x$phi.ae[i]+x$phi.cps[i]+x$phi.ua[i]+x$phi.lv.ef[i]+x$phi.rmi[i]+x$phi.ph[i]
  }
  #x<- data.frame(x,x$a.es)
  x.out <- subset(x,select=c(age,sex,cpd,eca,nd,pcs,creat,ae,cps,ua,lv.ef,rmi,ph,a.es))
  # return dataframe
  x.out <- data.frame(x.out)
  return(x.out)
}