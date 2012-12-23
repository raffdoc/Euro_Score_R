age <- c(95,90,85,80,75,70,65,60,55,50)
sex <- c(1,0,1,0,1,0,1,0,1,0)
cpd <- c(1,1,1,0,1,0,1,0,1,0)

x <- data.frame(age,sex,cpd)
source("EuroScore.R")
EuroScoreAdd(x)
a<- 0
EuroScoreAdd(a)




#