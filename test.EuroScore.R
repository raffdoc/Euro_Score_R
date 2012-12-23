age <- c(95,90,85,80,75,70,65,60,55,50)
sex <- c(1,0,1,0,1,0,1,0,1,0)
cpd <- c(1,1,1,0,1,0,1,0,1,0)
eca <- c(1,0,0,0,1,0,1,0,0,0)

x <- data.frame(age,sex,cpd,eca)
source("EuroScore.R")
EuroScoreAdd.1(x)
a<- 0
EuroScoreAdd.1(a)




#