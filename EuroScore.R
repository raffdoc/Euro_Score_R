# EuroScore is a mortality estimation model used in cardiovascular medicine
EuroScoreAdd.1 <- function(x,...) {
  if (is.null(x)) 
    stop("Dataframe must be specified", call. = FALSE)
  if (!is.data.frame(x)) {
    stop("Data must be a dataframe", call. = FALSE)
  }
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

  #Chronic Pulmonary disease (Long term use of bronchodilators or steroids for lung disease)
  x$phi.cpd <- NULL
  for (i in seq(along=x$cpd)) {
    if(x$cpd[i]==1){x$phi.cpd[i]<-1} else {x$phi.cpd[i]<- 0}
  }
 # Extracardiac arteriopathy (One or more of claudication, carotid occlusion  or >50% stenosis, previous or planned intervention on the abdominal aorta, limb arteries or carotids)
  x$phi.eca <- NULL
  for (i in seq(along=x$eca)) {
    if(x$eca[i]==1){x$phi.eca[i]<-2} else {x$phi.eca[i]<- 0}
  }

 # Neurological disfunction (Disease severely affecting ambulation or day-to-day functioning)
  x$phi.nd <- NULL
    for (i in seq(along=x$nd)) {
    if(x$nd[i]==1){x$phi.nd[i]<-2} else {x$phi.nd[i]<- 0}
  }
 # Previous Cardiac Surgery
  x$phi.pcs <- NULL
  for (i in seq(along=x$pcs)) {
    if(x$pcs[i]==1){x$phi.pcs[i]<-3} else {x$phi.pcs[i]<- 0}
  }
 # Creatinin level > 200 µmol/ L
  x$phi.creat <- NULL
  for (i in seq(along=x$creat)) {
    if(x$creat[i]==1){x$phi.creat[i]<-2} else {x$phi.creat[i]<- 0}
  }
 # Active endocardities (Patient still on antibiotic treatment for endocarditis at time of surgery)
  x$phi.ae <- NULL
  for (i in seq(along=x$ae)) {
    if(x$ae[i]==1){x$phi.ae[i]<-3} else {x$phi.ae[i]<- 0}
  }
 # Critical perioperative state (Ventricular Tachycardia / Ventricular Fibrillation or aborted sudden death, preoperative cardiac massage, preoperative ventilation before anaesthetic room, preoperative inotropes or IABP, preoperative Acute Renal Failure (anuria or oliguria <10ml/hr))
  x$phi.cps <- NULL
  for (i in seq(along=x$cps)) {
    if(x$cps[i]==1){x$phi.cps[i]<-3} else {x$phi.cps[i]<- 0}
  }
 # Anstable angina ( Rest angina requiring i.v. nitrates until arrival in anaesthetic room)
  x$phi.ua <- NULL
  for (i in seq(along=x$ua)) {
    if(x$ua[i]==1){x$phi.ua[i]<-2} else {x$phi.ua[i]<- 0}
  }
 # LV function espresed as EF
  x$phi.lv.ef <- NULL
  for(i in seq(along=x$lv.ef)) {
  if (x$lv.ef[i]>50) { x$phi.lv.ef[i] <- 0 } else { 
    if (x$lv.ef[i]>30) { x$phi.lv.ef[i] <- 1 } else { x$phi.lv.ef[i] <- 3}
  }}
 # Recent myocardial infarction (Myocardial infarction within 90 days)
  x$phi.rmi <- NULL
  for (i in seq(along=x$rmi)) {
    if(x$rmi[i]==1){x$phi.rmi[i]<-2} else {x$phi.rmi[i]<- 0}
  }
 # Pulmonary hypertension (Systolic pulmonary artery pressure >60mmHg)
  x$phi.ph <- NULL
  for (i in seq(along=x$ph)) {
    if(x$ph[i]==1){x$phi.ph[i]<-2} else {x$phi.ph[i]<- 0}
  }
 # Emergency (Operation before beginning of next working day)
  x$phi.em <- NULL
  for (i in seq(along=x$em)) {
    if(x$em[i]==1){x$phi.em[i]<-2} else {x$phi.em[i]<- 0}
  }
 # Other then isolated CABG 
  x$phi.ot.icabg <- NULL
  for (i in seq(along=x$ot.icabg)) {
    if(x$ot.icabg[i]==1){x$phi.ot.icabg[i]<-2} else {x$phi.ot.icabg[i]<- 0}
  }
 # Surgery on thoracic aorta
  x$phi.sta <- NULL
  for (i in seq(along=x$sta)) {
    if(x$sta[i]==1){x$phi.sta[i]<-3} else {x$phi.sta[i]<- 0}
  }
 # Post infactual septal rupture
  x$phi.pisr <- NULL
  for (i in seq(along=x$pisr)) {
    if(x$pisr[i]==1){x$phi.pisr[i]<-4} else {x$phi.pisr[i]<- 0}
  }
  # addative score output
  for (i in seq(along=x$phi.age)){
    x$a.es[i] <- x$phi.age[i]+x$phi.sex[i]+x$phi.cpd[i]+x$phi.eca[i]+x$phi.nd[i]+x$phi.pcs[i]+ x$phi.creat[i]+x$phi.ae[i]+x$phi.cps[i]+x$phi.ua[i]+x$phi.lv.ef[i]+x$phi.rmi[i]+x$phi.ph[i]+x$phi.em[i]+x$phi.ot.icabg[i]+x$phi.sta[i]+x$phi.pisr[i]
  }
  #x<- data.frame(x,x$a.es)
  x.out <- subset(x,select=c(age,sex,cpd,eca,nd,pcs,creat,ae,cps,ua,lv.ef,rmi,ph,em,ot.icabg,sta,pisr,a.es))
  # return dataframe
  x.out <- data.frame(x.out)
  #return(x.out)
  assign("x.out",x.out,pos=.GlobalEnv)
}


# EuroScore Logistic 1
EuroScoreLog.1  <- function(x,...) {
  if (is.null(x)) 
    stop("Dataframe must be specified", call. = FALSE)
  if (!is.data.frame(x)) {
    stop("Data must be a dataframe", call. = FALSE)
  }
    x$log.es <- NULL
  # age variable scoring by age groups
  x$log.age <- NULL
  for(i in seq(along=x$age)) { 
    if (x$age[i]< 59) { x$log.age[i] <- 0.0666354 } else {x$log.age[i] <- 0.0666354*(x$age[i]-58)}
  }
  #sex variable
  x$log.sex <- NULL
  for (i in seq(along=x$sex)) {
    if(x$sex[i]==1){x$log.sex[i]<-0.3304052} else {x$log.sex[i]<- 0}
  }
  
  #Chronic Pulmonary disease (Long term use of bronchodilators or steroids for lung disease)
  x$log.cpd <- NULL
  for (i in seq(along=x$cpd)) {
    if(x$cpd[i]==1){x$log.cpd[i]<-0.4931341} else {x$log.cpd[i]<- 0}
  }
  # Extracardiac arteriopathy (One or more of claudication, carotid occlusion  or >50% stenosis, previous or planned intervention on the abdominal aorta, limb arteries or carotids)
  x$log.eca <- NULL
  for (i in seq(along=x$eca)) {
    if(x$eca[i]==1){x$log.eca[i]<-0.6558917} else {x$log.eca[i]<- 0}
  }
  
  # Neurological disfunction (Disease severely affecting ambulation or day-to-day functioning)
  x$log.nd <- NULL
  for (i in seq(along=x$nd)) {
    if(x$nd[i]==1){x$log.nd[i]<-0.6558917} else {x$log.nd[i]<- 0}
  }
  # Previous Cardiac Surgery
  x$log.pcs <- NULL
  for (i in seq(along=x$pcs)) {
    if(x$pcs[i]==1){x$log.pcs[i]<-1.002625} else {x$log.pcs[i]<- 0}
  }
  # Creatinin level > 200 µmol/ L
  x$log.creat <- NULL
  for (i in seq(along=x$creat)) {
    if(x$creat[i]==1){x$log.creat[i]<-0.6521653} else {x$log.creat[i]<- 0}
  }
  # Active endocardities (Patient still on antibiotic treatment for endocarditis at time of surgery)
  x$log.ae <- NULL
  for (i in seq(along=x$ae)) {
    if(x$ae[i]==1){x$log.ae[i]<-1.101265} else {x$log.ae[i]<- 0}
  }
  # Critical perioperative state (Ventricular Tachycardia / Ventricular Fibrillation or aborted sudden death, preoperative cardiac massage, preoperative ventilation before anaesthetic room, preoperative inotropes or IABP, preoperative Acute Renal Failure (anuria or oliguria <10ml/hr))
  x$log.cps <- NULL
  for (i in seq(along=x$cps)) {
    if(x$cps[i]==1){x$log.cps[i]<-0.9058132} else {x$log.cps[i]<- 0}
  }
  # Anstable angina ( Rest angina requiring i.v. nitrates until arrival in anaesthetic room)
  x$log.ua <- NULL
  for (i in seq(along=x$ua)) {
    if(x$ua[i]==1){x$log.ua[i]<-0.9058132} else {x$log.ua[i]<- 0}
  }
  # LV function espresed as EF
  x$log.lv.ef <- NULL
  for(i in seq(along=x$lv.ef)) {
    if (x$lv.ef[i]>50) { x$log.lv.ef[i] <- 0 } else { 
      if (x$lv.ef[i]>30) { x$log.lv.ef[i] <- 0.4191643 } else { x$log.lv.ef[i] <- 1.094443}
    }}
  # Recent myocardial infarction (Myocardial infarction within 90 days)
  x$log.rmi <- NULL
  for (i in seq(along=x$rmi)) {
    if(x$rmi[i]==1){x$log.rmi[i]<-0.5460218} else {x$log.rmi[i]<- 0}
  }
  # Pulmonary hypertension (Systolic pulmonary artery pressure >60mmHg)
  x$log.ph <- NULL
  for (i in seq(along=x$ph)) {
    if(x$ph[i]==1){x$log.ph[i]<-0.7676924} else {x$log.ph[i]<- 0}
  }
  # Emergency (Operation before beginning of next working day)
  x$log.em <- NULL
  for (i in seq(along=x$em)) {
    if(x$em[i]==1){x$log.em[i]<-0.7127953} else {x$log.em[i]<- 0}
  }
  # Other then isolated CABG 
  x$log.ot.icabg <- NULL
  for (i in seq(along=x$ot.icabg)) {
    if(x$ot.icabg[i]==1){x$log.ot.icabg[i]<-0.5420364} else {x$log.ot.icabg[i]<- 0}
  }
  # Surgery on thoracic aorta
  x$log.sta <- NULL
  for (i in seq(along=x$sta)) {
    if(x$sta[i]==1){x$log.sta[i]<-1.159787} else {x$log.sta[i]<- 0}
  }
  # Post infactual septal rupture
  x$log.pisr <- NULL
  for (i in seq(along=x$pisr)) {
    if(x$pisr[i]==1){x$log.pisr[i]<- 1.159787} else {x$log.pisr[i]<- 0}
  }
  # addative score output
  for (i in seq(along=x$log.age)){
    x$log.es[i] <- exp(-4.789594+x$log.age[i]+x$log.sex[i]+x$log.cpd[i]+x$log.eca[i]+x$log.nd[i]+x$log.pcs[i]+ x$log.creat[i]+x$log.ae[i]+x$log.cps[i]+x$log.ua[i]+x$log.lv.ef[i]+x$log.rmi[i]+x$log.ph[i]+x$log.em[i]+x$log.ot.icabg[i]+x$log.sta[i]+x$log.pisr[i]
    )/(1+exp(-4.789594+x$log.age[i]+x$log.sex[i]+x$log.cpd[i]+x$log.eca[i]+x$log.nd[i]+x$log.pcs[i]+ x$log.creat[i]+x$log.ae[i]+x$log.cps[i]+x$log.ua[i]+x$log.lv.ef[i]+x$log.rmi[i]+x$log.ph[i]+x$log.em[i]+x$log.ot.icabg[i]+x$log.sta[i]+x$log.pisr[i]
    ))}
  #x<- data.frame(x,x$log.es)
  x.out <- subset(x,select=c(age,sex,cpd,eca,nd,pcs,creat,ae,cps,ua,lv.ef,rmi,ph,em,ot.icabg,sta,pisr,log.es))
  # return dataframe
  x.out <- data.frame(x.out)
  assign("x.out",x.out,pos=.GlobalEnv)
}


# EuroScore II ( only logistic)

EuroScoreLog.II  <- function(x,...) {
  if (is.null(x)) 
    stop("Dataframe must be specified", call. = FALSE)
  if (!is.data.frame(x)) {
    stop("Data must be a dataframe", call. = FALSE)
  }
  x$log.es <- NULL
  # age variable scoring by age groups
  x$log.age <- NULL
  for(i in seq(along=x$age)) { 
    if (x$age[i]< 60) { x$log.age[i] <- 0.0285181 } else {x$log.age[i] <- 0.0285181*(x$age[i]-60)}
  }
  #sex variable
  x$log.sex <- NULL
  for (i in seq(along=x$sex)) {
    if(x$sex[i]==1){x$log.sex[i]<-0.2196434} else {x$log.sex[i]<- 0}
  }
  
  # Renal impairment - there are now 3 categories based on creatinine clearance calculated using Cockcroft-Gault formula. Unlike serum creatinine in the old EuroSCORE model, some of the weighting for age is directly incorporated into this factor, as age is a component of creatinine clearance. The 3 categories are:
  # Creatinine clearance (ml/min) =   (140-age (years))   x   weight (kg)   x   (0.85 if female)    /   [72 x serum creatinine (mg/dl)]
  # on dialysis (regardless of serum creatinine level) code for database is 4
  # severely impaired renal function (<50 ml/min) off dialysis database is 3
  # moderately impaired renal function (50-85 ml/min) code for database is 2
  # normal renal function ( > 85 ml/min) code for database 1
  x$log.ri <- NULL
  for(i in seq(along=x$ri)) { 
    if (x$ri[i]==1) { x$log.ri[i] <- 0 } else { 
      if (x$ri[i]==2) { x$log.ri[i] <- 0.303553 } else { 
        if (x$ri[i]==3) { x$log.ri[i] <- 0.8592256 } else {x$log.ri[i] <- 0.6421508}
      }}} 
  
  # Extracardiac arteriopathy - one or more of the following
  # claudication
  # carotid occlusion  or >50% stenosis
  # amputation for arterial disease
  # previous or planned intervention on the abdominal aorta, limb arteries or carotids 
  x$log.eca <- NULL
  for (i in seq(along=x$eca)) {
    if(x$eca[i]==1){x$log.eca[i]<- 0.5360268} else {x$log.eca[i]<- 0}
  }
  # Poor mobility - severe impairment of mobility secondary to musculoskeletal 
  # or neurological dysfunction
  x$log.pm <- NULL
  for (i in seq(along=x$pm)) {
    if(x$pm[i]==1){x$log.pm[i]<- 0.2407181} else {x$log.pm[i]<- 0}
  }
  # Previous Cardiac Surgery
  x$log.pcs <- NULL
  for (i in seq(along=x$pcs)) {
    if(x$pcs[i]==1){x$log.pcs[i]<-1.118599} else {x$log.pcs[i]<- 0}
  }
  # Chronic lung disease - long term use of bronchodilators or steroids for lung disease
  x$log.cld <- NULL
  for (i in seq(along=x$cld)) {
    if(x$cld[i]==1){x$log.cld[i]<-0.1886564} else {x$log.cld[i]<- 0}
  }

  # Active endocarditis - patient still on antibiotic treatment for endocarditis at time of surgery
  x$log.ae <- NULL
  for (i in seq(along=x$ae)) {
    if(x$ae[i]==1){x$log.ae[i]<-0.6194522} else {x$log.ae[i]<- 0}
  }
  #  Critical preoperative state   ventricular tachycardia or ventricular fibrillation or aborted sudden death, preoperative cardiac massage, preoperative ventilation before anaesthetic room, preoperative inotropes or IABP, preoperative acute renal failure (anuria or oliguria <10ml/hr)
  x$log.cps <- NULL
  for (i in seq(along=x$cps)) {
    if(x$cps[i]==1){x$log.cps[i]<-1.086517} else {x$log.cps[i]<- 0}
  }
  # Diabetis on insulin therapy
  x$log.doi <- NULL
  for (i in seq(along=x$doi)) {
    if(x$doi[i]==1){x$log.doi[i]<-1.086517} else {x$log.doi[i]<- 0}
  }
  # NYHA class 
  # codes for the class are
  # I code is 1
  # II code is 2
  # III code is 3
  # IV code is 4
  x$log.nyha <- NULL
  for(i in seq(along=x$nyha)) { 
    if (x$nyha[i]==1) { x$log.nyha[i] <- 0 } else { 
      if (x$nyha[i]==2) { x$log.nyha[i] <- 0.1070545 } else { 
        if (x$nyha[i]==3) { x$log.nyha[i] <- 0.2958358 } else {x$log.nyha[i] <- 0.5597929}
      }}} 
  # CCS class 4 angina    angina at rest
  x$log.ccs <- NULL
  for (i in seq(along=x$ccs)) {
    if(x$ccs[i]==1){x$log.ccs[i]<-0.6558917} else {x$log.ccs[i]<- 0}
  }

  # LV function espresed as EF
  # good EF > 50 % 
  # moderate EF 31-50 % 
  # poor EF 21-30 % 
  # very poor < 21 % 
  x$log.lv.ef <- NULL
  for(i in seq(along=x$lv.ef)) { 
    if (x$lv.ef[i]>50) { x$log.lv.ef[i] <- 0 } else { 
      if (x$lv.ef[i]>31) { x$log.lv.ef[i] <- 0.3150652 } else { 
        if (x$lv.ef[i]>21) { x$log.lv.ef[i] <- .8084096 } else {x$log.lv.ef[i] <- 0.9346919}
      }}} 
  # Recent myocardial infarction (Myocardial infarction within 90 days)
  x$log.rmi <- NULL
  for (i in seq(along=x$rmi)) {
    if(x$rmi[i]==1){x$log.rmi[i]<-0.1528943} else {x$log.rmi[i]<- 0}
  }
  # Pulmonary hypertension   systolic pulmonary artery pressure, now in 3 classes 
  # no pulmonary hypertension < 31 mm Hg  code 1 
  # moderate: PA systolic pressure (31-55 mm Hg) code 2
  # severe: PA systolic pressure (>55mm Hg) code 3
  x$log.ph <- NULL
  for(i in seq(along=x$ph)) { 
    if (x$ph[i]==1) { x$log.ph[i] <- 0 } else {
      if (x$ph[i]==2) {x$log.ph[i] <- 0.1788899 } else { x$log.ri[i] <- 0.3491475}
      }} 
  #  Urgency   now four classes:
   # elective : routine admission for operation. code is 1
  # urgent: patients who have not been electively admitted for operation but who require intervention or surgery on the current admission for medical reasons. These patients cannot be sent home without a definitive procedure. code is 2
  # emergency: operation before the beginning of the next working day after decision to operate. code is 3
  # salvage: patients requiring cardiopulmonary resuscitation (external cardiac massage) en route to the operating theatre or prior to induction of anaesthesia. This does not include cardiopulmonary resuscitation following induction of anaesthesia code is 4
  x$log.em <- NULL
  for(i in seq(along=x$em)) { 
    if (x$em[i]==1) { x$log.em[i] <- 0 } else { 
      if (x$em[i]==2) { x$log.em[i] <- 0.3174673 } else { 
        if (x$em[i]==3) { x$log.em[i] <- 0.7039121 } else {x$log.em[i] <- 1.362947}
      }}}
  # Weight of the intervention - include major interventions on the heart such as
  #  isolated CABG code is 1
  # single non CABG intervention code is 2
  # two procedures 3
  # three procedures 4
  x$log.woi <- NULL
  for(i in seq(along=x$woi)) { 
    if (x$woi[i]==1) { x$log.woi[i] <- 0 } else { 
      if (x$woi[i]==2) { x$log.woi[i] <- 0.0062118 } else { 
        if (x$woi[i]==3) { x$log.woi[i] <- 0.5521478 } else {x$log.woi[i] <- 0.9724533}
      }}}
  # Surgery on thoracic aorta
  x$log.sta <- NULL
  for (i in seq(along=x$sta)) {
    if(x$sta[i]==1){x$log.sta[i]<-0.6527205 } else {x$log.sta[i]<- 0}
  }
  
  # addative score output
  for (i in seq(along=x$log.age)){
    x$log.es[i] <- exp(-5.324537+x$log.age[i]+x$log.sex[i]+x$log.ri[i]+x$log.eca[i]+x$log.pm[i]+x$log.pcs[i]+x$log.cld[i]+x$log.ae[i]+x$log.cps[i]+x$log.doi[i]+x$log.nyha[i]+x$log.ccs[i]+x$log.lv.ef[i]+x$log.rmi[i]+x$log.ph[i]+x$log.em[i]+x$log.woi[i]+x$log.sta[i]
    )/(1+exp(-5.324537+x$log.age[i]+x$log.sex[i]+x$log.ri[i]+x$log.eca[i]+x$log.pm[i]+x$log.pcs[i]+x$log.cld[i]+x$log.ae[i]+x$log.cps[i]+x$log.doi[i]+x$log.nyha[i]+x$log.ccs[i]+x$log.lv.ef[i]+x$log.rmi[i]+x$log.ph[i]+x$log.em[i]+x$log.woi[i]+x$log.sta[i]
    ))}
  #x<- data.frame(x,x$log.es)
  x.out <- subset(x,select=c(age,sex,ri,eca,pm,pcs,cld,ae,cps,doi,nyha,ccs,lv.ef,rmi,ph,em,woi,sta,log.es))
  # return dataframe
  x.out <- data.frame(x.out)
  assign("x.out",x.out,pos=.GlobalEnv)
}





