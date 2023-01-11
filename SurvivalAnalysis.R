#packages
library(tidyverse)
library(janitor)
library(gtsummary)
library(mice)
library(readxl)
library(fastDummies)
library(reshape2)
library(ggplot2)
library(flextable)
library(dcurves)
library(hdnom)
library(survminer)
library(survival)

#Data import
{
Dataset=read_excel("C:/Users/tlepa/OneDrive - Universite de Montreal/Projet mammites Sévères/Bases de données/Dataset.xlsx", sheet="Sheet1")
View(Dataset)

Dataset=subset(Dataset,select = -c(Died,Culled))

Dataset <- within(Dataset, {
  Bacteriology <- as.factor(Bacteriology)
  Breed <- as.factor(Breed)
  Dehydration <- as.factor(Dehydration)
  `Faeces Aspect` <- as.factor(`Faeces Aspect`)
  `Faeces Quantity` <- as.factor(`Faeces Quantity`)
  `General State` <- as.factor(`General State`)
  `Intrammary Antimicrobial` <- as.factor(`Intrammary Antimicrobial`)
  `Milk Examination` <- as.factor(`Milk Examination`)
  `Mucosal Aspect` <- as.factor(`Mucosal Aspect`)
  NSAID <- as.factor(NSAID)
  Prognosis <- as.factor(Prognosis)
  `Quarter examination` <- as.factor(`Quarter examination`)
  `Recapillary Filling Time` <- as.factor(`Recapillary Filling Time`)
  `Ruminal Contractions` <- as.factor(`Ruminal Contractions`)
  `Systemic Antimicrobial` <- as.factor(`Systemic Antimicrobial`)
  `Vet Practice` <- as.factor(`Vet Practice`)
  `Mean Corpuscular Hemoglobin`<- as.numeric(`Mean Corpuscular Hemoglobin`)
  `Lactation Number`<- as.numeric(`Lactation Number`)
  EventCulled<- as.logical(EventCulled)
  EventDeath<- as.logical(EventDeath)
  Censored<-as.logical(Censored)
  Time<-as.numeric(Time)
})

names(Dataset)[c(14)] <- c("Capillary Refilling Time")

Dataset=within(Dataset,{
  Aminolean <- as.logical(Aminolean)
  Calcium <- as.logical(Calcium)
  `CMT Score` <- as.factor(`CMT Score`)
  Dextrose <- as.logical(Dextrose)
  `Downer Cow` <- as.logical(`Downer Cow`)
  Hypertonic <- as.logical(Hypertonic)
  Isotonic <- as.logical(Isotonic)
  `Mastitis Historic` <- as.logical(`Mastitis Historic`)
  `Oral Fluids` <- as.logical(`Oral Fluids`)
  `Several affected quarters` <- as.logical(`Several affected quarters`)
  `Toxic Neutrophils` <- as.logical(`Toxic Neutrophils`)
  `Treatment before visit` <- as.logical(`Treatment before visit`)
})

Dataset$Dehydration <- with(Dataset, factor(Dehydration, levels=c('Absence','Light','Moderate','Severe'), ordered=TRUE))
Dataset$`Faeces Quantity` <- with(Dataset, factor(`Faeces Quantity`, levels=c('Normal','Low','None'), ordered=TRUE))
Dataset$`General State` <- with(Dataset, factor(`General State`, levels=c('Alert','Slightly Depressed','Severely Depressed'), ordered=TRUE))
Dataset$`CMT Score` <- with(Dataset, factor(`CMT Score`, levels=c('1','2','3'), ordered=TRUE))
Dataset$`Capillary Refilling Time` <- with(Dataset, factor(`Capillary Refilling Time`, levels=c('Minus 2s','2s','Plus 2s'), ordered=TRUE))
Dataset$Prognosis <- with(Dataset, factor(Prognosis, levels=c('Good','Fair','Poor'), ordered=TRUE))
Dataset$`Mucosal Aspect` <- with(Dataset, factor(`Mucosal Aspect`, levels = c('Normal','Pale','Congested')))

View(Dataset)
}

#Descriptive statistics
{
Resume_commemoratif=Dataset %>%
  dplyr::select(`Vet Practice`,Breed,`Days in milk`,`Lactation Number`,`Treatment before visit`,`Mastitis Historic`,EventCulled) %>%
  tbl_summary(
    missing = "no",
    type = list(`Lactation Number` ~ "continuous")
  ) %>%
  add_n() %>%
  bold_labels()%>%
  as_flex_table()

Resume_examclinique=Dataset %>% 
  dplyr::select(EventCulled,`Downer Cow`,`General State`,Appetite,Temperature,`Heart Rate`,`Respiratory Rate`,`Mucosal Aspect`,`Capillary Refilling Time`,`Ruminal Frequency`,`Ruminal Contractions`,`Dehydration`,`Faeces Aspect`,`Faeces Quantity`) %>%
  tbl_summary(
    by = EventCulled,
    missing = "no",
    type = list(`Ruminal Frequency` ~ "continuous"),
    digits = list(`Ruminal Frequency`~1),
    label = list(`Ruminal Frequency`~"Ruminal Frequency (/2mn)",
                 Appetite ~ "Appetite (compared to normal)",
                 `Ruminal Contractions`~"Ruminal Contractions Description")
  ) %>%
  add_n() %>%
  bold_labels()%>%
  add_overall() %>%
  add_p() %>%
  as_flex_table() 
print(Resume_examclinique)

Resume_mammaire=Dataset %>%
  dplyr::select(EventCulled,`Several affected quarters`,`CMT Score`,`Quarter examination`,`Milk Examination` ) %>%
  tbl_summary(
    missing = "no",
    by= EventCulled
  ) %>%
  add_n() %>%
  bold_labels()%>%
  add_overall() %>%
  add_p() %>%
  as_flex_table()
print(Resume_mammaire)

Resume_traitement=Dataset %>%
  dplyr::select(`Vet Practice`,Calcium,Aminolean,Dextrose,Isotonic,Hypertonic,`Oral Fluids`,NSAID,`Systemic Antimicrobial`,`Intrammary Antimicrobial`) %>%
  tbl_summary(
    missing = "no",
    by = `Vet Practice`
  ) %>%
  add_n() %>%
  bold_labels()%>%
  add_overall() %>%
  add_p() %>%
  as_flex_table()

print(Resume_traitement)   

Resume_labo=Dataset %>%
  dplyr::select(EventCulled,Troponin,Lactate,Erytrocytes,Hemoglobin,Hematocrit,`Mean Globular Volume`,`Mean Corpuscular Hemoglobin`,`Mean Corpuscular Hemoglobin Concentration`,Platelets,Leucocytes,Neutrophils,`Band Neutrophils`,Lymphocytes,Monocytes,Eosinophils,Basophils,Fibrinogen,`Toxic Neutrophils`,Bacteriology) %>%
  tbl_summary(
    missing = "no",
    by = EventCulled,
    type = list(`Mean Corpuscular Hemoglobin` ~ "continuous")
  ) %>%
  add_n() %>%
  bold_labels()%>%
  add_overall() %>%
  add_p() %>%
  as_flex_table()
print(Resume_labo)  


Resume_bacterio=Dataset %>%
  dplyr::select(EventCulled,Bacteriology) %>%
  tbl_summary(
    missing = "no",
    by = EventCulled
  ) %>%
  add_n() %>%
  bold_labels()%>%
  add_overall() %>%
  add_p() %>%
  as_flex_table()
print(Resume_bacterio) 

save_as_docx(Resume_commemoratif,Resume_examclinique,Resume_mammaire,Resume_traitement,Resume_labo,Resume_bacterio,path = "C:/Users/tlepa/OneDrive - Universite de Montreal/Projet mammites Sévères/Statistics/Descriptive_tables.docx" )
save_as_html(Resume_commemoratif,Resume_examclinique,Resume_mammaire,Resume_traitement,Resume_mammaire,Resume_bacterio,path = "C:/Users/tlepa/OneDrive - Universite de Montreal/Projet mammites Sévères/Statistics/Descriptive_tables.html" )

}

#Multiple Imputation
{
original.names=names(Dataset)
names(Dataset) <- make.names(names(Dataset))

impute_predictor=quickpred(Dataset)
impute_predictor["Prognosis",c("Troponin","Lactate","Erytrocytes","Hemoglobin","Hematocrit","Mean.Globular.Volume","Mean.Corpuscular.Hemoglobin","Mean.Corpuscular.Hemoglobin.Concentration","Platelets","Leucocytes","Neutrophils","Band.Neutrophils","Lymphocytes","Monocytes","Eosinophils","Basophils","Fibrinogen","Toxic.Neutrophils","Bacteriology","Censored","Time","EventCulled","EventDeath")]=0
impute_predictor["Erytrocytes",c("Hemoglobin","Hematocrit")]=0
impute_predictor["Hematocrit",c("Hemoglobin","Erytrocytes")]=0
impute_predictor["Hemoglobin",c("Erytrocytes","Hematocrit")]=0
ini_imput=mice(Dataset,maxit=0)
impute_method=ini_imput$meth

impute=mice(data = Dataset,predictorMatrix = impute_predictor,method = impute_method,vis="monotone")


png("impute_algorithm.png", width = 1000, height = 1000)
plot(impute)
dev.off()

png("impute_densityplot.png", width = 4000, height = 4000)
densityplot(impute)
dev.off()

png("impute_stripplot.png", width = 4000, height = 4000)
stripplot(impute)
dev.off()
}

#Stacking

stack_impute=complete(impute,action = "stacked")

stack_impute=within(stack_impute,{
  Aminolean <- as.logical(Aminolean)
  Calcium <- as.logical(Calcium)
  CMT.Score <- as.factor(CMT.Score)
  Dextrose <- as.logical(Dextrose)
  Downer.Cow <- as.logical(Downer.Cow)
  Hypertonic <- as.logical(Hypertonic)
  Isotonic <- as.logical(Isotonic)
  Mastitis.Historic <- as.logical(Mastitis.Historic)
  Oral.Fluids <- as.logical(Oral.Fluids)
  Several.affected.quarters <- as.logical(Several.affected.quarters)
  Toxic.Neutrophils <- as.logical(Toxic.Neutrophils)
  Treatment.before.visit <- as.logical(Treatment.before.visit)
  EventCulled<- as.logical(EventCulled)
  EventDeath<- as.logical(EventDeath)
  Censored<-as.logical(Censored)
})


#Dummy and dataset creation for model training
{
dummied_stack_impute=dummy_cols(stack_impute,split = "&",remove_selected_columns=TRUE,select_columns = c("Capillary.Refilling.Time","Vet.Practice","General.State","Dehydration","Faeces.Aspect","Bacteriology","Appetite"))


data_impute_clinical=dummied_stack_impute[,c("Days.in.milk","Lactation.Number","Downer.Cow","General.State_Slightly Depressed","General.State_Severely Depressed","Appetite_0","Appetite_0.25","Temperature","Heart.Rate","Respiratory.Rate","Capillary.Refilling.Time_2s","Capillary.Refilling.Time_Plus 2s","Ruminal.Frequency","Dehydration_Light","Dehydration_Moderate","Dehydration_Severe","Several.affected.quarters")]
data_impute_paraclinical=dummied_stack_impute[,c("Days.in.milk","Lactation.Number","Downer.Cow","General.State_Slightly Depressed","General.State_Severely Depressed","Appetite_0","Appetite_0.25","Temperature","Heart.Rate","Respiratory.Rate","Capillary.Refilling.Time_2s","Capillary.Refilling.Time_Plus 2s","Ruminal.Frequency","Dehydration_Light","Dehydration_Moderate","Dehydration_Severe","Several.affected.quarters","Troponin","Lactate","Hematocrit","Platelets","Leucocytes","Neutrophils","Band.Neutrophils","Toxic.Neutrophils","Monocytes","Fibrinogen")]

bacteriology.gramm.neg=c()
bacteriology.gramm.pos=c()
bacteriology.yeast=c()
bacteriology.no.pathogen=c()

for (i in 1:length(stack_impute$Bacteriology)) {
  
  if (
    stack_impute$Bacteriology[i]=="E. coli"
    | stack_impute$Bacteriology[i]=="Enterobacter spp."
    | stack_impute$Bacteriology[i]=="P. aeruginosa"
    | stack_impute$Bacteriology[i]=="Serratia spp."
    | stack_impute$Bacteriology[i]=="Klebsiella spp."
  )  
  {bacteriology.gramm.neg[i]=1 } else {bacteriology.gramm.neg[i]=0 }
  
  if (
    stack_impute$Bacteriology[i]=="S. aureus"
    | stack_impute$Bacteriology[i]=="Staphylococcus spp."
    | stack_impute$Bacteriology[i]=="T. pyogenes"
    | stack_impute$Bacteriology[i]=="S. dysgalactiae"
    | stack_impute$Bacteriology[i]=="S. uberis"
  ) 
  {bacteriology.gramm.pos[i]=1 } else {bacteriology.gramm.pos[i]=0 }
  
  if (
    stack_impute$Bacteriology[i]=="Levures"
  ) 
  {bacteriology.yeast[i]=1 } else {bacteriology.yeast[i]=0 }
  
  if (
    stack_impute$Bacteriology[i]=="No pathogen"
  )
  {bacteriology.no.pathogen[i]=1} else {
    bacteriology.no.pathogen[i]=0
  }
  
}

diarrhea=c()

for (i in 1:length(stack_impute$Faeces.Aspect)) {
  
  if (
    stack_impute$Faeces.Aspect[i]=="Diarrhea"
    | stack_impute$Faeces.Aspect[i]=="Melena"
    | stack_impute$Faeces.Aspect[i]=="Blood"
    | stack_impute$Faeces.Aspect[i]=="Fibrin"
  )  
  {diarrhea[i]=1 } else {diarrhea[i]=0 }
  
}

data_impute_clinical=cbind(data_impute_clinical,diarrhea)
data_impute_paraclinical=cbind(data_impute_paraclinical,bacteriology.gramm.neg,bacteriology.gramm.pos,bacteriology.yeast,diarrhea)

}


#Construction of Model 1

x <- as.matrix(data_impute_paraclinical)
time <- stack_impute$Time
time[time==0]=0.1
event <- stack_impute$EventCulled
y <- survival::Surv(time, event)
colnames(x)=make.names(colnames(x))

suppressMessages(library("doParallel"))
registerDoParallel(detectCores())

fit1 <- fit_aenet(x, y, nfolds = 10, rule = "lambda.1se", parallel = TRUE)
names(fit)

write.csv(x, file = "x.csv",row.names = FALSE)
write.csv(y, file = "y.csv",row.names = FALSE)
save(fit, file = "hdnom-model.rdata")

model <- fit1$model
alpha <- fit1$alpha
lambda <- fit1$lambda
adapen <- fit1$pen_factor

nom <- as_nomogram(
  fit1, x, time, event,
  pred.at=60,
  funlabel = "60-days Overall Survival Probability"
)

plot(nom)

#Calibration and discrimination of model 1

val_int <- validate(
  x, time, event,
  model.type = "aenet",
  alpha = alpha, lambda = lambda, pen.factor = adapen,
  method = "bootstrap", boot.times = 100,
  tauc.type = "UNO", tauc.time = seq(5, 55, 5),
  trace = FALSE
)

print(val_int)
summary(val_int)
plot(val_int)

cal_int <- calibrate(
  x, time, event,
  model.type = "aenet",
  alpha = alpha, lambda = lambda, pen.factor = adapen,
  method = "bootstrap", boot.times = 100,
  pred.at = 59, ngroup = 10,
  trace = FALSE,
)

print(cal_int)
summary(cal_int)

plot(cal_int, xlim = c(0.5, 1), ylim = c(0.5, 1))+theme_classic()

kmplot(
  cal_int,
  time.at = seq(1, 60, 5)
)


#Decision curve analysis


pred_fit=1-predict(fit,x,y,x,pred.at =60)
dca_table=as.data.frame(cbind(y,pred_fit))

dca_obj=dca(Surv(time,status)~`60`,data = dca_table,time=60,
        label = list(`60`="Severe Mastitis Nomogram"),
        thresholds = seq(0.5,0.9,0.01))

#plot(dca_obj, smooth = TRUE, style = c("bw"),show_ggplot_code = TRUE)
 
as_tibble(dca_obj) %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x = threshold, y = net_benefit, linetype = label)) +
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", 
              span = 0.3, color = "black") +
  coord_cartesian(ylim = c(-0.2, 0.2)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Probability of Non-surival Above Which euthanasia is Decided", y = "Net Benefit", linetype = "") +
  theme_bw()
ggsave( "DCA 60 model 1.png",width = 6, height = 5)


#MCT
{
status=as.numeric(c())
for (i in 1:length(event)){
  if (time[i]>=60){
    status[i]=0}
  else {
    if (event[i]==1){
      status[i]=1
    }
    else if (event [i]==0){
      status[i]=NA
    }
  }
  
}

MCT=matrix(nrow=100,ncol=5)
colnames(MCT)=c(1,2,3,4,5)

# real prevalence
pos=0
neg=0
for (j in 1:length(status)){
  if (is.na(status[j])){}
  else if(status[j]==1){
    pos=pos+1
  }
  else if(status[j]==0){
    neg=neg+1
  }
}
prev=(pos/(pos+neg))

# 5 ratios 
for(k in 1:5){
  ratio=c(0.20,0.4,0.6,0.8,1)[k]
  
  # 100 thresholds
  for(i in 1:100){
    
    #vrairable initialisation
    tp=0
    tn=0
    
    #true negatives and positives
    for (j in 1:length(event)){
      if (is.na(status[j])){}
      
      else if(status[j]==1){
        
        if(pred_fit[j]>(i/100)){
          tp=tp+1
        }
      }
      
      else if(status[j]==0){
        
        if(pred_fit[j]<(i/100)){
          tn=tn+1
        }
      }
    }
    
    MCT[i,k]=(1-prev)*(1-(tn/neg))+ratio*prev*(1-(tp/pos))
  }
}

df_MCT <- data.frame(x = seq_along(MCT[, 1]),MCT)
df_MCT <- melt(df_MCT, id.vars = "x")

ggplot(data = df_MCT , aes(x = x, y = value, group = variable)) +
  geom_line(aes(linetype=variable))+
  scale_linetype_discrete(labels = paste(1:5))+
  guides(linetype = guide_legend(title = "Ratio of cost FN/FP "))+
  labs(title="Missclassification Cost Term",subtitle = "60-days overall survival",x="Death Probability Threshold for Euthanasia",y="Misclassification Cost")+
  scale_linetype_discrete(labels =c(0.20,0.4,0.6,0.8,1))+
  theme_bw()
ggsave( "60-days overall survival Model 1 MCT.png",width = 6, height = 5)
}

#Model 2

x <- as.matrix(data_impute_clinical)
time <- stack_impute$Time
time[time==0]=0.1
event <- stack_impute$EventCulled
y <- survival::Surv(time, event)
colnames(x)=make.names(colnames(x))

suppressMessages(library("doParallel"))
registerDoParallel(detectCores())

fit2 <- fit_aenet(x, y, nfolds = 10, rule = "lambda.1se", parallel = TRUE)
names(fit)

write.csv(x, file = "x.csv",row.names = FALSE)
write.csv(y, file = "y.csv",row.names = FALSE)
save(fit, file = "hdnom-model.rdata")

model <- fit2$model
alpha <- fit2$alpha
lambda <- fit2$lambda
adapen <- fit2$pen_factor

nom <- as_nomogram(
  fit1, x, time, event,
  pred.at=60,
  funlabel = "60-days Overall Survival Probability"
)

plot(nom)

#Calibration and discrimination of model 1

val_int <- validate(
  x, time, event,
  model.type = "aenet",
  alpha = alpha, lambda = lambda, pen.factor = adapen,
  method = "bootstrap", boot.times = 100,
  tauc.type = "UNO", tauc.time = seq(5, 55, 5),
  trace = FALSE
)

print(val_int)
summary(val_int)
plot(val_int)

cal_int <- calibrate(
  x, time, event,
  model.type = "aenet",
  alpha = alpha, lambda = lambda, pen.factor = adapen,
  method = "bootstrap", boot.times = 100,
  pred.at = 59, ngroup = 10,
  trace = FALSE,
)

print(cal_int)
summary(cal_int)

plot(cal_int, xlim = c(0.5, 1), ylim = c(0.5, 1))+theme_classic()

kmplot(
  cal_int,
  time.at = seq(1, 60, 5)
)


#Decision curve analysis


pred_fit=1-predict(fit,x,y,x,pred.at =60)
dca_table=as.data.frame(cbind(y,pred_fit))

dca_obj=dca(Surv(time,status)~`60`,data = dca_table,time=60,
            label = list(`60`="Severe Mastitis Clinical Nomogram"),
            thresholds = seq(0.5,0.9,0.01))

#plot(dca_obj, smooth = TRUE, style = c("bw"),show_ggplot_code = TRUE)

as_tibble(dca_obj) %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x = threshold, y = net_benefit, linetype = label)) +
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", 
              span = 0.3, color = "black") +
  coord_cartesian(ylim = c(-0.2, 0.1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Probability of Non-surival Above Which euthanasia is Decided", y = "Net Benefit", linetype = "") +
  theme_bw()
ggsave( "DCA 60 model 2.png",width = 6, height = 5)


#MCT
{
  status=as.numeric(c())
  for (i in 1:length(event)){
    if (time[i]>=60){
      status[i]=0}
    else {
      if (event[i]==1){
        status[i]=1
      }
      else if (event [i]==0){
        status[i]=NA
      }
    }
    
  }
  
  MCT=matrix(nrow=100,ncol=5)
  colnames(MCT)=c(1,2,3,4,5)
  
  # real prevalence
  pos=0
  neg=0
  for (j in 1:length(status)){
    if (is.na(status[j])){}
    else if(status[j]==1){
      pos=pos+1
    }
    else if(status[j]==0){
      neg=neg+1
    }
  }
  prev=(pos/(pos+neg))
  
  # 5 ratios 
  for(k in 1:5){
    ratio=c(0.20,0.4,0.6,0.8,1)[k]
    
    # 100 thresholds
    for(i in 1:100){
      
      #vrairable initialisation
      tp=0
      tn=0
      
      #true negatives and positives
      for (j in 1:length(event)){
        if (is.na(status[j])){}
        
        else if(status[j]==1){
          
          if(pred_fit[j]>(i/100)){
            tp=tp+1
          }
        }
        
        else if(status[j]==0){
          
          if(pred_fit[j]<(i/100)){
            tn=tn+1
          }
        }
      }
      
      MCT[i,k]=(1-prev)*(1-(tn/neg))+ratio*prev*(1-(tp/pos))
    }
  }
  
  df_MCT <- data.frame(x = seq_along(MCT[, 1]),MCT)
  df_MCT <- melt(df_MCT, id.vars = "x")
  
  ggplot(data = df_MCT , aes(x = x, y = value, group = variable)) +
    geom_line(aes(linetype=variable))+
    scale_linetype_discrete(labels = paste(1:5))+
    guides(linetype = guide_legend(title = "Ratio of cost FN/FP "))+
    labs(title="Missclassification Cost Term",subtitle = "60-days overall survival",x="Death Probability Threshold for Euthanasia",y="Misclassification Cost")+
    scale_linetype_discrete(labels =c(0.20,0.4,0.6,0.8,1))+
    theme_bw()
  ggsave( "60-days overall survival Model 2 MCT.png",width = 6, height = 5)
}


#veterinarian prognosis

km_fit=survfit(Surv(Time,EventCulled)~Prognosis,data=stack_impute)

prognosis=as.numeric(stack_impute$Prognosis)

prognosis[prognosis==1]=0.25
prognosis[prognosis==2]=0.5
prognosis[prognosis==3]=0.75

prognosis_table=as.data.frame(cbind(prognosis,stack_impute$Time,stack_impute$EventCulled))

dca_obj=dca(Surv(V2,V3)~prognosis,data=prognosis_table,time=60,
            label = list(`60`="Progonis of Veterinarians"),
            thresholds = seq(0.5,0.9,0.01))

as_tibble(dca_obj) %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x = threshold, y = net_benefit, linetype = label)) +
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", 
              span = 0.2, color = "black") +
  coord_cartesian(ylim = c(-0.2, 0.1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Probability of Non-surival Above Which euthanasia is Decided", y = "Net Benefit", linetype = "") +
  theme_bw()
ggsave( "DCA 60 prognosis.png",width = 6, height = 5)


ggsurvplot(km_fit,data=Dataset,pval = TRUE)

