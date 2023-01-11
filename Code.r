#packages
library(tidyverse)
library(janitor)
library(gtsummary)
library(mice)
library(gglasso)
library(readxl)
library(fastDummies)
library(pROC)
library(reshape2)
library(ggplot2)
library(caret)
library(CalibrationCurves)
library(generalhoslem)
library(rms)
library(flextable)
library(sparsegl)
library(glmnet)
library(dcurves)


#•Fonction de dession des matrices de confusion
{
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 445), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(paste0("Confusion matrix : ",entree_modele," ",sortie_modele), cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
  
}


#Fonction des données de calibration

draw_calibration_stats=function(calcurve,hosmer){
  
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "Calibration statistics", xaxt='n', yaxt='n')
  text(10, 85, names(calcurve$stats[15]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(calcurve$stats[15]), 3), cex=1.2)
  text(50, 85, c("Hosmer and Lemeshow test"), cex=1.2, font=2)
  text(50, 70, signif(as.numeric(hosmer$p.value[1]),4), cex=1.2)
  text(90, 85, c("Unreliability p"), cex=1.2, font=2)
  text(90, 70, round(as.numeric(calcurve$stats[15]), 3), cex=1.2)
}
}

#Import de la BD

Dataset=read_excel("C:/Users/tlepa/OneDrive - Universite de Montreal/Projet mammites Sévères/Bases de données/Dataset.xlsx", sheet="Sheet1")
View(Dataset)

Dataset=subset(Dataset,select = -c(EventDeath,EventCulled,Time,Censored))

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
})

names(Dataset)[c(14)] <- c("Capillary Refilling Time")

Dataset=within(Dataset,{
     Aminolean <- as.logical(Aminolean)
     Calcium <- as.logical(Calcium)
     `CMT Score` <- as.factor(`CMT Score`)
     Culled <- as.logical(Culled)
     Dextrose <- as.logical(Dextrose)
     Died <- as.logical(Died)
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

#Stats descriptives
#{
  Resume_commemoratif=Dataset %>%
    dplyr::select(`Vet Practice`,Breed,`Days in milk`,`Lactation Number`,`Treatment before visit`,`Mastitis Historic`,Died,Culled) %>%
    tbl_summary(
      missing = "no",
      type = list(`Lactation Number` ~ "continuous")
    ) %>%
    add_n() %>%
    bold_labels()%>%
    as_flex_table()
  
  Resume_examclinique=Dataset %>% 
    dplyr::select(EventCulled,`Downer Cow`,`General State`,Appetite,Temperature,`Heart Rate`,`Mucosal Aspect`,`Capillary Refilling Time`,`Ruminal Frequency`,`Ruminal Contractions`,`Dehydration`,`Faeces Aspect`,`Faeces Quantity`) %>%
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
    dplyr::select(EventCulled,Calcium,Aminolean,Dextrose,Isotonic,Hypertonic,`Oral Fluids`,NSAID,`Systemic Antimicrobial`,`Intrammary Antimicrobial`) %>%
    tbl_summary(
      missing = "no",
      by = EventCulled
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
    
  save_as_docx(Resume_commemoratif,Resume_examclinique,Resume_mammaire,Resume_traitement,Resume_labo,Resume_bacterio,path = "C:/Users/Ambulatoire/OneDrive - Universite de Montreal/Projet mammites Sévères/Statistics/Descriptive_tables.docx" )
  save_as_html(Resume_commemoratif,Resume_examclinique,Resume_mammaire,Resume_traitement,Resume_mammaire,Resume_bacterio,path = "C:/Users/Ambulatoire/OneDrive - Universite de Montreal/Projet mammites Sévères/Statistics/Descriptive_tables.html" )
}
    
#Imputation

original.names=names(Dataset)
names(Dataset) <- make.names(names(Dataset))

impute_predictor=quickpred(Dataset)
impute_predictor["Prognosis",c("Troponin","Lactate","Erytrocytes","Hemoglobin","Hematocrit","Mean.Globular.Volume","Mean.Corpuscular.Hemoglobin","Mean.Corpuscular.Hemoglobin.Concentration","Platelets","Leucocytes","Neutrophils","Band.Neutrophils","Lymphocytes","Monocytes","Eosinophils","Basophils","Fibrinogen","Toxic.Neutrophils","Bacteriology","Died","Culled")]=0
impute_predictor["Erytrocytes",c("Hemoglobin","Hematocrit")]=0
impute_predictor["Hematocrit",c("Hemoglobin","Erytrocytes")]=0
impute_predictor["Hemoglobin",c("Erytrocytes","Hematocrit")]=0
ini_imput=mice(Dataset,maxit=0)
impute_method=ini_imput$meth

impute=mice(data = Dataset,predictorMatrix = impute_predictor,method = impute_method,vis="monotone")

stack_impute=complete(impute,action = "stacked")

stack_impute=within(stack_impute,{
    Aminolean <- as.logical(Aminolean)
    Calcium <- as.logical(Calcium)
    CMT.Score <- as.factor(CMT.Score)
    Culled <- as.logical(Culled)
    Dextrose <- as.logical(Dextrose)
    Died <- as.logical(Died)
    Downer.Cow <- as.logical(Downer.Cow)
    Hypertonic <- as.logical(Hypertonic)
    Isotonic <- as.logical(Isotonic)
    Mastitis.Historic <- as.logical(Mastitis.Historic)
    Oral.Fluids <- as.logical(Oral.Fluids)
    Several.affected.quarters <- as.logical(Several.affected.quarters)
    Toxic.Neutrophils <- as.logical(Toxic.Neutrophils)
    Treatment.before.visit <- as.logical(Treatment.before.visit)
})

png("impute_algorithm.png", width = 1000, height = 1000)
plot(impute)
dev.off()

png("impute_densityplot.png", width = 4000, height = 4000)
densityplot(impute)
dev.off()

png("impute_stripplot.png", width = 4000, height = 4000)
stripplot(impute)
dev.off()


#Creation des dummy et des dataset d'entrainement

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

groupes_data_clinical=c(1,1,1,1,2,3,4,5,5,6,6,7,8,9,10,10,11,12,12,12,13,14)
groupes_data_paraclinical=c(1,1,1,1,2,3,4,5,5,6,6,7,8,9,10,10,11,12,12,12,14,15,16,17,18,19,20,21,22,23,25,24,24,24,13)

#Creation de quatre modeles

for (entree_modele in c("clinical","paraclinical")){ 


    for (sortie_modele in c("Died","Culled")){

        print(paste0("preparation du modèle ",entree_modele," ",sortie_modele))

        #fit des modèles et cross-validation pour selecion du modele gglasso
         #assign(paste0("cv_gglasso_",entree_modele,"_",sortie_modele),cv.gglasso(
          #x=as.matrix(get(paste0("data_impute_",entree_modele))),
          # y=2*stack_impute[,sortie_modele]-1,
          # group=get(paste0("groupes_data_",entree_modele)),
          # loss="logit",nfolds=5,
          # pred.loss="loss",
          # eps=1e-4))

         #fit des modèles et cross-validation pour selecion du modele sparsegl
         #assign(paste0("cv_gglasso_",entree_modele,"_",sortie_modele),cv.sparsegl(
          # x=as.matrix(get(paste0("data_impute_",entree_modele))),
          # y=stack_impute[,sortie_modele],
          # group=get(paste0("groupes_data_",entree_modele)),
          # family = "binomial",
          # nfolds=10,
          # pred.loss="binomial"))
         
         #fit des modèles et cross-validation pour selecion du modele adaptive lasso
         best_coef=coef(cv.glmnet(x=as.matrix(get(paste0("data_impute_",entree_modele))),y=stack_impute[,sortie_modele],family="binomial",parallel = TRUE,alpha=0,type.measure = "deviance"),s="lambda.min")[-1]
         assign(paste0("cv_gglasso_",entree_modele,"_",sortie_modele),cv.glmnet(x=as.matrix(get(paste0("data_impute_",entree_modele))),y=stack_impute[,sortie_modele],family="binomial",parallel = TRUE,alpha=1,type.measure = "auc",penalty.factor = 1 / abs(best_coef),keep=TRUE))
         1
         png(paste0("lasso_",entree_modele,"_",sortie_modele,".png"), width = 500, height = 500)
         
         plot(get(paste0("cv_gglasso_",entree_modele,"_",sortie_modele)))
         
         dev.off()
         
        
         
        #predictions
         
         

         #Logit de prediction
         assign(paste0("logit_",entree_modele,"_",sortie_modele),predict(
            get(paste0("cv_gglasso_",entree_modele,"_",sortie_modele)),
            newx=as.matrix(get(paste0("data_impute_",entree_modele))),
            s=c("lambda.1se"),
            type="link"))

         #Prediction
          #assign(paste0("pred_",entree_modele,"_",sortie_modele),as.logical((1+
           #  predict(
            #    get(paste0("cv_gglasso_",entree_modele,"_",sortie_modele)),
             #   newx=as.matrix(get(paste0("data_impute_",entree_modele))),
              #  s=c("lambda.1se"))
            #)/2))
         
         assign(paste0("pred_",entree_modele,"_",sortie_modele),as.logical(
                predict(get(paste0("cv_gglasso_",entree_modele,"_",sortie_modele)),
                       newx=as.matrix(get(paste0("data_impute_",entree_modele))),
                       s=c("lambda.1se"),
                       type="class")))

         #Proba de prediction    
            #assign(paste0("proba_",entree_modele,"_",sortie_modele),exp(get(paste0("logit_",entree_modele,"_",sortie_modele)))/(1+exp(get(paste0("logit_",entree_modele,"_",sortie_modele)))))
         assign(paste0("proba_",entree_modele,"_",sortie_modele),predict(
                get(paste0("cv_gglasso_",entree_modele,"_",sortie_modele)),
                newx=as.matrix(get(paste0("data_impute_",entree_modele))),
                s=c("lambda.1se"),
                type="response"))
         
        #Discrimination et calobration

            #Courbe roc
            assign(paste0("roc_",entree_modele,"_",sortie_modele),roc(stack_impute[,sortie_modele],as.numeric(as.vector(get(paste0("proba_",entree_modele,"_",sortie_modele))))))

            png(paste0("ROCcurve_",entree_modele,"_",sortie_modele,".png"), width = 500, height = 500)
            
            plot(get(paste0("roc_",entree_modele,"_",sortie_modele))) 
            
            dev.off()
            
            
            #courbe de calibration
            
            png(paste0("CalCurve_",entree_modele,"_",sortie_modele,".png"), width = 700, height = 700)
            
            assign(paste0("CalCurve_",entree_modele,"_",sortie_modele),val.prob.ci.2(get(paste0("proba_",entree_modele,"_",sortie_modele)),stack_impute[,sortie_modele]))
  
            dev.off()
            
            #stats de calibration + matrice de confusion
            
            assign(paste0("HosmerTest_",entree_modele,"_",sortie_modele),logitgof(as.numeric(as.vector(stack_impute[,sortie_modele])),as.vector(get(paste0("proba_",entree_modele,"_",sortie_modele)))))
            
            assign(paste0("Confusion_",entree_modele,"_",sortie_modele),confusionMatrix(as.factor(stack_impute[,sortie_modele]),as.factor(get(paste0("pred_",entree_modele,"_",sortie_modele)))))
            
            png(paste0("Confusion_",entree_modele,"_",sortie_modele,".png"), width = 550, height = 550)
            
            draw_confusion_matrix(get(paste0("Confusion_",entree_modele,"_",sortie_modele)))
            
            dev.off()
            
            png(paste0("CalStats_",entree_modele,"_",sortie_modele,".png"), width = 500, height = 300)
            
            draw_calibration_stats(get(paste0("CalCurve_",entree_modele,"_",sortie_modele)),get(paste0("HosmerTest_",entree_modele,"_",sortie_modele)))
              
            dev.off()
            
            
            
        #Calcul du MCT

         MCT=matrix(nrow=100,ncol=5)
         colnames(MCT)=c(1,2,3,4,5)

         # calcul des prévalences réelles
         pos=0
         neg=0
         for (j in 1:length(stack_impute[,sortie_modele])){
            if(stack_impute[,sortie_modele][j]==1){
                pos=pos+1
                }
            if(stack_impute[,sortie_modele][j]==0){
                neg=neg+1
                }
         }
         prev=(pos/(pos+neg))

         # 5 ratios de MCT différents
         for(k in 1:5){
            ratio=c(0.5,1,1.25,1.5,2)[k]

            # 100 niveau de seuils différents
            for(i in 1:100){

                #initialisation des variables
                 tp=0
                 tn=0

                # stack_impute$deces est le vrai résultat
                # pred_clini est le résultat calculé par le modèle

                #calcul des true positive et negative pour le calcul de la sensibilité et spécificité
                 for (j in 1:length(stack_impute[,sortie_modele])){
         
                     if(stack_impute[,sortie_modele][j]==1){

                         if(get(paste0("proba_",entree_modele,"_",sortie_modele))[j]>(i/100)){
                             tp=tp+1
                             }
                     }

                     if(stack_impute[,sortie_modele][j]==0){

                         if(get(paste0("proba_",entree_modele,"_",sortie_modele))[j]<(i/100)){
                             tn=tn+1
                         }
                     }
                 }

                 #Calcul du MCT
                   MCT[i,k]=(1-prev)*(1-(tn/neg))+ratio*prev*(1-(tp/pos))
             }
         }

         #Mise en forme des données pour le graphique
           df_MCT <- data.frame(x = seq_along(MCT[, 1]),MCT)
           df_MCT <- melt(df_MCT, id.vars = "x")

         #Mise en place du graphique
            ggplot(data = df_MCT , aes(x = x, y = value, color = variable)) +
                   geom_line()+
                   scale_color_discrete(labels = paste(1:5))+
                   guides(color = guide_legend(title = "Ratio"))+
                   labs(title="Missclassification Cost Term",subtitle = paste0(entree_modele," Model for ",sortie_modele),y="MCT",x="Threshold")+
                   scale_color_discrete(labels =c(0.2,0.4,0.75,1,1.5))
            ggsave(paste0("MCT_",entree_modele," Model for ",sortie_modele,".png"),width = 5, height = 5)


    }

}

tab_coef1=as.data.frame(as.matrix(coef(cv_gglasso_clinical_Died,s="lambda.1se")))
tab_coef2=as.data.frame(as.matrix(coef(cv_gglasso_clinical_Culled,s="lambda.1se")))
tab_coef3=as.data.frame(as.matrix(coef(cv_gglasso_paraclinical_Died,s="lambda.1se")))
tab_coef4=as.data.frame(as.matrix(coef(cv_gglasso_paraclinical_Culled,s="lambda.1se")))
tab_coef_clinical=as.data.frame(cbind(row.names(tab_coef1),tab_coef1,tab_coef2))
tab_coef_paraclinical=as.data.frame(cbind(row.names(tab_coef4),tab_coef3,tab_coef4))
colnames(tab_coef_clinical)=c("Estimators","Clinical Died","Clinical Culled")
colnames(tab_coef_paraclinical)=c("Estimators","Paraclinical Died","Paraclinical Culled")
save_as_docx(nice_table(tab_coef_clinical),nice_table(tab_coef_paraclinical),path = "C:/Users/Ambulatoire/OneDrive - Universite de Montreal/Projet mammites Sévères/Statistics/coef_tables.docx" )


