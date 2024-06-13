#SCRIPT BANKNOTE IDENTIFICATION

library(ggplot2)
library(tidyverse)
library(heatmaply)
library(caret)
library(gridExtra)
library(GGally)
library(mclust)
library(Rmixmod)
library(flexmix)

#importo il dataset e do i nomi alle variabili 
banconote<-read.csv("C:\\Users\\alber\\Downloads\\banknote+authentication\\data_banknote_authentication.txt", header=F)
colnames(banconote)<-c("varianza", "asimmetria","curtosi", "entropia","tipo")
banconote<-as_tibble(banconote)#as.tibble non esiste più dalla versione 2.0.0
str(banconote)
banconote$tipo<-as.factor(banconote$tipo)
levels(banconote$tipo)<-c("vere","false")
str(banconote)


#ANALISI DESCRITTIVA------------------------------------------------------------

#analisi MISSING
n<-nrow(banconote) #137
sum(complete.cases(banconote)) #i casi completi sono 1372 che coincide con il numero di righe
#non ci sono missing

#analisi per scoprire se nel dataset sono presenti duplicati

which(duplicated(banconote))
# [1]  48 140 191 196 227 269 285 301 316 321 346 352 353 405 428 437 477 499 605 616
# [21] 658 692 717 728

#queste sono tutte le unità statistiche duplicate, tuttavia solo l'us allla riga 352 e 353 sono duplicati e consecutivi
banconote[c(352,353),]

# varianza asimmetria curtosi entropia tipo 
# 
# 1    0.520      -3.26    3.09   -0.985 vere 
# 2    0.329      -4.46    4.57   -0.989 vere 

#si noti che nonostante siano duplicate e consecutive non assumono lo stesso valore, per cui non c'è un errore dovuto ad un doppio inserimento
print(banconote[duplicated(banconote),],n=24)

# varianza asimmetria curtosi entropia tipo 
#
# 1   -0.787     9.57    -3.79    -7.50  vere 
# 2   -0.206     9.22    -3.70    -6.81  vere 
# 3    0.930    -3.80     4.64    -0.296 vere 
# 4   -1.86      7.89    -1.66    -1.84  vere 
# 5    0.571    -0.0248   1.24    -0.562 vere 
# 6    0.930    -3.80     4.64    -0.296 vere 
# 7   -1.3      10.3     -2.95    -5.86  vere 
# 8    0.329    -4.46     4.57    -0.989 vere 
# 9    0.329    -4.46     4.57    -0.989 vere 
# 10    0.520    -3.26     3.09    -0.985 vere 
# 11   -1.86      7.89    -1.66    -1.84  vere 
# 12    0.520    -3.26     3.09    -0.985 vere 
# 13    0.329    -4.46     4.57    -0.989 vere 
# 14    0.380     0.710    0.757   -0.444 vere 
# 15   -1.3      10.3     -2.95    -5.86  vere 
# 16    0.380     0.710    0.757   -0.444 vere 
# 17    0.380     0.710    0.757   -0.444 vere 
# 18    0.571    -0.0248   1.24    -0.562 vere 
# 19   -2.65     10.1     -1.33    -5.47  vere 
# 20   -0.206     9.22    -3.70    -6.81  vere 
# 21   -0.278     8.19    -3.13    -2.53  vere 
# 22    0.571    -0.0248   1.24    -0.562 vere 
# 23    0.520    -3.26     3.09    -0.985 vere 
# 24   -2.65     10.1     -1.33    -5.47  vere 


#risulta evidente che ci siano diversi duplicati anche presenti 3 volte all'interno del dataset, allo stesso tempo tutti i duplicati sono banconote vere
#perciò si può suppporre che in realtà non sia la stessa unità statistica duplicata ma diverse banconote provenienti dagli stessi "lotti", quindi può essere che assumino gli stessi valori 
#per questo motivo non verranno tolte dal dataset ma verranno trattate come unità statistiche diverse, nonostante i valori delle loro variabili assumino valori uguali(fino a 2/3 decimali)


summary(banconote)

# varianza         asimmetria         curtosi       
# Min.   :-7.0421   Min.   :-13.773   Min.   :-5.2861  
# 1st Qu.:-1.7730   1st Qu.: -1.708   1st Qu.:-1.5750  
# Median : 0.4962   Median :  2.320   Median : 0.6166  
# Mean   : 0.4337   Mean   :  1.922   Mean   : 1.3976  
# 3rd Qu.: 2.8215   3rd Qu.:  6.815   3rd Qu.: 3.1793  
# Max.   : 6.8248   Max.   : 12.952   Max.   :17.9274  

# entropia          tipo    
# Min.   :-8.5482   vere :762  
# 1st Qu.:-2.4135   false:610  
# Median :-0.5867              
# Mean   :-1.1917              
# 3rd Qu.: 0.3948              
# Max.   : 2.4495 

#correlazioni 
heatmaply_cor(cor(banconote[,-5]),cellnote=cor(banconote[,-5]),cellnote_textposition = "middle center",
              column_text_angle=0)

#boxplot varianza
ggplot(banconote,aes(x=tipo,y=varianza, color=tipo))+
  geom_boxplot(fill="lightgrey", outlier.shape = 1)+
  labs(title="Boxplot della varianza delle banconote")+
  xlab("tipologia di banconote")+
  scale_color_manual(values=c("darkgreen","darkred"))+
  theme_classic()
#c'è una differenza abbastanza marcata nei valori assunti dalle due tipologie di banconote

#boxplot asimmetria
ggplot(banconote,aes(x=tipo,y=asimmetria, color=tipo))+
  geom_boxplot(fill="lightgrey", outlier.shape = 1)+
  labs(title="Boxplot della asimmetria delle banconote")+
  xlab("tipologia di banconote")+
  scale_color_manual(values=c("darkgreen","darkred"))+
  theme_classic()
#anche in questo caso i valori assunti sono distinti e con intervalli di variabilità abbastanza uguali 

#boxplot curtosi 
ggplot(banconote,aes(x=tipo,y=curtosi, color=tipo))+
  geom_boxplot(fill="lightgrey", outlier.shape = 1)+
  labs(title="Boxplot della curtosi delle banconote")+
  xlab("tipologia di banconote")+
  scale_color_manual(values=c("darkgreen","darkred"))+
  theme_classic()
#in questo caso l'intervallo di variazione della curtosi per le banconote false è maggiore di quello delle banconote vere

#boxplot entropia
ggplot(banconote,aes(x=tipo,y=entropia, color=tipo))+
  geom_boxplot(fill="lightgrey", outlier.shape = 1)+
  labs(title="Boxplot della entropia delle banconote")+
  xlab("tipologia di banconote")+
  scale_color_manual(values=c("darkgreen","darkred"))+
  theme_classic()
#sono pressoche identici


#analsi delle componenti principali
pca<-princomp(banconote[,-5])
summary(pca)

# Importance of components:
#   Comp.1    Comp.2     Comp.3     Comp.4
# Standard deviation     7.0600135 3.0480315 2.09466897 1.39568875
# Proportion of Variance 0.7613241 0.1419049 0.06701767 0.02975334
# Cumulative Proportion  0.7613241 0.9032290 0.97024666 1.0000000
(nomi<-names(banconote)[apply(pca$loadings[,1:2], 2, function(x) which.max(abs(x)))]) #cerco quali sono le variabili collegatr ai loadings della pca
#asimmmetria e varianza spiegano insieme circa il 90% della varianza

ggplot(banconote,aes(x=asimmetria, y=varianza, colour=tipo))+
  geom_point(alpha=0.5)+
  scale_color_manual(values=c("darkgreen","darkred"))+
  theme_bw()
#si noti che i due tipi di banconote si distinguono,però si intravedono anche una sorta di sottograppumento nelle u.s. dovuta probabilmente alle diverse valute contenute nel dataset 

#provo a fare un binary glm per vedere quali variabili influiscono di più sulla label
banconoteglm<-banconote
banconoteglm$tipo<-ifelse(banconoteglm$tipo=="vere", 0,1)
str(banconoteglm)
modellobin<-glm(tipo~., data=banconoteglm, family=binomial)
summary(modellobin)
# Call:
#   glm(formula = tipo ~ ., family = binomial, data = banconote)
# 
# Coefficients:
#               Estimate Std. Error z value  Pr(>|z|)    
#   (Intercept)   7.3218     1.5589   4.697 2.64e-06 ***
#   varianza     -7.8593     1.7383  -4.521 6.15e-06 ***
#   asimmetria   -4.1910     0.9041  -4.635 3.56e-06 ***
#   curtosi      -5.2874     1.1612  -4.553 5.28e-06 ***
#   entropia     -0.6053     0.3307  -1.830   0.0672 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 1885.122  on 1371  degrees of freedom
# Residual deviance:   49.891  on 1367  degrees of freedom
# AIC: 59.891
# 
# Number of Fisher Scoring iterations: 12

#si noti che l'entropia è la variabile meno significativa tra le 4

exp(-7.8593) #0.0003861441
(exp(-7.8593)-1)*100 
#a parità delle altre covariate, se la varianza aumenta di una unità l'odds diminuisce del 99,96%

#pairs
ggpairs(banconote)


#MODEL BASED CLUSTERING---------------------------------------------------------

modello0.5<-Mclust(banconote[,-5],)
summary(modello0.5)

modello<-Mclust(banconote[,-5],G=1:30) #provo a fare il cluster con tutti le variabili 
summary(modello)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VEV (ellipsoidal, equal shape) model with 29 components: 
#   
#   log-likelihood    n  df       BIC      ICL
# -9242.002 1372 350 -21012.41 -21093.1
# 
# Clustering table:
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22 
# 69  69  77  29 107  45  68  53  42  81  23  28  40  31  72  71  38  32  30  20  50  56 
# 23  24  25  26  27  28  29 
# 38  40  30  20  56  27  30 


# Top 3 models based on the ICL criterion: 
#   VVV,9     EVV,9     VEV,9 
# -22464.40 -22691.95 -22707.88 
plot(modello$BIC)
plot(mclustICL(banconote[,-5],G=1:30))

mclustICL(banconote[,-5],G=29:40)
#provo a plottare i cluster individuati

disegna_cluster<-function(model, dati){
  dati$cluster<-as.factor(model$classification)
  plotv<-ggplot(dati,aes(x=asimmetria, y=varianza, colour=tipo))+
    geom_point()+
    scale_color_manual(values=c("green3","red3"))+
    labs(title="vere label")+
    theme_bw()
  plotc<-ggplot(dati,aes(x=asimmetria, y=varianza, colour=cluster))+
    geom_point()+
    labs(title="clusterizzazione")+
    theme_bw()
  grid.arrange(plotv, plotc, ncol = 2)
}

disegna_cluster(modello,banconote)

#il clustering con tutti le variabili individua molti più gruppi di quelli presenti in realtà e sembra clusterizzare unità distanti tra loro e ne distingue alcune vicine tra loro
#numero alto di cluster dovuto alla variabile non registrata della tipologia di banconote

#provo a clusterizzare con le due variabili scelte con la pca

banconotepc<-banconote[,nomi]
modello3<-mclustICL(banconotepc,G=1:30)
summary(modello3)
plot(modello3)

# Best ICL values:
#   VVV,25       EEE,27        VVV,10
# ICL      -14945.04 -14948.45311 -14950.663877
# ICL diff      0.00     -3.41301     -5.623778


#con il criterio ICL il modello migliore è VVV con 25 cluster, siccome ICL è il criterio migliore scelgo il modello VVV

modello2<-Mclust(banconotepc,G=25, modelNames = "VVV")
summary(modello2)


# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVV (ellipsoidal, varying volume, shape, and orientation) model with 25 components: 
#   
#   log-likelihood    n  df       BIC       ICL
# -6807.038 1372 149 -14690.45 -14945.04
# 
# Clustering table:
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22 
# 122  81  81  24  75  29 111  49  54  84 119   6 106  30  11  12  69  41  35 108  30  38 
# 23  24  25 
# 35  10  12 

modello2.5<-Mclust(banconotepc,G=10, modelNames = "VVV")
adjustedRandIndex(modello2.5)
# summary(modello2.5)
# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVV (ellipsoidal, varying volume, shape, and orientation) model with 10 components: 
#   
#   log-likelihood    n df       BIC       ICL
# -7105.912 1372 59 -14638.04 -14950.66
# 
# Clustering table:
#   1   2   3   4   5   6   7   8   9  10 
# 123  89 116 109 252  40 140 211 227  65 
banconotepcc<-banconote[,-c(3,4)]
disegna_cluster(modello2.5,banconotepcc)

#provo a forzare il numero di gruppi uguale a 2
modello4<-Mclust(banconotepc,G=2)
summary(modello4)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust VVI (diagonal, varying volume and shape) model with 2 components: 
#   
#   log-likelihood    n df       BIC       ICL
#        -7622.314 1372  9 -15309.64 -15629.83
# 
# Clustering table:
#   1   2 
# 814 558 
mclustICL(banconotepc,G=2)
# Top 3 models based on the ICL criterion: 
#   EVV,2     EVE,2     EVI,2 
# -15613.58 -15617.58 -15622.64

#secondo ICL il modello migliore per due cluster è il modello EVV, rieseguo mclust imponendo questo metodo

modello4<-Mclust(banconotepc,G=2,modelNames = "EVV")
summary(modello4)

# ---------------------------------------------------- 
#   Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
#   
#   Mclust EVV (ellipsoidal, equal volume) model with 2 components: 
#   
#   log-likelihood    n df       BIC       ICL
# -7621.642 1372 10 -15315.52 -15613.58
# 
# Clustering table:
#   1   2 
# 919 453 

etichette.cl<-as.factor(modello4$classification)
levels(etichette.cl)<-c("vere","false")
length(etichette.cl)
etichette.ve<-unlist(banconote[,5])
classError(etichette.cl, etichette.ve)

# $misclassified
# [1]   35   96  100  108  124  127  152  169  173  185  195  196  201  221  228  231
# [17]  232  242  292  304  323  327  346  350  360  375  378  387  389  395  421  464
# [33]  466  479  493  517  563  582  594  605  674  685  728  733  741  763  764  770
# [49]  771  775  776  779  783  784  790  791  796  797  798  803  804  805  811  812
# [65]  819  825  831  832  836  837  844  845  851  852  857  858  859  864  865  866
# [81]  872  873  880  885  886  892  893  897  898  901  905  906  911  912  913  918
# [97]  919  920  924  925  926  927  932  933  934  941  947  953  954  958  959  966
# [113]  967  972  973  974  979  980  981  985  986  987  988  994  995 1002 1007 1008
# [129] 1014 1015 1019 1020 1022 1027 1028 1033 1034 1035 1040 1041 1042 1046 1047 1048
# [145] 1049 1054 1055 1056 1063 1068 1069 1075 1076 1080 1081 1088 1089 1094 1095 1096
# [161] 1101 1102 1103 1107 1108 1109 1110 1115 1116 1117 1124 1130 1137 1141 1142 1149
# [177] 1150 1156 1157 1162 1163 1164 1169 1170 1171 1177 1178 1185 1191 1198 1202 1203
# [193] 1210 1211 1217 1218 1223 1224 1225 1230 1231 1232 1238 1239 1246 1252 1259 1263
# [209] 1264 1271 1272 1278 1279 1284 1285 1286 1291 1292 1293 1299 1300 1307 1312 1313
# [225] 1318 1319 1320 1324 1325 1327 1328 1332 1333 1338 1339 1340 1345 1346 1347 1351
# [241] 1352 1353 1354 1359 1360 1361 1368
# 
# $errorRate
# [1] 0.1800292

par(mfrow=c(1,1))
misclassificate<-(classError(etichette.cl, etichette.ve)$misclassified)

length(misclassificate)/nrow(banconote)*100 #18% delle us sono allocate in un cluster sbagliato 

adjustedRandIndex(etichette.cl,etichette.ve) #ARI 40,8%
banconotepcc2<-banconotepcc
banconotepcc2$cluster<-etichette.cl
plotv<-ggplot(banconotepcc2,aes(x=asimmetria, y=varianza, colour=tipo))+
    geom_point()+
    scale_color_manual(values=c("green3","red3"))+
    labs(title="vere label")+
    theme_bw()
plotc<-ggplot(banconotepcc2,aes(x=asimmetria, y=varianza, color=cluster))+
    geom_point()+
    geom_point(data=banconotepcc2[misclassificate,], col=aes("blue"))+
    
    scale_color_manual(values=c("green3","red3","blue"), 
                       labels=c("vere","false","misclassificate"))+
    theme_bw()+
    labs(title="clusterizzazione")

grid.arrange(plotv, plotc, ncol = 2)

banconote_df<-as.data.frame(banconote)
uncerPlot (z=modello4$z,truth=etichette.ve)
coordProj (data=banconote_df, dimens=c(2,1), what="uncertainty",
           parameters=modello4$parameters , z=modello4$z, main=T) 


confusionMatrix(etichette.cl,etichette.ve, positive = "vere")           

# Confusion Matrix and Statistics
# 
#            Reference
# Prediction vere false
#     vere    717   202
#     false    45   408
# 
# Accuracy : 0.82          
# 95% CI : (0.7986, 0.84)
# No Information Rate : 0.5554        
# P-Value [Acc > NIR] : < 2.2e-16     
# 
# Kappa : 0.6259        
# 
# Mcnemar's Test P-Value : < 2.2e-16     
#                                         
#             Sensitivity : 0.9409        
#             Specificity : 0.6689        
#          Pos Pred Value : 0.7802        
#          Neg Pred Value : 0.9007        
#              Prevalence : 0.5554        
#          Detection Rate : 0.5226        
#    Detection Prevalence : 0.6698        
#       Balanced Accuracy : 0.8049        
#                                         
#        'Positive' Class : vere

# ha particolare importanza nel nostro caso il valore della specificità in quanto rileva il rapporto delle banconote false clusterizzate come tali rispetto al numero totali di banconote false.
# dunque solamente il 66,89% delle banconote false vengono rilevate come tali, lasciando un 33,11% di banconote false in circolazione in quanto rilevate come vere. 
# la clusterizzazione non sembra dunque un ottimo metodo, viste le considerazioni sulle diverse tipologie di banconote e gli esiti sulla variabile target in analisi
# Proviamo a usare metodi di classificazione avendo a dispozione le vere etichette


#MODEL BASED CLASSIFICATION-----------------------------------------------------

data <- banconote[,-5] #tengo solo le variabili
class <- unlist (banconote[,5])#true labels
n <- nrow(data)

#EDDA

set.seed(123)
test.set.labels <- sample(1:n,250) #test set pari a circa il 20% delle obs

edda<-mixmodLearn(data[-test.set.labels,], #tolgo le etichette del test set
                  class[-test.set.labels], 
                  models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                  criterion=c('CV','BIC'))

edda@bestResult  

# * nbCluster   =  2 
# * model name  =  Gaussian_pk_Lk_Ck 
# * criterion   =  CV(0.0169) BIC(21032.6659)
# * likelihood  =  -10414.5143 

edda@results[[1]]@model #Gaussian_pk_Lk_Ck

#CV e BIC

BIC = CV = rep(NA ,length(edda@models@listModels) )
for (i in 1: length(edda@models@listModels)){
  ind = which(edda@results [[i]] @model #ordinati 
              == edda@models@listModels) #non ordinati
  CV[ind] = edda@results [[i]] @criterionValue [1]
  BIC[ind] = edda@results [[i]] @criterionValue [2]
}

criteri<-as.data.frame(cbind(c("Gaussian_pk_L_I","Gaussian_pk_Lk_I","Gaussian_pk_L_B","Gaussian_pk_Lk_B","Gaussian_pk_L_Bk",
                               "Gaussian_pk_Lk_Bk","Gaussian_pk_L_C","Gaussian_pk_Lk_C","Gaussian_pk_L_D_Ak_D","Gaussian_pk_Lk_D_Ak_D",
                               "Gaussian_pk_L_Dk_A_Dk","Gaussian_pk_Lk_Dk_A_Dk","Gaussian_pk_L_Ck","Gaussian_pk_Lk_Ck"),
                             round(CV,3),round(BIC,1)))
colnames(criteri)<-c('modello','CV','BIC')
criteri$modello[which.min(criteri$CV)]

cv<-ggplot(criteri, aes(x=modello,y=CV,group=FALSE))+
  geom_point(col='blue2')+
  geom_line(linetype='dashed',linewidth=.2, col='blue2')+
  geom_line(aes(x=modello[which.min(CV)]), linewidth=.3,col='red')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bic<-ggplot(criteri, aes(x=modello,y=BIC,group=FALSE))+
  geom_point(col='darkgreen')+
  geom_line(linetype='dashed',linewidth=.2, col='darkgreen')+
  geom_line(aes(x=modello[which.min(BIC)]), linewidth=.3,col='red')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(cv, bic, nrow = 2)


#step 2 della classificazione: data la regola di classificazione assegno le osservazioni
#appartenenti al test step a ciascuno dei due gruppi

prev<- mixmodPredict(data[test.set.labels,], edda["bestResult"])
mean(as.integer(class[test.set.labels]) == prev["partition"])
#il classificatore ne indovina il 98.8%

#confronto le lables previste con quelle vere e conto quante sono le u.s. misclassificate

previsioni<-as.factor(prev['partition'])
levels(previsioni)<-c('vere','false')

confusionMatrix(previsioni,class[test.set.labels])

sum(previsioni!= class[test.set.labels])
#ci sono 3 unità misclassificate (su 250)
#il classificatore sbaglia sull'1% delle obs

#individuo quali u.s. sono state classificate male 
mis <- test.set.labels[which(prev['partition']!= class[test.set.labels])]
mis
#195 671 170


#grafico della classificazione con evidenziati i punti misclassificati
tipologia<-class[test.set.labels]
ggplot(data[test.set.labels,], aes(x=asimmetria, y=varianza, color=tipologia)) +
  geom_point()+
  geom_point(data=data[mis,],aes(col='blue'))+
  scale_color_manual(values = c('green3','red3','blue'),
                     labels=c('vere','false','misclassificate'))+
  theme_bw()+
  labs(title='EDDA classification')


#MDA

#prova1

set.seed(123)
test.set.labels <- sample(1:n,250) 

mda1<- MclustDA(data[-test.set.labels,], #training set
                class[-test.set.labels])
summary(mda1)

# MclustDA model summary: 
#   
#   log-likelihood    n  df       BIC
# -8671.641 1121 136 -18298.27
# 
# Classes   n     % Model G
# 1 623 55.58   VEV 5
# 2 498 44.42   VVV 5
# 
# Training confusion matrix:
#   Predicted
# Class   1   2
# 1 623   0
# 2   0 498
# Classification error = 0 
# Brier score          = 0 

#usando tutte le us
mda.tot<-MclustDA(data,class)
cv<-cvMclustDA(mda.tot)
cv$ce #0

#sul test set
prev2 <- predict(mda1, data[test.set.labels ,])
sum(predict(mda1, data[test.set.labels ,])$class != class[test.set.labels])
previsioni2<-as.factor(prev2$classification)
levels(previsioni2)<-c('vere','false')
confusionMatrix(prev2$classification, class[test.set.labels])

# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   0   1
#          0 138   0
#          1   0 112
# 
# Accuracy : 1          
# Sensitivity : 1.000      
# Specificity : 1.000  

#Con MDA la classificazione è perfetta: non ci sono unità misclassificate
#potevamo aspettarcelo in quanto anche il training del classificatore avviene senza errori
#E' fondamentale che la specificity sia 1: significa che tutte le banconote realmente
#false vengono classificate come tali


#prova2
set.seed(123)
test.set.labels <- sample(1:n,250) 
mda2<- MclustDA(data[-test.set.labels,], #training set
                class[-test.set.labels],
                G=(1:10))#true labels
summary(mda2)

# MclustDA model summary: 
#   
#   log-likelihood    n  df       BIC
# -7883.085 1121 235 -17416.33
# 
# Classes   n     % Model  G
# 1 623 55.58   VEV 10
# 2 498 44.42   EEV 10

classError(predict(mda2, data[test.set.labels ,])$classification, class[test.set.labels])


#prova3
set.seed(123)
test.set.labels <- sample(1:n,250) 
mda3<- MclustDA(data[-test.set.labels,c(2,1)], #training set
                class[-test.set.labels])#true labels
summary(mda3)


# MclustDA model summary: 
#   
#   log-likelihood    n df       BIC
# -5849.844 1121 44 -12008.66
# 
# Classes   n     % Model G
# 1 623 55.58   VII 5
# 2 498 44.42   VVE 5

classError(predict(mda3, data[test.set.labels ,c(2,1)])$classification, class[test.set.labels])

# $misclassified
# [1]  25  35  67  68  77  80  90  95 116 117 158 169
# [13] 185 197 198 199 230 238
# 
# $errorRate
# [1] 0.072

#grafico prova1
tipologia<-class[test.set.labels]
ggplot(data[test.set.labels,], aes(x=asimmetria, y=varianza, color=tipologia)) +
  geom_point()+
  scale_color_manual(values = c('green3','red3'),
                     labels=c('vere','false'))+
  theme_bw()+
  labs(title='MDA classification')


#REGRESSION WITH COVARIATES-----------------------------------------------------

ggpairs(banconote[,-5])
banconotereg<-banconote
banconotereg$tipo<-ifelse(banconotereg$tipo == "vere", 0, 1)
str(banconotereg)

#esempio di come NON andrebbe fatto
f1<-flexmix(tipo ~ asimmetria+varianza+curtosi+entropia,data=banconotereg, k = 2, model = FLXMCmvcombi())
parameters(f1)
summary(f1)
table(clusters(f1), etichette.ve)
#NON HA SENSO, infatti non si osservano a priori i tipi, inoltre si dovrebbero studiare i legami tra le variabili per cercare di creare cluster 

ggpairs(data=banconote_df, columns=1:4, ggplot2::aes(col=tipo))
f2<-flexmix(entropia~curtosi+asimmetria+varianza, data=banconote, k=2,model = FLXMCmvnorm())
summary(f2)
table(clusters(f2), etichette.ve)
# è sbagliato perchè assume che ogni esplicativa abbia una distribuzione normale multivariata, nel nostro caso in realtà sono delle misture


f3<-stepFlexmix(varianza~curtosi, data=banconote, k=2, concomitant =FLXMCmvnorm(), model=FLXMRglm(family="gaussian"))
cf3<-refit(f3)
summary(cf3)
summary(f3)
f3@components
f3.cluster<-as.factor((f3@cluster))
levels(f3.cluster)<-c("vere","false")

# $Comp.1
# Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  3.939817   0.090080  43.737 < 2.2e-16 ***
#   curtosi     -0.196917   0.019318 -10.194 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# $Comp.2
# Estimate Std. Error  z value Pr(>|z|)    
# (Intercept) -0.192759   0.127145  -1.5161   0.1295    
# curtosi     -0.226132   0.016786 -13.4715   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


confusionMatrix(f3.cluster,
                etichette.ve)
KLdiv(f3)
# vere
# predette vere false
# vere   362     0
# false  400   610
# [,1]     [,2]
# [1,]     0.00 3224.064
# [2,] 19441.82    0.000
misclassificate2<- ifelse(banconoteregcur$cluster==banconoteregcur$tipo,FALSE,TRUE) 

#non buono ma identifica bene le banconote false
plotv<-ggplot(banconote,aes(x=curtosi, y=varianza, colour=tipo))+
  geom_point()+
  scale_color_manual(values=c("green3","red3"))+
  labs(title="vere label")+
  theme_bw()
plotc<-ggplot(banconoteregcur,aes(x=curtosi, y=varianza, color=cluster))+
  geom_point()+
  geom_point(data=banconoteregcur[misclassificate2,], col=aes("blue"))+
  geom_abline(slope=-0.2252, intercept = -0.2088, col="red3")+
  geom_abline(slope=-0.1964, intercept=3.9280, col="green3")+
  scale_color_manual(values=c("green3","red3","blue"),label=c("vere","false", "misclassificate"))+
  theme_bw()+
  labs(title="clusterizzazione")

grid.arrange(plotv, plotc, ncol = 2)
plot(f3)

f4<-stepFlexmix(varianza~curtosi+entropia, data=banconote, k=2, concomitant =FLXMCmvnorm(varianza~curtosi+entropia), model=FLXMRglm(family="gaussian"))
summary(f4)
summary(refit(f4))
table(clusters(f4), etichette.ve)

banconoteregcur2<-banconote
banconoteregcur2$cluster<-clusters(f4)

banconoteregcur2$cluster<-as.factor(banconoteregcur2$cluster)
levels(banconoteregcur2$cluster)<-c("vere","false")

table(predette=banconoteregcur2$cluster,vere=banconoteregcur2$tipo)
confusionMatrix(banconoteregcur2$cluster,banconoteregcur2$tipo)

# Reference
# Prediction vere false
# vere   659    72
# false  103   538
# 
# Accuracy : 0.8724          
# 95% CI : (0.8536, 0.8897)
# No Information Rate : 0.5554          
# P-Value [Acc > NIR] : < 2e-16         
# 
# Kappa : 0.743           
# 
# Mcnemar's Test P-Value : 0.02334         
#                                           
#             Sensitivity : 0.8648          
#             Specificity : 0.8820          
#          Pos Pred Value : 0.9015          
#          Neg Pred Value : 0.8393          
#              Prevalence : 0.5554          
#          Detection Rate : 0.4803          
#    Detection Prevalence : 0.5328          
#       Balanced Accuracy : 0.8734          
#                                           
#        'Positive' Class : vere    

(CER<-(72+103)/1372  * 100) #12.7551% non molto bene ma meglio del modello precedente, che però era perfetto nel rilevare le banconote false

misclassificate3<- ifelse(banconoteregcur2$cluster==banconoteregcur2$tipo,FALSE,TRUE) 

#non buono ma identifica bene le banconote false
plotv<-ggplot(banconote,aes(x=curtosi, y=varianza, colour=tipo))+
  geom_point()+
  scale_color_manual(values=c("green3","red3"))+
  labs(title="vere label")+
  theme_bw()
plotc<-ggplot(banconoteregcur2,aes(x=curtosi, y=varianza, color=cluster))+
  geom_point()+
  geom_point(data=banconoteregcur2[misclassificate3,], col=aes("blue"))+
  geom_smooth(method="lm",se=F)+
  scale_color_manual(values=c("green3","red3","blue"),label=c("vere","false", "misclassificate"))+
  theme_bw()+
  labs(title="clusterizzazione")

grid.arrange(plotv, plotc, ncol = 2)

plot(f4)
#notevole diminuzione dei putni misclassificati

ggplot(data=banconote, mapping = aes(x=asimmetria, y=varianza,color=tipo))+
  geom_point()+
  geom_smooth(se=F)+
  scale_color_manual(values=c("green3","red3"))+
  theme_bw()
#si vede che il geom_smooth in realtà è un loess, non implementabile

str(banconote_df)
ggpairs(data=banconote_df, columns=1:4, ggplot2::aes(col=tipo))
