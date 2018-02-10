# R script to replicate Manger - Pickup paper with historical data
# The Coevolution of Trade Agreement Networks and Democracy
# Journal of Conflict Resolutionracy

#install.packages("RSiena", repos = "http://R-Forge.R-project.org")
library(RSiena)

rm(list = ls(all = TRUE))
gc()

#Read in data files from main project data directory
#setwd("C:\\Research\\GoldStandard\\Data\\Input")
setwd("U:\\Research\\GoldStandard\\Data\\Input")  #Server version





#####Data#####

#Treaty Network
load("Pahre_array.RData")

#Treaties<-Treaties[,,57:97]
Treaties<-Pahre_array[,,36:99]  #use code from other project to blanc out zeros
#Structural Zero
#BGR 1850-1878
Treaties["bgr",,c(1:29)]<-10
Treaties[,"bgr",c(1:29)]<-10
#CAn 1850-1867
Treaties["can",,c(1:18)]<-10
Treaties[,"can",c(1:18)]<-10
#ITA 1850-1861
Treaties["ita",,c(1:12)]<-10
Treaties[,"ita",c(1:12)]<-10
#Rum 1850-1878
Treaties["rom",,c(1:29)]<-10
Treaties[,"rom",c(1:29)]<-10
#SRB 1850-1872
Treaties["srb",,c(1:18)]<-10
Treaties[,"srb",c(1:18)]<-10

#CUB 1850-1898
Treaties["cub",,c(1:49)]<-10
Treaties[,"cub",c(1:49)]<-10

Treaties<-Treaties[,,22:64]
#Trade agreement network



#Political variables
load("polityIV.RData")
PolityIV<-polityIV[,57:97]
PolityIV<-PolityIV+10



##Alliance network
load("alliancenetwork.RData")        
Alliances<-alliance_nw[,,57:97]

#BGR 1850-1878
Alliances["bgr",,c(1:8)]<-10
Alliances[,"bgr",c(1:8)]<-10
#Rum 1850-1878
Treaties["rom",,c(1:8)]<-10
Treaties[,"rom",c(1:8)]<-10
#CUB 1850-1898
Treaties["cub",,c(1:28)]<-10
Treaties[,"cub",c(1:28)]<-10


#read-in other Covariates (Rui)
load("Covariates_other.RData")

#GDP Variables
load("gdp.RData")
GDPpc<-gdp[,57:97]
GDPpc<-GDPpc/10000   #10000


#Population and total GDP
Population<-Covariates[,c(22:62),"popn"]
GDP<-GDPpc*Population/100000


#Distance 
load("DistanceMatrix.RData")
dist<-distance_mat
dist<-dist/1000

#Contiguity
load("contiguity.RData")         #contiguity
contiguity_nw<-data.matrix(contiguity)

#War
load("warnetwork.RData")         #war_nw
war_nw<-war_nw[,,57:99]
War<-apply(war_nw,3,rowSums)
War[War>1]<-1

#Language
load("language.RData")           #language


#Trade data
#load("RIC_Trade.RData")
#Trade_RIC<-Trade_nw[,,22:62]/10000000
#Trade_RIC<-round(Trade_RIC, 5)


load("TradHist_full.RData")
TradeFlow<-round(Tradefull_nw[,,22:64]/100000000,5)

load("TradHistFull_share.RData")
TradeShare<-round(Tradefull_share[,,22:64],4)

#read Monetary Standard
load("Money_matrix.RData")  #4 levels
MoneyStandard<-Money_matrix[,22:64]

#States list
load("states.RData")
states<-as.data.frame(states_list)
states[,1]<-rownames(dist)




#Take out one wave to check convergence
AllPer<-c(1:40) #c(1:17,19:31,33:40)
AllObs<-c(1:41)#c(1:17,19:31,33:41)


########################################
####Set Working Directory to project specific

#setwd("C:/Research/GoldStandard/replication/working")
setwd("U:/Research/GoldStandard/replication/working")


#########################Variables
#Dependent Variable
Treaties  <- sienaNet(Treaties[,,AllObs],allowOnly=FALSE)
Alliances <- sienaNet(Alliances[,,AllObs],allowOnly=FALSE)
Democracy<- sienaNet(PolityIV[,AllObs], type="behavior",allowOnly=FALSE)


#Independent Variables
MoneyStandard <- varCovar(MoneyStandard[,AllPer])
GDP       <- varCovar(GDP[,AllPer])
GDPpc     <- varCovar(GDPpc[,AllPer])
Distance  <- coDyadCovar(dist)
Contiguity <-coDyadCovar(contiguity_nw)
Language   <-coDyadCovar(language)
#War       <-varDyadCovar(war_nw[,,AllPer])
War        <-varCovar(War[,AllPer],centered=FALSE)
TradeFlow  <-varDyadCovar(TradeFlow[,,AllPer],centered=FALSE)
TradeShare  <-varDyadCovar(TradeShare[,,AllPer],centered=FALSE)



#Create Siena DAta Object
myDataAll<-sienaDataCreate(Treaties,Alliances,Democracy, Distance, Contiguity,Language,War,MoneyStandard, GDP,GDPpc,TradeFlow,TradeShare)



#####################Effects
myEffAll<-getEffects(myDataAll)
modelAll <- sienaModelCreate(projname = "period_All", modelType = 3, n3=3000, dolby=FALSE,nsub=5)

#############
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=3)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=4)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=5)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=6)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=9)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=18)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=19)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=14)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=15)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=14)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=21)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=22)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=23)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=24)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=26)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=28)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=29)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Alliances",type="rate", period=32)

myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Democracy",type="rate", period=11)
#myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Democracy",type="rate", period=13)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Democracy",type="rate", period=14)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Democracy",type="rate", period=16)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Democracy",type="rate", period=22)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Democracy",type="rate", period=26)
myEffAll <- setEffect(myEffAll, Rate, initialValue=0.0001,fix=TRUE, name="Democracy",type="rate", period=28)


#####################################################################################

#Treaties

#Triads
myEffAll<-includeEffects(myEffAll, name = "Treaties", transTriads, include=TRUE)
myEffAll<-includeEffects(myEffAll, name = "Alliances", transTriads, include=TRUE)



#Partner at Distance 2
myEffAll<- includeEffects(myEffAll, name = "Treaties",  nbrDist2)
myEffAll<- includeEffects(myEffAll, name = "Alliances",  nbrDist2)

#myEffAll<- includeEffects(myEffAll, name = "Treaties",  between)
#myEffAll<- includeEffects(myEffAll, name = "Alliances", between)

#Degrees
myEffAll<-includeEffects(myEffAll, name = "Treaties", outOutAss, include=TRUE)
myEffAll<-includeEffects(myEffAll, name = "Alliances", outOutAss, include=TRUE)


#Interaction

#Crossproduct Networks
myEffAll<- includeEffects(myEffAll, name = "Alliances",  crprod, interaction1 = "Treaties")
myEffAll<- includeEffects(myEffAll, name = "Treaties",  crprod, interaction1 = "Alliances")

#Influence behaviour on networks
myEffAll<- includeEffects(myEffAll, name = "Alliances",  altX, interaction1 = "Democracy")
myEffAll<- includeEffects(myEffAll, name = "Treaties",  altX, interaction1 = "Democracy")


#myEffAll<- includeEffects(myEffAll, name = "Alliances",  egoX, interaction1 = "Democracy")
#myEffAll<- includeEffects(myEffAll, name = "Treaties",  egoX, interaction1 = "Democracy")

#DiffX seemingly has NA standard error 
myEffAll<- includeEffects(myEffAll, name = "Alliances",  diffSqX, interaction1 = "Democracy")
myEffAll<- includeEffects(myEffAll, name = "Treaties",  diffSqX, interaction1 = "Democracy")

#myEffAll<- includeEffects(myEffAll, name = "Alliances",  simX, interaction1 = "Democracy")
#myEffAll<- includeEffects(myEffAll, name = "Treaties",  simX, interaction1 = "Democracy")



#Influence of networks on behaviour
myEffAll<- includeEffects(myEffAll, name = "Democracy", outdeg,  interaction1 = "Treaties")
myEffAll<- includeEffects(myEffAll, name = "Democracy", outdeg,  interaction1 = "Alliances")

myEffAll<- includeEffects(myEffAll, name = "Democracy", isolate,   interaction1 = "Treaties")
myEffAll<- includeEffects(myEffAll, name = "Democracy", isolate,  interaction1 = "Alliances")

#myEffAll<- includeEffects(myEffAll, name = "Democracy", avSim,  interaction1 = "Treaties")
#myEffAll<- includeEffects(myEffAll, name = "Democracy", avSim,  interaction1 = "Alliances")

myEffAll<- includeEffects(myEffAll, name = "Democracy", avAlt,  interaction1 = "Treaties")
myEffAll<- includeEffects(myEffAll, name = "Democracy", avAlt,  interaction1 = "Alliances")


#myEffAll<- includeEffects(myEffAll, name = "Democracy", FFDeg, interaction1 = "Treaties" ,interaction2 = "Alliances")

ansBase <- siena07(modelAll, data = myDataAll, effects = myEffAll, nbrNodes=10, useCluster=TRUE, initC=TRUE)


#Covariates GDP
myEffAll<- includeEffects(myEffAll, name = "Treaties",  altX, interaction1 = "GDP")
myEffAll<- includeEffects(myEffAll, name = "Alliances",  altX, interaction1 = "GDP")


#myEffAll<- includeEffects(myEffAll, name = "Treaties",  egoX, interaction1 = "GDPpc")
#myEffAll<- includeEffects(myEffAll, name = "Alliances",  egoX, interaction1 = "GDPpc")


myEffAll <- includeEffects(myEffAll, effFrom,name = "Democracy", interaction1="GDPpc")

myEffAll <- includeEffects(myEffAll, avXAlt,name = "Democracy", interaction1="GDPpc",interaction2 = "Treaties")
myEffAll <- includeEffects(myEffAll, avXAlt,name = "Democracy", interaction1="GDPpc",interaction2 = "Alliances")

myEffAll <- includeEffects(myEffAll, avAltW, name="Democracy",interaction1="Treaties", interaction2="TradeShare")
myEffAll <- includeEffects(myEffAll, avAltW, name="Democracy",interaction1="Alliances", interaction2="TradeShare")


ansGDP<-siena07(modelAll, data = myDataAll, effects = myEffAll, nbrNodes=10, useCluster=TRUE, initC=TRUE, prevAns=ansBase)

#Covariates Other

myEffAll <- includeEffects(myEffAll, X,name="Alliances", interaction1="Distance")
myEffAll <- includeEffects(myEffAll, X,name="Treaties", interaction1="Distance")

myEffAll <- includeEffects(myEffAll, X,name="Alliances", interaction1="Contiguity")
myEffAll <- includeEffects(myEffAll, X,name="Treaties", interaction1="Contiguity")

myEffAll <- includeEffects(myEffAll, X,name="Alliances", interaction1="Language")
myEffAll <- includeEffects(myEffAll, X,name="Treaties", interaction1="Language")


#myEffAll <- includeEffects(myEffAll, X,name="Alliances", interaction1="War")
#myEffAll <- includeEffects(myEffAll, X,name="Treaties", interaction1="War")
myEffAll <- includeEffects(myEffAll, effFrom,name="Democracy", interaction1="War")


#ansDist<-siena07(modelAll, data = myDataAll, effects = myEffAll, nbrNodes=10, useCluster=TRUE, initC=TRUE, prevAns=ansGDP)
#


myEffAll <- includeEffects(myEffAll, X, name="Treaties", interaction1="TradeShare")
myEffAll <- includeEffects(myEffAll, X, name="Alliances", interaction1="TradeShare")


myEffAll <- includeEffects(myEffAll, effFrom,name = "Democracy", interaction1="MoneyStandard")
myEffAll <- includeEffects(myEffAll, avXAlt,name = "Democracy", interaction1="MoneyStandard",interaction2 = "Treaties")
myEffAll <- includeEffects(myEffAll, avXAlt,name = "Democracy", interaction1="MoneyStandard",interaction2 = "Alliances")



ansAll<-siena07(modelAll, data = myDataAll, effects = myEffAll, nbrNodes=10, useCluster=TRUE, initC=TRUE, prevAns=ansGDP)


#library(texreg)
#texreg(list(ansBase,ansGDP,ansAll),file="ResultTable.txt",stars = c(0.001, 0.01, 0.05, 0.1),longtable=TRUE,single.row = TRUE)































