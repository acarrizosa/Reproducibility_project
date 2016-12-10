setwd("C:/Users/amc27/Desktop/Replication_project")

dir.create("imagesvaccines")
#install.packages(c("car", "MASS", "Hmisc","gmodels", "xlsx", "foreign", "ggplot2", "survey", "questionr"))
library(car)
library(MASS)
library(Hmisc)
library(questionr)
library(foreign)
library(ggplot2)
library(gmodels)
library(xlsx)
library(survey)
rm(list=ls(all=TRUE))
#=======================================
#
# STUDY 1 
#
#=======================================

vaccines<-read.dta("vaccines replication data.dta")
names(vaccines)<- tolower(names(vaccines))
names(vaccines)[2]<-"weights"

#=======================
#       TABLE1
#
#=======================
detach("package:Hmisc") #hmisc and questionr don't like each other

# AGE X TREATMENT
agextreat<-with(vaccines,  prop.table(wtd.table(agecat4, dar333_35_treat, weights=weights),2))
agextreat<-agextreat[,1:3]
agextreat<-cbind(agextreat,total=prop.table(wtd.table(vaccines$agecat4, weights=vaccines$weights)))
rownames(agextreat)<- c("18-29", "30-44", "45-59", "60+")

#GENDER X TREATMENT
genderxtreat<-with(vaccines,  prop.table(wtd.table(gender, dar333_35_treat, weights=weights),2))
genderxtreat<-genderxtreat[,1:3]
genderxtreat<-cbind(genderxtreat,total=prop.table(wtd.table(vaccines$gender, weights=vaccines$weights)))
genderxtreat<-genderxtreat[1:2,]

#EDUCATION X TREATMENT
eduxtreat<-with(vaccines,  prop.table(wtd.table(educat4, dar333_35_treat, weights=weights),2))
eduxtreat<-eduxtreat[,1:3]
eduxtreat<-cbind(eduxtreat,total=prop.table(wtd.table(vaccines$educat4, weights=vaccines$weights)))
rownames(eduxtreat)<- c("High School or Less", "Some college", "College grad", "Post-grad")

#RACE X TREATMENT
racextreat<-with(vaccines,  prop.table(wtd.table(racecat4, dar333_35_treat, weights=weights),2))
racextreat<-racextreat[,1:3]
racextreat<-cbind(racextreat,total=prop.table(wtd.table(vaccines$racecat4, weights=vaccines$weights)))
rownames(racextreat)<- c("White", "Black", "Hispanic", "Other")

#CONCERN X TREATMENT
concernxtreat<-with(vaccines,  prop.table(wtd.table(dar300, dar333_35_treat, weights=weights),2))
concernxtreat<-concernxtreat[1:5,1:3]
concernxtreat<-cbind(concernxtreat,total=prop.table(wtd.table(vaccines$dar300, weights=vaccines$weights))[1:5])


#=======================
#       Figure 3
#
#=======================
getvaxtable<-data.frame(prop.table(wtd.table(vaccines$getvax, weights=vaccines$weights)))

getvaxtable$Var1<-factor(c("Very unlikely",
                      "Somewhat unlikely",
                      "Slightly unlikely",
                      "Slightly likely",
                      "Somewhat likely",
                      "Very likely"), 
                      levels=c("Very unlikely",
                      "Somewhat unlikely",
                      "Slightly unlikely",
                      "Slightly likely",
                      "Somewhat likely",
                       "Very likely"))

x11()
figure3<-ggplot(getvaxtable, aes(Var1, Freq))+
        geom_bar(stat="identity")+
        ylim(0, 0.5)+
        xlab("")+
        ylab("Percent")+
        ggtitle("Likelihood of Vaccination")
figure3
ggsave("imagesvaccines\\figure3.pdf")
dev.off()


#=======================
#       Figure 2
#
#=======================
vaxunsafetable<-data.frame(prop.table(wtd.table(vaccines$vaxunsafe, weights=vaccines$weights)))

vaxunsafetable$Var1<-factor(c("Very safe",
                           "Somewhat safe",
                           "Not very safe",
                           "Not at all safe"), 
                         levels=c("Very safe",
                                  "Somewhat safe",
                                  "Not very safe",
                                  "Not at all safe"))

x11()
figure2<-ggplot(vaxunsafetable, aes(Var1, Freq))+
        geom_bar(stat="identity")+
        ylim(0, 0.5)+
        xlab("")+
        ylab("Percent")+
        ggtitle("Perceived safety of flu vaccine")
figure2
ggsave("imagesvaccines\\figure2.pdf")
dev.off()


#=======================
#       Figure 1
#
#=======================
vaxgivesflutable<-data.frame(prop.table(wtd.table(vaccines$vaxgivesflu, weights=vaccines$weights)))

vaxgivesflutable$Var1<-factor(c("Very inaccurate",
                                "Somewhat inaccurate",
                                "Somewhat accurate",
                                "Very accurate"), 
                            levels=c("Very inaccurate",
                                     "Somewhat inaccurate",
                                     "Somewhat accurate",
                                     "Very accurate"))

x11()
figure1<-ggplot(vaxgivesflutable, aes(Var1, Freq))+
        geom_bar(stat="identity")+
        ylim(0, 0.5)+
        xlab("")+
        ylab("Percent")+
        ggtitle("Vaccine can give you the flu")
figure1
ggsave("imagesvaccines\\figure1.pdf")
dev.off()

#=======================
#       Table 2
#
#=======================
library(Hmisc)

#polr likes factor variables as the independent variable
vaccines$getvax<-as.factor(vaccines$getvax)
levels(vaccines$getvax)<-c("Very unlikely",
                 "Somewhat unlikely",
                 "Slightly unlikely",
                 "Slightly likely",
                 "Somewhat likely",
                 "Very likely")

vaccines$vaxunsafe<-as.factor(vaccines$vaxunsafe)
levels(vaccines$vaxunsafe)<-c("Very safe",
                "Somewhat safe",
                "Not very safe",
                "Not at all safe")

vaccines$vaxgivesflu<- as.factor(vaccines$vaxgivesflu)
levels(vaccines$vaxgivesflu)<-c("Very inaccurate",
                "Somewhat inaccurate",
                "Somewhat accurate",
                "Very accurate")


#subset vaccines
lowconcern<-subset(vaccines, vaccines$lowconcern==1)
highconcern<-subset(vaccines, vaccines$highconcern==1)

#vaxgivesflu ~ danger + correction 
#on vaccines and highconcern/lowconcern subsets
#===========
model1stable<-polr(vaxgivesflu ~ danger + correction, method="probit", weights=weights, data=vaccines)
model2stable<-polr(vaxgivesflu ~ danger + correction, method="probit", weights=weights, data=lowconcern)
model3stable<-polr(vaxgivesflu ~ danger + correction, method="probit", weights=weights, data=highconcern)

#vaxunsafe ~danger + correction
#on vaccines and highconcern/lowconcern subsets
#===========
model4stable<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=vaccines)
model5stable<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=lowconcern)
model6stable<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=highconcern)

#getvax ~danger + correction
#on vaccines and highconcern/lowconcern subsets
#===========
model7stable<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=vaccines)
model8stable<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=lowconcern)
model9stable<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=highconcern)

#==============================================================================================================
#       
#
#
#
##                                              BOOTSRAPS
#
#
#
#
#==============================================================================================================


#==============================================================================================================
#       
#
#
#
##                                      BOOTSTRAPPING vaxgivesflu models
#
#
#
#
#==============================================================================================================

#==============
#danger=0 correction=0 (control) model2stable and model3stable
#==============
danger<-0
correction<-0
newdata<- data.frame(danger,correction)
newdata

x<-1:1000
y<-NULL
z<-NULL
lowconcernsample<-NULL
highconcernsample<-NULL
predictmodel2<-list()
predictmodel3<-list()

for (i in 1:1000) {
y<-sample(774, 774, replace=TRUE)
z<- sample(226, 226, replace=TRUE)
lowconcernsample<-lowconcern[y,]
highconcernsample<-highconcern[z,]
model2<-polr(vaxgivesflu ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
model3<-polr(vaxgivesflu ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
predictmodel2[[i]]<-as.vector(predict(model2, newdata=newdata, type="probs"))
predictmodel3[[i]]<-as.vector(predict(model3, newdata=newdata, type="probs"))
}
bootmodel200<-matrix(unlist(predictmodel2), byrow=TRUE, ncol=4)
bootmodel300<-matrix(unlist(predictmodel3), byrow=TRUE, ncol=4)

#==============
#danger=0 correction=1 (correction)
#==============
danger<-0
correction<-1
newdata<- data.frame(danger,correction)
newdata

x<-1:1000
y<-NULL
z<-NULL
lowconcernsample<-NULL
highconcernsample<-NULL
predictmodel2<-list()
predictmodel3<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model2<-polr(vaxgivesflu ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model3<-polr(vaxgivesflu ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel2[[i]]<-as.vector(predict(model2, newdata=newdata, type="probs"))
        predictmodel3[[i]]<-as.vector(predict(model3, newdata=newdata, type="probs"))
}
bootmodel201<-matrix(unlist(predictmodel2), byrow=TRUE, ncol=4)
bootmodel301<-matrix(unlist(predictmodel3), byrow=TRUE, ncol=4)

#==============
#danger=1 correction=0 (danger)
#==============
danger<-1
correction<-0
newdata<- data.frame(danger,correction)
newdata

x<-1:1000
y<-NULL
z<-NULL
lowconcernsample<-NULL
highconcernsample<-NULL
predictmodel2<-list()
predictmodel3<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model2<-polr(vaxgivesflu ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model3<-polr(vaxgivesflu ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel2[[i]]<-as.vector(predict(model2, newdata=newdata, type="probs"))
        predictmodel3[[i]]<-as.vector(predict(model3, newdata=newdata, type="probs"))
}
bootmodel210<-matrix(unlist(predictmodel2), byrow=TRUE, ncol=4)
bootmodel310<-matrix(unlist(predictmodel3), byrow=TRUE, ncol=4)

#========================
# GET MEAN AND PERCENTILES
#
#========================

givesflupredbt200<-rowSums(bootmodel200[,3:4]) #model2stable bootstrapped coefficients danger 0 correction 0
givesflupredbt300<-rowSums(bootmodel300[,3:4]) #model3stable bootstrapped coefficients danger 0 correction 0
givesflupredbt201<-rowSums(bootmodel201[,3:4]) #model2stable bootstrapped coefficients danger 0 correction 1
givesflupredbt301<-rowSums(bootmodel301[,3:4]) #model3stable bootstrapped coefficients danger 0 correction 1
givesflupredbt210<-rowSums(bootmodel210[,3:4]) #model2stable bootstrapped coefficients danger 1 correction 0
givesflupredbt310<-rowSums(bootmodel310[,3:4]) #model3stable bootstrapped coefficients danger 1 correction 0

tilegivesflupredbt200<-c(quantile(givesflupredbt200, c(0.05, 0.95)), mean=mean(givesflupredbt200))
tilegivesflupredbt300<-c(quantile(givesflupredbt300, c(0.05, 0.95)), mean=mean(givesflupredbt300))
tilegivesflupredbt201<-c(quantile(givesflupredbt201, c(0.05, 0.95)), mean=mean(givesflupredbt201))
tilegivesflupredbt301<-c(quantile(givesflupredbt301, c(0.05, 0.95)), mean=mean(givesflupredbt301))
tilegivesflupredbt210<-c(quantile(givesflupredbt210, c(0.05, 0.95)), mean=mean(givesflupredbt210))
tilegivesflupredbt310<-c(quantile(givesflupredbt310, c(0.05, 0.95)), mean=mean(givesflupredbt310))


#=========================================================================
#
#
##       interactions for model2stable danger correction 
#
#
#=========================================================================
#just a reminder 
###model2stable<-polr(vaxgivesflu ~ danger + correction, data = lowconcern, weights = weights, method = "probit")

#===================
###interactions for model2stable danger 0 correction 0 (control)
#===================

me.vec<- mean(givesflupredbt200)
me.se<-sd(givesflupredbt200) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegivesflupredbt200<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegivesflupredbt200
newtilegivesflupredbt200

#calculate the interaction effect using degrees of freedom of model2stable
fdrInteraction(me.vec, me.se, df=model2stable$df, level=0.95) #1.963122
ci.hi200 <- me.vec + 1.963122 * me.se
ci.lo200 <- me.vec - 1.963122 * me.se

interacttilegivesflupredbt200<- data.frame(ci.lo200,ci.hi200, me.vec)

#look at all 3 of them
tilegivesflupredbt200
newtilegivesflupredbt200
interacttilegivesflupredbt200

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi200, ci.lo200)

#===================
###interactions for model2stable danger 1 correction 0 (danger)
#===================
me.vec<- mean(givesflupredbt210)
me.se<-sd(givesflupredbt210) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegivesflupredbt210<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegivesflupredbt210
newtilegivesflupredbt210

#calculate the interaction effect using degrees of freedom of model2stable
fdrInteraction(me.vec, me.se, df=model2stable$df, level=0.95) #1.963122
ci.hi210 <- me.vec + 1.963122 * me.se
ci.lo210 <- me.vec - 1.963122 * me.se

interacttilegivesflupredbt210<- data.frame(ci.lo210,ci.hi210, me.vec)

#look at all 3 of them
tilegivesflupredbt210
newtilegivesflupredbt210
interacttilegivesflupredbt210

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi210, ci.lo210)

#===================
###interactions for model2stable danger 0 correction 1 (givesflupredbt201)
#===================
me.vec<- mean(givesflupredbt201)
me.se<-sd(givesflupredbt201) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegivesflupredbt201<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegivesflupredbt201
newtilegivesflupredbt201

#calculate the interaction effect using degrees of freedom of model2stable
fdrInteraction(me.vec, me.se, df=model2stable$df, level=0.95) #1.963122
ci.hi201 <- me.vec + 1.963122 * me.se
ci.lo201 <- me.vec - 1.963122 * me.se

interacttilegivesflupredbt201<- data.frame(ci.lo201,ci.hi201, me.vec)

#look at all 3 of them
tilegivesflupredbt201
newtilegivesflupredbt201
interacttilegivesflupredbt201

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi201, ci.lo201)

#===================================================================
#       
#
##       DOTPLOT FOR MODEL2STABLE CONTROL, DANGER AND CORRECTION
#
#
#====================================================================
#just a reminder
###model2stable<-polr(vaxgivesflu ~ danger + correction, data = lowconcern, weights = weights, method = "probit")

#standardize format so all of them are vectors with the same "column" names
names(interacttilegivesflupredbt200)<- names(tilegivesflupredbt200)
names(interacttilegivesflupredbt210)<- names(tilegivesflupredbt210)
names(interacttilegivesflupredbt201)<-names(tilegivesflupredbt201)

interacttilegivesflupredbt200<- as.vector(interacttilegivesflupredbt200)
interacttilegivesflupredbt210<- as.vector(interacttilegivesflupredbt210)
interacttilegivesflupredbt201<-as.vector(interacttilegivesflupredbt201)

interacttilegivesflupredbt200<- as.numeric(as.character(interacttilegivesflupredbt200))
interacttilegivesflupredbt210<- as.numeric(as.character(interacttilegivesflupredbt210))
interacttilegivesflupredbt201<-as.numeric(as.character(interacttilegivesflupredbt201))


predmeanCIlow<-data.frame(tilegivesflupredbt200,
                          tilegivesflupredbt210,
                          tilegivesflupredbt201,
                          interacttilegivesflupredbt200,
                          interacttilegivesflupredbt210,
                          interacttilegivesflupredbt201)


predmeanCIlow<-as.data.frame(t(predmeanCIlow))
predmeanCIlow$names<- factor(c("Control", "Danger", "Correction"), levels=c("Control", "Danger", "Correction"))

levels(predmeanCIlow$names)

#====================
# graph without interaction effects
#====================

x11()
figure4<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        #annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        #annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        #annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("Low Side Effects Concern")+
        ggtitle("Vaccine can give you flu")+
        ylab("")+ 
        theme_bw()
figure4
ggsave("imagesvaccines\\figure4.pdf")
dev.off()

#=======================
# graph with interaction effects
#=======================

x11()
figure4.4<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("Low Side Effects Concern")+
        ggtitle("Vaccine can give you flu")+
        ylab("")+ 
        theme_bw()
figure4.4
ggsave("imagesvaccines\\figure4.4.pdf")
dev.off()

#===========================================================================================
#       
#
#       interactions for model3stable danger correction
#
#
#============================================================================================
#just a reminder 
###model3stable<-polr(vaxgivesflu ~ danger + correction, method="probit", weights=weights, data=highconcern)

#===================
###interactions for model3stable danger 0 correction 0 (control)
#===================

me.vec<- mean(givesflupredbt300)
me.se<-sd(givesflupredbt300) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegivesflupredbt300<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegivesflupredbt300
newtilegivesflupredbt300

#calculate the interaction effect using degrees of freedom of model2stable
fdrInteraction(me.vec, me.se, df=model2stable$df, level=0.95) #1.963122
ci.hi300 <- me.vec + 1.963122 * me.se
ci.lo300 <- me.vec - 1.963122 * me.se

interacttilegivesflupredbt300<- data.frame(ci.lo300,ci.hi300, me.vec)

#look at all 3 of them
tilegivesflupredbt300
newtilegivesflupredbt300
interacttilegivesflupredbt300

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi300, ci.lo300)

#===================
###interactions for model3stable danger 1 correction 0 (danger)
#===================
me.vec<- mean(givesflupredbt310)
me.se<-sd(givesflupredbt310) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegivesflupredbt310<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegivesflupredbt310
newtilegivesflupredbt310

#calculate the interaction effect using degrees of freedom of model2stable
fdrInteraction(me.vec, me.se, df=model2stable$df, level=0.95) #1.963122
ci.hi310 <- me.vec + 1.963122 * me.se
ci.lo310 <- me.vec - 1.963122 * me.se

interacttilegivesflupredbt310<- data.frame(ci.lo310,ci.hi310, me.vec)

#look at all 3 of them
tilegivesflupredbt310
newtilegivesflupredbt310
interacttilegivesflupredbt310

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi310, ci.lo310)

#===================
###interactions for model3stable danger 0 correction 1 (givesflupredbt301)
#===================
me.vec<- mean(givesflupredbt301)
me.se<-sd(givesflupredbt301) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegivesflupredbt301<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegivesflupredbt301
newtilegivesflupredbt301

#calculate the interaction effect using degrees of freedom of model2stable
fdrInteraction(me.vec, me.se, df=model2stable$df, level=0.95) #1.963122
ci.hi301 <- me.vec + 1.963122 * me.se
ci.lo301 <- me.vec - 1.963122 * me.se

interacttilegivesflupredbt301<- data.frame(ci.lo301,ci.hi301, me.vec)

#look at all 3 of them
tilegivesflupredbt301
newtilegivesflupredbt301
interacttilegivesflupredbt301

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi301, ci.lo301)


#========================================================================
#       
#      DOTPLOT FOR MODEL2STABLE CONTROL, DANGER AND CORRECTION
#
#
#========================================================================
#just a reminder
###model2stable<-polr(vaxgivesflu ~ danger + correction, data = lowconcern, weights = weights, method = "probit")

#standardize format so all of them are vectors with the same "column" names
names(interacttilegivesflupredbt300)<- names(tilegivesflupredbt300)
names(interacttilegivesflupredbt310)<- names(tilegivesflupredbt310)
names(interacttilegivesflupredbt301)<-names(tilegivesflupredbt301)

interacttilegivesflupredbt300<- as.vector(interacttilegivesflupredbt300)
interacttilegivesflupredbt310<- as.vector(interacttilegivesflupredbt310)
interacttilegivesflupredbt301<-as.vector(interacttilegivesflupredbt301)

interacttilegivesflupredbt300<- as.numeric(as.character(interacttilegivesflupredbt300))
interacttilegivesflupredbt310<- as.numeric(as.character(interacttilegivesflupredbt310))
interacttilegivesflupredbt301<-as.numeric(as.character(interacttilegivesflupredbt301))


predmeanCIlow<-data.frame(tilegivesflupredbt300,
                          tilegivesflupredbt310,
                          tilegivesflupredbt301,
                          interacttilegivesflupredbt300,
                          interacttilegivesflupredbt310,
                          interacttilegivesflupredbt301)


predmeanCIlow<-as.data.frame(t(predmeanCIlow))
predmeanCIlow$names<- factor(c("Control", "Danger", "Correction"), levels=c("Control", "Danger", "Correction"))

levels(predmeanCIlow$names)

#====================
# graph without interaction effects
#====================

x11()
figure5<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        #annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        #annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        #annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("Low Side Effects Concern")+
        ggtitle("Vaccine can give you flu")+
        ylab("")+ 
        theme_bw()
figure5
ggsave("imagesvaccines\\figure5.pdf")
dev.off()

#=======================
# graph with interaction effects
#=======================

x11()
figure5.5<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("Low Side Effects Concern")+
        ggtitle("Vaccine can give you flu")+
        ylab("")+ 
        theme_bw()
figure5.5
ggsave("imagesvaccines\\figure5.5.pdf")
dev.off()



#=================================================================================================================
#       
#
#
#
#                                              BOOTSTRAPPING vaxunsafe models
#
#
#
#
#==================================================================================================================

#==============
#danger=0 correction=0 (control) model5stable and model6stable
#==============
danger<-0
correction<-0
newdata<- data.frame(danger,correction)
newdata

x<-1:1000
y<-NULL
z<-NULL
lowconcernsample<-NULL
highconcernsample<-NULL
predictmodel2<-list()
predictmodel3<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model2<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model3<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel2[[i]]<-as.vector(predict(model2, newdata=newdata, type="probs"))
        predictmodel3[[i]]<-as.vector(predict(model3, newdata=newdata, type="probs"))
}
bootmodel200<-matrix(unlist(predictmodel2), byrow=TRUE, ncol=4)
bootmodel300<-matrix(unlist(predictmodel3), byrow=TRUE, ncol=4)

#==============
#danger=0 correction=1 (correction) model5stable and model6stable
#==============
danger<-0
correction<-1
newdata<- data.frame(danger,correction)
newdata

x<-1:1000
y<-NULL
z<-NULL
lowconcernsample<-NULL
highconcernsample<-NULL
predictmodel2<-list()
predictmodel3<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model2<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model3<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel2[[i]]<-as.vector(predict(model2, newdata=newdata, type="probs"))
        predictmodel3[[i]]<-as.vector(predict(model3, newdata=newdata, type="probs"))
}
bootmodel201<-matrix(unlist(predictmodel2), byrow=TRUE, ncol=4)
bootmodel301<-matrix(unlist(predictmodel3), byrow=TRUE, ncol=4)
#==============
#danger=1 correction=0 (danger) model5stable and model6stable
#==============
danger<-1
correction<-0
newdata<- data.frame(danger,correction)
newdata

x<-1:1000
y<-NULL
z<-NULL
lowconcernsample<-NULL
highconcernsample<-NULL
predictmodel2<-list()
predictmodel3<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model2<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model3<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel2[[i]]<-as.vector(predict(model2, newdata=newdata, type="probs"))
        predictmodel3[[i]]<-as.vector(predict(model3, newdata=newdata, type="probs"))
}
bootmodel210<-matrix(unlist(predictmodel2), byrow=TRUE, ncol=4)
bootmodel310<-matrix(unlist(predictmodel3), byrow=TRUE, ncol=4)

#========================
# GET MEAN AND PERCENTILES
#
#========================


unsafepredbt200<-rowSums(bootmodel200[,3:4])#model5stable bootstrapped coefficients
unsafepredbt300<-rowSums(bootmodel300[,3:4])#model6stable bootstrapped coefficients
unsafepredbt201<-rowSums(bootmodel201[,3:4])#model5stable bootstrapped coefficients
unsafepredbt301<-rowSums(bootmodel301[,3:4])#model6stable bootstrapped coefficients
unsafepredbt210<-rowSums(bootmodel210[,3:4])#model5stable bootstrapped coefficients
unsafepredbt310<-rowSums(bootmodel310[,3:4])#model6stable bootstrapped coefficients

tileunsafepredbt200<-c(quantile(unsafepredbt200, c(0.05, 0.95)), mean=mean(unsafepredbt200))
tileunsafepredbt300<-c(quantile(unsafepredbt300, c(0.05, 0.95)), mean=mean(unsafepredbt300))
tileunsafepredbt201<-c(quantile(unsafepredbt201, c(0.05, 0.95)), mean=mean(unsafepredbt201))
tileunsafepredbt301<-c(quantile(unsafepredbt301, c(0.05, 0.95)), mean=mean(unsafepredbt301))
tileunsafepredbt210<-c(quantile(unsafepredbt210, c(0.05, 0.95)), mean=mean(unsafepredbt210))
tileunsafepredbt310<-c(quantile(unsafepredbt310, c(0.05, 0.95)), mean=mean(unsafepredbt310))


#==========================
# Dotplot for low (figure6)
#==========================
predmeanCIlow<-data.frame(tileunsafepredbt200,
                          tileunsafepredbt210,
                          tileunsafepredbt201)


predmeanCIlow<-as.data.frame(t(predmeanCIlow))
predmeanCIlow$names<- factor(c("Control", "Danger", "Correction"), levels=c("Control", "Danger", "Correction"))

levels(predmeanCIlow$names)

x11()
figure6<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point()+ ylim(0, 1)+ 
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2])+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2]) +
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2])+
        xlab("Low Side Effects Concern")+
        ggtitle("Flu vaccine unsafe")+
        ylab("")

figure6
ggsave("imagesvaccines\\figure6.pdf")
dev.off()


#==========================
# Dotplot for high (figure7)
#==========================
predmeanCIhigh<-data.frame(tileunsafepredbt300,
                           tileunsafepredbt310,
                           tileunsafepredbt301)


predmeanCIhigh<-as.data.frame(t(predmeanCIhigh))
predmeanCIhigh$names<- factor(c("Control", "Danger", "Correction"), levels=c("Control", "Danger", "Correction"))

x11()
figure7<-ggplot(predmeanCIhigh, aes(names,mean))+
        geom_point()+ ylim(0, 1)+
        annotate("segment", x=predmeanCIhigh$names[1], xend=predmeanCIhigh$names[1], y=predmeanCIhigh[1,1], yend=predmeanCIhigh[1,2])+
        annotate("segment", x=predmeanCIhigh[2,4], xend=predmeanCIhigh[2,4], y=predmeanCIhigh[2,1], yend=predmeanCIhigh[2,2]) +
        annotate("segment", x=predmeanCIhigh[3,4], xend=predmeanCIhigh[3,4], y=predmeanCIhigh[3,1], yend=predmeanCIhigh[3,2])+
        xlab("High Side Effects Concern")+
        ggtitle("Flu vaccine unsafe")+
        ylab("")
figure7
ggsave("imagesvaccines\\figure7.pdf")
dev.off()


#====================================
#       
#
#
#
##       BOOTSTRAPPING getvax models
#
#
#
#
#====================================

#==============
#danger=0 correction=0 (control) model8stable and model9stable
#==============
danger<-0
correction<-0
newdata<- data.frame(danger,correction)
newdata

x<-1:1000
y<-NULL
z<-NULL
lowconcernsample<-NULL
highconcernsample<-NULL
predictmodel2<-list()
predictmodel3<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model2<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model3<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel2[[i]]<-as.vector(predict(model2, newdata=newdata, type="probs"))
        predictmodel3[[i]]<-as.vector(predict(model3, newdata=newdata, type="probs"))
}
bootmodel200<-matrix(unlist(predictmodel2), byrow=TRUE, ncol=6)
bootmodel300<-matrix(unlist(predictmodel3), byrow=TRUE, ncol=6)

#==============
#danger=0 correction=1 (correction)
#==============
danger<-0
correction<-1
newdata<- data.frame(danger,correction)
newdata

x<-1:1000
y<-NULL
z<-NULL
lowconcernsample<-NULL
highconcernsample<-NULL
predictmodel2<-list()
predictmodel3<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model2<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model3<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel2[[i]]<-as.vector(predict(model2, newdata=newdata, type="probs"))
        predictmodel3[[i]]<-as.vector(predict(model3, newdata=newdata, type="probs"))
}
bootmodel201<-matrix(unlist(predictmodel2), byrow=TRUE, ncol=6)
bootmodel301<-matrix(unlist(predictmodel3), byrow=TRUE, ncol=6)
#==============
#danger=1 correction=0 (danger)
#==============
danger<-1
correction<-0
newdata<- data.frame(danger,correction)
newdata

x<-1:1000
y<-NULL
z<-NULL
lowconcernsample<-NULL
highconcernsample<-NULL
predictmodel2<-list()
predictmodel3<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model2<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model3<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel2[[i]]<-as.vector(predict(model2, newdata=newdata, type="probs"))
        predictmodel3[[i]]<-as.vector(predict(model3, newdata=newdata, type="probs"))
}
bootmodel210<-matrix(unlist(predictmodel2), byrow=TRUE, ncol=6)
bootmodel310<-matrix(unlist(predictmodel3), byrow=TRUE, ncol=6)

#========================
# GET MEAN AND PERCENTILES
#
#========================


getvaxpredbt200<-rowSums(bootmodel200[,4:6])#model8stable bootstrapped coefficients
getvaxpredbt300<-rowSums(bootmodel300[,4:6])#model9stable bootstrapped coefficients
getvaxpredbt201<-rowSums(bootmodel201[,4:6])#model8stable bootstrapped coefficients
getvaxpredbt301<-rowSums(bootmodel301[,4:6])#model9stable bootstrapped coefficients
getvaxpredbt210<-rowSums(bootmodel210[,4:6])#model8stable bootstrapped coefficients
getvaxpredbt310<-rowSums(bootmodel310[,4:6])#model9stable bootstrapped coefficients

tilegetvaxpredbt200<-c(quantile(getvaxpredbt200, c(0.05, 0.95)), mean=mean(getvaxpredbt200))
tilegetvaxpredbt300<-c(quantile(getvaxpredbt300, c(0.05, 0.95)), mean=mean(getvaxpredbt300))
tilegetvaxpredbt201<-c(quantile(getvaxpredbt201, c(0.05, 0.95)), mean=mean(getvaxpredbt201))
tilegetvaxpredbt301<-c(quantile(getvaxpredbt301, c(0.05, 0.95)), mean=mean(getvaxpredbt301))
tilegetvaxpredbt210<-c(quantile(getvaxpredbt210, c(0.05, 0.95)), mean=mean(getvaxpredbt210))
tilegetvaxpredbt310<-c(quantile(getvaxpredbt310, c(0.05, 0.95)), mean=mean(getvaxpredbt310))


#==========================
# Dotplot for low (figure8)
#==========================
predmeanCIlow<-data.frame(tilegetvaxpredbt200,
                          tilegetvaxpredbt210,
                          tilegetvaxpredbt201)


predmeanCIlow<-as.data.frame(t(predmeanCIlow))
predmeanCIlow$names<- factor(c("Control", "Danger", "Correction"), levels=c("Control", "Danger", "Correction"))

levels(predmeanCIlow$names)

x11()
figure8<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point()+ ylim(0, 1)+ 
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2])+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2]) +
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2])+
        xlab("Low Side Effects Concern")+
        ggtitle("Likely to get flu vaccine")+
        ylab("")

figure8
ggsave("imagesvaccines\\figure8.pdf")
dev.off()


#==========================
# Dotplot for high (figure9)
#==========================
predmeanCIhigh<-data.frame(tilegetvaxpredbt300,
                           tilegetvaxpredbt310,
                           tilegetvaxpredbt301)


predmeanCIhigh<-as.data.frame(t(predmeanCIhigh))
predmeanCIhigh$names<- factor(c("Control", "Danger", "Correction"), levels=c("Control", "Danger", "Correction"))


figure9<-ggplot(predmeanCIhigh, aes(names,mean))+
        geom_point()+ ylim(0, 1)+
        annotate("segment", x=predmeanCIhigh$names[1], xend=predmeanCIhigh$names[1], y=predmeanCIhigh[1,1], yend=predmeanCIhigh[1,2])+
        annotate("segment", x=predmeanCIhigh[2,4], xend=predmeanCIhigh[2,4], y=predmeanCIhigh[2,1], yend=predmeanCIhigh[2,2]) +
        annotate("segment", x=predmeanCIhigh[3,4], xend=predmeanCIhigh[3,4], y=predmeanCIhigh[3,1], yend=predmeanCIhigh[3,2])+
        xlab("High Side Effects Concern")+
        ggtitle("Likely to get flu vaccine")+
        ylab("")
figure9
ggsave("imagesvaccines\\figure9.pdf")
dev.off()



#==============================================================================================================
#       
#
#
#
##                                              APPENDIX
#
#
#
#==============================================================================================================
detach("package:Hmisc")

#convert three binary treatment columns into a single variable
x<-vector()
for (i in 1:nrow(vaccines)){
if(vaccines$wave2danger[i]==1){
        x[i]<- "danger"
}else if (vaccines$wave2correction[i]==1){
                x[i]<- "correction"
}else if (vaccines$wave2control[i]==1){
        x[i]<- "control"
}
} 
#append to vaccines data frame.  Also create an attrition variable. 
vaccines$treat2wave<-x
vaccines$attrition<-is.na(vaccines$treat2wave)

#=======================
#wave 2 tables
#=======================

#agextreat wave 2
agextreatw2<-with(vaccines, prop.table(wtd.table(agecat4, treat2wave, weights=weights),2))
agextreatw2<-cbind(agextreatw2 ,total=with(vaccines, prop.table(wtd.table(agecat4, attrition, weights=weights),2 ))[,1])

#agextreat wave 2
agextreatw2<-with(vaccines, prop.table(wtd.table(agecat4, treat2wave, weights=weights),2))
agextreatw2<-cbind(agextreatw2 ,total=with(vaccines, prop.table(wtd.table(agecat4, attrition, weights=weights),2 ))[,1])
rownames(agextreatw2)<- c("18-29", "30-44", "45-59", "60+")

#genderxtreat wave2
genderxtreatw2<-with(vaccines, prop.table(wtd.table(gender, treat2wave, weights=weights),2))
genderxtreatw2<-cbind(genderxtreatw2 ,total=with(vaccines, prop.table(wtd.table(gender, attrition, weights=weights),2 ))[,1])[1:2,]

#eduxtreat wave2
eduxtreatw2<-with(vaccines, prop.table(wtd.table(educat4, treat2wave, weights=weights),2))
eduxtreatw2<-cbind(eduxtreatw2 ,total=with(vaccines, prop.table(wtd.table(educat4, attrition, weights=weights),2 ))[,1])
rownames(eduxtreatw2)<- c("High School or Less", "Some college", "College grad", "Post-grad")

#racextreat wave2
racextreatw2<-with(vaccines, prop.table(wtd.table(racecat4, treat2wave, weights=weights),2))
racextreatw2<-cbind(racextreatw2 ,total=with(vaccines, prop.table(wtd.table(racecat4, attrition, weights=weights),2 ))[,1])
rownames(racextreatw2)<- c("White", "Black", "Hispanic", "Other")

#concernxtreat wave2
concernxtreatw2<-with(vaccines, prop.table(wtd.table(dar300, treat2wave, weights=weights),2))
concernxtreatw2<-cbind(concernxtreatw2 ,total=with(vaccines, prop.table(wtd.table(dar300, attrition, weights=weights),2 ))[,1])[1:5,]

#==================
#Attrition tables
#==================
#recode attrition variable to make nicer tables
vaccines$attrition<-as.factor(vaccines$attrition)
levels(vaccines$attrition)<- c("Retention", "Attrition")

#tables
attritionage<-with(vaccines, prop.table(wtd.table(agecat4, attrition, weights=weights), 1))

attritiongender<-with(vaccines, prop.table(wtd.table(gender, attrition, weights=weights), 1))

attritionedu<-with(vaccines, prop.table(wtd.table(educat4, attrition, weights=weights), 1))

attritionrace<-with(vaccines, prop.table(wtd.table(racecat4, attrition, weights=weights), 1))

attritionconcern<-with(vaccines, prop.table(wtd.table(dar300, attrition, weights=weights), 1))

#=====================
# Appendix table 2
#=====================

polr(vaxgivesflu ~ danger*highconcern + correction*highconcern, method="probit", weights=weights, data=vaccines)

polr(vaxunsafe ~ danger*highconcern + correction*highconcern, method="probit", weights=weights, data=vaccines)

polr(getvax ~ danger*highconcern + correction*highconcern, method="probit", weights=weights, data=vaccines)


#=====================
# Appendix table 3
#=====================

polr(as.factor(vaxgivesflu2) ~ danger*highconcern + correction*highconcern, method="probit", weights=weights, data=lowconcern)
polr(as.factor(vaxgivesflu2) ~ danger*highconcern + correction*highconcern, method="probit", weights=weights, data=highconcern)

polr(as.factor(vaxunsafe2) ~ danger*highconcern + correction*highconcern, method="probit", weights=weights, data=lowconcern)
polr(as.factor(vaxunsafe2) ~ danger*highconcern + correction*highconcern, method="probit", weights=weights, data=highconcern)

polr(as.factor(getvax2) ~ danger*highconcern + correction*highconcern, method="probit", weights=weights, data=lowconcern)
polr(as.factor(getvax2) ~ danger*highconcern + correction*highconcern, method="probit", weights=weights, data=highconcern)


#=====================
# Robustness: OLS results for tables 2 and 3
#=====================


lm(as.numeric(vaxgivesflu) ~ danger + correction, data=vaccines, weights=weights)
lm(as.numeric(vaxgivesflu) ~ danger + correction, data=lowconcern, weights=weights)
lm(as.numeric(vaxgivesflu) ~ danger + correction, data=highconcern, weights=weights)

lm(as.numeric(vaxunsafe) ~ danger + correction, data=vaccines, weights=weights)
lm(as.numeric(vaxunsafe) ~ danger + correction, data=lowconcern, weights=weights)
lm(as.numeric(vaxunsafe) ~ danger + correction, data=highconcern, weights=weights)

lm(as.numeric(getvax) ~ danger + correction, data=vaccines, weights=weights)
lm(as.numeric(getvax) ~ danger + correction, data=lowconcern, weights=weights)
lm(as.numeric(getvax) ~ danger + correction, data=highconcern, weights=weights)

#=====================
# Robustness: binary results for table 2
#=====================

glm(givesflubinary ~ danger + correction, data=vaccines, weights=weights,family=binomial(link="probit"))
glm(givesflubinary ~ danger + correction, data=lowconcern, weights=weights,family=binomial(link="probit"))
glm(givesflubinary ~ danger + correction, data=highconcern, weights=weights,family=binomial(link="probit"))

glm(unsafebinary ~ danger + correction, data=vaccines, weights=weights,family=binomial(link="probit"))
glm(unsafebinary ~ danger + correction, data=lowconcern, weights=weights,family=binomial(link="probit"))
glm(unsafebinary ~ danger + correction, data=highconcern, weights=weights,family=binomial(link="probit"))















