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
library(interactionTest)
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
























