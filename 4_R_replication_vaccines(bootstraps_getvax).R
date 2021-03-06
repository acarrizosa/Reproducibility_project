#dir.create("imagesvaccines")
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

#==============================================================================================================
#       
#
#
#
##                                      BOOTSTRAPPING getvax models
#
#
#
#
#==============================================================================================================


#must run this for everything here to work.  There are things we need from 
source("1_R_replication_vaccines(tables_stable_models).R")



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
predictmodel8<-list()
predictmodel9<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model8<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model9<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel8[[i]]<-as.vector(predict(model8, newdata=newdata, type="probs"))
        predictmodel9[[i]]<-as.vector(predict(model9, newdata=newdata, type="probs"))
}
bootmodel800<-matrix(unlist(predictmodel8), byrow=TRUE, ncol=6)
bootmodel900<-matrix(unlist(predictmodel9), byrow=TRUE, ncol=6)

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
predictmodel8<-list()
predictmodel9<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model8<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model9<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel8[[i]]<-as.vector(predict(model8, newdata=newdata, type="probs"))
        predictmodel9[[i]]<-as.vector(predict(model9, newdata=newdata, type="probs"))
}
bootmodel801<-matrix(unlist(predictmodel8), byrow=TRUE, ncol=6)
bootmodel901<-matrix(unlist(predictmodel9), byrow=TRUE, ncol=6)

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
predictmodel8<-list()
predictmodel9<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model8<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model9<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel8[[i]]<-as.vector(predict(model8, newdata=newdata, type="probs"))
        predictmodel9[[i]]<-as.vector(predict(model9, newdata=newdata, type="probs"))
}
bootmodel810<-matrix(unlist(predictmodel8), byrow=TRUE, ncol=6)
bootmodel910<-matrix(unlist(predictmodel9), byrow=TRUE, ncol=6)

#========================
# GET MEAN AND PERCENTILES
#
#========================

getvaxpredbt800<-rowSums(bootmodel800[,4:6]) #model8stable bootstrapped coefficients danger 0 correction 0
getvaxpredbt900<-rowSums(bootmodel900[,4:6]) #model9stable bootstrapped coefficients danger 0 correction 0
getvaxpredbt801<-rowSums(bootmodel801[,4:6]) #model8stable bootstrapped coefficients danger 0 correction 1
getvaxpredbt901<-rowSums(bootmodel901[,4:6]) #model9stable bootstrapped coefficients danger 0 correction 1
getvaxpredbt810<-rowSums(bootmodel810[,4:6]) #model8stable bootstrapped coefficients danger 1 correction 0
getvaxpredbt910<-rowSums(bootmodel910[,4:6]) #model9stable bootstrapped coefficients danger 1 correction 0

tilegetvaxpredbt800<-c(quantile(getvaxpredbt800, c(0.05, 0.95)), mean=mean(getvaxpredbt800))
tilegetvaxpredbt900<-c(quantile(getvaxpredbt900, c(0.05, 0.95)), mean=mean(getvaxpredbt900))
tilegetvaxpredbt801<-c(quantile(getvaxpredbt801, c(0.05, 0.95)), mean=mean(getvaxpredbt801))
tilegetvaxpredbt901<-c(quantile(getvaxpredbt901, c(0.05, 0.95)), mean=mean(getvaxpredbt901))
tilegetvaxpredbt810<-c(quantile(getvaxpredbt810, c(0.05, 0.95)), mean=mean(getvaxpredbt810))
tilegetvaxpredbt910<-c(quantile(getvaxpredbt910, c(0.05, 0.95)), mean=mean(getvaxpredbt910))


#=========================================================================
#
#
##       interactions for model8stable danger correction 
#
#
#=========================================================================
#just a reminder 
###model8stable<-polr(getvax ~ danger + correction, data = lowconcern, weights = weights, method = "probit")

#===================
###interactions for model8stable danger 0 correction 0 (control)
#===================

me.vec<- mean(getvaxpredbt800)
me.se<-sd(getvaxpredbt800) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegetvaxpredbt800<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegetvaxpredbt800
newtilegetvaxpredbt800

#calculate the interaction effect using degrees of freedom of model8stable
newerror<-fdrInteraction(me.vec, me.se, df=model8stable$df, level=0.95)
ci.hi800 <- me.vec + newerror * me.se
ci.lo800 <- me.vec - newerror * me.se

interacttilegetvaxpredbt800<- data.frame(ci.lo800,ci.hi800, me.vec)

#look at all 3 of them
tilegetvaxpredbt800
newtilegetvaxpredbt800
interacttilegetvaxpredbt800

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi800, ci.lo800,newerror)

#===================
###interactions for model8stable danger 1 correction 0 (danger)
#===================
me.vec<- mean(getvaxpredbt810)
me.se<-sd(getvaxpredbt810) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegetvaxpredbt810<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegetvaxpredbt810
newtilegetvaxpredbt810

#calculate the interaction effect using degrees of freedom of model8stable
newerror<-fdrInteraction(me.vec, me.se, df=model8stable$df, level=0.95) #1.963122
ci.hi810 <- me.vec + newerror * me.se
ci.lo810 <- me.vec - newerror * me.se

interacttilegetvaxpredbt810<- data.frame(ci.lo810,ci.hi810, me.vec)

#look at all 3 of them
tilegetvaxpredbt810
newtilegetvaxpredbt810
interacttilegetvaxpredbt810

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi810, ci.lo810, newerror)

#===================
###interactions for model8stable danger 0 correction 1 (getvaxpredbt801)
#===================
me.vec<- mean(getvaxpredbt801)
me.se<-sd(getvaxpredbt801) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegetvaxpredbt801<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegetvaxpredbt801
newtilegetvaxpredbt801

#calculate the interaction effect using degrees of freedom of model8stable
newerror<-fdrInteraction(me.vec, me.se, df=model8stable$df, level=0.95) #1.963122
ci.hi801 <- me.vec + newerror * me.se
ci.lo801 <- me.vec - newerror * me.se

interacttilegetvaxpredbt801<- data.frame(ci.lo801,ci.hi801, me.vec)

#look at all 3 of them
tilegetvaxpredbt801
newtilegetvaxpredbt801
interacttilegetvaxpredbt801

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi801, ci.lo801,newerror)

#===================================================================
#       
#
##       DOTPLOT FOR model8STABLE CONTROL, DANGER AND CORRECTION
#
#
#====================================================================
#just a reminder
###model8stable<-polr(getvax ~ danger + correction, data = lowconcern, weights = weights, method = "probit")

#standardize format so all of them are vectors with the same "column" names
names(interacttilegetvaxpredbt800)<- names(tilegetvaxpredbt800)
names(interacttilegetvaxpredbt810)<- names(tilegetvaxpredbt810)
names(interacttilegetvaxpredbt801)<-names(tilegetvaxpredbt801)

interacttilegetvaxpredbt800<- as.vector(interacttilegetvaxpredbt800)
interacttilegetvaxpredbt810<- as.vector(interacttilegetvaxpredbt810)
interacttilegetvaxpredbt801<-as.vector(interacttilegetvaxpredbt801)

interacttilegetvaxpredbt800<- as.numeric(as.character(interacttilegetvaxpredbt800))
interacttilegetvaxpredbt810<- as.numeric(as.character(interacttilegetvaxpredbt810))
interacttilegetvaxpredbt801<-as.numeric(as.character(interacttilegetvaxpredbt801))


predmeanCIlow<-data.frame(tilegetvaxpredbt800,
                          tilegetvaxpredbt810,
                          tilegetvaxpredbt801,
                          interacttilegetvaxpredbt800,
                          interacttilegetvaxpredbt810,
                          interacttilegetvaxpredbt801)


predmeanCIlow<-as.data.frame(t(predmeanCIlow))
predmeanCIlow$names<- factor(c("Control", "Danger", "Correction"), levels=c("Control", "Danger", "Correction"))

levels(predmeanCIlow$names)

#====================
# graph without interaction effects
#====================

x11()
figure8<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        #annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        #annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        #annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("Low Side Effects Concern")+
        ggtitle("Likely to get flu vaccine")+
        ylab("")+ 
        theme_bw()
figure8
ggsave("imagesvaccines\\figure8.pdf")
dev.off()

#=======================
# graph with interaction effects
#=======================

x11()
figure8.8<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("Low Side Effects Concern")+
        ggtitle("Likely to get flu vaccine")+
        ylab("")+ 
        theme_bw()
figure8.8
ggsave("imagesvaccines\\figure8.8.pdf")
dev.off()

#===========================================================================================
#       
#
#       interactions for model9stable danger correction
#
#
#============================================================================================
#just a reminder 
###model9stable<-polr(getvax ~ danger + correction, method="probit", weights=weights, data=highconcern)

#===================
###interactions for model9stable danger 0 correction 0 (control)
#===================

me.vec<- mean(getvaxpredbt900)
me.se<-sd(getvaxpredbt900) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegetvaxpredbt900<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegetvaxpredbt900
newtilegetvaxpredbt900

#calculate the interaction effect using degrees of freedom of model8stable
newerror<-fdrInteraction(me.vec, me.se, df=model9stable$df, level=0.95) #1.963122
ci.hi900 <- me.vec + 1.963122 * me.se
ci.lo900 <- me.vec - 1.963122 * me.se

interacttilegetvaxpredbt900<- data.frame(ci.lo900,ci.hi900, me.vec)

#look at all 3 of them
tilegetvaxpredbt900
newtilegetvaxpredbt900
interacttilegetvaxpredbt900

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi900, ci.lo900, newerror)

#===================
###interactions for model9stable danger 1 correction 0 (danger)
#===================
me.vec<- mean(getvaxpredbt910)
me.se<-sd(getvaxpredbt910) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegetvaxpredbt910<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegetvaxpredbt910
newtilegetvaxpredbt910

#calculate the interaction effect using degrees of freedom of model8stable
newerror<-fdrInteraction(me.vec, me.se, df=model9stable$df, level=0.95) #1.963122
ci.hi910 <- me.vec + newerror * me.se
ci.lo910 <- me.vec - newerror * me.se

interacttilegetvaxpredbt910<- data.frame(ci.lo910,ci.hi910, me.vec)

#look at all 3 of them
tilegetvaxpredbt910
newtilegetvaxpredbt910
interacttilegetvaxpredbt910

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi910, ci.lo910, newerror)

#===================
###interactions for model9stable danger 0 correction 1 (getvaxpredbt901)
#===================
me.vec<- mean(getvaxpredbt901)
me.se<-sd(getvaxpredbt901) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtilegetvaxpredbt901<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tilegetvaxpredbt901
newtilegetvaxpredbt901

#calculate the interaction effect using degrees of freedom of model8stable
newerror<-fdrInteraction(me.vec, me.se, df=model9stable$df, level=0.95) #1.963122
ci.hi901 <- me.vec + newerror * me.se
ci.lo901 <- me.vec - newerror * me.se

interacttilegetvaxpredbt901<- data.frame(ci.lo901,ci.hi901, me.vec)

#look at all 3 of them
tilegetvaxpredbt901
newtilegetvaxpredbt901
interacttilegetvaxpredbt901

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi901, ci.lo901, newerror)


#========================================================================
#       
#      DOTPLOT FOR model9STABLE CONTROL, DANGER AND CORRECTION
#
#
#========================================================================
#just a reminder
###model9stable<-polr(formula = getvax ~ danger + correction, data = highconcern, weights = weights, method = "probit")

#standardize format so all of them are vectors with the same "column" names
names(interacttilegetvaxpredbt900)<- names(tilegetvaxpredbt900)
names(interacttilegetvaxpredbt910)<- names(tilegetvaxpredbt910)
names(interacttilegetvaxpredbt901)<-names(tilegetvaxpredbt901)

interacttilegetvaxpredbt900<- as.vector(interacttilegetvaxpredbt900)
interacttilegetvaxpredbt910<- as.vector(interacttilegetvaxpredbt910)
interacttilegetvaxpredbt901<-as.vector(interacttilegetvaxpredbt901)

interacttilegetvaxpredbt900<- as.numeric(as.character(interacttilegetvaxpredbt900))
interacttilegetvaxpredbt910<- as.numeric(as.character(interacttilegetvaxpredbt910))
interacttilegetvaxpredbt901<-as.numeric(as.character(interacttilegetvaxpredbt901))


predmeanCIlow<-data.frame(tilegetvaxpredbt900,
                          tilegetvaxpredbt910,
                          tilegetvaxpredbt901,
                          interacttilegetvaxpredbt900,
                          interacttilegetvaxpredbt910,
                          interacttilegetvaxpredbt901)


predmeanCIlow<-as.data.frame(t(predmeanCIlow))
predmeanCIlow$names<- factor(c("Control", "Danger", "Correction"), levels=c("Control", "Danger", "Correction"))

levels(predmeanCIlow$names)

#====================
# graph without interaction effects
#====================

x11()
figure9<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        #annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        #annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        #annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("High Side Effects Concern")+
        ggtitle("Likely to get flu vaccine")+
        ylab("")+ 
        theme_bw()
figure9
ggsave("imagesvaccines\\figure9.pdf")
dev.off()

#=======================
# graph with interaction effects
#=======================

x11()
figure9.9<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("High Side Effects Concern")+
        ggtitle("Likely to get flu vaccine")+
        ylab("")+ 
        theme_bw()
figure9.9
ggsave("imagesvaccines\\figure9.9.pdf")
dev.off()