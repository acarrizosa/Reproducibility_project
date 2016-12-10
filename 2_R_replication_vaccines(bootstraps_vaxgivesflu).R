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


#must run this for everything here to work.  There are things we need from 
source("1_R_replication_vaccines(tables_stable_models).R")



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
newerror<-fdrInteraction(me.vec, me.se, df=model3stable$df, level=0.95) #1.963122
ci.hi300 <- me.vec + newerror * me.se
ci.lo300 <- me.vec - newerror * me.se

interacttilegivesflupredbt300<- data.frame(ci.lo300,ci.hi300, me.vec)

#look at all 3 of them
tilegivesflupredbt300
newtilegivesflupredbt300
interacttilegivesflupredbt300

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi300, ci.lo300, newerror)

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
newerror<-fdrInteraction(me.vec, me.se, df=model3stable$df, level=0.95) #1.963122
ci.hi310 <- me.vec + newerror * me.se
ci.lo310 <- me.vec - newerror * me.se

interacttilegivesflupredbt310<- data.frame(ci.lo310,ci.hi310, me.vec)

#look at all 3 of them
tilegivesflupredbt310
newtilegivesflupredbt310
interacttilegivesflupredbt310

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi310, ci.lo310, newerror)

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
newerror<-fdrInteraction(me.vec, me.se, df=model3stable$df, level=0.95) #1.963122
ci.hi301 <- me.vec + newerror * me.se
ci.lo301 <- me.vec - newerror * me.se

interacttilegivesflupredbt301<- data.frame(ci.lo301,ci.hi301, me.vec)

#look at all 3 of them
tilegivesflupredbt301
newtilegivesflupredbt301
interacttilegivesflupredbt301

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi301, ci.lo301, newerror)


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
        xlab("High Side Effects Concern")+
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
        xlab("High Side Effects Concern")+
        ggtitle("Vaccine can give you flu")+
        ylab("")+ 
        theme_bw()
figure5.5
ggsave("imagesvaccines\\figure5.5.pdf")
dev.off()