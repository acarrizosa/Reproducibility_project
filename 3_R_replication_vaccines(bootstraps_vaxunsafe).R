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
##                                      BOOTSTRAPPING vaxunsafe models
#
#
#
#
#==============================================================================================================


#must run this for everything here to work.  There are things we need from 
source("1_R_replication_vaccines(tables_stable_models).R")



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
predictmodel5<-list()
predictmodel6<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model5<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model6<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel5[[i]]<-as.vector(predict(model5, newdata=newdata, type="probs"))
        predictmodel6[[i]]<-as.vector(predict(model6, newdata=newdata, type="probs"))
}
bootmodel500<-matrix(unlist(predictmodel5), byrow=TRUE, ncol=4)
bootmodel600<-matrix(unlist(predictmodel6), byrow=TRUE, ncol=4)

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
predictmodel5<-list()
predictmodel6<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model5<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model6<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel5[[i]]<-as.vector(predict(model5, newdata=newdata, type="probs"))
        predictmodel6[[i]]<-as.vector(predict(model6, newdata=newdata, type="probs"))
}
bootmodel501<-matrix(unlist(predictmodel5), byrow=TRUE, ncol=4)
bootmodel601<-matrix(unlist(predictmodel6), byrow=TRUE, ncol=4)

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
predictmodel5<-list()
predictmodel6<-list()

for (i in 1:1000) {
        y<-sample(774, 774, replace=TRUE)
        z<- sample(226, 226, replace=TRUE)
        lowconcernsample<-lowconcern[y,]
        highconcernsample<-highconcern[z,]
        model5<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=lowconcernsample)
        model6<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=highconcernsample)
        predictmodel5[[i]]<-as.vector(predict(model5, newdata=newdata, type="probs"))
        predictmodel6[[i]]<-as.vector(predict(model6, newdata=newdata, type="probs"))
}
bootmodel510<-matrix(unlist(predictmodel5), byrow=TRUE, ncol=4)
bootmodel610<-matrix(unlist(predictmodel6), byrow=TRUE, ncol=4)

#========================
# GET MEAN AND PERCENTILES
#
#========================

unsafepredbt500<-rowSums(bootmodel500[,3:4]) #model5stable bootstrapped coefficients danger 0 correction 0
unsafepredbt600<-rowSums(bootmodel600[,3:4]) #model6stable bootstrapped coefficients danger 0 correction 0
unsafepredbt501<-rowSums(bootmodel501[,3:4]) #model5stable bootstrapped coefficients danger 0 correction 1
unsafepredbt601<-rowSums(bootmodel601[,3:4]) #model6stable bootstrapped coefficients danger 0 correction 1
unsafepredbt510<-rowSums(bootmodel510[,3:4]) #model5stable bootstrapped coefficients danger 1 correction 0
unsafepredbt610<-rowSums(bootmodel610[,3:4]) #model6stable bootstrapped coefficients danger 1 correction 0

tileunsafepredbt500<-c(quantile(unsafepredbt500, c(0.05, 0.95)), mean=mean(unsafepredbt500))
tileunsafepredbt600<-c(quantile(unsafepredbt600, c(0.05, 0.95)), mean=mean(unsafepredbt600))
tileunsafepredbt501<-c(quantile(unsafepredbt501, c(0.05, 0.95)), mean=mean(unsafepredbt501))
tileunsafepredbt601<-c(quantile(unsafepredbt601, c(0.05, 0.95)), mean=mean(unsafepredbt601))
tileunsafepredbt510<-c(quantile(unsafepredbt510, c(0.05, 0.95)), mean=mean(unsafepredbt510))
tileunsafepredbt610<-c(quantile(unsafepredbt610, c(0.05, 0.95)), mean=mean(unsafepredbt610))


#=========================================================================
#
#
##       interactions for model5stable danger correction 
#
#
#=========================================================================
#just a reminder 
###model5stable<-polr(vaxunsafe ~ danger + correction, data = lowconcern, weights = weights, method = "probit")

#===================
###interactions for model5stable danger 0 correction 0 (control)
#===================

me.vec<- mean(unsafepredbt500)
me.se<-sd(unsafepredbt500) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtileunsafepredbt500<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tileunsafepredbt500
newtileunsafepredbt500

#calculate the interaction effect using degrees of freedom of model5stable
newerror<-fdrInteraction(me.vec, me.se, df=model5stable$df, level=0.95)
ci.hi500 <- me.vec + newerror * me.se
ci.lo500 <- me.vec - newerror * me.se

interacttileunsafepredbt500<- data.frame(ci.lo500,ci.hi500, me.vec)

#look at all 3 of them
tileunsafepredbt500
newtileunsafepredbt500
interacttileunsafepredbt500

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi500, ci.lo500,newerror)

#===================
###interactions for model5stable danger 1 correction 0 (danger)
#===================
me.vec<- mean(unsafepredbt510)
me.se<-sd(unsafepredbt510) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtileunsafepredbt510<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tileunsafepredbt510
newtileunsafepredbt510

#calculate the interaction effect using degrees of freedom of model5stable
newerror<-fdrInteraction(me.vec, me.se, df=model5stable$df, level=0.95) #1.963122
ci.hi510 <- me.vec + newerror * me.se
ci.lo510 <- me.vec - newerror * me.se

interacttileunsafepredbt510<- data.frame(ci.lo510,ci.hi510, me.vec)

#look at all 3 of them
tileunsafepredbt510
newtileunsafepredbt510
interacttileunsafepredbt510

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi510, ci.lo510, newerror)

#===================
###interactions for model5stable danger 0 correction 1 (unsafepredbt501)
#===================
me.vec<- mean(unsafepredbt501)
me.se<-sd(unsafepredbt501) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtileunsafepredbt501<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tileunsafepredbt501
newtileunsafepredbt501

#calculate the interaction effect using degrees of freedom of model5stable
newerror<-fdrInteraction(me.vec, me.se, df=model5stable$df, level=0.95) #1.963122
ci.hi501 <- me.vec + newerror * me.se
ci.lo501 <- me.vec - newerror * me.se

interacttileunsafepredbt501<- data.frame(ci.lo501,ci.hi501, me.vec)

#look at all 3 of them
tileunsafepredbt501
newtileunsafepredbt501
interacttileunsafepredbt501

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi501, ci.lo501,newerror)

#===================================================================
#       
#
##       DOTPLOT FOR model5STABLE CONTROL, DANGER AND CORRECTION
#
#
#====================================================================
#just a reminder
###model5stable<-polr(vaxunsafe ~ danger + correction, data = lowconcern, weights = weights, method = "probit")

#standardize format so all of them are vectors with the same "column" names
names(interacttileunsafepredbt500)<- names(tileunsafepredbt500)
names(interacttileunsafepredbt510)<- names(tileunsafepredbt510)
names(interacttileunsafepredbt501)<-names(tileunsafepredbt501)

interacttileunsafepredbt500<- as.vector(interacttileunsafepredbt500)
interacttileunsafepredbt510<- as.vector(interacttileunsafepredbt510)
interacttileunsafepredbt501<-as.vector(interacttileunsafepredbt501)

interacttileunsafepredbt500<- as.numeric(as.character(interacttileunsafepredbt500))
interacttileunsafepredbt510<- as.numeric(as.character(interacttileunsafepredbt510))
interacttileunsafepredbt501<-as.numeric(as.character(interacttileunsafepredbt501))


predmeanCIlow<-data.frame(tileunsafepredbt500,
                          tileunsafepredbt510,
                          tileunsafepredbt501,
                          interacttileunsafepredbt500,
                          interacttileunsafepredbt510,
                          interacttileunsafepredbt501)


predmeanCIlow<-as.data.frame(t(predmeanCIlow))
predmeanCIlow$names<- factor(c("Control", "Danger", "Correction"), levels=c("Control", "Danger", "Correction"))

levels(predmeanCIlow$names)

#====================
# graph without interaction effects
#====================

x11()
figure6<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        #annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        #annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        #annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("Low Side Effects Concern")+
        ggtitle("Flu vaccine unsafe")+
        ylab("")+ 
        theme_bw()
figure6
ggsave("imagesvaccines\\figure6.pdf")
dev.off()

#=======================
# graph with interaction effects
#=======================

x11()
figure6.6<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("Low Side Effects Concern")+
        ggtitle("Flu vaccine unsafe")+
        ylab("")+ 
        theme_bw()
figure6.6
ggsave("imagesvaccines\\figure6.6.pdf")
dev.off()

#===========================================================================================
#       
#
#       interactions for model6stable danger correction
#
#
#============================================================================================
#just a reminder 
###model6stable<-polr(vaxunsafe ~ danger + correction, method="probit", weights=weights, data=highconcern)

#===================
###interactions for model6stable danger 0 correction 0 (control)
#===================

me.vec<- mean(unsafepredbt600)
me.se<-sd(unsafepredbt600) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtileunsafepredbt600<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tileunsafepredbt600
newtileunsafepredbt600

#calculate the interaction effect using degrees of freedom of model5stable
newerror<-fdrInteraction(me.vec, me.se, df=model6stable$df, level=0.95) #1.963122
ci.hi600 <- me.vec + 1.963122 * me.se
ci.lo600 <- me.vec - 1.963122 * me.se

interacttileunsafepredbt600<- data.frame(ci.lo600,ci.hi600, me.vec)

#look at all 3 of them
tileunsafepredbt600
newtileunsafepredbt600
interacttileunsafepredbt600

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi600, ci.lo600, newerror)

#===================
###interactions for model6stable danger 1 correction 0 (danger)
#===================
me.vec<- mean(unsafepredbt610)
me.se<-sd(unsafepredbt610) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtileunsafepredbt610<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tileunsafepredbt610
newtileunsafepredbt610

#calculate the interaction effect using degrees of freedom of model5stable
newerror<-fdrInteraction(me.vec, me.se, df=model6stable$df, level=0.95) #1.963122
ci.hi610 <- me.vec + newerror * me.se
ci.lo610 <- me.vec - newerror * me.se

interacttileunsafepredbt610<- data.frame(ci.lo610,ci.hi610, me.vec)

#look at all 3 of them
tileunsafepredbt610
newtileunsafepredbt610
interacttileunsafepredbt610

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi610, ci.lo610, newerror)

#===================
###interactions for model6stable danger 0 correction 1 (unsafepredbt601)
#===================
me.vec<- mean(unsafepredbt601)
me.se<-sd(unsafepredbt601) #get standard error

ci.hi <- me.vec + 1.96 * me.se
ci.lo <- me.vec - 1.96 * me.se
newtileunsafepredbt601<-data.frame(ci.lo,ci.hi, me.vec) 

#these are the "old" and "new" confidence intervals.
#one calculated by ordering and getting precentiles, and the other
#calculating mean, standard error, and multiplying by 1.96 for 85%
#they are slightly different but close. 

tileunsafepredbt601
newtileunsafepredbt601

#calculate the interaction effect using degrees of freedom of model5stable
newerror<-fdrInteraction(me.vec, me.se, df=model6stable$df, level=0.95) #1.963122
ci.hi601 <- me.vec + newerror * me.se
ci.lo601 <- me.vec - newerror * me.se

interacttileunsafepredbt601<- data.frame(ci.lo601,ci.hi601, me.vec)

#look at all 3 of them
tileunsafepredbt601
newtileunsafepredbt601
interacttileunsafepredbt601

rm(ci.hi, ci.lo, me.vec, me.se, ci.hi601, ci.lo601, newerror)


#========================================================================
#       
#      DOTPLOT FOR model6STABLE CONTROL, DANGER AND CORRECTION
#
#
#========================================================================
#just a reminder
###model6stable<-polr(formula = vaxunsafe ~ danger + correction, data = highconcern, weights = weights, method = "probit")

#standardize format so all of them are vectors with the same "column" names
names(interacttileunsafepredbt600)<- names(tileunsafepredbt600)
names(interacttileunsafepredbt610)<- names(tileunsafepredbt610)
names(interacttileunsafepredbt601)<-names(tileunsafepredbt601)

interacttileunsafepredbt600<- as.vector(interacttileunsafepredbt600)
interacttileunsafepredbt610<- as.vector(interacttileunsafepredbt610)
interacttileunsafepredbt601<-as.vector(interacttileunsafepredbt601)

interacttileunsafepredbt600<- as.numeric(as.character(interacttileunsafepredbt600))
interacttileunsafepredbt610<- as.numeric(as.character(interacttileunsafepredbt610))
interacttileunsafepredbt601<-as.numeric(as.character(interacttileunsafepredbt601))


predmeanCIlow<-data.frame(tileunsafepredbt600,
                          tileunsafepredbt610,
                          tileunsafepredbt601,
                          interacttileunsafepredbt600,
                          interacttileunsafepredbt610,
                          interacttileunsafepredbt601)


predmeanCIlow<-as.data.frame(t(predmeanCIlow))
predmeanCIlow$names<- factor(c("Control", "Danger", "Correction"), levels=c("Control", "Danger", "Correction"))

levels(predmeanCIlow$names)

#====================
# graph without interaction effects
#====================

x11()
figure7<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        #annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        #annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        #annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("High Side Effects Concern")+
        ggtitle("Flu vaccine unsafe")+
        ylab("")+ 
        theme_bw()
figure7
ggsave("imagesvaccines\\figure7.pdf")
dev.off()

#=======================
# graph with interaction effects
#=======================

x11()
figure7.7<-ggplot(predmeanCIlow, aes(names,mean))+
        geom_point(size=3)+ ylim(0, 1)+ 
        annotate("segment", x=predmeanCIlow[4,4], xend=predmeanCIlow[4,4], y=predmeanCIlow[4,1], yend=predmeanCIlow[4,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[1,4], xend=predmeanCIlow[1,4], y=predmeanCIlow[1,1], yend=predmeanCIlow[1,2], lwd=2)+
        annotate("segment", x=predmeanCIlow[5,4], xend=predmeanCIlow[5,4], y=predmeanCIlow[5,1], yend=predmeanCIlow[5,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[2,4], xend=predmeanCIlow[2,4], y=predmeanCIlow[2,1], yend=predmeanCIlow[2,2], lwd=2) +
        annotate("segment", x=predmeanCIlow[6,4], xend=predmeanCIlow[6,4], y=predmeanCIlow[6,1], yend=predmeanCIlow[6,2], col="gray", lwd=2)+
        annotate("segment", x=predmeanCIlow[3,4], xend=predmeanCIlow[3,4], y=predmeanCIlow[3,1], yend=predmeanCIlow[3,2], lwd=2)+
        xlab("High Side Effects Concern")+
        ggtitle("Flu vaccine unsafe")+
        ylab("")+ 
        theme_bw()
figure7.7
ggsave("imagesvaccines\\figure7.7.pdf")
dev.off()