#setwd("C:/Users/amc27/Dropbox/Rice docs/3_docs_clases/1_2016_fall/justin work/interactionTest")

dir.create("politicalimages")

#install.packages(c("car", "MASS", "Hmisc","gmodels", "xlsx", "foreign", "ggplot2", "survey", "questionr"))
library(foreign)
library(ggplot2)
library(Hmisc)
library(gmodels)
rm(list=ls(all=TRUE))
#=======================================
#
# STUDY 1
#
#=======================================

#==================
#IMPORT AND CLEAN DATA
#==================
fall05<-read.dta("fall05-data.dta")

table(spring06$issue_iraq) #the error only affects the value labels, not the data itself.

#1=strongly disagree 5=strongly agree
fall05$wmds<- as.numeric(fall05$wmds)

#-3 very liberal, liberal, somewhat left, centrist, somewhat right, conservative, 3= very conservative
fall05$ideolcen<- as.numeric(fall05$ideolcen) 
fall05$ideolcen<- fall05$ideolcen-4

#==================
#Regressions they perform
#==================

model1.1<-lm(wmds ~ iraqcorr + ideolcen + know + ms, data=fall05)
summary(model1.1)

model1.2<-lm(wmds ~ iraqcorr*ideolcen + know + ms, data=fall05)
summary(model1.2)

#all coefficients are exactly the same as their stata output

#==================
#Figure 1
#==================
#same as model 1.2
model.interact<-lm(wmds ~ iraqcorr*ideolcen + know + ms, data=fall05)

b1<- coef(model.interact)[2]
b2<-coef(model.interact)[3]
b3<-coef(model.interact)[6]
MV<- -3:3
varb1<- vcov(model.interact)[2,2]
varb2<- vcov(model.interact)[3,3]
varb3<- vcov(model.interact)[6,6]
covb1b3<- vcov(model.interact)[2,6]
covb2b3<- vcov(model.interact)[3,6]

conb<- b1+b3*MV
conse<- sqrt(varb1+varb3*(MV^2)+2*covb1b3*MV)
a<- 1.96*conse
upper<- conb+a
lower<- conb-a

values.frame<- data.frame(conb, conse, a, upper, lower, MV)


values.frame$MVname<- as.factor(values.frame$MV)
levels(values.frame$MVname)<- c("Very Liberal", "Liberal", "Leans Left", "Center", "Lean Right", "Conservative", "Very Conservative")


# pretty good plot but need to annotate better

x11()
figure1<-ggplot(values.frame, aes(MV, conb))+
        geom_ribbon(aes(ymin=lower, ymax=upper), fill="black", alpha=0.1)+
        geom_hline(yintercept=0)+ 
        geom_point(aes(MV,conb))+ 
        geom_smooth(aes(MV,conb), method="lm", col="black", lty=2, lwd=0.5)+
        ylab("")+ xlab("(-3) Very Liberal --- (3) Very Conservative") + ggtitle("Effect of correction on WMD misperception
Estimated marginal effect by ideology: Fall 2005") +theme_bw()
figure1
ggsave("politicalimages\\Figure_1.pdf")
dev.off()

#raw data#

conservatives<- subset(fall05, roc=="Conservative")
notconservatives<- subset(fall05, roc=="Not conservative")

CrossTable(conservatives$binarywmd, conservatives$iraqcorr, prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(notconservatives$binarywmd, notconservatives$iraqcorr, prop.r=F, prop.t=F, prop.chisq=F)

#================================
#correlation: Iraq was right thing & WMD misperception
#================================

#I checked, converting levels to numbers keeps levels in right order (I also get the same coefficient)
with(conservatives, cor(as.numeric(iraqdecision), wmds, use="pairwise.complete.obs"))

#=======================================
#
# STUDY 2
#
#=======================================

#==================
#IMPORT AND CLEAN DATA
#==================
spring06<- read.dta("spring06-data.dta")

spring06$iraqwmd<-as.numeric(spring06$iraqwmd)
spring06$ideolcen<- as.numeric(spring06$ideolcen)-4
spring06$iraqdecision<- as.numeric(spring06$iraqdecision)

conservative<-subset(spring06, roc=="Conservative")
notconservative<-subset(spring06, roc=="Not conservative")

#==================
# Regressions
#==================

model2.1<-lm(iraqwmd~ iraqcorr + ideolcen + know, data=spring06)
summary(model2.1)

model2.2<-lm(iraqwmd~ iraqcorr*ideolcen + know, data=spring06)
summary(model2.2)

model2.3<-lm(iraqwmd~ know+iraqcorr*ideolcen + iraqcorr*issue_iraq  +
                issue_iraq*ideolcen + iraqcorr*issue_iraq*ideolcen, data=spring06)
summary(model2.3)

#same as model 2.3
model.interact<-lm(iraqwmd~ know+iraqcorr*ideolcen + iraqcorr*issue_iraq  +
                issue_iraq*ideolcen + iraqcorr*issue_iraq*ideolcen, data=spring06)

#==================
# Correlations
#==================

with(conservative, cor(iraqdecision, iraqwmd, use="pairwise.complete.obs"))

with(conservative[conservative$iraqcorr==1,], 
     cor(iraqdecision, iraqwmd, use="pairwise.complete.obs"))

with(conservative[conservative$iraqcorr==0,], 
     cor(iraqdecision, iraqwmd, use="pairwise.complete.obs"))

#==================
# Figure 2 and 3
#==================

W0<-0
W1<-1

MV<- -3:3
b1<- as.data.frame(coef(model.interact))[3,]
b2<- as.data.frame(coef(model.interact))[4,]
b3<- as.data.frame(coef(model.interact))[5,]
b4<- as.data.frame(coef(model.interact))[6,]
b5<- as.data.frame(coef(model.interact))[7,]
b6<- as.data.frame(coef(model.interact))[8,]
b7<- as.data.frame(coef(model.interact))[9,]
varb1<- vcov(model.interact)[3,3]
varb2<- vcov(model.interact)[4,4]
varb3<- vcov(model.interact)[5,5]
varb4<- vcov(model.interact)[6,6]
varb5<- vcov(model.interact)[7,7]
varb6<- vcov(model.interact)[8,8]
varb7<- vcov(model.interact)[9,9]
covb1b4<-vcov(model.interact)[6,3]
covb1b5<- vcov(model.interact)[7,3]
covb1b7<-vcov(model.interact)[9,3]
covb4b5<-vcov(model.interact)[6,7]
covb4b7<-vcov(model.interact)[9,6]
covb5b7<-vcov(model.interact)[9,7]


conb0<-b1+b4*MV+b5*W0+b7*(MV*W0)
conb1<-b1+b4*MV+b5*W1+b7*(MV*W1)

conse0<-sqrt(varb1 + varb4*(MV^2) + varb5*(W0^2) + 
                varb7*(MV^2)*(W0^2) + 2*MV*covb1b4 + 2*W0*covb1b5 + 
                2*MV*W0*covb1b7 + 2*MV*W0*covb4b5 + 2*W0*(MV^2)*covb4b7 +
                2*(W0^2)*MV*covb5b7)

conse1<-sqrt(varb1 + varb4*(MV^2) + varb5*(W1^2) + varb7*(MV^2)*(W1^2) 
                + 2*MV*covb1b4 + 2*W1*covb1b5 + 2*MV*W1*covb1b7 + 2*MV*W1*covb4b5 
                + 2*W1*(MV^2)*covb4b7 + 2*(W1^2)*MV*covb5b7)

t0<-conb0/conse0
t1<-conb1/conse1

consb0<-conb0
consb1<-conb1

consb0[abs(t0)<1.96]<-NA
consb1[abs(t1)<1.96]<-NA

ar0<-1.96*conse0
upper0<-conb0+ar0
lower0<-conb0-ar0

ar1<-1.96*conse1
upper1<-conb1+ar1
lower1<-conb1-ar1

ar2<-1.64*conse1
upper2<-conb1+ar2
lower2<-conb1-ar2

zero<-0

values.frame<-data.frame(upper0,lower0,MV, conb0)

x11()
figure2<-ggplot(values.frame, aes(MV, conb0))+
        geom_ribbon(aes(ymin=lower0, ymax=upper0), fill="black", alpha=0.1)+
        geom_hline(yintercept=0)+ 
        geom_point(aes(MV,conb0)) + 
        geom_smooth(aes(MV,conb0), method="lm", col="black", lty=2, lwd=0.5)+
        ylab("Marginal effect on misperception")+ 
        xlab("(-3) Very Liberal --- (3) Very Conservative") +
        ggtitle("Iraq not most important") +theme_bw()+
        ylim(-4,4)
figure2
ggsave("politicalimages\\Figure_2.pdf")
dev.off()

values.frame<-data.frame(upper1,lower1,MV, conb1)

x11()
figure3<-ggplot(values.frame, aes(MV, conb1))+
        geom_ribbon(aes(ymin=lower1, ymax=upper1), fill="black", alpha=0.1)+
        geom_hline(yintercept=0)+ 
        geom_point(aes(MV,conb1)) + 
        geom_smooth(aes(MV,conb1), method="lm", col="black", lty=2, lwd=0.5)+
        ylab("Marginal effect on misperception")+ xlab("(-3) Very Liberal --- (3) Very Conservative") + 
        ggtitle("Iraq most important")+ theme_bw()+
        ylim(-4,4)
figure3
ggsave("politicalimages\\Figure_3.pdf")
dev.off()

#estimated marginal effect & 90% CI for most strongly committed conservatives
data.frame(MV, conb1, lower2, upper2)[7,]

#t.test of marginal effect among liberals
t.test(spring06$iraqwmd[spring06$ideolcen<0 & spring06$iraqcorr==0], spring06$iraqwmd[spring06$ideolcen<0 & spring06$iraqcorr==1])


#====================================
#tax cuts/ revenue
#====================================
spring06<- read.dta("spring06-data.dta")

spring06$iraqwmd<-as.numeric(spring06$iraqwmd)
spring06$ideolcen<- as.numeric(spring06$ideolcen)-4
spring06$iraqdecision<- as.numeric(spring06$iraqdecision)
spring06$taxcutrev<- as.numeric(spring06$taxcutrev)

model3.1<-lm(taxcutrev ~ taxcutcorr + ideolcen + know, data=spring06)
summary(model3.1)

model3.2<- lm(taxcutrev ~ know + taxcutcorr*ideolcen, data=spring06)
summary(model3.2)

model.interact<-model3.2

#==================
# Figure 4
#==================

MV<- -3:3
b1<- coef(model.interact)[3]
b2<- coef(model.interact)[4]
b3<- coef(model.interact)[5]
varb1<- vcov(model.interact)[3,3]
varb2<- vcov(model.interact)[4,4]
varb3<-vcov(model.interact)[5,5]
covb1b3<- vcov(model.interact)[5,3]
covb2b3<- vcov(model.interact)[5,4]

conb <- b1+b3*MV 
conse <- sqrt(varb1+varb3*(MV^2)+2*covb1b3*MV)
a <- 1.96*conse
upper <- conb+a
lower <- conb-a

values.frame<-data.frame(MV, conb, upper, lower)

x11()
figure4<-ggplot(values.frame, aes(MV, conb))+
        geom_ribbon(aes(ymin=lower, ymax=upper), fill="black", alpha=0.1)+
        geom_hline(yintercept=0)+ 
        geom_point(aes(MV,conb)) + 
        geom_smooth(aes(MV,conb), method="lm", col="black", lty=2, lwd=0.5)+
        ylab("Marginal effect on misperception")+ xlab("(-3) Very Liberal --- (3) Very Conservative") + 
        ggtitle("Effect of correction on tax/revenue misperception
Estimated marginal effect by ideology: Spring 2006")+theme_bw()+
        ylim(-1,1.5)
figure4
ggsave("politicalimages\\Figure_4.pdf")
dev.off()

#raw data
conservatives<-subset(spring06, roc=="Conservative")
notconservatives<-subset(spring06, roc=="Not conservative")

CrossTable(conservatives$binarytc, conservatives$taxcutcorr, prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(notconservatives$binarytc, notconservatives$taxcutcorr, prop.r=F, prop.t=F, prop.chisq=F)

#====================================
#Stem Cell Ban
#====================================
spring06<- read.dta("spring06-data.dta")

spring06$iraqwmd<-as.numeric(spring06$iraqwmd)
spring06$ideolcen<- as.numeric(spring06$ideolcen)-4
spring06$iraqdecision<- as.numeric(spring06$iraqdecision)
spring06$stemcellban<-as.numeric(spring06$stemcellban)

conservative<-subset(spring06, roc=="Conservative")
notconservative<-subset(spring06, roc=="Not conservative")

model4.1<-lm(stemcellban~sccorr + ideolcen + know, data=spring06)
model4.2<-lm(stemcellban~ sccorr*ideolcen + know, data=spring06)

model.interact <- model4.2

#====================================
#Figure 5
#====================================

MV<- -3:3
b1<- coef(model.interact)[2] 
b2<- coef(model.interact)[3] 
b3<- coef(model.interact)[5] 
varb1<- vcov(model.interact)[2,2]
varb2<- vcov(model.interact)[3,3]
varb3<- vcov(model.interact)[5,5]
covb1b3 <- vcov(model.interact)[2,5]
covb2b3 <- vcov(model.interact)[3,5]

conb <- b1+b3*MV
conse <- sqrt(varb1+varb3*(MV^2)+2*covb1b3*MV)
a <- 1.96*conse
upper <- conb+a
lower <- conb-a

values.frame<- data.frame(MV, conb, lower, upper)

x11()
figure5<-ggplot(values.frame, aes(MV, conb))+
        geom_ribbon(aes(ymin=lower, ymax=upper), fill="black", alpha=0.1)+
        geom_hline(yintercept=0)+ 
        geom_point(aes(MV,conb)) + 
        geom_smooth(aes(MV,conb), method="lm", col="black", lty=2, lwd=0.5)+
        ylab("Marginal effect on misperception")+ xlab("(-3) Very Liberal --- (3) Very Conservative") + 
        ggtitle("Effect of correction on tax/revenue misperception
                Estimated marginal effect by ideology: Spring 2006")+theme_bw()+
        ylim(-2,1)
figure5
ggsave("politicalimages\\Figure_5.pdf")
dev.off()



























