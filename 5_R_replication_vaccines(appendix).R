
source("1_R_replication_vaccines(tables_stable_models).R")

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