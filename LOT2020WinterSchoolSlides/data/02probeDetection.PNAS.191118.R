# probe Detection
# analyses for PNAS manuscript
# June 2010, D. Schad

library(ggplot2)
library(reshape)
library(Matrix)
library(glmmADMB)

rm(list=ls())
setwd("/Users/daniel/Documents/Projects_ML/02TrueML1/true1_06Analysis")

load("../true1_05RData/tML_response.rda")

setwd("02probeDetection")

nrow(r) # 6508
r	<- r[r$prCnd_par>0,]
nrow(r) # 1868
		# 62*30 # 1860 -> total number of target sentences
# 50% of words in target sentence not fixated
r2	<- r[r$bad_t_idx==0,] # sum(r$bad_t_idx!=0)
r2	<- r[r$targetNoViewTRT2<0.5,] # sum(r$targetNoViewTRT2>=0.5)
r2	<- r[r$targetNoViewTRT<=0.5,] # sum(r$targetNoViewTRT>=0.5)
nrow(r2)

r	<- r2

str(r)
nrow(r) # 1793


r$prCnd					<- r$prCnd_par
r$prCnd[r$prCnd_par==2]	<- 3
r$prCnd[r$prCnd_par==3]	<- 4
r$prCnd[r$prCnd_par==4]	<- 2

r$prCndF		<- factor(r$prCnd)#, levels=c("1","4","2","3","5","6"))
r$prCnd			<- r$prCndF
levels(r$prCnd)	<- paste(levels(r$prCnd),c(": no probe",": gibberish",": context",": factual knowledge",": syntax",": non-word"),sep="")
levels(r$prCnd)	<- c("ORI","GIB","CON","KNO","SYN","NON")
r$prCnd		<- factor(r$prCnd, levels=c("ORI","GIB","CON","KNO","SYN","NON"))
levels(r$prCnd)	<- c("meaningful","gibberish","context","knowledge","syntax","lexical") # pseudo\n-word


r$time		<- ceiling(r$screenID / 2)
r$Signal	<- as.numeric(r$probe)-2 # as.numeric(r$prCnd!="meaningful")
#r$Signal	<- r$Signal-mean(r$Signal) # as.numeric(r$prCnd!="meaningful")
r$AnswerYes	<- r$prDet

r$s			<- r$Signal
r$Experiment<- r$Exp - mean(r$Exp)
r$time		<- scale(r$screenID, scale=FALSE)

r$time1		<- r$screenID
r$time2		<- r$screenID
r$time1[r$Exp==2]	<- NA
r$time2[r$Exp==1]	<- NA
r$time1		<- scale(r$time1, scale=FALSE)
r$time2		<- scale(r$time2, scale=FALSE)
r$time1[r$Exp==2]	<- 0
r$time2[r$Exp==1]	<- 0
r$timeN[r$Exp==1]	<- r$time1[r$Exp==1]
r$timeN[r$Exp==2]	<- r$time2[r$Exp==2]


#--------------------------------------------------------------------
# simple plot
#--------------------------------------------------------------------
library(plyr)
pr
r.id <- ddply(r, c("prCnd","Exp","id"), summarize, AnswerYes=mean(AnswerYes))
r.m <- ddply(r.id, c("prCnd","Exp"), summarize, 
		M=mean(AnswerYes), se=sd(AnswerYes)/sqrt(length(AnswerYes)))

ggplot(data=r.m, aes(y=M, x= prCnd, fill= prCnd)) + 
	geom_bar(stat="identity") + 
	geom_errorbar(aes(ymin=M-se, ymax=M+se)) + facet_grid(.~Exp) + 
	coord_cartesian(ylim=c(0,1))+
	theme_bw() + theme(axis.text.x = element_text(angle = 35, hjust=1))



######################
# some glmer - tests #
######################
library(lme4)
library(MASS)




#### using contrast coding ####
r$prCndExp	<- factor(paste(r$Exp, r$prCnd, sep="_"))
r$prCndExp	<- factor(r$prCndExp, levels=levels(r$prCndExp)[c(4,2,1,3,  8,5,6,9,7)])

contrasts(r$prCndExp)
contrasts(r$prCndExp)[1:9,1:8]		<-  0
# Exp2
contrasts(r$prCndExp)[,2]			<-  0
#contrasts(r$prCndExp)[1,2]			<- -0.5
#contrasts(r$prCndExp)[5,2]			<- +0.5
contrasts(r$prCndExp)[1:4,2]		<-  1
colnames(contrasts(r$prCndExp))[2]	<-  "Exp2_c_1_2"
# signals
contrasts(r$prCndExp)[ ,1]			<-  1
contrasts(r$prCndExp)[1,1]			<-  0
contrasts(r$prCndExp)[5,1]			<-  0
#contrasts(r$prCndExp)[5,1]			<- -4/5
#contrasts(r$prCndExp)[6:9,1]		<-  1/5
colnames(contrasts(r$prCndExp))[1]	<-  "2_Signal"
contrasts(r$prCndExp)[,3]			<-  contrasts(r$prCndExp)[,1] * contrasts(r$prCndExp)[,2]
#contrasts(r$prCndExp)[,3]			<-  0
#contrasts(r$prCndExp)[2:4,3]		<-  1
#contrasts(r$prCndExp)[1,3]			<- -3/4
#contrasts(r$prCndExp)[2:4,3]		<- +1/4
colnames(contrasts(r$prCndExp))[3]	<-  "1_Signal"
# probe Conditions
contrasts(r$prCndExp)[2:4,4:5]		<- contr.sdif(3)[1:3,1:2]
#contrasts(r$prCndExp)[2:4,4:5]		<- contr.treatment(3, base=3)[1:3,1:2]
colnames(contrasts(r$prCndExp))[4:5]<- c("1_context_gibberish", "1_knowledge_context")
contrasts(r$prCndExp)[6:9,6:8]		<- contr.sdif(4)[1:4,1:3]
#contrasts(r$prCndExp)[6:9,6:8]		<- contr.treatment(4, base=4)[1:4,1:3]
colnames(contrasts(r$prCndExp))[6:8]<- c("2_knowledge_gibberish", "2_syntax_knowledge", 										 "2_lexical_syntax")
contrasts(r$prCndExp)

r$Exp1		<-  r$Exp    -1
r$Exp2		<- (r$Exp*-1)+2
table(r$Exp2)

print(m	<- glmer(AnswerYes ~ prCndExp + timeN + timeN:Signal+ (timeN + timeN:Signal):Exp2
			+(1|id) +(1|pID) +(0+Signal|id) +(0+Signal|pID),
			data=r, family=binomial(link="probit")), cor=F)
#Generalized linear mixed model fit by the Laplace approximation 
#Formula: AnswerYes ~ prCndExp + timeN + timeN:Signal + (timeN + timeN:Signal):Exp2 +      (1 | id) + (1 | pID) #+ (0 + Signal | id) + (0 + Signal |      pID) 
#   Data: r 
#  AIC  BIC logLik deviance
# 1854 1947   -910     1820
#Random effects:
# Groups Name        Variance Std.Dev.
# pID    Signal      0.142677 0.37773 
# pID    (Intercept) 0.047856 0.21876 
# id     Signal      0.128747 0.35881 
# id     (Intercept) 0.070620 0.26574 
#Number of obs: 1793, groups: pID, 62; id, 30
#
#Fixed effects:
#                               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                   -1.993471   0.229479  -8.687  < 2e-16 ***
#prCndExp2_Signal               2.170567   0.245871   8.828  < 2e-16 ***
#prCndExpExp2_c_1_2             0.434476   0.264015   1.646 0.099836 .  
#prCndExp1_Signal              -0.250674   0.285242  -0.879 0.379505    
#prCndExp1_context_gibberish    0.309188   0.125474   2.464 0.013733 *  
#prCndExp1_knowledge_context    0.803340   0.136623   5.880  4.1e-09 ***
#prCndExp2_knowledge_gibberish  0.540516   0.144463   3.742 0.000183 ***
#prCndExp2_syntax_knowledge     0.039266   0.143854   0.273 0.784883    
#prCndExp2_lexical_syntax       0.463651   0.150563   3.079 0.002074 ** 
#timeN                          0.008908   0.007115   1.252 0.210554    
#timeN:Signal                  -0.014117   0.007315  -1.930 0.053600 .  
#timeN:Exp2                    -0.011908   0.008283  -1.438 0.150539    
#timeN:Signal:Exp2              0.017210   0.008615   1.998 0.045750 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
