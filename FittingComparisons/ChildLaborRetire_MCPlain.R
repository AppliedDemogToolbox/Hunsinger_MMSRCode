##############################################################################################################################
##EDDIE'S R CODE FOR FITTING THE MODEL MIGRATION SCHEDULE WITH STUDENT PEAK - COMPARISON CODE
##
##FOR MORE INFO, SEE https://raw.githubusercontent.com/AppliedDemogToolbox/Hunsinger_MMSRCode/master/SPMMSRCode.R
##via https://applieddemogtoolbox.github.io/Toolbox/#MMSRCode
##
##EDDIE HUNSINGER, OCTOBER 2018 (UPDATED DECEMBER 2018)
##http://www.demog.berkeley.edu/~eddieh/
##
##IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS CODE, BE SURE TO CITE THE SOURCE
##This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. More information: https://creativecommons.org/licenses/by-nc-sa/4.0/? 
##
##INFO AND SOURCES ON THE MODEL
##Wilson, T. (2010). “Model Migration Schedules Incorporating Student Migration Peaks.” Demographic Research, 23(8): 191–222.
##	Available online: https://www.demographic-research.org/volumes/vol23/8/default.htm 
##	Related Excel Workbook by Tom Wilson: http://www.demog.berkeley.edu/~eddieh/toolbox.html#spmms  
##Rogers A & Castro LJ (1981). “Model Migration Schedules.” IIASA Research Report. IIASA, Laxenburg, Austria: RR-81-030
##	Available online: http://pure.iiasa.ac.at/id/eprint/1543/  
##############################################################################################################################


##############################
##INPUTS
##############################

###############
#DATA
SPMMSTestingData<-read.table(file="https://github.com/AppliedDemogToolbox/Hunsinger_MMSRCode/raw/master/SPMMSData.csv",header=TRUE,sep=",")
migprob<-(SPMMSTestingData$Migration.probability[1:90])

#NUMBER OF ITERATIONS - USED FOR FITTING
ITER<-100000
###############

###############
##STEP 1 INPUTS
#PROPORTIONALLY ADJUST DATA TO SUM TO 1 - NO PARAMETERS
###############

###############
##STEP 2 INPUTS
#NUMBER OF SMALLEST VALUES TO USE AVERAGE OF AS LEVEL TERM
level<-5
###############

###############
##STEP 3 INPUTS
#MIN AND MAX OF CHILDHOOD AGES TO FIT OVER
childmin<-0
childmax<-16

#HEIGHT OF THE CHILDHOOD CURVE
childparam1tries<-array(runif(ITER,0,.1))

#RATE OF DESCENT OF THE CHILDHOOD CURVE
childparam2tries<-array(runif(ITER,0,1))
###############

###############
##STEP 4 INPUTS
#MIN AND MAX OF LABOR FORCE AGES TO FIT OVER
labormin<-17
labormax<-45

#STUDENT AGES TO EXCLUDE - CURRENTLY MUST BE ADJACENT AGES - TO EXCLUDE STUDENT PEAK FROM MODEL CAN SET AS JUST '0'
studentages<-c(18,19) #c(0)

#HEIGHT OF THE LABOR FORCE CURVE
labparam1tries<-array(runif(ITER,.04,.08))

#RATE OF DESCENT OF THE LABOR FORCE CURVE
labparam2tries<-array(runif(ITER,.06,.10))

#POSITION OF THE LABOR FORCE CURVE ON THE AGE-AXIS
labparam3tries<-array(runif(ITER,20,23))

#RATE OF ASCENT OF THE LABOR FORCE CURVE
labparam4tries<-array(runif(ITER,.1,.5))
###############

###############
##STEP 5 INPUTS
#MIN AND MAX OF RETIREMENT AGES TO FIT OVER
retmin<-50
retmax<-75

#HEIGHT OF RETIREMENT CURVE
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10' (MUST BE HIGHER THAN MIN HERE)
retparam1tries<-array(runif(ITER,.0,.01)) #runif(ITER,0,1e-10)

#RATE OF DESCENT OF RETIREMENT CURVE
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10' (MUST BE HIGHER THAN MIN HERE)
retparam2tries<-array(runif(ITER,2.5,10)) #runif(ITER,0,1e-10)

#POSITION OF THE RETIREMENT CURVE ON THE AGE-AXIS
retparam3tries<-array(runif(ITER,55,65))
###############


##############################
##FIT THE DATA
##############################

##STEP 1 FIT - PROPORTIONAL TO SUM TO 1
step1<-array(,length(migprob))
for (i in 1:length(migprob)) {step1[i]<-migprob[i]/sum(migprob)}

##STEP 2 FIT - MAKE MEAN AGE AND SET LEVEL TERM BASED ON SELECTED NUMBER OF SMALLEST VALUES
step2<-array(,length(step1))
for (i in 1:length(step2)) {if(step1[i] != 0){step2[i]<-step1[i]}}
step2<-array(mean(sort(step2)[1:level]),length(step2))
ages<-c(0:(length(step1)-1))
meanages<-c(0+1:length(step1))

##STEP 3 FIT - SELECT BEST OF ITER BASED ON INPUT DISTRIBUTIONS
step3tries<-array(step1-step2,dim=c(length(step1),ITER))
for (i in 1:ITER) {step3tries[1:90,i]<-childparam1tries[i]*exp(-childparam2tries[i]*(meanages[]))}
childresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=childmin)&(meanages[j]<=childmax)) {childresidtries[j,i]<-(step3tries[j,i]-(step1-step2)[j])^2}}}
sumchildresidtries<-array(,ITER)
for (i in 1:ITER) {sumchildresidtries[i]<-sum(childresidtries[,i])}
childparam1tries[which(sumchildresidtries==min(sumchildresidtries))]
childparam2tries[which(sumchildresidtries==min(sumchildresidtries))]
sumchildresidtries[which(sumchildresidtries==min(sumchildresidtries))]
step3<-step2+step3tries[,which(sumchildresidtries==min(sumchildresidtries))]

##STEP 4 FIT - SELECT BEST OF ITER BASED ON INPUT DISTRIBUTIONS
step4tries<-array(step1-step3,dim=c(length(step1),ITER))
for (i in 1:ITER) {step4tries[1:90,i]<-labparam1tries[i]*exp(-labparam2tries[i]*(meanages[]-labparam3tries[i])-exp(-labparam4tries[i]*(meanages[]-labparam3tries[i])))}
labresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=labormin)&(meanages[j]<=labormax)&((meanages[j]<min(studentages))|(meanages[j]>max(studentages)))) {labresidtries[j,i]<-(step4tries[j,i]-(step1-step3)[j])^2}}}
sumlabresidtries<-array(,ITER)
for (i in 1:ITER) {sumlabresidtries[i]<-sum(labresidtries[,i])}
labparam1tries[which(sumlabresidtries==min(sumlabresidtries))]
labparam2tries[which(sumlabresidtries==min(sumlabresidtries))]
labparam3tries[which(sumlabresidtries==min(sumlabresidtries))]
labparam4tries[which(sumlabresidtries==min(sumlabresidtries))]
sumlabresidtries[which(sumlabresidtries==min(sumlabresidtries))]
step4<-step3+step4tries[,which(sumlabresidtries==min(sumlabresidtries))]

##STEP 5 FIT - SELECT BEST OF ITER BASED ON INPUT DISTRIBUTIONS
step5tries<-array(step1-step4,dim=c(length(step1),ITER))
for (i in 1:ITER) {step5tries[1:90,i]<-retparam1tries[i]*exp(-((meanages[]-retparam3tries[i])/retparam2tries[i])*((meanages[]-retparam3tries[i])/retparam2tries[i]))}
retresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=retmin)&(meanages[j]<=retmax)) {retresidtries[j,i]<-(step5tries[j,i]-(step1-step4)[j])^2}}}
sumretresidtries<-array(,ITER)
for (i in 1:ITER) {sumretresidtries[i]<-sum(retresidtries[,i])}
retparam1tries[which(sumretresidtries==min(sumretresidtries))]
retparam2tries[which(sumretresidtries==min(sumretresidtries))]
retparam3tries[which(sumretresidtries==min(sumretresidtries))]
sumretresidtries[which(sumretresidtries==min(sumretresidtries))]
step5<-step4+step5tries[,which(sumretresidtries==min(sumretresidtries))]

##REVIEW FIT
#SQUARED SUM OF RESIDUALS FOR ENTIRE MODEL
squaredsumoffullmodelresiduals<-sum((step5-step1)^2)

##############################
##PLOT THE DATA
##############################

##PLOT ACCUMULATED FIT
plot(step1,xlab="Age",ylab="Migration Rate (proportional)",ylim=c(-.005,.04),pch=1)
lines(step5,col="black",lwd=3)

##PLOT INDIVIDUAL STEP FITTING
lines(step5-step4,col="purple",lwd=2,lty=2)
lines(step4-step3,col="green",lwd=2,lty=2)
lines(step3-step2,col="blue",lwd=2,lty=2)
lines(step2,col="red",lwd=2,lty=2)

##PLOT RESIDUALS
lines(step5-step1,col="dark grey")

legend(55,.04, 
legend=c("Scaled data", "Full model curve", "Level", "Childhood curve", "Labor force curve", "Retirement curve", "Full model residuals"), 
col=c("black", "black", "red", "blue", "green", "purple", "grey"), 
lwd=c(1,2,2,2,2,2,1), lty=c(NA,1,2,2,2,2,1), pch=c(1,NA,NA,NA,NA,NA,NA), cex=0.8)

squaredsumoffullmodelresiduals


##############################
##WRITE THE DATA
##############################

#write.table(###, file="G:/###/###.csv", sep=",")
