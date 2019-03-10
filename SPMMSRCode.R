##############################################################################################################################
##EDDIE'S R CODE FOR FITTING THE MULTI-EXPONENTIAL MODEL MIGRATION SCHEDULE WITH STUDENT PEAK
##
##EDDIE HUNSINGER, AUGUST 2018 (LAST UPDATED MARCH 2019)
##http://www.demog.berkeley.edu/~eddieh/
##
##IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS CODE, BE SURE TO CITE THE SOURCE
##
##EXAMPLE DATA IS LINKED, SO YOU SHOULD BE ABLE TO SIMPLY COPY ALL AND PASTE INTO R
##
##THERE IS NO WARRANTY FOR THIS CODE
##THIS CODE HAS NOT BEEN PEER-REVIEWED OR CAREFULLY TESTED - PLEASE LET ME KNOW IF YOU FIND ANY PROBLEMS (edyhsgr@gmail.com)
##
##FOR EACH OF THE STEP 4 THROUGH 7 (WILSON, 2010) PARAMETERS, IT GIVES A BEST FIT BASED ON SAMPLING FROM RECURSIVELY SMALLER UNIFORM DISTRIBUTIONS 
##THIS IS JUST A REASONED DIY SOLUTION - PLEASE LET ME KNOW (edyhsgr@gmail.com) IF YOU KNOW OF AN EXISTING REFERENCE/EXAMPLE/NAME/CRITIQUE/ETC FOR IT OR SIMILAR - AND ANY THOUGHTS ARE WELCOME
##
##THE STARTING PARAMETER DISTRIBUTIONS FOR THIS EXAMPLE ARE AROUND EXISTING INFO FROM ROGERS-CASTRO AND WILSON, AND NOT CAREFULLY FIGURED OR CALIBRATED
##PLEASE FEEL ENCOURAGED TO TEST OUT DIFFERENT SETTINGS, OF COURSE
##
##IF YOU GET A POOR FIT FOR SOME DATA, ONE ITEM TO REVIEW IS COMPARISON OF THE PARAMETER ESTIMATES TO THE STARTING PARAMETER DISTRIBUTIONS (MAY NEED TO SIMPLY EXPAND OR CHANGE THE BOUNDS OF AN INPUT)
##
##COPIES WITH SOME APPLICATION OF AND COMPARISON TO R's lm() AND nls() FUNCTIONS, AND TO ~PLAIN MONTE CARLO, 
##ARE AVAILABLE AT https://github.com/AppliedDemogToolbox/Hunsinger_SPMMSRCode/tree/master/FittingComparisons
##
##FOR MORE INFO ON THE SPMMS MODEL, SEE: Wilson, T. (2010). “Model migration schedules incorporating student migration peaks.” Demographic Research, 23(8): 191–222.
##AVAILABLE ONLINE: https://www.demographic-research.org/Volumes/Vol23/8/default.htm
##RELATED EXCEL WORKBOOK BY TOM WILSON: http://www.demog.berkeley.edu/~eddieh/toolbox.html#SPMMS (MANY THANKS TO TOM WILSON FOR SHARING IT - A TERRIFIC RESOURCE AND GUIDE)
##(NOTE BELOW HAS PARAMETERS SET TO RUN ALL STEPS AS DEFAULT BUT THE EXCEL WORKBOOK HAS ELDERLY STEP TURNED OFF AS DEFAULT)
##
##GREAT RESOURCE ON THE ROGERS-CASTRO MODEL: Rogers A & Castro LJ (1981). Model Migration Schedules. IIASA Research Report. IIASA, Laxenburg, Austria: RR-81-030
##AVAILABLE ONLINE: http://pure.iiasa.ac.at/id/eprint/1543/
##
##This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. More information: https://creativecommons.org/licenses/by-nc-sa/4.0/? 
##############################################################################################################################


##############################
##INPUTS
##############################

###############
#DATA FROM WILSON (2010)
SPMMSTestingData<-read.table(file="https://github.com/AppliedDemogToolbox/Hunsinger_SPMMSRCode/raw/master/SPMMSData.csv",header=TRUE,sep=",")
migprob<-(SPMMSTestingData$Migration.probability[1:90])

#SIZE OF migprob (DATA BY AGE) USED
SIZE<-90

#NUMBER OF TRIES - USED FOR FITTING
TRIES<-1000

#PROPORTION TO ITER DISTRIBUTION BOUND SELECTION WITH
BEST<-.015

#CONVERGENCE INDEX
FITTO<-1e-10
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
childparam1tries<-array(runif(TRIES,0,.1))

#RATE OF DESCENT OF THE CHILDHOOD CURVE
childparam2tries<-array(runif(TRIES,0,1))
###############

###############
##STEP 4 INPUTS
#MIN AND MAX OF LABOR FORCE AGES TO FIT OVER
labormin<-17
labormax<-45

#STUDENT AGES TO EXCLUDE - CURRENTLY MUST BE ADJACENT AGES - TO EXCLUDE STUDENT PEAK FROM MODEL CAN SET AS JUST '0'
studentages<-c(18,19) #studentages<-c(0)

#HEIGHT OF THE LABOR FORCE CURVE
labparam1tries<-array(runif(TRIES,.04,.08))

#RATE OF DESCENT OF THE LABOR FORCE CURVE
labparam2tries<-array(runif(TRIES,.06,.10))

#POSITION OF THE LABOR FORCE CURVE ON THE AGE-AXIS
labparam3tries<-array(runif(TRIES,20,23))

#RATE OF ASCENT OF THE LABOR FORCE CURVE
labparam4tries<-array(runif(TRIES,.1,.5))
###############

###############
##STEP 5 INPUTS
#MIN AND MAX OF RETIREMENT AGES TO FIT OVER
retmin<-50
retmax<-75

#HEIGHT OF RETIREMENT CURVE
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
retparam1tries<-array(runif(TRIES,.0,.01)) #retparam1tries<-array(runif(TRIES,0,1e-10))

#RATE OF DESCENT OF RETIREMENT CURVE
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
retparam2tries<-array(runif(TRIES,2.5,10)) #retparam2tries<-array(runif(TRIES,0,1e-10))

#POSITION OF THE RETIREMENT CURVE ON THE AGE-AXIS
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET LOW AS '55' AND HIGH AS '55+1e-10'
retparam3tries<-array(runif(TRIES,55,65)) #retparam1tries<-array(runif(TRIES,55,55+1e-10))
###############

###############
##STEP 6 INPUTS
#MIN AND MAX OF ELDERLY AGES TO FIT OVER
eldmin<-70
eldmax<-84

#HEIGHT OF THE ELDERLY CURVE
#TO APPROXIMATELY EXCLUDE ELDERLY CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
eldparam1tries<-array(runif(TRIES,0,.000005)) #eldparam1tries<-array(runif(TRIES,0,1e-10))

#RATE OF ASCENT OF THE ELDERLY CURVE
#TO APPROXIMATELY EXCLUDE ELDERLY CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
eldparam2tries<-array(runif(TRIES,.05,.1)) #eldparam2tries<-array(runif(TRIES,0,1e-10))
###############

###############
##STEP 7 INPUTS
#MIN AND MAX OF STUDENT AGES TO FIT OVER - STUDENT AGES SET ABOVE UNDER STEP 4 INPUTS
stumin<-min(studentages)
stumax<-max(studentages+1)

#HEIGHT OF STUDENT CURVE
#TO APPROXIMATELY EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
stuparam1tries<-array(runif(TRIES,.001,.1)) #stuparam1tries<-array((runif(TRIES,0,1e-10)))

#RATE OF DESCENT OF STUDENT CURVE
#TO APPROXIMATELY EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
stuparam2tries<-array(runif(TRIES,0,5)) #stuparam2tries<-array((runif(TRIES,0,1e-10)))

#POSITION OF THE STUDENT CURVE ON THE AGE-AXIS
#TO APPROXIMATELY EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
stuparam3tries<-array(runif(TRIES,17,21)) #stuparam3tries<-array((runif(TRIES,0,1e-10)))

#RATE OF ASCENT OF STUDENT CURVE
#TO APPROXIMATELY EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10'
stuparam4tries<-array(runif(TRIES,0,3)) #stuparam4tries<-array((runif(TRIES,0,1e-10)))
###############


##############################
##FIT TO THE DATA
##############################

##STEP 1 FIT - PROPORTIONAL TO SUM TO 1
step1<-array(,length(migprob))
for (i in 1:length(migprob)) {step1[i]<-migprob[i]/sum(migprob)}

##STEP 2 FIT - MAKE MEAN AGE AND SET LEVEL TERM BASED ON SELECTED NUMBER OF SMALLEST VALUES
step2<-array(,length(step1))
for (i in 1:length(step2)) {if(step1[i] != 0){step2[i]<-step1[i]}}
step2<-array(mean(sort(step2)[1:level]),length(step2))
meanages<-c(0+1:length(step1))

##STEP 3 FIT - SLOPE AND INTERCEPT FOR TRANSFORMATION
#THIS IS DIRECTLY ESTIMATED FIT - I COMMENTED OUT AND DID BY SAMPLING TO MAKE CONSISTENT WITH STEPS 4 THROUGH 7
#step3<-array(,childmax-childmin)
#for (i in 1:length(step3)) {step3[i]<-log(step1[i]-step2[i])}
#meanchildmin<-childmin+1
#childages<-c(childmin+1:childmax)
#childfit<-lm(step3~childages)
#for (i in 1:length(meanages)) {step3[i]<-exp(childfit$coefficients[1])*exp(-(-childfit$coefficients[2])*meanages[i])}
#step3<-step2+step3

##STEP 3 FIT - SELECT BEST PERCENT PARAMETER VALUES OF TRIES BASED ON INPUT DISTRIBUTIONS, THEN REPEAT TRIES WITH THE UNIFORM BOUNDS OF BEST PERCENT AND SELECT BEST PARAMETER VALUES 
step3triesfit<-function(childparam1tries,childparam2tries){
step3tries<-array(step1-step2,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {step3tries[1:SIZE,i]<-childparam1tries[i]*exp(-childparam2tries[i]*(meanages[]))}
childresidtries<-array(0,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {for (j in 1:length(meanages)) {if((meanages[j]>=childmin)&(meanages[j]<=childmax)) {childresidtries[j,i]<-(step3tries[j,i]-(step1-step2)[j])^2}}}
sumchildresidtries<-array(,TRIES)
for (i in 1:TRIES) {sumchildresidtries[i]<-sum(childresidtries[,i])}
childparam1tries<-runif(TRIES,min(childparam1tries[match(head(sort(sumchildresidtries),TRIES*BEST),sumchildresidtries)]), max(childparam1tries[match(head(sort(sumchildresidtries),TRIES*BEST),sumchildresidtries)]))
childparam2tries<-runif(TRIES,min(childparam2tries[match(head(sort(sumchildresidtries),TRIES*BEST),sumchildresidtries)]), max(childparam2tries[match(head(sort(sumchildresidtries),TRIES*BEST),sumchildresidtries)]))
childparamtries<-data.frame(sumchildresidtries=sumchildresidtries,childparam1tries=childparam1tries,childparam2tries=childparam2tries)
return(c(step3tries,childparamtries))
}
step3repeatpass<-step3triesfit(childparam1tries,childparam2tries)
ITER<-0
while (abs(max(step3repeatpass$childparam1tries)-min(step3repeatpass$childparam1tries))>FITTO & 
abs(max(step3repeatpass$childparam2tries)-min(step3repeatpass$childparam2tries))>FITTO)
{step3repeatpass<-step3triesfit(step3repeatpass$childparam1tries,step3repeatpass$childparam2tries)
ITER=ITER+1
}
step3repeatpass$childparam1tries[1]
step3repeatpass$childparam2tries[1]
step3repeatpass$sumchildresidtries[1]
step3best<-array(step1-step2,dim=c(length(step1)))
ITER
step3best[1:SIZE]<-step3repeatpass$childparam1tries[1]*exp(-step3repeatpass$childparam2tries[1]*(meanages[]))
step3<-step2+step3best

##STEP 4 FIT - SELECT BEST PERCENT PARAMETER VALUES OF TRIES BASED ON INPUT DISTRIBUTIONS, THEN REPEAT TRIES WITH THE UNIFORM BOUNDS OF BEST PERCENT UNTIL CONVERGENCE  
step4triesfit<-function(labparam1tries,labparam2tries,labparam3tries,labparam4tries){
step4tries<-array(step1-step2,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {step4tries[1:SIZE,i]<-labparam1tries[i]*exp(-labparam2tries[i]*(meanages[]-labparam3tries[i])-exp(-labparam4tries[i]*(meanages[]-labparam3tries[i])))}
labresidtries<-array(0,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {for (j in 1:length(meanages)) {if((meanages[j]>=labormin)&(meanages[j]<=labormax)&((meanages[j]<min(studentages))|(meanages[j]>max(studentages)))) {labresidtries[j,i]<-(step4tries[j,i]-(step1-step3)[j])^2}}}
sumlabresidtries<-array(,TRIES)
for (i in 1:TRIES) {sumlabresidtries[i]<-sum(labresidtries[,i])}
labparam1tries<-runif(TRIES,min(labparam1tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]),max(labparam1tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]))
labparam2tries<-runif(TRIES,min(labparam2tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]),max(labparam2tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]))
labparam3tries<-runif(TRIES,min(labparam3tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]),max(labparam3tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]))
labparam4tries<-runif(TRIES,min(labparam4tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]),max(labparam4tries[match(head(sort(sumlabresidtries),TRIES*BEST),sumlabresidtries)]))
labparamtries<-data.frame(sumlabresidtries=sumlabresidtries,labparam1tries=labparam1tries,labparam2tries=labparam2tries,labparam3tries=labparam3tries,labparam4tries=labparam4tries)
return(c(step4tries,labparamtries))
}
step4repeatpass<-step4triesfit(labparam1tries,labparam2tries,labparam3tries,labparam4tries)
ITER<-0
while (abs(max(step4repeatpass$labparam1tries)-min(step4repeatpass$labparam1tries))>FITTO & 
abs(max(step4repeatpass$labparam2tries)-min(step4repeatpass$labparam2tries))>FITTO &
abs(max(step4repeatpass$labparam3tries)-min(step4repeatpass$labparam3tries))>FITTO &
abs(max(step4repeatpass$labparam4tries)-min(step4repeatpass$labparam4tries))>FITTO
)
{step4repeatpass<-step4triesfit(step4repeatpass$labparam1tries,step4repeatpass$labparam2tries,step4repeatpass$labparam3tries,step4repeatpass$labparam4tries)
ITER=ITER+1
}
step4repeatpass$labparam1tries[1]
step4repeatpass$labparam2tries[1]
step4repeatpass$labparam3tries[1]
step4repeatpass$labparam4tries[1]
step4repeatpass$sumlabresidtries[1]
ITER
step4best<-array(step1-step3,dim=c(length(step1)))
step4best[1:SIZE]<-step4repeatpass$labparam1tries[1]*exp(-step4repeatpass$labparam2tries[1]*(meanages[]-step4repeatpass$labparam3tries[1])-exp(-step4repeatpass$labparam4tries[1]*(meanages[]-step4repeatpass$labparam3tries[1])))
step4<-step3+step4best

##STEP 5 FIT - SELECT BEST PERCENT PARAMETER VALUES OF TRIES BASED ON INPUT DISTRIBUTIONS, THEN REPEAT TRIES WITH THE UNIFORM BOUNDS OF BEST PERCENT UNTIL CONVERGENCE   
step5triesfit<-function(retparam1tries,retparam2tries,retparam3tries){
step5tries<-array(step1-step2,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {step5tries[1:SIZE,i]<-retparam1tries[i]*exp(-((meanages[]-retparam3tries[i])/retparam2tries[i])*((meanages[]-retparam3tries[i])/retparam2tries[i]))}
retresidtries<-array(0,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {for (j in 1:length(meanages)) {if((meanages[j]>=retmin)&(meanages[j]<=retmax)&((meanages[j]<min(studentages))|(meanages[j]>max(studentages)))) {retresidtries[j,i]<-(step5tries[j,i]-(step1-step4)[j])^2}}}
sumretresidtries<-array(,TRIES)
for (i in 1:TRIES) {sumretresidtries[i]<-sum(retresidtries[,i])}
retparam1tries<-runif(TRIES,min(retparam1tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]),max(retparam1tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]))
retparam2tries<-runif(TRIES,min(retparam2tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]),max(retparam2tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]))
retparam3tries<-runif(TRIES,min(retparam3tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]),max(retparam3tries[match(head(sort(sumretresidtries),TRIES*BEST),sumretresidtries)]))
retparamtries<-data.frame(sumretresidtries=sumretresidtries,retparam1tries=retparam1tries,retparam2tries=retparam2tries,retparam3tries=retparam3tries)
return(c(step5tries,retparamtries))
}
step5repeatpass<-step5triesfit(retparam1tries,retparam2tries,retparam3tries)
ITER<-0
while (abs(max(step5repeatpass$retparam2tries)-min(step5repeatpass$retparam2tries))>FITTO)
{step5repeatpass<-step5triesfit(step5repeatpass$retparam1tries,step5repeatpass$retparam2tries,step5repeatpass$retparam3tries)
ITER=ITER+1
}
step5repeatpass$retparam1tries[1]
step5repeatpass$retparam2tries[1]
step5repeatpass$retparam3tries[1]
step5repeatpass$sumretresidtries[1]
ITER
step5best<-array(step1-step4,dim=c(length(step1)))
step5best[1:SIZE]<-step5repeatpass$retparam1tries[1]*exp(-((meanages[]-step5repeatpass$retparam3tries[1])/step5repeatpass$retparam2tries[1])*((meanages[]-step5repeatpass$retparam3tries[1])/step5repeatpass$retparam2tries[1]))
step5<-step4+step5best

##STEP 6 FIT - SELECT BEST PERCENT PARAMETER VALUES OF TRIES BASED ON INPUT DISTRIBUTIONS, THEN REPEAT TRIES WITH THE UNIFORM BOUNDS OF BEST PERCENT UNTIL CONVERGENCE  
step6triesfit<-function(eldparam1tries,eldparam2tries){
step6tries<-array(step1-step2,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {step6tries[1:SIZE,i]<-eldparam1tries[i]*exp(eldparam2tries[i]*meanages[])}
eldresidtries<-array(0,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {for (j in 1:length(meanages)) {if((meanages[j]>=eldmin)&(meanages[j]<=eldmax)) {eldresidtries[j,i]<-(step6tries[j,i]-(step1-step5)[j])^2}}}
sumeldresidtries<-array(,TRIES)
for (i in 1:TRIES) {sumeldresidtries[i]<-sum(eldresidtries[,i])}
eldparam1tries<-runif(TRIES,min(eldparam1tries[match(head(sort(sumeldresidtries),TRIES*BEST),sumeldresidtries)]),max(eldparam1tries[match(head(sort(sumeldresidtries),TRIES*BEST),sumeldresidtries)]))
eldparam2tries<-runif(TRIES,min(eldparam2tries[match(head(sort(sumeldresidtries),TRIES*BEST),sumeldresidtries)]),max(eldparam2tries[match(head(sort(sumeldresidtries),TRIES*BEST),sumeldresidtries)]))
eldparamtries<-data.frame(sumeldresidtries=sumeldresidtries,eldparam1tries=eldparam1tries,eldparam2tries=eldparam2tries)
return(c(step6tries,eldparamtries))
}
step6repeatpass<-step6triesfit(eldparam1tries,eldparam2tries)
ITER<-0
while (abs(max(step6repeatpass$eldparam1tries)-min(step6repeatpass$eldparam1tries))>FITTO &
abs(max(step6repeatpass$eldparam2tries)-min(step6repeatpass$eldparam2tries))>FITTO
)
{step6repeatpass<-step6triesfit(step6repeatpass$eldparam1tries,step6repeatpass$eldparam2tries)
ITER=ITER+1
}
step6repeatpass$eldparam1tries[1]
step6repeatpass$eldparam2tries[1]
step6repeatpass$sumeldresidtries[1]
ITER
step6best<-array(step1-step5,dim=c(length(step1)))
step6best[1:SIZE]<-step6repeatpass$eldparam1tries[1]*exp(step6repeatpass$eldparam2tries[1]*meanages[])
step6<-step5+step6best

##STEP 7 FIT - SELECT BEST PERCENT PARAMETER VALUES OF TRIES BASED ON INPUT DISTRIBUTIONS, THEN REPEAT TRIES WITH THE UNIFORM BOUNDS OF BEST PERCENT UNTIL CONVERGENCE  
step7triesfit<-function(stuparam1tries,stuparam2tries,stuparam3tries,stuparam4tries){
step7tries<-array(step1-step2,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {step7tries[1:SIZE,i]<-stuparam1tries[i]*exp(-stuparam2tries[i]*(meanages[]-stuparam3tries[i])-exp(-stuparam4tries[i]*(meanages[]-stuparam3tries[i])))}
sturesidtries<-array(0,dim=c(length(step1),TRIES))
for (i in 1:TRIES) {for (j in 1:length(meanages)) {if((meanages[j]>=stumin)&(meanages[j]<=stumax)) {sturesidtries[j,i]<-(step7tries[j,i]-(step1-step6)[j])^2}}}
sumsturesidtries<-array(,TRIES)
for (i in 1:TRIES) {sumsturesidtries[i]<-sum(sturesidtries[,i])}
stuparam1tries<-runif(TRIES,min(stuparam1tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]),max(stuparam1tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]))
stuparam2tries<-runif(TRIES,min(stuparam2tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]),max(stuparam2tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]))
stuparam3tries<-runif(TRIES,min(stuparam3tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]),max(stuparam3tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]))
stuparam4tries<-runif(TRIES,min(stuparam4tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]),max(stuparam4tries[match(head(sort(sumsturesidtries),TRIES*BEST),sumsturesidtries)]))
stuparamtries<-data.frame(sumsturesidtries=sumsturesidtries,stuparam1tries=stuparam1tries,stuparam2tries=stuparam2tries,stuparam3tries=stuparam3tries,stuparam4tries=stuparam4tries)
return(c(step7tries,stuparamtries))
}
step7repeatpass<-step7triesfit(stuparam1tries,stuparam2tries,stuparam3tries,stuparam4tries)
ITER<-0
while (abs(max(step7repeatpass$stuparam1tries)-min(step7repeatpass$stuparam1tries))>FITTO &
abs(max(step7repeatpass$stuparam2tries)-min(step7repeatpass$stuparam2tries))>FITTO &
abs(max(step7repeatpass$stuparam3tries)-min(step7repeatpass$stuparam3tries))>FITTO &
abs(max(step7repeatpass$stuparam4tries)-min(step7repeatpass$stuparam4tries))>FITTO
)
{step7repeatpass<-step7triesfit(step7repeatpass$stuparam1tries,step7repeatpass$stuparam2tries,step7repeatpass$stuparam3tries,step7repeatpass$stuparam4tries)
ITER=ITER+1
}
step7repeatpass$stuparam1tries[1]
step7repeatpass$stuparam2tries[1]
step7repeatpass$stuparam3tries[1]
step7repeatpass$stuparam4tries[1]
step7repeatpass$sumsturesidtries[1]
ITER
step7best<-array(step1-step6,dim=c(length(step1)))
step7best[1:SIZE]<-step7repeatpass$stuparam1tries[i]*exp(-step7repeatpass$stuparam2tries[i]*(meanages[]-step7repeatpass$stuparam3tries[i])-exp(-step7repeatpass$stuparam4tries[i]*(meanages[]-step7repeatpass$stuparam3tries[i])))
step7<-step6+step7best

##REVIEW FIT
#SQUARED SUM OF RESIDUALS FOR ENTIRE MODEL
squaredsumoffullmodelresiduals<-sum((step7-step1)^2)


##############################
##PLOT THE DATA
##############################

##PLOT ACCUMULATED FIT
plot(step1,xlab="Age",ylab="Migration Rate (proportional)",ylim=c(-.005,.04),pch=1)
lines(step7,col="black",lwd=3)

##PLOT INDIVIDUAL STEP FITTING
lines(step7-step6,col="yellow",lwd=2,lty=2)
lines(step6-step5,col="orange",lwd=2,lty=2)
lines(step5-step4,col="purple",lwd=2,lty=2)
lines(step4-step3,col="green",lwd=2,lty=2)
lines(step3-step2,col="blue",lwd=2,lty=2)
lines(step2,col="red",lwd=2,lty=2)

##PLOT RESIDUALS
lines(step7-step1,col="dark grey")

legend(55,.04, 
legend=c("Scaled data", "Full model curve", "Level", "Childhood curve", "Labor force curve", "Retirement curve", "Elderly curve", "Student curve", "Full model residuals"), 
col=c("black", "black", "red", "blue", "green", "purple", "orange", "yellow", "grey"), 
lwd=c(1,2,2,2,2,2,2,2,1), lty=c(NA,1,2,2,2,2,2,2,1), pch=c(1,NA,NA,NA,NA,NA,NA,NA,NA), cex=0.8)

squaredsumoffullmodelresiduals


##############################
##WRITE THE DATA
##############################

#write.table(###, file="G:/###/###.csv", sep=",")
