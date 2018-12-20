##############################################################################################################################
##AN R CODE ADAPTATION OF TOM WILSON'S SPMMS
##(ROGERS AND CASTRO MODEL MIGRATION WITH STUDENT PEAK)
##
##EDDIE HUNSINGER, AUGUST 2018 (UPDATED DECEMBER 2018)
##http://www.demog.berkeley.edu/~eddieh/
##
##IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS CODE, BE SURE TO CITE THE SOURCE
##
##EXAMPLE DATA IS LINKED, SO YOU SHOULD BE ABLE TO SIMPLY COPY ALL AND PASTE INTO R
##
##THERE IS NO WARRANTY FOR THIS CODE
##THIS CODE HAS NOT BEEN TESTED AT ALL-- PLEASE LET ME KNOW IF YOU FIND ANY PROBLEMS (edyhsgr@gmail.com)
##
##IT GIVES A BEST FIT BASED ON THE SELECTED NUMBER (ITER) OF VALUES DRAWN WITH SELECTED PARAMETER RANGES FOR WILSON'S STEPS 3 THROUGH 7
##NOTE: I AIMED TO KEEP THE RANGES NAIVE FOR THIS EXAMPLE, AROUND EXISTING INFO FROM ROGERS, CASTRO, AND WILSON
##IMPORTANT: IT'S NOT SET UP TO FULLY OR RELIABLY CONVERGE, SO THE PARAMETER ESTIMATES FROM IT ARE NOT QUITE STABLE
##
##FOR MORE INFO ON THE SPMMS MODEL, SEE: Wilson, T. (2010). “Model migration schedules incorporating student migration peaks.” Demographic Research, 23(8): 191–222.
##AVAILABLE ONLINE: https://www.demographic-research.org/Volumes/Vol23/8/default.htm
##
##RELATED EXCEL WORKBOOK BY TOM WILSON: http://www.demog.berkeley.edu/~eddieh/toolbox.html#SPMMS 
##(NOTE BELOW HAS PARAMETERS SET TO RUN ALL STEPS AS DEFAULT BUT THE EXCEL WORKBOOK HAS ELDERLY STEP TURNED OFF AS DEFAULT)
##
##A NEAT R PACKAGE ("migest") THAT INCLUDES ROGERS AND CASTRO MODEL, BY GUY ABEL: 
##https://cran.r-project.org/web/packages/migest/index.html. 
##############################################################################################################################

##############################
##INPUTS
##############################

###############
#DATA
SPMMSTestingData<-read.table(file="https://github.com/AppliedDemogToolbox/Hunsinger_SPMMSRCode/raw/master/SPMMSData.csv",header=TRUE,sep=",")
migprob<-(SPMMSTestingData$Migration.probability[1:90])

#SIZE OF migprob (DATA BY AGE) USED
SIZE<-90

#NUMBER OF ITERATIONS - USED FOR FITTING
ITER<-10000

#PROPORTION TO REPEAT DISTRIBUTION BOUND SELECTION WITH
BEST<-.001
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
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET LOW AS '55' AND HIGH AS '55+1e-10' (MUST BE HIGHER THAN MIN HERE)
retparam3tries<-array(runif(ITER,55,65)) #runif(ITER,55,55+1e-10)
###############

###############
##STEP 6 INPUTS
#MIN AND MAX OF ELDERLY AGES TO FIT OVER
eldmin<-70
eldmax<-84

#HEIGHT OF THE ELDERLY CURVE
#TO APPROXIMATELY EXCLUDE ELDERLY CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10' (MUST BE HIGHER THAN MIN HERE)
eldparam1tries<-array(runif(ITER,0,.000005)) #runif(ITER,0,1e-10)

#RATE OF ASCENT OF THE ELDERLY CURVE
#TO APPROXIMATELY EXCLUDE ELDERLY CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10' (MUST BE HIGHER THAN MIN HERE)
eldparam2tries<-array(runif(ITER,0,.1)) #runif(ITER,0,1e-10)
###############

###############
##STEP 7 INPUTS
#MIN AND MAX OF STUDENT AGES TO FIT OVER - STUDENT AGES SET ABOVE UNDER STEP 4 INPUTS
stumin<-min(studentages)
stumax<-max(studentages+1)

#HEIGHT OF STUDENT CURVE
#TO APPROXIMATELY EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10' (MUST BE HIGHER THAN MIN HERE)
stuparam1tries<-array(runif(ITER,.00001,.1)) #(runif(ITER,0,1e-10))

#RATE OF DESCENT OF STUDENT CURVE
#TO APPROXIMATELY EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10' (MUST BE HIGHER THAN MIN HERE)
stuparam2tries<-array(runif(ITER,0,5)) #(runif(ITER,0,1e-10))

#POSITION OF THE STUDENT CURVE ON THE AGE-AXIS
#TO APPROXIMATELY EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10' (MUST BE HIGHER THAN MIN HERE)
stuparam3tries<-array(runif(ITER,17,21)) #(runif(ITER,0,1e-10))

#RATE OF ASCENT OF STUDENT CURVE
#TO APPROXIMATELY EXCLUDE STUDENT CURVE FROM MODEL CAN SET LOW AS '0' AND HIGH AS '1e-10' (MUST BE HIGHER THAN MIN HERE)
stuparam4tries<-array(runif(ITER,0,3)) #(runif(ITER,0,1e-10))
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

##STEP 3 FIT - SLOPE AND INTERCEPT FOR TRANSFORMATION
#THIS IS DIRECTLY ESTIMATED FIT - I COMMENTED OUT AND DID BY SAMPLING TO MAKE CONSISTENT WITH STEPS 4 THROUGH 7
#step3<-array(,childmax-childmin)
#for (i in 1:length(step3)) {step3[i]<-log(step1[i]-step2[i])}
#meanchildmin<-childmin+1
#childages<-c(childmin+1:childmax)
#childfit<-lm(step3~childages)
#for (i in 1:length(ages)) {step3[i]<-exp(childfit$coefficients[1])*exp(-(-childfit$coefficients[2])*meanages[i])}
#step3<-step2+step3

##STEP 3 FIT - SELECT BEST PERCENT PARAMETER VALUES OF ITER BASED ON INPUT DISTRIBUTIONS, THEN REPEAT ITER WITH THE UNIFORM BOUNDS OF BEST PERCENT AND SELECT BEST PARAMETER VALUES 
step3tries<-array(step1-step2,dim=c(length(step1),ITER))
for (i in 1:ITER) {step3tries[1:SIZE,i]<-childparam1tries[i]*exp(-childparam2tries[i]*(meanages[]))}
childresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=childmin)&(meanages[j]<=childmax)) {childresidtries[j,i]<-(step3tries[j,i]-(step1-step2)[j])^2}}}
sumchildresidtries<-array(,ITER)
for (i in 1:ITER) {sumchildresidtries[i]<-sum(childresidtries[,i])}
childparam1triesnew<-runif(ITER,min(childparam1tries[match(head(sort(sumchildresidtries),ITER*BEST),sumchildresidtries)]), max(childparam1tries[match(head(sort(sumchildresidtries),ITER*BEST),sumchildresidtries)]))
childparam2triesnew<-runif(ITER,min(childparam2tries[match(head(sort(sumchildresidtries),ITER*BEST),sumchildresidtries)]), max(childparam2tries[match(head(sort(sumchildresidtries),ITER*BEST),sumchildresidtries)]))
step3tries<-array(step1-step2,dim=c(length(step1),ITER))
for (i in 1:ITER) {step3tries[1:SIZE,i]<-childparam1triesnew[i]*exp(-childparam2triesnew[i]*(meanages[]))}
childresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=childmin)&(meanages[j]<=childmax)) {childresidtries[j,i]<-(step3tries[j,i]-(step1-step2)[j])^2}}}
sumchildresidtries<-array(,ITER)
for (i in 1:ITER) {sumchildresidtries[i]<-sum(childresidtries[,i])}
childparam1triesnew[which(sumchildresidtries==min(sumchildresidtries))]
childparam2triesnew[which(sumchildresidtries==min(sumchildresidtries))]
sumchildresidtries[which(sumchildresidtries==min(sumchildresidtries))]
step3<-step2+step3tries[,which(sumchildresidtries==min(sumchildresidtries))]

##STEP 4 FIT - SELECT BEST PERCENT PARAMETER VALUES OF ITER BASED ON INPUT DISTRIBUTIONS, THEN REPEAT ITER WITH THE UNIFORM BOUNDS OF BEST PERCENT AND SELECT BEST PARAMETER VALUES  
step4tries<-array(step1-step3,dim=c(length(step1),ITER))
for (i in 1:ITER) {step4tries[1:SIZE,i]<-labparam1tries[i]*exp(-labparam2tries[i]*(meanages[]-labparam3tries[i])-exp(-labparam4tries[i]*(meanages[]-labparam3tries[i])))}
labresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=labormin)&(meanages[j]<=labormax)&((meanages[j]<min(studentages))|(meanages[j]>max(studentages)))) {labresidtries[j,i]<-(step4tries[j,i]-(step1-step3)[j])^2}}}
sumlabresidtries<-array(,ITER)
for (i in 1:ITER) {sumlabresidtries[i]<-sum(labresidtries[,i])}
labparam1triesnew<-runif(ITER,min(labparam1tries[match(head(sort(sumlabresidtries),ITER*BEST),sumlabresidtries)]), max(labparam1tries[match(head(sort(sumlabresidtries),ITER*BEST),sumlabresidtries)]))
labparam2triesnew<-runif(ITER,min(labparam2tries[match(head(sort(sumlabresidtries),ITER*BEST),sumlabresidtries)]), max(labparam2tries[match(head(sort(sumlabresidtries),ITER*BEST),sumlabresidtries)]))
labparam3triesnew<-runif(ITER,min(labparam3tries[match(head(sort(sumlabresidtries),ITER*BEST),sumlabresidtries)]), max(labparam3tries[match(head(sort(sumlabresidtries),ITER*BEST),sumlabresidtries)]))
labparam4triesnew<-runif(ITER,min(labparam4tries[match(head(sort(sumlabresidtries),ITER*BEST),sumlabresidtries)]), max(labparam4tries[match(head(sort(sumlabresidtries),ITER*BEST),sumlabresidtries)]))
step4tries<-array(step1-step3,dim=c(length(step1),ITER))
for (i in 1:ITER) {step4tries[1:SIZE,i]<-labparam1triesnew[i]*exp(-labparam2triesnew[i]*(meanages[]-labparam3triesnew[i])-exp(-labparam4triesnew[i]*(meanages[]-labparam3triesnew[i])))}
labresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=labormin)&(meanages[j]<=labormax)&((meanages[j]<min(studentages))|(meanages[j]>max(studentages)))) {labresidtries[j,i]<-(step4tries[j,i]-(step1-step3)[j])^2}}}
sumlabresidtries<-array(,ITER)
for (i in 1:ITER) {sumlabresidtries[i]<-sum(labresidtries[,i])}
labparam1triesnew[which(sumlabresidtries==min(sumlabresidtries))]
labparam2triesnew[which(sumlabresidtries==min(sumlabresidtries))]
labparam3triesnew[which(sumlabresidtries==min(sumlabresidtries))]
labparam4triesnew[which(sumlabresidtries==min(sumlabresidtries))]
sumlabresidtries[which(sumlabresidtries==min(sumlabresidtries))]
step4<-step3+step4tries[,which(sumlabresidtries==min(sumlabresidtries))]

##STEP 5 FIT - SELECT BEST PERCENT PARAMETER VALUES OF ITER BASED ON INPUT DISTRIBUTIONS, THEN REPEAT ITER WITH THE UNIFORM BOUNDS OF BEST PERCENT AND SELECT BEST PARAMETER VALUES  
step5tries<-array(step1-step4,dim=c(length(step1),ITER))
for (i in 1:ITER) {step5tries[1:SIZE,i]<-retparam1tries[i]*exp(-((meanages[]-retparam3tries[i])/retparam2tries[i])*((meanages[]-retparam3tries[i])/retparam2tries[i]))}
retresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=retmin)&(meanages[j]<=retmax)) {retresidtries[j,i]<-(step5tries[j,i]-(step1-step4)[j])^2}}}
sumretresidtries<-array(,ITER)
for (i in 1:ITER) {sumretresidtries[i]<-sum(retresidtries[,i])}
retparam1triesnew<-runif(ITER,min(retparam1tries[match(head(sort(sumretresidtries),ITER*BEST),sumretresidtries)]), max(retparam1tries[match(head(sort(sumretresidtries),ITER*BEST),sumretresidtries)]))
retparam2triesnew<-runif(ITER,min(retparam2tries[match(head(sort(sumretresidtries),ITER*BEST),sumretresidtries)]), max(retparam2tries[match(head(sort(sumretresidtries),ITER*BEST),sumretresidtries)]))
retparam3triesnew<-runif(ITER,min(retparam3tries[match(head(sort(sumretresidtries),ITER*BEST),sumretresidtries)]), max(retparam3tries[match(head(sort(sumretresidtries),ITER*BEST),sumretresidtries)]))
step5tries<-array(step1-step4,dim=c(length(step1),ITER))
for (i in 1:ITER) {step5tries[1:SIZE,i]<-retparam1triesnew[i]*exp(-((meanages[]-retparam3triesnew[i])/retparam2triesnew[i])*((meanages[]-retparam3triesnew[i])/retparam2triesnew[i]))}
retresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=retmin)&(meanages[j]<=retmax)) {retresidtries[j,i]<-(step5tries[j,i]-(step1-step4)[j])^2}}}
sumretresidtries<-array(,ITER)
for (i in 1:ITER) {sumretresidtries[i]<-sum(retresidtries[,i])}
retparam1triesnew[which(sumretresidtries==min(sumretresidtries))]
retparam2triesnew[which(sumretresidtries==min(sumretresidtries))]
retparam3triesnew[which(sumretresidtries==min(sumretresidtries))]
sumretresidtries[which(sumretresidtries==min(sumretresidtries))]
step5<-step4+step5tries[,which(sumretresidtries==min(sumretresidtries))]

##STEP 6 FIT - SELECT BEST PERCENT PARAMETER VALUES OF ITER BASED ON INPUT DISTRIBUTIONS, THEN REPEAT ITER WITH THE UNIFORM BOUNDS OF BEST PERCENT AND SELECT BEST PARAMETER VALUES  
step6tries<-array(step1-step5,dim=c(length(step1),ITER))
for (i in 1:ITER) {step6tries[1:SIZE,i]<-eldparam1tries[i]*exp(eldparam2tries[i]*meanages[])}
eldresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=eldmin)&(meanages[j]<=eldmax)) {eldresidtries[j,i]<-(step6tries[j,i]-(step1-step5)[j])^2}}}
sumeldresidtries<-array(,ITER)
for (i in 1:ITER) {sumeldresidtries[i]<-sum(eldresidtries[,i])}
eldparam1triesnew<-runif(ITER,min(eldparam1tries[match(head(sort(sumeldresidtries),ITER*BEST),sumeldresidtries)]), max(eldparam1tries[match(head(sort(sumeldresidtries),ITER*BEST),sumeldresidtries)]))
eldparam2triesnew<-runif(ITER,min(eldparam2tries[match(head(sort(sumeldresidtries),ITER*BEST),sumeldresidtries)]), max(eldparam2tries[match(head(sort(sumeldresidtries),ITER*BEST),sumeldresidtries)]))
step6tries<-array(step1-step5,dim=c(length(step1),ITER))
for (i in 1:ITER) {step6tries[1:SIZE,i]<-eldparam1triesnew[i]*exp(eldparam2triesnew[i]*meanages[])}
eldresidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=eldmin)&(meanages[j]<=eldmax)) {eldresidtries[j,i]<-(step6tries[j,i]-(step1-step5)[j])^2}}}
sumeldresidtries<-array(,ITER)
for (i in 1:ITER) {sumeldresidtries[i]<-sum(eldresidtries[,i])}
eldparam1triesnew[which(sumeldresidtries==min(sumeldresidtries))]
eldparam2triesnew[which(sumeldresidtries==min(sumeldresidtries))]
sumeldresidtries[which(sumeldresidtries==min(sumeldresidtries))]
step6<-step5+step6tries[,which(sumeldresidtries==min(sumeldresidtries))]

##STEP 7 FIT - SELECT BEST PERCENT PARAMETER VALUES OF ITER BASED ON INPUT DISTRIBUTIONS, THEN REPEAT ITER WITH THE UNIFORM BOUNDS OF BEST PERCENT AND SELECT BEST PARAMETER VALUES  
step7tries<-array(step1-step6,dim=c(length(step1),ITER))
for (i in 1:ITER) {step7tries[1:SIZE,i]<-stuparam1tries[i]*exp(-stuparam2tries[i]*(meanages[]-stuparam3tries[i])-exp(-stuparam4tries[i]*(meanages[]-stuparam3tries[i])))}
sturesidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=stumin)&(meanages[j]<=stumax)) {sturesidtries[j,i]<-(step7tries[j,i]-(step1-step6)[j])^2}}}
sumsturesidtries<-array(,ITER)
for (i in 1:ITER) {sumsturesidtries[i]<-sum(sturesidtries[,i])}
stuparam1triesnew<-runif(ITER,min(stuparam1tries[match(head(sort(sumsturesidtries),ITER*BEST),sumsturesidtries)]), max(stuparam1tries[match(head(sort(sumsturesidtries),ITER*BEST),sumsturesidtries)]))
stuparam2triesnew<-runif(ITER,min(stuparam2tries[match(head(sort(sumsturesidtries),ITER*BEST),sumsturesidtries)]), max(stuparam2tries[match(head(sort(sumsturesidtries),ITER*BEST),sumsturesidtries)]))
stuparam3triesnew<-runif(ITER,min(stuparam3tries[match(head(sort(sumsturesidtries),ITER*BEST),sumsturesidtries)]), max(stuparam3tries[match(head(sort(sumsturesidtries),ITER*BEST),sumsturesidtries)]))
stuparam4triesnew<-runif(ITER,min(stuparam4tries[match(head(sort(sumsturesidtries),ITER*BEST),sumsturesidtries)]), max(stuparam4tries[match(head(sort(sumsturesidtries),ITER*BEST),sumsturesidtries)]))
step7tries<-array(step1-step6,dim=c(length(step1),ITER))
for (i in 1:ITER) {step7tries[1:SIZE,i]<-stuparam1triesnew[i]*exp(-stuparam2triesnew[i]*(meanages[]-stuparam3triesnew[i])-exp(-stuparam4triesnew[i]*(meanages[]-stuparam3triesnew[i])))}
sturesidtries<-array(0,dim=c(length(step1),ITER))
for (i in 1:ITER) {for (j in 1:length(meanages)) {if((meanages[j]>=stumin)&(meanages[j]<=stumax)) {sturesidtries[j,i]<-(step7tries[j,i]-(step1-step6)[j])^2}}}
sumsturesidtries<-array(,ITER)
for (i in 1:ITER) {sumsturesidtries[i]<-sum(sturesidtries[,i])}
stuparam1triesnew[which(sumsturesidtries==min(sumsturesidtries))]
stuparam2triesnew[which(sumsturesidtries==min(sumsturesidtries))]
stuparam3triesnew[which(sumsturesidtries==min(sumsturesidtries))]
stuparam4triesnew[which(sumsturesidtries==min(sumsturesidtries))]
sumsturesidtries[which(sumsturesidtries==min(sumsturesidtries))]
step7<-step6+step7tries[,which(sumsturesidtries==min(sumsturesidtries))]

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


