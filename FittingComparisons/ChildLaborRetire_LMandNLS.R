##############################################################################################################################
##EDDIE'S R CODE FOR FITTING THE MODEL MIGRATION SCHEDULE WITH STUDENT PEAK - COMPARISON CODE
##
##FOR MORE INFO, SEE https://raw.githubusercontent.com/AppliedDemogToolbox/Hunsinger_SPMMSRCode/master/SPMMSRCode.R
##via https://applieddemogtoolbox.github.io/Toolbox/#SPMMSRCode
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
SPMMSTestingData<-read.table(file="https://github.com/AppliedDemogToolbox/Hunsinger_SPMMSRCode/raw/master/SPMMSData.csv",header=TRUE,sep=",")
migprob<-(SPMMSTestingData$Migration.probability[1:90])

#SIZE OF migprob (DATA BY AGE) USED
SIZE<-90

#NUMBER OF ITERATIONS - USED FOR FITTING
ITER<-10000

#PROPORTION TO REPEAT DISTRIBUTION BOUND SELECTION WITH
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
###############

###############
##STEP 4 INPUTS
#MIN AND MAX OF LABOR FORCE AGES TO FIT OVER
labormin<-17
labormax<-45

#STUDENT AGES TO EXCLUDE - CURRENTLY MUST BE ADJACENT AGES - TO EXCLUDE STUDENT PEAK FROM MODEL CAN SET AS JUST '0'
studentages<-c(18,19)#studentages<-c(0)

#HEIGHT OF THE LABOR FORCE CURVE
labparam1tries<-.05

#RATE OF DESCENT OF THE LABOR FORCE CURVE
labparam2tries<-.08

#POSITION OF THE LABOR FORCE CURVE ON THE AGE-AXIS
labparam3tries<-22

#RATE OF ASCENT OF THE LABOR FORCE CURVE
labparam4tries<-.26
###############

###############
##STEP 5 INPUTS
#MIN AND MAX OF RETIREMENT AGES TO FIT OVER
retmin<-50
retmax<-75

#HEIGHT OF RETIREMENT CURVE
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET AS '0'
retparam1tries<-.002 #retparam1tries<-0

#RATE OF DESCENT OF RETIREMENT CURVE
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET AS '0'
retparam2tries<-7 #retparam2tries<-0

#POSITION OF THE RETIREMENT CURVE ON THE AGE-AXIS
#TO APPROXIMATELY EXCLUDE RETIREMENT CURVE FROM MODEL CAN SET AS '55'
retparam3tries<-61 #retparam1tries<-55
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
ages<-c(0:(length(step1)-1))
meanages<-c(0+1:length(step1))

##STEP 3 FIT - SLOPE AND INTERCEPT FOR TRANSFORMATION
#THIS IS R's lm() FUNCTION FIT
step3<-array(,childmax-childmin)
for (i in 1:length(step3)) {step3[i]<-log(step1[i]-step2[i])}
meanchildmin<-childmin+1
childages<-c(childmin+1:childmax)
childfit<-lm(step3~childages)
childfit
for (i in 1:length(ages)) {step3[i]<-exp(childfit$coefficients[1])*exp(-(-childfit$coefficients[2])*meanages[i])}
step3<-step2+step3

##STEP 4 FIT - USING nls()  
step4inputdata<-step1[c(labormin:(studentages[1]-1),(studentages[2]+1):labormax)]-step3[c(labormin:(studentages[1]-1),(studentages[2]+1):labormax)]
stepmeanages<-c(meanages[labormin:(studentages[1]-1)],meanages[(studentages[2]+1):labormax])
laborfit<-nls(step4inputdata~labparam1tries*exp(-labparam2tries*(stepmeanages[]-labparam3tries)-exp(-labparam4tries*(stepmeanages[]-labparam3tries))),
start=list(labparam1tries=labparam1tries,labparam2tries=labparam2tries,labparam3tries=labparam3tries,labparam4tries=labparam4tries))
labparam1fit<-coef(laborfit)[1]
labparam2fit<-coef(laborfit)[2]
labparam3fit<-coef(laborfit)[3]
labparam4fit<-coef(laborfit)[4]
labparam1fit
labparam2fit
labparam3fit
labparam4fit
step4fit<-array(step1-step3,dim=c(length(step1)))
step4fit[]<-labparam1fit*exp(-labparam2fit*(meanages[]-labparam3fit)-exp(-labparam4fit*(meanages[]-labparam3fit)))
step4<-step3+step4fit

##STEP 5 FIT - USING nls()
step5inputdata<-step1[retmin:retmax]-step4[retmin:retmax]
stepmeanages<-meanages[retmin:retmax]
retfit<-nls(step5inputdata~retparam1tries*exp(-((stepmeanages[]-retparam3tries)/retparam2tries)*((stepmeanages[]-retparam3tries)/retparam2tries)),
start=list(retparam1tries=retparam1tries,retparam2tries=retparam2tries,retparam3tries=retparam3tries))
retparam1fit<-coef(retfit)[1]
retparam2fit<-coef(retfit)[2]
retparam3fit<-coef(retfit)[3]
retparam1fit
retparam2fit
retparam3fit
step5fit<-array(step1-step4,dim=c(length(step1)))
step5fit[]<-retparam1fit*exp(-((meanages[]-retparam3fit)/retparam2fit)*((meanages[]-retparam3fit)/retparam2fit))
step5<-step4+step5fit

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
