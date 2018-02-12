# Classification-Audit-Tracker
Classification Audit Tracker


#/********************************************************************************/#
#/*     Objective : Classification Audit Tracker                                  /#                           */                           */
#/*                                                                               /#
#/*     Prepared By : Rajendra Mehta                                              /#
#/*                                                                               /#  
#/********************************************************************************/#

rm(list=(ls()))

w <- gwindow("C
             lassification Audit Tracker",visible=T)
g <- ggroup(horizontal = F, container = w)

ga <- gframe("SELECT FILE/FOLDER PATH",pos=0.5,horizontal = F, container = g)

browse.file1 <- gfilebrowse("SELECT MASTER File FILE",container = ga,quote=F)

b1<-glabel("Month:                                                                                           ",cont=gb,anchor=c(-1,0))
b11<-gedit(cont=gb)

library(ff)
library(ffbase)
library(foreign)
library(plyr)
library(data.table)
library(gtools)
library(ggplot2)
library(plotly)

Master_File<-read.csv("C:/Users/rajemeht/Desktop/GPS/Project_3/Input/Master_File_Input.csv")
Pontoon_File<-read.csv("C:/Users/rajemeht/Desktop/GPS/Project_3/Input/Pontoon SOW_Quality_Audit_December_2017_Results.csv",header = T)
Header_File<-read.csv("C:/Users/rajemeht/Desktop/GPS/Project_3/Input/Headers_File.csv")

#Appending File
Master_File_12<-Pontoon_File
Master_File_2<-rbindlist(list(Header_File,Pontoon_File),fill=TRUE)
#Avik's Use smartbind to do it ggplot2 and plotly
#Master_File_3<-smartbind(Header_File,Pontoon_File,fill = NA,sep=':',verbose = FALSE)

##########################################################1
Master_File_1<-Master_File_2
#Master_File_1$MSP.SCOPE.Results..Applicable.to.MP...MS.<-ifelse(Master_File_1)
#V # MSP SCOPE Results (Applicable to MP & MS)

Master_File_1$Count_other_than_NO_V<-apply(Master_File_1[,24:28],1, function(x) {ifelse(any(x=="No"),
                                                              sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})
Master_File_1[,c(22)]<-ifelse(Master_File_1$Count_other_than_NO_V<5,'FAIL','PASS')


#AD #MSP WORK PRODUCT Results (Applicable to MS & MP)30-32+35
Master_File_1$Count_other_than_NO_V2<-apply(Master_File_1[,32:35],1, function(x) {ifelse(any(x=="No"),
                                                                                        sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})
Master_File_1[,c(30)]<-ifelse(Master_File_1$Count_other_than_NO_V2<4,'FAIL','PASS')

#AK #MSP WRITTEN REPORTS Results (Applicable to MP & MS)30-39+42
Master_File_1$Count_other_than_NO_V3<-apply(Master_File_1[,39:42],1, function(x) {ifelse(any(x=="No"),
                                                                                         sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})
Master_File_1[,c(30)]<-ifelse(Master_File_1$Count_other_than_NO_V3<4,'FAIL','PASS')

#AR #MSP PENALTIES & REMEDIES Results (Applicable to MS & MP)44-46+
Master_File_1$Count_other_than_NO_V4<-apply(Master_File_1[,46:50],1, function(x) {ifelse(any(x=="No"),
                                                                                         sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})
Master_File_1$Count_other_than_NO_V4_1<-apply(Master_File_1[,52:57],1, function(x) {ifelse(any(x=="No"),
                                                                                         sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})
Master_File_1$No_V4_2<-ifelse(Master_File_1$Count_other_than_NO_V4<5,1,0)
Master_File_1$No_V4_3<-ifelse(Master_File_1$Count_other_than_NO_V4_1<6,1,0)
Master_File_1$No_V4_4<-Master_File_1$No_V4_2+Master_File_1$No_V4_3
#Master_File_1[,c(44)]<-ifelse(Master_File_1$No_V4_4>0,'FAIL','PASS')
Master_File_1[,c(44)]<-ifelse(Master_File_1[,c(8)]=="IT" & Master_File_1[,c(7)]=="MP","N/A For IT SOWs",ifelse(Master_File_1$No_V4_4>0,'FAIL','PASS'))


#BG#MSP SUPPLIER KEY PERSONNEL Results (Applicable to MP & MS)
Master_File_1$Count_other_than_NO_V5<-apply(Master_File_1[,61:63],1, function(x) {ifelse(any(x=="No"),
                                                                                         sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})
Master_File_1[,c(59)]<-ifelse(Master_File_1$Count_other_than_NO_V5<3,'FAIL','PASS')

#BL#MSP SUPPLIER KEY PERSONNEL Results (Applicable to MP & MS)
Master_File_1$Count_other_than_NO_V6_NA<-apply(Master_File_1[,66:68],1, function(x) {ifelse(any(x=="N/A for MS"),
                                                                                         sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})
Master_File_1$Count_other_than_NO_V6_NO<-apply(Master_File_1[,66:68],1, function(x) {ifelse(any(x=="No"),
                                                                                         sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})
Master_File_1$V6_NA<-ifelse(Master_File_1$Count_other_than_NO_V6_NA<3,'N/A for MS Indicated',0)
Master_File_1$V6_NO<-ifelse(Master_File_1$Count_other_than_NO_V6_NO<3,'FAIL',0)
Master_File_1[,c(64)]<-ifelse(Master_File_1$V6_NA=='0' & Master_File_1$V6_NO =='0','PASS',ifelse(Master_File_1$Count_other_than_NO_V6_NA<3,'N/A for MS Indicated','FAIL'))
#P_Factor_2_10$COUNT_2<-ifelse(P_Factor_2_10$SALES==0 & P_Factor_2_10$ZERO_1!=1,P_Factor_2_10_1<-P_Factor_2_10,(P_Factor_2_10_1<-P_Factor_2_10[which(P_Factor_2_10$NO<13),]))

#############   Need to Put multiple condition               ##############
#L#MSP Classification Results (PASS/FAIL)

Master_File_1$V_7<-apply(Master_File_1[,c(22,30,59,64)],1, function(x) {ifelse(any(x=="FAIL"),
                                                                                         sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})
Master_File_1$V_7_1<-ifelse(Master_File_1$V_7<4,"FAIL","PASS")

Master_File_1$V_8<-apply(Master_File_1[,c(22,30,44,59,64)],1, function(x) {ifelse(any(x=="FAIL"),
                                                                               sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})
Master_File_1$V_8_1<-ifelse(Master_File_1$V_8<5,"FAIL","PASS")

Master_File_1[,c(12)]<-ifelse(Master_File_1[,c(8)]=="IT" & Master_File_1[,c(7)]=="MP",Master_File_1$V_7_1,
                              ifelse(Master_File_1[,c(8)]=="IT" & Master_File_1[,c(7)]=="MS",Master_File_1$V_8_1,
                                     ifelse(Master_File_1[,c(7)]=="MP",Master_File_1$V_7_1,Master_File_1$V_8_1)))
#DU to EW   ###MSP
Master_File_1[,c(125)]<-Master_File_1[,c(4)]
#DV
Master_File_1[,c(126)]<-ifelse(Master_File_1[,c(7)]=='MS',4,5)
#DW

Master_File_1$V11<-apply(Master_File_1[,c(24:28)],1, function(x) {ifelse(any(x=="Yes"),
                                                                                  sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})

Master_File_1[,c(127)]<-ifelse(Master_File_1$V11<5,Master_File_1$V11,0)

#DX
Master_File_1$V12<-apply(Master_File_1[,c(24:28)],1, function(x) {ifelse(any(x=="No"),
                                                                         sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})

Master_File_1[,c(128)]<-ifelse(Master_File_1$V12>0,5-(Master_File_1$V12),0)

####################################### start again
#DZ
Master_File_1$V13<-apply(Master_File_1[,c(32:35)],1, function(x) {ifelse(any(x=="Yes"),
                                                                         sum(length(x))-sum(length(any(x=="No" | x==" N/A for MS"))),sum(length(x)))})


Master_File_1$V13<-apply(Master_File_1[,c(32:35)],1, function(x) {ifelse(any(x=="Yes"),
                                                                         sum(length(x))-sum(length(any(x=="No"))),sum(length(x)))})

Master_File_1[,c(130)]<-ifelse(Master_File_1$V13<4,Master_File_1$V13,0)

Master_File_1$V81<-ifelse(Master_File_1[,c(32)]=='Yes',1,0)
Master_File_1$V82<-ifelse(Master_File_1[,c(33)]=='Yes',1,0)
Master_File_1$V83<-ifelse(Master_File_1[,c(34)]=='Yes',1,0)
Master_File_1$V84<-ifelse(Master_File_1[,c(35)]=='Yes',1,0)
Master_File_1$V86<-Master_File_1$V81+Master_File_1$V82+Master_File_1$V83+Master_File_1$V84
Master_File_1[,c(130)]<-ifelse(Master_File_1$V86>0,Master_File_1$V86,0)
#EA
Master_File_1[,c(131)]<-ifelse(Master_File_1$V16>0,Master_File_1$V16,0)
#EC
Master_File_1$V91<-ifelse(Master_File_1[,c(39)]=='Yes',1,0)
Master_File_1$V92<-ifelse(Master_File_1[,c(40)]=='Yes',1,0)
Master_File_1$V93<-ifelse(Master_File_1[,c(41)]=='Yes',1,0)
Master_File_1$V94<-ifelse(Master_File_1[,c(42)]=='Yes',1,0)
Master_File_1$V96<-Master_File_1$V91+Master_File_1$V92+Master_File_1$V93+Master_File_1$V94
Master_File_1[,c(133)]<-ifelse(Master_File_1$V96>0,Master_File_1$V96,0)
#Ed
Master_File_1[,c(134)]<-ifelse(Master_File_1$V26>0,Master_File_1$V26,0)
#EE#         Need to ask
Master_File_1[,c(135)]<-ifelse(Master_File_1[,c(7)]=='MS',4,5)
##EF
Master_File_1$V131<-ifelse(Master_File_1[,c(46)]=='Yes',1,0)
Master_File_1$V132<-ifelse(Master_File_1[,c(47)]=='Yes',1,0)
Master_File_1$V133<-ifelse(Master_File_1[,c(48)]=='Yes',1,0)
Master_File_1$V134<-ifelse(Master_File_1[,c(49)]=='Yes',1,0)
Master_File_1$V135<-ifelse(Master_File_1[,c(50)]=='Yes',1,0)
Master_File_1$V136<-ifelse(Master_File_1[,c(51)]=='Yes',1,0)
Master_File_1$V137<-ifelse(Master_File_1[,c(52)]=='Yes',1,0)
Master_File_1$V138<-ifelse(Master_File_1[,c(53)]=='Yes',1,0)
Master_File_1$V139<-ifelse(Master_File_1[,c(54)]=='Yes',1,0)
Master_File_1$V140<-ifelse(Master_File_1[,c(55)]=='Yes',1,0)
Master_File_1$V141<-ifelse(Master_File_1[,c(56)]=='Yes',1,0)
Master_File_1$V142<-Master_File_1$V131+Master_File_1$V132+Master_File_1$V133+Master_File_1$V134+Master_File_1$V135+Master_File_1$V136+Master_File_1$V137+Master_File_1$V138+Master_File_1$V139+Master_File_1$V140
Master_File_1[,c(136)]<-ifelse(Master_File_1$V142>0,Master_File_1$V142,0)
#EG
Master_File_1$V231<-ifelse(Master_File_1[,c(46)]=='No',1,0)
Master_File_1$V232<-ifelse(Master_File_1[,c(47)]=='No',1,0)
Master_File_1$V233<-ifelse(Master_File_1[,c(48)]=='No',1,0)
Master_File_1$V234<-ifelse(Master_File_1[,c(49)]=='No',1,0)
Master_File_1$V235<-ifelse(Master_File_1[,c(50)]=='No',1,0)
Master_File_1$V236<-ifelse(Master_File_1[,c(51)]=='No',1,0)
Master_File_1$V237<-ifelse(Master_File_1[,c(52)]=='No',1,0)
Master_File_1$V238<-ifelse(Master_File_1[,c(53)]=='No',1,0)
Master_File_1$V239<-ifelse(Master_File_1[,c(54)]=='No',1,0)
Master_File_1$V240<-ifelse(Master_File_1[,c(55)]=='No',1,0)
Master_File_1$V241<-ifelse(Master_File_1[,c(56)]=='No',1,0)
Master_File_1$V242<-Master_File_1$V231+Master_File_1$V232+Master_File_1$V233+Master_File_1$V234+Master_File_1$V235+Master_File_1$V236+Master_File_1$V237+Master_File_1$V238+Master_File_1$V239+Master_File_1$V240
Master_File_1[,c(137)]<-ifelse(Master_File_1$V242>0,Master_File_1$V242,0)
#EH
Master_File_1[,c(138)]<-ifelse(Master_File_1[,c(7)]=='MS',0,3)
#EI  # Weather we need for three column or four column
Master_File_1$X1<-ifelse(Master_File_1[,c(66)]=='Yes',1,0)
Master_File_1$X2<-ifelse(Master_File_1[,c(67)]=='Yes',1,0)
Master_File_1$X3<-ifelse(Master_File_1[,c(68)]=='Yes',1,0)
Master_File_1$X4<-Master_File_1$X1+Master_File_1$X2+Master_File_1$X3
MConfidence Intervalaster_File_1[,c(139)]<-ifelse(Master_File_1$X4>0,Master_File_1$X4,0)
#EJ
Master_File_1$X5<-Master_File_1$V51+Master_File_1$V52+Master_File_1$V53
Master_File_1[,c(140)]<-ifelse(Master_File_1$X5>0,Master_File_1$X5,0)
#EL-142=61,62,63
Master_File_1$X6<-ifelse(Master_File_1[,c(61)]=='Yes',1,0)
Master_File_1$X7<-ifelse(Master_File_1[,c(62)]=='Yes',1,0)
Master_File_1$X8<-ifelse(Master_File_1[,c(63)]=='Yes',1,0)
Master_File_1$X9<-Master_File_1$X6+Master_File_1$X7+Master_File_1$X8
Master_File_1[,c(142)]<-ifelse(Master_File_1$X9>0,Master_File_1$X9,0)
#EM-143=61,62,63-Master_File_1$V46
Master_File_1[,c(143)]<-ifelse(Master_File_1$V46>0,Master_File_1$V46,0)
##EN-144=126+129+132+135+138+141  #######3                      Check for NA
Master_File_1[,c(144)]<-Master_File_1[,c(126)]+Master_File_1[,c(129)]+Master_File_1[,c(132)]+Master_File_1[,c(135)]+Master_File_1[,c(138)]+Master_File_1[,c(141)]
##


 
########################################################################################################################################################################
#########                Output file                         #############
write.csv(Master_File_2,file="C:/Users/rajemeht/Desktop/GPS/Project_3//Output/Final_Output.csv",row.names=FALSE)

write.csv(Master_File_Final,file=paste0(OUTPUT_File,"\\","COMBINED_OUTPUT",".csv"),row.names=FALSE)

#Missing Value Imputation
#Outlier Detaction
#Hypothesis Testing
#NA replace by mean
#
