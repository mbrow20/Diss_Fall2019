
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
*/RUN proc freq data=Work.Pisa2015
tables VAR
run; */;
data CompleteCases;
   set Work.Pisa2015;
   if ESCS>4 then ESCS=.;
   if nmiss(SC013Q01TA,ESCS)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA  W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA SC048Q01 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=ESCS,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.ESCSDiffCan);
				  run;
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename ST011Q06TA=ST011Q06;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if ST011Q06>3 then ST011Q06=.;
   if nmiss(SC013Q01TA,ST011Q06)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA ST011Q06 W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA ST011Q06 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=ST011Q06,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.ST011Q06DiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename ST011Q07TA=ST011Q07;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if ST011Q07>3 then ST011Q07=.;
   if nmiss(SC013Q01TA,ST011Q07)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA ST011Q07 W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA ST011Q07 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
%include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=ST011Q07,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.ST011Q07DiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename ST013Q01TA=ST013Q01;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if ST013Q01>7 then ST013Q01=.;
   if nmiss(SC013Q01TA,ST013Q01)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA ST013Q01 W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA ST013Q01 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
%include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=ST013Q01,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.ST013Q01DiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if HISCED>7 then HISCED=.;
   if nmiss(SC013Q01TA,HISCED)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA HISCED W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA HISCED W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=HISCED,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.HISCEDDiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if MISCED>7 then MISCED=.;
   if nmiss(SC013Q01TA,MISCED)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA MISCED W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA MISCED W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=MISCED,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.MISCEDDiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if HOMEPOS>7 then HOMEPOS=.;
   if nmiss(SC013Q01TA,HOMEPOS)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA HOMEPOS W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA HOMEPOS W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=HOMEPOS,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.HOMEPOSDiffCan);
				  run; 

PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename SC048Q01NA=SC048Q01;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if SC048Q01>100 then SC048Q01=.;
   if nmiss(SC013Q01TA,SC048Q01)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA SC048Q01 W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA SC048Q01 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=SCO48Q01,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.SC048Q01DiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename SC048Q02NA=SC048Q02;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if SC048Q02>100 then SC048Q02=.;
   if nmiss(SC013Q01TA,SC048Q02)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA SC048Q02 W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA SC048Q02 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=SC048Q02,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.SC048Q02DiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename SC048Q03NA=SC048Q03;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if SC048Q03>100 then SC048Q03=.;
   if nmiss(SC013Q01TA,SC048Q03)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA SC048Q03 W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA SC048Q03 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=SC048Q03,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.SC048Q03DiffCan);
				  run; 
                                                                                        
  */RANGE OF CLSIZE IS 13-53*/;                          

PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data CompleteCases;
   set Work.Pisa2015;
   if CLSIZE>54 then CLSIZE=.;
   if nmiss(SC013Q01TA,CLSIZE)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA CLSIZE W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA CLSIZE W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=CLSIZE,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.CLSIZEDiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename SC061Q05TA=SC061Q05;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if SC061Q05>5 then SC061Q05=.;
   if nmiss(SC013Q01TA,SC061Q05)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA SC061Q05 W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA SC061Q05 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
 %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=SC061Q05,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.SC061Q05DiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename ST062Q01TA=ST062Q01;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if ST062Q01>8 then ST062Q01=.;
   if nmiss(SC013Q01TA,ST062Q01)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA ST062Q01 W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA ST062Q01 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=ST062Q01,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.ST062Q01DiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if BELONG>5 then BELONG=.;
   if nmiss(SC013Q01TA,BELONG)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA BELONG W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA BELONG W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=BELONG,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.BELONGDiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename ST034Q01TA=ST034Q01;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if ST034Q01>8 then ST034Q01=.;
   if nmiss(SC013Q01TA,ST034Q01)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA ST034Q01 W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA ST034Q01 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=ST034Q01,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.ST034Q01DiffCan);
				  run; 
 PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename ST062Q01TA=ST062Q01;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if mmins>3000 then mmins=.;
   if nmiss(SC013Q01TA,mmins)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA mmins W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA mmins W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=mmins,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.MMINSDiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data CompleteCases;
   set Work.Pisa2015;
   if PROAT5AM>2 then PROAT5AM=.;
   if nmiss(SC013Q01TA,PROAT5AM)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA PROAT5AM W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA PROAT5AM W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=PROAT5AM,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.PROAT5AMDiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data CompleteCases;
   set Work.Pisa2015;
   if PROATCE>2 then PROATCE=.;
   if nmiss(SC013Q01TA,PROATCE)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA PROATCE W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA PROATCE W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=PROATCE,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.PROATCEDiffCan);
				  run; 
PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data PISA2015;
rename SC061Q07TA=SC061Q07;
set PISA2015;
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if SC061Q07>5 then SC061Q07=.;
   if nmiss(SC013Q01TA,SC061Q07)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA SC061Q07 W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA SC061Q07 W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=SC061Q07,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=Work.SC061Q07DiffCan);
				  run; 
