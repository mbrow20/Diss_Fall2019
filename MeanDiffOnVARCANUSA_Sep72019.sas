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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =ESCS,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.ESCScan);                                                                                         
                              RUN; 


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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =ST011Q06,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.ST011Q06can);                                                                                         
                              RUN; 
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =ST011Q07,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.ST011Q07can);                                                                                         
                              RUN; 
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =ST013Q01,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.ST013Q01can);                                                                                         
                              RUN; 
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =HISCED,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.HISCEDcan);                                                                                         
                              RUN; 
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =MISCED,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.MISCEDcan);                                                                                         
                              RUN; 
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =HOMEPOS,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.HOMEPOScan);                                                                                         
                              RUN;

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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =SC048Q01,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.SC048Q01can);                                                                                         
                              RUN; 
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =SC048Q02,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.SC048Q02can);                                                                                         
                              RUN;
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =SC048Q03,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.SC048Q03can);                                                                                         
                              RUN;
                                                                                        
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =CLSIZE,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.CLSIZEcan);                                                                                         
                              RUN;
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =SC061Q05,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.SC061Q05can);                                                                                         
                              RUN;
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =ST062Q01,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.ST062Q01can);                                                                                         
                              RUN;
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =BELONG,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.BELONGcan);                                                                                         
                              RUN;
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =ST034Q01,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.ST034Q01can);                                                                                         
                              RUN;
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =mmins,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.MMINScan);                                                                                         
                              RUN;
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =PROAT5AM,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.PROAT5AMcan);                                                                                         
                              RUN;
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =PROATCE,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.PROATCEcan);                                                                                         
                              RUN;
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
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =SC061Q07,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =Work.SC061Q07can);                                                                                         
                              RUN;
