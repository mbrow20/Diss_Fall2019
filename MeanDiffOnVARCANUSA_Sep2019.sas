PROC IMPORT OUT= WORK.PISA2015 
            DATAFILE= "C:\Users\mbrow20\Desktop\PISA2015\STUDSCH_MERGED_
2015USACAN.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
data CompleteCases;
   set Work.Pisa2015;
   if nmiss(SC013Q01TA,st034q01TA)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ESCS SC013Q01TA ST034Q01TA W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS SC013Q01TA ST034Q01TA W_FSTURWT0-W_FSTURWT80;
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
                    OUTFILE =ESCS);                                                                                         
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
   if nmiss(SC013Q01TA,ST011Q07TA)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ST011Q07TA ESCS SC013Q01TA ST034Q01TA W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS ST011Q07TA SC013Q01TA ST034Q01TA W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =ST011Q07TA,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =ST011Q07);                                                                                         
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
   if nmiss(SC013Q01TA,ST011Q07TA)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ST011Q07TA ESCS SC013Q01TA ST034Q01TA W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS ST011Q07TA SC013Q01TA ST034Q01TA W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =ST011Q07TA,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =ST011Q07);                                                                                         
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
   if nmiss(SC013Q01TA,ST011Q07TA)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ST011Q07TA ESCS SC013Q01TA ST034Q01TA W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS ST011Q07TA SC013Q01TA ST034Q01TA W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =ST011Q07TA,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =ST011Q07);                                                                                         
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
   if nmiss(SC013Q01TA,ST004D01T)=0;       /* specify list of numeric vars */   /* output complete cases for all numeric vars */
run;
data CAN2015_nomiss;
set CompleteCases;
if cnt="CAN";
run;
%include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";                                                                               
data temp55;                                                                                                                            
set Work.CAN2015_nomiss;                                                                                                                      
W_FSTURWT0=W_FSTUWT;                                                                             
keep cnt ST004D01T ESCS SC013Q01TA ST034Q01TA W_FSTURWT1-W_FSTURWT80 W_FSTURWT0;                                                                                                                 
  data temp1;
   retain cnt ESCS ST004D01T SC013Q01TA ST034Q01TA W_FSTURWT0-W_FSTURWT80;
   set temp55;
   if SC013Q01TA=9 then delete;
  run; 
  %include "c:\Users\mbrow20\Desktop\proc_means_no_pv.sas";  
%BRR_PROCMEAN(INFILE =temp1,                                                                                                          
                    REPLI_ROOT =W_FSTURWT,                                                                                                 
                    BYVAR =SC013Q01TA,                                                                                              
                    VAR =ST004D01T,                                                                                                    
                    STAT =mean,                                                                                                         
                                 LIMIT=no,                                                                                              
                                 LIMIT_CRITERIA=,                                                                                       
                                 ID_SCHOOL=,                                                                                            
                    OUTFILE =ST004D01T);                                                                                         
                              RUN;  
