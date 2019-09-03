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
  %include "c:\Users\mbrow20\Desktop\proc_dif_no_pv.sas"; 
%BRR_PROCMEAN_DIF(infile=temp1,
                  REPLI_ROOT=w_fsturwt,
				  VAR=ST004D01T,
				  COMPARE=SC013Q01TA,
				  CATEGORY=1 2,
				  STAT=mean,
				  OUTFILE=ST004D01TDIF);
