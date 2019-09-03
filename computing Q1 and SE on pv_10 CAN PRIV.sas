libname PISANEW "C:\Users\mbrow20\Desktop\PISA2015\";
options nofmterr notes;
run;
data temp55;
set Work.Pisa2015_CAN;
if (cnt in ("CAN"));
W_FSTURWT0=W_FSTUWT;
math1=pv1math;
math2=pv2math;
math3=pv3math;
math4=pv4math;
math5=pv5math;
math6=pv6math;
math7=pv7math;
math8=pv8math;
math9=pv9math;
math10=pv10math;
keep cnt SC013Q01TA W_FSTURWT0-W_FSTURWT80 math1-math10;
run;
%include "c:\Users\mbrow20\Desktop\PISA2015\proc_means_pv_10_new.sas";
%macro repeat;
%do kk=1 %to 5;
%BRR_PROCMEAN_PV(INFILE=temp55,
REPLI_ROOT=W_FSTURWT,
PV_ROOT=math,

BYVAR=SC013Q01TA,
STAT=Q1,
LIMIT=NO,
OUTFILE=CAN_PUBPRIV_Q1);
run;
%end;
%mend;
%repeat;
run;
