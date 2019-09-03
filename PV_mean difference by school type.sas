data temp3;
set pisa2003.stud;
if (cnt=“DEU”);
mcomb1=pv1math;
mcomb2=pv2math;
mcomb3=pv3math;
mcomb4=pv4math;
mcomb5=pv5math;
w_fstr0=w_fstuwt;
keep cnt schoolid stidstd bsmj st03q01
mcomb1-mcomb5 w_fstr0-w_fstr80;
run;
%include “c:\pisa\macro\proc_dif_pv.sas”;
%BRR_PROCMEAN_DIF_PV(INFILE=temp3,
REPLI_ROOT=w_fstr,
BYVAR=cnt,
PV_ROOT=mcomb,
COMPARE=st03q01,
CATEGORY=1 2,
STAT=mean,
OUTFILE=exercise4);
run;
