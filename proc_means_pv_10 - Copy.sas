
data temp114;
set Work.Pisa2015;
if (cnt="CAN") THEN country=1;
if (cnt="USA") THEN country=0;
w_fsturwt0=w_fstuwt;
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

keep cntschid cntstuid math1-math10 w_fsturwt0-w_fsturwt80
cnt country;
run;


%MACRO BRR_PROCMEAN_PV(INFILE =temp114,
                       REPLI_ROOT =w_fsturwt,
                       BYVAR = cnt ,
                       PV_ROOT =math,
                       STAT =mean,
			   		   LIMIT=no,
			   		   LIMIT_CRITERIA=,
			   		   ID_SCHOOL=,
                       OUTFILE =PISANEW.can_us_pv_comp);
RUN;
/* 

MEANING OF THE MACRO ARGUMENTS

INFILE			= 	INPUT DATA FILE.
REPLI_ROOT		= 	ROOT OF THE FINAL WEIGHT AND 80 REPLICATES VARIABLE NAMES. 
					FINAL WEIGHT VARIABLE NAME MUST BE THE REPLICATION ROOT FOLLOWED BY 0.
BYVAR			= 	BREAKDOWN VARIABLES
PV_ROOT			= 	ROOT OF THE 5 PLAUSIBLE VALUES VARIABLES NAMES
STAT			= 	REQUESTED STATISTIQUE
						SUMWGT		=	SUM OF THE WEIGHT
						MEAN		= 	MEAN
						VAR			=	VARIANCE
						STD			=	STANDARD DEVIATION
						CV			=	COEFFICIENT OF VARIATION
						MEDIAN		=	MEDIAN
						Q1			=	FIRST QUARTILE
						Q3			=	THIRD QUARTILE
						QRANGE		=	RANGE BETWEEN Q1 AND Q3
						PX			=	PERCENTILE, WITH X BETWEEN 1 AND 99
LIMIT          	=    FLAGGING  YES OR NO
LIMIT_CRITERIA 	=   1) NUMBER OF STUDENTS 2) NUMBER OF SCHOOLS 3) PERCENTAGE OF STUDENTS AND 4) NUMBER OF VARIABLES FROM 
					THE BYVAR ARGUMENT FOR DEFINING THE POPULATION OF REFERENCE 
ID_SCHOOL	   	=	VARIABLE NAME FOR THE SCHOOL IDENTIFICATION

OUTFILE			= 	FILE WITH THE STATISTIC ESTIMATES AND THEIR STANDARD ERROR ESTIMATE

*/ 

OPTIONS NONOTES;

PROC DATASETS LIBRARY=WORK NOLIST;
	DELETE BRR_TEMP1;
RUN;

PROC SORT DATA=&INFILE
	OUT=BRRDATA(KEEP=&REPLI_ROOT.0-&REPLI_ROOT.80 &BYVAR &PV_ROOT.1-&PV_ROOT.10 &ID_SCHOOL);
  	BY &BYVAR;
RUN;

%DO I = 0 %TO 80;
 	PROC MEANS DATA=BRRDATA VARDEF=WGT NOPRINT;
         VAR &PV_ROOT.1 - &PV_ROOT.10 ;
    	 BY &BYVAR;
    	 WEIGHT &REPLI_ROOT&I;
    	 OUTPUT OUT=MEAN_TEMP &STAT=PV1 - PV10;
 	RUN;

 	DATA MEAN_TEMP;
   		SET MEAN_TEMP;
   		L=&I;
 	RUN;

 	PROC APPEND BASE = BRR_TEMP1 DATA=MEAN_TEMP;
 	RUN;

%END;

DATA BRR_TEMP2(DROP=STAT FIN1-FIN10 MESVAR) BRR_TEMP3(KEEP=&BYVAR STAT FIN1-FIN10 MESVAR);
 	SET BRR_TEMP1;
    IF L > 0 THEN OUTPUT BRR_TEMP2;     
    ELSE DO;
       	STAT =(PV1+PV2+PV3+PV4+PV5+PV6+PV7+PV8+PV9+PV10)/10;
	   	FIN1=PV1;
	   	FIN2=PV2;
	   	FIN3=PV3;
	   	FIN4=PV4;
	   	FIN5=PV5;
		FIN6=PV6;
		FIN7=PV7;
		FIN8=PV8;
		FIN9=PV9;
		FIN10=PV10;
       	MESVAR=(((STAT-FIN1)**2)+((STAT-FIN2)**2)+((STAT-FIN3)**2)+((STAT-FIN4)**2)+((STAT-FIN5)**2)+((STAT-FIN6)**2)+((STAT-FIN7)**2)+((STAT-FIN8)**2)+((STAT-FIN9)**2)+((STAT-FIN10)**2))/9;
      	OUTPUT BRR_TEMP3;
   	END;
RUN;

PROC SORT DATA=BRR_TEMP2;
 	BY &BYVAR;
RUN;

PROC SORT DATA=BRR_TEMP3;
 	BY &BYVAR;
RUN;

DATA BRR_TEMP4;
	MERGE BRR_TEMP2 BRR_TEMP3;
	BY &BYVAR;
	ARRAY A (10)
		PV1-PV10;
	ARRAY B (10)
		FIN1-FIN10;
	ARRAY C(10)
		VAR1-VAR10;
	DO I=1 TO 10;
	C(I)=(1/20)*((A(I)-B(I))**2);
	END;
RUN;

PROC UNIVARIATE DATA=BRR_TEMP4 NOPRINT;
	VAR VAR1 VAR2 VAR3 VAR4 VAR5 VAR6 VAR7 VAR8 VAR9 VAR10;
	BY &BYVAR;
	OUTPUT OUT=BRR_TEMP5 SUM=SS1 SS2 SS3 SS4 SS5 SS6 SS7 SS8 SS9 SS10;
RUN;

DATA BRR_TEMP6;
	MERGE BRR_TEMP3 BRR_TEMP5;
	BY &BYVAR;
	SAMP=(SS1+SS2+SS3+SS4+SS5+SS6+SS7+SS8+SS9+SS10)/10;
	FINVAR=(SAMP+(1.2*MESVAR));
	SESTAT=(FINVAR)**0.5;
	FORMAT STAT F10.1;
	FORMAT SESTAT F10.2;
	sestat2=(ss1+(1.2*MESVAR))**0.5;
	KEEP &BYVAR STAT SESTAT sestat2 ;
RUN;

%IF (%UPCASE(&LIMIT)=NO) %THEN %DO;

	DATA &OUTFILE;
		SET BRR_TEMP6;
	RUN;

%END;
%ELSE %DO;
	DATA BRR_TEMP7;
		SET BRRDATA;
		NB_MISS=0;
	RUN;
	PROC FREQ DATA=BRR_TEMP7 NOPRINT;
		TABLE NB_MISS /OUT=BRR_TEMP8;
		BY &BYVAR;
	RUN;
	%LET FLAG_STUD=%SCAN(&LIMIT_CRITERIA,1);
	DATA BRR_TEMP8;
		SET BRR_TEMP8;
		IF (COUNT < &FLAG_STUD) THEN FLAG_STUD=1;
		ELSE FLAG_STUD=0;
		KEEP &BYVAR FLAG_STUD;
	RUN;
	PROC SORT DATA=BRR_TEMP7;
		BY &BYVAR &ID_SCHOOL;
	RUN;
	PROC FREQ DATA=BRR_TEMP7 NOPRINT;
		TABLE NB_MISS /OUT=BRR_TEMP9;
		BY &BYVAR &ID_SCHOOL;
	RUN;		
	PROC FREQ DATA=BRR_TEMP9 NOPRINT;
		TABLE NB_MISS /OUT=BRR_TEMP10;
		BY &BYVAR;
	RUN;
	%LET FLAG_SCH=%SCAN(&LIMIT_CRITERIA,2);
	DATA BRR_TEMP10;
		SET BRR_TEMP10;
		IF (COUNT < &FLAG_SCH) THEN FLAG_SCH=1;
		ELSE FLAG_SCH=0;
		KEEP &BYVAR FLAG_SCH;
	RUN;
	PROC SORT DATA=BRR_TEMP7;
		BY &BYVAR NB_MISS;
	RUN;
	PROC FREQ DATA=BRR_TEMP7 NOPRINT;
		TABLE NB_MISS/OUT=BRR_TEMP11;
		BY &BYVAR;
		WEIGHT &REPLI_ROOT.0;
	RUN;
	%LET K=1;
	%LET POPREF=;
	%LET NBVAR=%SCAN(&LIMIT_CRITERIA,4);
	%DO %WHILE(&K <= &NBVAR);
		%LET POPREF_ADD=%SCAN(&BYVAR,&K);
		%LET POPREF=&POPREF &POPREF_ADD;
		%LET K=%EVAL(&K+1);
	%END;
	
	PROC MEANS DATA=BRR_TEMP11 NOPRINT;
		VAR COUNT;
		BY &POPREF;
		OUTPUT OUT=BRR_TEMP12 SUM=SOMWGT;
	RUN;
	%LET FLAG_PCT=%SCAN(&LIMIT_CRITERIA,3);
	DATA BRR_TEMP13;
		MERGE BRR_TEMP11 BRR_TEMP12;
		BY &POPREF;
		PCT=(COUNT/SOMWGT)*100;
		IF (PCT < &FLAG_PCT) THEN FLAG_PCT=1;
		ELSE FLAG_PCT=0;
		KEEP &BYVAR FLAG_PCT;
	RUN;
	DATA &OUTFILE;
		MERGE BRR_TEMP6 BRR_TEMP8 BRR_TEMP10 BRR_TEMP13;
		BY &BYVAR;
	RUN;
	PROC DATASETS LIBRARY=WORK NOLIST;
		DELETE BRR_TEMP7 BRR_TEMP8 BRR_TEMP9 BRR_TEMP10 BRR_TEMP11 BRR_TEMP12 BRR_TEMP13;
	RUN;

%END;

PROC DATASETS LIBRARY=WORK NOLIST;
	DELETE BRR_TEMP1 BRR_TEMP2 BRR_TEMP3 BRR_TEMP4 BRR_TEMP5 BRR_TEMP6 MEAN_TEMP BRRDATA;
RUN;

OPTIONS NOTES;

%MEND BRR_PROCMEAN_PV;
