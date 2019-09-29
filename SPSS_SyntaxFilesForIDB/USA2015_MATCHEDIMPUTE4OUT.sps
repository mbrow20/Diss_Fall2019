* Encoding: UTF-8.
* Script created using the IEA IDB Analyzer (Version 4.0.26).
* Created on 9/28/2019 at 5:25 PM.
* Press Ctrl+A followed by Ctrl+R to submit this analysis. 

include file = "C:\Users\Mark V. Brow\AppData\Roaming\IEA\IDBAnalyzerV4\bin\Data\Templates\SPSS_Macros\JB_RegGP.ieasps".

JB_RegGP 	 infile="C:\Users\Mark V. Brow\Desktop\DissLitReview2\Diss_FinalSPSSFilesForAnalysis\USA2015_MATCHEDIMPUTE4.sav"/
	 cvar=CNTRYID /
	 convar=ESCS ST011Q06 ST011Q07 ST013Q01 MISCED HISCED HOMEPOS SC048Q01 SC048Q02 SC048Q03 CLSIZE /
	 catvar=GENDER TREAT /
	 codings=D D/
	 refcats=1 1/
	 ncats=2 2/
	 PVRoots=/
	 PVTails=/
	 dvar0=/
	 rootpv=PV /
	 tailpv=MATH /
	 npv=10/
	 wgt=W_FSTUWT/
	 nrwgt=80 /
	 rwgt=W_FSTURWT/
	 jkz=/
	 jkr=/
	 jk2type=/
	 nomiss=Y/
	 method=BRR/
	 missing=listwise/
	 kfac=0.5/
	 shrtcut=N/
	 viewcod=N/
	 ndec=2/
	 clean = Y/
	 strctry = N/
	 viewprgs=Y/
	 viewlbl=Y/
	 qcstats=Y/
	 newout=Y/
	 intavg = Y/
	 selcrit = /
	 selvar = /
	 outdir="C:\Users\Mark V. Brow\Desktop\DissLitReview2\Diss_FinalSPSSFilesForAnalysis"/
	 outfile="USA2015_MATCHEDIMPUTE4OUT".

