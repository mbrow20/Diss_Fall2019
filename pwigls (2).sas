*-----------------------------------------------------------------------------------------------------------;
* PROGRAM	pwigls.sas                                                                                  ;
* FUNCTION	A SAS macro to compute weights for multilevel modeling software packages that               ;
* 		use Probability-Weighted Iterative Generalized Least Squares Estimators (PWIGLS)            ;
* LIMITATIONS 	Weights are scaled for use with two-level models.                                           ;
*                                                                                                           ;
* ESTIMATION TECHNIQUE:  Probability-Weighted Iterative Generalized Least Squares Estimators (PWIGLS)       ; 
* REFERENCE:    Pfeffermann, D., Skinner, C.,  Holmes, D. , Goldstein, H. and Rasbash, J. (1998) Weighting  ;
*               for unequal selection probabilities in multilevel models. J.Roy.Statist.Soc. B 60, 23-56    ;
* NOTE:         This reference two different methods of scaling the weights.  A separate macro is available ;
*               for each scaled weight.                                                                     ;
* MLM SOFTWARE USING THIS TECHNIQUE:                                                                        ;
*               MLwiN version ?                                                                             ;
*               Stata user-written command gllamm (per email communication with Sophia Rabe-Hesket)         ;
* WRITTEN BY	Kim Chantala                                                                                ;
* DATE		Nov 17, 2004                                                                                ;
* MOD-DATE	05Oct2005: Dan Blanchette                                                                   ;
*                                                                                                           ;
* MOD-DATE	18Jan2006: Dan Blanchette -- changed labels for &psu_m1wt and &psu_m2wt                     ;
*                                                                                                           ;
*-----------------------------------------------------------------------------------------------------------;
*;
*;
%macro pwigls(input_set=input_set, 
	psu_id=psu_id, 
	fsu_id=fsu_id, 
	psu_wt=psu_wt, 
	fsu_wt=fsu_wt, 
	output_set=pwigl_wt, 
	psu_m1wt=psu_m1wt, 
	fsu_m1wt=fsu_m1wt, 
	psu_m2wt=psu_m2wt, 
	fsu_m2wt=fsu_m2wt,
/***
        uss_wt=uss_wt,
        sum_wt=sum_wt ,
        n_sample=n_sample,
***/
	replace=,
        pmeans=0);

 ** preserve these to restore at end **;
 %let s_dsn=&sysdsn.;  
 %let s_last=&syslast.;
 %let notes=%sysfunc(getoption(notes));
 %let obs=%sysfunc(getoption(obs));

 %** if libref supplied with dataset name figure out what the libref and dataset name are **;
 %if %index(&input_set.,.) %then %do;
  %let in_lib =%substr(&input_set.,1,%index(&input_set.,.)-1);
  %let decpos =%index(&input_set.,.);
  %let in_dsn =%substr(&input_set.,&decpos. + 1,%length(&input_set.) - &decpos.);
 %end;
 %else %do;
  %let in_lib =WORK;
  %let in_dsn =&input_set.;
 %end;

 %** if libref supplied with dataset name figure out what the libref and dataset name are **;
 %if %index(&output_set.,.) %then %do;
  %let out_lib =%substr(&output_set.,1,%eval(%index(&output_set.,.)-1));
  %let decpos =%index(&output_set.,.);
  %let out_dsn =%substr(&output_set.,&decpos. + 1,%length(&output_set.) - &decpos.));
 %end;
 %else %do; 
  %let out_lib =WORK;
  %let out_dsn =&output_set.;
 %end;


 options obs=MAX;   ** Reason for maximizing it is because user could have
                     * set it lower than the number of variables in the dataset. **;
 options nonotes;   ** Shut off notes while program is running in order to reduce log size. **;


 ************ test a few things first **************;
 *-------------------------------------------------*;
 %* test that psu_wt and fsu_wt variables are numeric **;
 %let done=0;
 data _null_;
  set sashelp.vcolumn end=lastobs;
  where  compress(upcase(libname)) = compress(upcase("&in_lib."))
     and compress(upcase(memname)) = compress(upcase("&in_dsn."))
     and compress(upcase(memtype)) = compress(upcase("data"));

  if  compress(upcase(name)) = compress(upcase("&psu_wt."))  and lowcase(type) = "char" then do;
    put "%upcase(error): PWIGLS macro was not run.  Variable &psu_wt. needs to be a numeric variable. ";
    call symput('done',1);
  end;
  if  compress(upcase(name)) = compress(upcase("&fsu_wt."))  and lowcase(type) = "char" then do;
    put "%upcase(error): PWIGLS macro was not run.  Variable &fsu_wt. needs to be a numeric variable. ";
    call symput('done',1);
  end;

 run;
 %if &done=1 %then %goto done;

 ** this tests if &psu_wt. and &fsu_wt. have zero values **;
 %do i =  1 %to 2;
 %let wt =%scan(&psu_wt. &fsu_wt.,&i,%nrstr( ));
  data _null_;
   set &input_set.;
   where (&wt. = 0);
   if _n_ = 1 then put "WARNING: Your sample weight &wt. contains zero values.";
  run;
 %end;

 %if %length(&replace.) = 0  and 
      %cmpres(%upcase("&input_set.")) ^= %cmpres(%upcase("&output_set.")) %then %do;
   %if %sysfunc(exist(&out_lib..&out_dsn.)) %then %do;
     %put %upcase(error): PWIGLS macro was not run.  The output dataset &output_set. already exists.  Use the replace option if you want to replace it.;
     %goto done; 
   %end;
 %end;

 %* test that variables do not already exist **;
 %let done=0;
 data _null_;
  set sashelp.vcolumn;
  where  compress(upcase(libname)) = compress(upcase("&in_lib."))
     and compress(upcase(memname)) = compress(upcase("&in_dsn."))
     and compress(upcase(memtype)) = compress(upcase("data")) and 
      compress(upcase(name)) 
       in (%upcase("&psu_m1wt." "&fsu_m1wt." "&psu_m2wt." "&fsu_m2wt." "_uss_wt" "_sum_wt" "_n_sample"));
  if _n_ = 1 then do; 
    call symput('done',1);
    put "%upcase(error): PWIGLS macro was not run.  The following variables are created "
        "in this program so either drop them or rename them: ";
  end;
  put "  " name;

 run;
 %if &done=1 %then %goto done;

 %** test that input datset has observations **;
 %let done=0;
 data _null_;
  set sashelp.vtable;
  where  compress(upcase(libname)) = compress(upcase("&in_lib."))
     and compress(upcase(memname)) = compress(upcase("&in_dsn."))
     and compress(upcase(memtype)) = (upcase("data"));
  if nobs = 0 then do;
   put "%upcase(error): PWIGLS macro was not run.  The input data set &input_set. contains "
         " no observations.";
   call symput('done',1);
  end;

 run;
 %if &done = 1 %then %goto done;


 %* now on with the show! *;

 proc sort data = &input_set. 
            out = &output_set.; 
  by &psu_id.; 
 run;

 proc summary data = &output_set.;
  by &psu_id.;
  var &fsu_wt.;
  output out = ___tm___ 
         uss = _uss_wt
         sum = _sum_wt 
           n = _n_sample;
 run;
 
 *Create scaled weights for the Primary Sampling Units (PSU);
 
 data &output_set. 
   %if &pmeans ^= 1 %then %do;
     (drop = _uss_wt _sum_wt _n_sample)
   %end;
  ;
  merge &output_set.
        ___tm___ (keep = &psu_id. _uss_wt _sum_wt _n_sample);
  by &psu_id.;

  label _uss_wt   = "Sum of &fsu_wt. * &fsu_wt. for &psu_id. ";
  label _sum_wt   = "Sum of &fsu_wt. for &psu_id. ";
  label _n_sample = "Number of &fsu_id. in &psu_id. ";
 
  *Scaling method 1 (Pfefferman, 1998, p 29-30) ;
  &psu_m1wt. = _uss_wt / _sum_wt;
  label &psu_m1wt. = "SS(&fsu_wt.)/SUM(&fsu_wt.) for &psu_id. (Level 2 - PWIGLS Method 1)";
 
  *Scaling method 2 (Pfefferman, 1998, p 30) ;
  &psu_m2wt. = _sum_wt / _n_sample;
  label &psu_m2wt. = "MEAN(&fsu_wt.) for &psu_id. (Level 2 - PWIGLS Method 2)";
 

  *Create scaled weights for the Final Sampling Units (FSU);
 
  &fsu_m1wt. = &fsu_wt. / &psu_m1wt.;
  label &fsu_m1wt. = "Scaled weight for &fsu_id. (Level 1 - PWIGLS Method 1)";
  
  &fsu_m2wt. = &fsu_wt. / &psu_m2wt.;
  label &fsu_m2wt. = "Scaled weight for &fsu_id. (Level 1 - PWIGLS Method 2)";

 run;

 proc datasets nodetails nolist nowarn;
  delete ___tm___;
 run;

 proc sort data = &output_set.;
  by &fsu_id.;
 run;
 
 %if &pmeans = 1 %then %do;
   proc means data = &output_set.;
    var &psu_wt. &fsu_wt. &psu_m1wt. &fsu_m1wt. &psu_m2wt. &fsu_m2wt. _uss_wt _sum_wt _n_sample;
   run;
   data &output_set.; 
    set &output_set. (drop = _uss_wt _sum_wt _n_sample);
   run;
 %end;

 options notes;  ** turn notes on **;

 ** if got to here then it ran successfully **;
 %put ;
 %put NOTE:  The PWIGLS macro successfully created the data set: &output_set. ;
 %put ;

 options nonotes;  ** turn notes back off **;
 
 %** goto here when done **;
 %done:;

 ** Make sure that the last dataset created in work is the users dataset. **;
 %let sysdsn=&s_dsn;
 %let syslast=&s_last;

 options obs=&obs. &notes.;  ** Restore options. **;
 
%mend pwigls;

