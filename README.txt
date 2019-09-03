README 

BROW_Dissertation SAS Files
September 3, 2019

The computing Q1-Q3 and SE on pv files compute the statistics
(quantiles, medians, SEs) for the 10 plausible math values on 
the PISA 2015. These SAS files were used to compute the estimates
for Tables 3.1 and 3.2 in the Methodology section using the MACROs
proc_means_pv_10 (see the %include statements in the individual
SAS files). The dataset used for these file is
STUDSCH_MERGED_2015USACAN.csv. There is a file import statement
in the file PISA2015, MeanDiffOnVARCANUSA_Sept2019, and 
PROCDIFNOPV_Sep2019. One of these files should be run first.
The means, SEs, and contrasts for the covariates by school type used
the files MeanDiffOnVARCANUSA_Sep2019 (Means and SEs) and difference
(PROCDIFNOPV_Sep2019). Desired comparison variable must be inputted
directly into the file with a corresponding output file (keep statements,
retain statement, and VAR= in the BRR MACRO statement). If it is HOMEPOS,
then input this variable into the appropriate statements.