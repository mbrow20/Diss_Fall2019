README 

BROW_Dissertation SAS Files
September 8, 2019

The computing Q1-Q3 and SE on pv files compute the statistics
(quantiles, medians, SEs) for the 10 plausible math values on 
the PISA 2015. These SAS files were used to compute the estimates
for Tables 3.1 and 3.2 in the Methodology section using the MACROs
proc_means_pv_10 (see the %include statements in the individual
SAS files). The dataset used for these file is
STUDSCH_MERGED_2015USACAN.csv. There is a file import statement
in the file PISA2015, MeanDiffOnVARCANUSA_Sep72019, and 
PROCDIFNOPV_Sep72019. The means, SEs, and contrasts for the covariates
by school type used the files MeanDiffOnVARCANUSA_Sep72019 (Means and SEs)
and difference (PROCDIFNOPV_Sep72019). These programs will provide the full
output of Table 4.20 and Table 4.21. Results will appear in the temporary
work library. To change the country from Canada to the US, use the 
following function in the data command:if cnt="USA"; . 

