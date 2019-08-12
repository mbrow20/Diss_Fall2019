# Diss_Fall2019
<p>These are the functions and the .csv data and datasets used for the simulation of the dissertation. Below is a description of the different functions. Many of the functions are run through "ExampleCodeSim15Vars2LevNormWt.R". To utilize the various functions within this R script, specify using the source(), as in 'source("Functions13Sub.R")'. In the last line of the 'ExampleCodeSim15Vars2Lev'-class of launch applications you can specify the number of iterations to run for the simulation. Here is an example,</p>
<p> <strong>SimulationWithCovMat(CovMatrixWt,data1,10,mu_beta)</strong>,</p> 

<p>where the number of simulations to run is ten. Here is an example where the launch' application "ExampleCodeSim15Vars2LevlNormWt.R' calls the function <strong><i>Functions6SR.R</i></strong> (a functions that performs data trimming) with 500 iterations:</p>

>rm(list=ls())<br>
>gc()<br>
>setwd("C:/Users/Mark V. Brow/Desktop/PISA2015/PropensityScoreAnalysis")#Change to your file directory<br>
>source(<strong>"Functions6SR.R"</strong>)<br>
>library(MCMCpack)<br>
>library(MatchIt)<br>
>library(Zelig)<br>
>library(caret)<br>
>library(dplyr)<br>
>library(plyr)<br>
>CovMatrixWt<-read.csv("CovMat15Vars2LevZWt.csv",header=TRUE,sep=",")<br>
>data1<-read.csv("CAN2015_15Vars2LevWithNormWt.csv",header=TRUE,sep=",")<br>
>mu_beta<<-c(1.1, .02, -1, 2, 1.5, 0.04, 1.2, -1.3, 0.8, -0.5, -1.4, 0.3, -0.3, 0.75, 1.6, -0.98)<br>
>#The 4th variable has a true beta coefficient of '2'###<br>
>SimulationWithCovMat(CovMatrixWt, data1, <strong>500</strong>, mu_beta)<br> 
<p>(Also, within the functions themselves, it is possible to run a truncated version of the dataset (for speed of computation; not recommended for analysis) than creates a random sample of the data proportional to treatment status. If **.2** is chosen, for example, it will randomly select 20% of the data, maintaining the ratio of treated and control observations. Within the functions themselves, you can comment out (# before a command line)</p> for the full data set:
>S2<<-as.matrix(CovMatrixWt,nrow=nrow(CovMatrixWt),ncol=ncol(CovMatrixWt))<br>
>#S2<<-as.matrix(CovMatrixWt,nrow=5,ncol=5)<br>
>set.seed(6)#ensures same results for random components (e.g., partitioning data, random variable selection, etc.)<br>
>colnames(data1)[1]<<-"SC048Q01"<br>
><strong>#</strong>dpart<<-createDataPartition(data1$treat, p=0.2, list=F)<br>
><strong>#</strong>data2<<-data1 [dpart,]<br>
>data2<<-data1<br>
or for the partial data set (in this case, 20 percent):
>dpart<<-createDataPartition(data1$treat, <strong>p=0.2</strong>, list=F)<br>
>data2<<-data1 [dpart,]<br>
>#data2<<-data1<br>
for 20% (p = 0.2). Search the function themselves to find these lines. With some functions, e.g., Functions13Sub.R, Functions18.R, and Functions5VAR500.R, it is not possible to use the partition option as these functions require the full number of observations. Here is a brief explanation of the functions. A full explanation can be found in the Appendix of the dissertation.
Functions5SR.R--Performs matching on the data and is used with "ExampleCodeSim15Vars2LevNormWt.R".
Functions6SR.R--Performs trimming on data based on algorithm by Imbens and Rubin (2015). Also used with  
    "ExampleCodeSim15Vars2LevNormWt.R".
Functions7SR.R--Performs joint modeling imputation based on van Buuren (2012). The lauching application for this function is "ExampleMARMissingJOMO.R".
Functions8SR.R--Performs matching on multiply imputed data sets (n = 5) through the SPSS multiple imputation function. Missing-at-random (MAR) data was created through a van Buuren (2012) algorithm.
ExampleCodeSim15Vars2LevNormWtEMImpute--Used in conjunction with Functions5SR.R for performing mathcing on expectation maximization (EM) imputed data (SPSS). 
Functions12.R--Performs a bootstrapping procedure outlined by Zou et al. (2016) for determine the S.E. for the average treatment effect (ATE). This saves the treatment estimates and s.e. of the treatment effect in an Excel file in the current directory names "boot.est2.csv".
Functions13Sub.R--Analyzes data subclassified by propensity score strata or ranges. Used in junction with its launch application, "ExampleCodeSime15Vars2LevSub.R". It is not allowed to used the partition feature on this data.
Functions14.R--Performs mathcing with propensity score estimated through generalized boosted modeling (GBM; Ridgeway, 1999).Used with "ExampleCodeSim15Vars2LevNormWt.R".
Functions16.R--Performs coarsened exact matching (CEM; Iacus et al., 2004) and is used with "ExampleCodeSim15Vars2LevNormWt.R".
Functions18.R--Performs Bayesian Additive Regression Trees (BART; Hill, 2011) for estimating ATE. Used with "ExampleCodeSim15Vars2LevBART.R". The partition feature is not allowed with this function.
FunctionsRobustRegression.R--Performs robust regression (i.e., with interactions and polynomials) with matched and unmatched data. Used with "ExampleCodeSim15Vars2LevNormWt.R".
FunctionsDR.R--Performs "doubly"-robust estimation (Robins et al., 1995). Used with "ExampleCodeSim15Vars2LevNormWt.R".
Functions5VAR500.R--Generates 500 matched data sets that are saved to the global environment and must be run prior to running "FunctionsImbensRubinVarEst500.R". This function must be fun with 500 iterations, i.e. SimulationWithCovMat(CovMatrixWt,data1,500,mu_beta). 
FunctionsImbensRubinVarEst500.R--Estimates variance of ATE based on algorithm by Imbens and Rubin (2015). Must be run after running "Functions5VAR500.R".
SensitivityAnalysis--Performs sensitivity analysis on simulated data. Based on design-based model proposed by Imbens and Rubin (2015).
