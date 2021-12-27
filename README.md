# Patterns of Social Mixing Relevant to Transmission of Respiratory Pathogens: A Systematic Review of Contact Surveys 🤝🤧 

## Overview
This repository contains the code used to analyse the results of a systematic review of social contact surveys. Briefly, a systematic review was carried out in order to identify social contact surveys that had been carried out in Lower, Lower-Middle and Upper-Middle Income Countries (LICs, LMICs and UMICs respectively). Alongside data from previously carried out surveys in High Income Countries (HICs), collated individual-level data from these surveys was then analysed using a Bayesian Regression based approach in order to explore the factors driving and shaping the patterns of social contact across populations. The corresponding scientific article has been published and is available at:

Mousa, A., Winskill, P., Watson, O. J., Ratmann, O., Monod, M., Ajelli, M., ... & Whittaker, C. (2021). Social Contact Patterns and Implications for Infectious Disease Transmission: A Systematic Review and Meta-Analysis of Contact Surveys. <i>eLife</i> (2021). Available here: https://elifesciences.org/articles/70294

## Repo Contents
- [Analyses](./Analyses): Code running the analyses and generating the figures featured in the paper.
- [Functions](./Functions): Extra functions required for the analyses presented in the paper.
- [Data](./Data): Containing the individual-level social contact data collated as part of the systematic review and that are used in the analyses presented here, as well as a data dictionary and information about the studies.
- [Figures](./Figures): Containing .PDF versions of the raw figure outputs from the analyses.
- [Outputs](./Outputs): Containing .rds outputs from model fitting (undertaken using a Bayesian Regression approach and implemented  using BRMS) for each of the analyses conducted. These are grouped according to whether:
    - The analysis is [Univarable](./Outputs/Univariable) or [Multivariable](./Outputs/Multivariable) i.e. whether or not age and sex were adjusted for when examining the role of individual factors. 
    - The analysis included all total contacts or whether additional contacts (such as additional work contacts, group contacts, and number missed out, which were recorded separately and in less detail by participants compared to their other contacts) were included in the analyses or not. 
    - The analysis was weighted or not, with the weighting a sensitivity analysis where all surveys were weighted equally (main analyses implicitly weight by sample size).

## Software Requirements
Running the code contained in this repository requires the following
- The R Programming Language (Version 4.0.2 "Taking Off Again" used here) 
- The package **rstan** (Version 2.21.2 used here) (see: https://cran.r-project.org/web/packages/rstan/index.html)
- The package **brms** (Version 2.13.5 used here) (see: https://cran.r-project.org/web/packages/brms/index.html)
- The packages contained within **tidyverse** (Version 2.13.5 used here) (see: https://cran.r-project.org/web/packages/tidyverse/index.html)
- The package **gdata** (version 2.13.5 used here) (see: https://cran.r-project.org/web/packages/gdata/index.html)
- The package **friendlyeval** (see: https://github.com/MilesMcBain/friendlyeval). 

This work utilises the probabilistic programming language STAN for model fitting (implemented via the package BRMS). STAN is a program for analysis of Bayesian models using Markov Chain Monte Carlo (MCMC) simulation. More information and details about the software and its use via R are available here: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started.

## Installation Guide and Instructions for Use
The following instructions require that all the relevant `R` packages have been installed by the user and that STAN has been installed. To replicate and reproduce the analyses presented in this paper, do the following: 

1. Clone this Github repository and make a local copy on your desktop.
2. Run the `R` code from  [Analyses](./Analyses) for the particular part of the analysis you are trying to reproduce.
    - Note that we utilised the high performance computing cluster available to members of the Department of Infectious Disease Epidemiology (DIDE) at Imperial College to generate the outputs in [Outputs](./Outputs). The script that generated these outputs is 1_DIDE_Cluster_BRMS_Model_Running.R. 
    - However, we also provide a script not dependent on cluster access (1_Non-Cluster_BRMS_Model_Running.R), that can be run locally to regenerate any of the results available in [Outputs](./Outputs). 
3. The output from running this code will be a number of MCMC objects (also available in [Outputs](./Outputs)), as well as a series of plots representing the output from MCMC based fitting of the relevant model to the collated data. These plots form the basis for the figures presented in main text of the associated publication. 
