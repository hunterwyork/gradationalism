# Gradationalism

## Manifest

### Code
- **01-16.R or .Rmd**  
  Code for execution of analysis and making figures and tables
  NB: Any appendix tables not explicitly referenced by a filename are made in 01 or 02 files. All main text figures/tables have their own code file. Some supplementary figures/tables are wrapped into analysis code files for simplicity.
- **99.R**  
  Helper functions called by files 01, 02. 

### Inputs
- **pooled_data(1).rds**  
  Pooled data from 11 surveys

### Ref
- **usa_00022.csv**  
  Extract from IPUMS matching 1950 census occ codes with SEI and Prestige scales
- **db_27_0_excel**  
  2022 version of O*NET database, downloadable from online
- **skills_xwalk.csv**  
  A helper file to rename gradational variables back to full names. Some programs abbreviate them.
- **nem-occcode-cps-crosswalk.xlsx**  
  [BLS CPS SOC crosswalk](https://www.bls.gov/emp/documentation/crosswalks.htm)
- **occ1950-recode.csv**  
  1950 Census occ codes and titles
- **occ1950_mc_xwalk_70.dta**  
  Micro/meso/macroclass crosswalk to census 1970 codes  
  _Song, Xi, Catherine G. Massey, Karen A. Rolf, Joseph P. Ferrie, Jonathan L. Rothbaum, and Yu Xie. 2020. “Long-Term Decline in Intergenerational Mobility in the United States since the 1850s.” Proceedings of the National Academy of Sciences 117(1): 251–58._
- **Census_integrated_occ_crosswalks.xlsx**  
  Crosswalks across all census years, from IPUMS website, but unable to find source.

### Outputs
[EMPTY] - To be filled by code.


## Instructions

- You must use psych package version 2.2.9 for this code. More recent updates have used a new method of calculating scores for factor variables.
- Set the home directory to the "code" subdirectory. 
- Code should most likely be run on a cluster. The major bottleneck is fitting the models. They can all be done on a laptop except for the gnm models (RC2) models, as they are too complex for my laptop. You could edit the code slightly to get rid of these models and run it locally. 
- Even without the RC2 models, the entire code takes probably 10 GBs of permanent memory to run, due to some very large intermediate files that are produced. 
- The entire pipeline can be run in about 4-6 hours. 
- Some tables are modified by hand in latex before being added to the paper, hence why some are compiled in R Markdown and some in base R. Usually, I'd just use base R and write a .tex file, but we hard coded some outputs from .Rmd outputs, and it's just easier to leave them as they are. 