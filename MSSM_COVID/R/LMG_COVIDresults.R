#### Use hierachical partition to find variable contribution on MSSM covid data ####

source("load_mssm_covid_data.R")
source("stat_function.R")
source("aesthetics_function.R")
library("hier.part")

# LMG analyses to identify contribution to variability
# may need to be modified
included_predictors = c("AGE","SEX", "RACE","ETHNICITY","FACILITY","SMOKING_STATUS","HTN","OBESITY","CANCER_FLAG","COVID_RESULT_BINARY" )
covid_main_20200406wResults_Predictors = covid_main_20200406wResults[,included_predictors]

# all variants 
# current implementation only allows for <13 variables
# currently only take categorical variables
cat("LMG for each variable\n")
pdf("out/covid_main_20200406wResults_COVIDresult_lmg.pdf")
hier.part(covid_main_20200406wResults_Predictors$COVID_RESULT_BINARY, covid_main_20200406wResults_Predictors[,c(1:9)], 
          fam = "binomial",gof = "logLik")
dev.off()

# randomization test #
rand.hp(covid_main_20200406wResults_Predictors$COVID_RESULT_BINARY, covid_main_20200406wResults_Predictors[,c(1:9)], 
         fam = "binomial",gof = "logLik")
