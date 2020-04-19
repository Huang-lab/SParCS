#### Load and preprocess MSSM covid data ####

data_dir = "~/Box\ Sync/Huang_lab/Huang_lab_data/MSSM_COVID/MSSM_COVID_20200413/" 

### Load data files 
# 20200406 version
covid_main_20200406_f = paste(data_dir,"../MSSM_COVID_20200406/COVID_19_De-identified-20200406.txt",sep = "")
covid_main_20200406 = read.table(sep="|",header=T,covid_main_20200406_f, quote="")

covid_main_f = paste(data_dir,"COVID_19_De-identified.txt",sep = "")
covid_main_20200413 = read.table(header = T, sep="|", fill = T, file = covid_main_f, quote="")

covid_meds_f = paste(data_dir,"COVID19_MEDICATIONS_DE-Identified.txt",sep = "")
covid_meds_20200413 = read.table(header = T, sep="|", fill = T, file = covid_meds_f, quote="")

covid_labs_f = paste(data_dir,"COVID_19_Labs_De-Identified.txt",sep = "")
covid_labs_20200413 = read.table(header = T, sep="|", fill = T, file = covid_labs_f, quote="")

covid_vitals_f = paste(data_dir,"COVID_19_Vitals_De-identified.txt",sep = "")
covid_vitals_20200413 = read.table(header = T, sep="|", fill = T, file = covid_vitals_f, quote="")

covid_enterprise_f =  paste(data_dir,"Enterprise_COVID19_Datamart_De_Identified.txt",sep = "")
covid_enterprise_20200413 = read.table(header = T, sep=",", fill = T, row.names = NULL, file = covid_enterprise_f) #this one's a csv file
colnames(covid_enterprise_20200413 ) <- c(colnames(covid_enterprise_20200413 )[-1], NULL) #remove the "row.names"

### summary of the 20200406 dataset ###

cat("Summary of the 20200406 dataset\n")
dim(covid_main_20200406)
summary(covid_main_20200406)

# Only the cases that have test results.
cat("Summary of the 20200406 dataset, only the ones with positive results\n")
covid_main_20200406wResults = covid_main_20200406[covid_main_20200406$COVID_RESULT %in% c("DETECTED","NOT DETECTED"),]
covid_main_20200406wResults$COVID_RESULT_BINARY = 0
covid_main_20200406wResults$COVID_RESULT_BINARY[covid_main_20200406wResults$COVID_RESULT == "DETECTED"] = 1
dim(covid_main_20200406wResults)
summary(covid_main_20200406wResults)

### TODO: merge and clean the 20200413 dataset ###

cat("Summary of the 20200413 dataset\n")
cat("covid_main_20200413\n")
dim(covid_main_20200413)
cat("covid_meds_20200413\n")
dim(covid_meds_20200413 )
cat("covid_labs_20200413\n")
dim(covid_labs_20200413 )
cat("covid_vitals_20200413\n")
dim(covid_vitals_20200413 )
cat("covid_enterprise_20200413\n")
dim(covid_enterprise_20200413)
