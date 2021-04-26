## 1_prepare_0805_cohort.R
## Tomi Jun @ 8/2020
## Prepare the MSSM 0805 COVID dataset for the following:
### Sex/ Ethnicity Risk Factors Analysis
### Inpatient resilience Analysis
### Longitudinal Inpatient Labs Analysis
### Testing Positive Analysis


library(tidyverse)
source("../../../../../../../OneDrive/PROJECTS/CODING/R/tomi_functions.R")


# Load 0805 data ---------------------------------------------------------------

data_dir = "../../../../../Huang_lab_data/MSSM_COVID/MSSM_COVID_20200805/" 

fn = paste(data_dir,"COVID_19_De-identified.zip",sep = "")
covid_main = read.table(header = T, sep="|", fill = T, file = unz(description = fn, filename = "COVID_19_De-identified.txt"), quote="")

fn = paste(data_dir,"COVID19_MEDICATIONS_DE-Identified.zip", sep = "")
covid_meds = read.table(header = T, sep="|", fill = T, file = unz(description = fn, filename = "COVID19_MEDICATIONS_De-Identified.txt"), quote="")

fn = paste(data_dir,"COVID_19_Labs_De-Identified.zip",sep = "")
covid_labs = read.table(header = T, sep="|", fill = T, file = unz(description = fn, filename = "COVID_19_Labs_De-Identified.txt"), quote="")

fn = paste(data_dir,"COVID_19_Vitals_De-identified.zip",sep = "")
covid_vitals = read.table(header = T, sep="|", fill = T, file = unz(description = fn, filename = "COVID_19_Vitals_De-identified.txt"), quote="")

## XR file is hard to load because of commas in the impression  
# fn = paste(data_dir,"COVID_19_Radiology_Impression_De-Identified_20200730.txt",sep = "")
# covid_xr = read.table(header = T, sep =",", fill= T, file = fn, quote= "")


# Load 0413 and 0602 ------------------------------------------------------
## Define which encounters were in 0413 vs 0602
fn <- "../../../../../Huang_lab_data/MSSM_COVID/MSSM_COVID_20200413/COVID_19_De-identified.txt"
all.0413 <- read.table(fn, header=T, sep="|", fill=T, quote="")

fn <- "../../../../../Huang_lab_data/MSSM_COVID/MSSM_COVID_20200602/COVID_19_De-identified.txt"
all.0602 <-  read.table(fn, header = T, sep="|", fill = T, quote="")

enc.0413 <- all.0602 %>% 
  filter(MASKED_ENCOUNTER_EPIC_CSN %in% all.0413$MASKED_ENCOUNTER_EPIC_CSN) %>% 
  pull(NEW_MASKED_ENCOUNTER_EPIC_CSN) %>% 
  unique()

enc.0602 <- all.0602 %>% 
  filter(!MASKED_ENCOUNTER_EPIC_CSN %in% all.0413$MASKED_ENCOUNTER_EPIC_CSN) %>% 
  pull(NEW_MASKED_ENCOUNTER_EPIC_CSN) %>% 
  unique()

rm(all.0413, all.0602)

## Output lists of encounters from 0413 vs 0602
fn <- "out/cohorts/0413_LIST_OF_ALL_ENCOUNTERS.txt"
write.table(enc.0413, fn, quote=F, sep="\t", row.names=F)
fn <- "out/cohorts/0602_LIST_OF_ALL_ENCOUNTERS.txt"
write.table(enc.0602, fn, quote=F, sep="\t", row.names=F)

# ##COVID 19 Admissions prior to April 13
# fn <- "../covid_0602/out/0413_ADMITS_0602_UPDATED_OUTCOMES.txt"
# adm.46 <- read.table(fn, header = T, sep ="\t", quote="", stringsAsFactors = F)
# 
# ##COVID 19 Admissions April 13- June2
# fn <- "../covid_0602/out/0602_ADMITS_MINUS0413.txt"
# adm.06 <- read.table(fn, header = T, sep ="\t", quote="", stringsAsFactors = F)


# Simplify Labs -------------------------------------------------------------------------
## Pick key labs of interest
## Simplify the names of these labs
## This will be the main DF from which to make other lab DFs below 

sort(unique(covid_labs$LAB_COMPONENT_TYPE))
covid_labs %>% group_by(LAB_COMPONENT_TYPE) %>% summarise(num = n()) %>% print(n = 100)
#Exclude ALKPHOS because there are very few values (surprisingly) 

### THINK ABOUT WHAT LABS TO INCLUDE 
#LFTs, CBC, Coags, Renal, Putative biomarkers, COVID Ab
chosen_labs <- c("AST", "ALT", "TBILI", "ALBUMIN", #LFTs
                 "WBC", "HGB", "PLT", "ANC", "LYMPHS", "BASO", "EOS", "MONO", #CBC
                 "LYMPHS_PCT", "NEUTRO_PCT", "BASO_PCT", "EOS_PCT", "MONO_PCT", 
                 "IL6", "IL1B", "IL8", "TNFa", #Cytokines
                 "PT", "PTT", "INR", "FIBRINOGEN", "DDIMER", #Coags
                 "CREATININE", "BUN", "CALCIUM", "CHLORIDE",  "SODIUM", "POTASSIUM", #Renal function
                 "CRP","TROPONIN", "PROCALCITONIN", "FERRITIN", "LDH", "ESR", "BNP", #Putative biomarkers
                 "COVID_QUANT_AB")


simplified_labs <- covid_labs %>%   
  mutate(LAB_SIMPLE = case_when(LAB_COMPONENT_TYPE == "TOTAL BILIRUBIN" ~ "TBILI",
                                LAB_COMPONENT_TYPE %in% c("HEMOGLOBIN") ~ "HGB",
                                LAB_COMPONENT_TYPE %in% c("PLATELET") ~ "PLT",
                                LAB_COMPONENT_TYPE %in% c("NEUTROPHIL ") ~ "ANC",
                                LAB_COMPONENT_TYPE %in% c("LYMPHOCYTE ") ~ "LYMPHS",
                                LAB_COMPONENT_TYPE %in% c("BASOPHIL ") ~ "BASO",
                                LAB_COMPONENT_TYPE %in% c("EOSINOPHIL ") ~ "EOS",
                                LAB_COMPONENT_TYPE %in% c("MONOCYTE ") ~ "MONO",
                                LAB_COMPONENT_TYPE %in% c("LYMPHOCYTE (%)") ~ "LYMPHS_PCT",
                                LAB_COMPONENT_TYPE %in% c("NEUTROPHIL (%)") ~ "NEUTRO_PCT",
                                LAB_COMPONENT_TYPE %in% c("BASOPHIL (%)") ~ "BASO_PCT",
                                LAB_COMPONENT_TYPE %in% c("EOSINOPHIL (%)") ~ "EOS_PCT",
                                LAB_COMPONENT_TYPE %in% c("MONOCYTE (%)") ~ "MONO_PCT",
                                LAB_COMPONENT_TYPE %in% c("INTERLEUKIN-6") ~ "IL6",
                                LAB_COMPONENT_TYPE %in% c("C-REACTIVE PROTEIN") ~ "CRP",
                                LAB_COMPONENT_TYPE %in% c("INTERLEUKIN 1 BETA") ~ "IL1B",
                                LAB_COMPONENT_TYPE %in% c("INTERLEUKIN 8") ~ "IL8",
                                LAB_COMPONENT_TYPE %in% c("TNF ALPHA") ~ "TNFa",
                                LAB_COMPONENT_TYPE %in% c("PROTHROMBIN TIME") ~ "PT",
                                LAB_COMPONENT_TYPE %in% c("SERUM CREATININE") ~ "CREATININE",
                                LAB_COMPONENT_TYPE %in% c("TROPONIN I") ~ "TROPONIN",
                                LAB_COMPONENT_TYPE == "D-DIMER" ~ "DDIMER",
                                LAB_COMPONENT_TYPE %in% c("BRAIN NATRIURETIC PROTEIN") ~ "BNP",
                                LAB_COMPONENT_TYPE %in% c("QUANTITATIVE COVID-19 ANTIBODIES") ~ "COVID_QUANT_AB",
                                TRUE ~ as.character(LAB_COMPONENT_TYPE))) %>% 
  filter(LAB_SIMPLE %in% chosen_labs)


# Pick baseline labs for All Encounters, if available  -----------------
# The baseline lab value is the one that is the closest to and within 24 hours of the encounter start
# range(simplified_labs$LAB_ORDER_DAYS_SINCE_ENC, na.rm=T)
bl_labs <- simplified_labs %>% 
  group_by(NEW_MASKED_ENCOUNTER_EPIC_CSN, LAB_SIMPLE) %>% 
  mutate(lab_order_days = abs(LAB_ORDER_DAYS_SINCE_ENC)) %>% 
  filter(lab_order_days <= 1) %>% 
  arrange(lab_order_days, .by_group=T) %>% 
  slice(1) 

# fn <- "out/0805_LABS_BASELINE_ALL_LONG.txt"
# write.table(bl_labs, fn, quote=F, sep="\t", row.names=F)

## Pivot the table to make it 1 row per pt/enc
labs_wide <- bl_labs %>% 
  select(NEW_MASKED_MRN, NEW_MASKED_ENCOUNTER_EPIC_CSN, LAB_SIMPLE, RESULT_NUMERIC) %>% 
  pivot_wider(names_from = LAB_SIMPLE, values_from = RESULT_NUMERIC)

map(labs_wide[3:length(labs_wide)], function(x) sum(is.na(x))/nrow(labs_wide)) ## outputs how many are missing 

  
fn <- "out/cohorts/0805_LABS_BASELINE_ALL_WIDE.txt"
write.table(labs_wide, fn, quote=F, sep="\t", row.names=F)
# labs_wide <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F)


# Identify Encounters with Steroid administration (presumed inpatient)  ------------------------
steroid_meds <- c("PREDNISONE", "DEXAMETHASONE", "HYDROCORTISONE", "METHYLPREDNISOLONE")
steroid_enc <- covid_meds %>% 
  filter(MEDICATION_NAME %in% steroid_meds) %>% 
  filter(RECORD_TYPE == "Medication_Administration") %>% 
  pull(NEW_MASKED_ENCOUNTER_EPIC_CSN) %>% 
  unique() %>% 
  as.character()

dexa_enc <- covid_meds %>% 
  filter(MEDICATION_NAME %in% "DEXAMETHASONE") %>% 
  filter(RECORD_TYPE == "Medication_Administration") %>% 
  pull(NEW_MASKED_ENCOUNTER_EPIC_CSN) %>% 
  unique() %>% 
  as.character()

# Prepare 0805 All Encounters  -----------------------------------------------------
## Variables to generate: 
## RACE_SIMPLE -- Mutually exclusive categories of Black, White, Hispanic, Asian, Other (incl. American Indian, Pacific Islander), and NA
#### RACE_ETHNICITY_COMBINED -- created by Mount Sinai COVID Disparities Task Force
## ENC_ACUITY -- The highest acuity of the encounter: ICU, INPT, ED, OUTPT
## COVID_POS -- Boolean. Positive or presumptive positive tests are T, Not detected are F
## ENC_TIME_PERIOD -- Indicates whether the encounter was in the "April13", "June2", or "August5" datasets; mutually exclusive 
## PRE_APRIL13 -- Boolean. Indicates whether the encounter was pre-April 13, which was the surge period
## COPD_ASTHMA -- 1/0 for presence of COPD or Asthma.
## O2_LOW -- 1/0 for O2 <92%
## BMI_CAT -- Factor. BMI categories 
## BMI_CLEAN -- Numeric. Eliminate any BMI less than 5 or more than 60. 
## CHRONIC_LIVER_DISEASE -- 1/0 to indicate either viral heptitis or other liver disease  

# sort(unique(covid_main$RACE))
# sort(unique(covid_main$RACE_ETHNICITY_COMBINED))

all_enc <- covid_main %>% 
  select(-c(DOB, ENCOUNTER_DATE, STREET, VISIT_TYPE, DIAGNOSIS_DESCRIPTION)) %>% 
  mutate(COVID_POS = case_when(COVID_RESULT %in% c("DETECTED", "PRESUMPTIVE POSITIVE") ~ T,
                               COVID_RESULT %in% c("NOT DETECTED") ~ F)
  ) %>% 
  mutate(RACE_SIMPLE = case_when(RACE_ETHNICITY_COMBINED == "WHITE" ~ "NH White",
                                 RACE_ETHNICITY_COMBINED == "BLACK OR AFRICAN-AMERICAN" ~ "NH Black",
                                 RACE_ETHNICITY_COMBINED == "HISPANIC" ~ "Hispanic",
                                 RACE_ETHNICITY_COMBINED == "ASIAN" ~ "Asian",
                                 RACE_ETHNICITY_COMBINED %in% c("OTHER", "AMERICAN INDIAN OR ALASKA NATIVE", "NATIVE HAWAIIAN OR PACIFIC ISLANDER") ~ "Other"),
  SMOKING_SIMPLE = case_when(SMOKING_STATUS == "NEVER" ~"NEVER",
                             SMOKING_STATUS == "YES" ~ "CURRENT",
                             SMOKING_STATUS == "QUIT" ~ "FORMER"),
  SMOKING_HX = case_when(SMOKING_SIMPLE %in% c("CURRENT", "FORMER") ~ 1,
                         SMOKING_SIMPLE == "NEVER" ~ 0),
  ENC_ACUITY = case_when(ICU == 1 ~ "ICU",
                         INPATIENT_NON_ICU == 1 ~ "INPT",
                         EMERGENCY_DEPARTMENT == 1 ~ "ED",
                         TRUE ~ "OUTPT"),
  ADMITTED = ENC_ACUITY %in% c("INPT", "ICU"),
  ENC_TIME_PERIOD = case_when(NEW_MASKED_ENCOUNTER_EPIC_CSN %in% enc.0413 ~ "April13",
                              NEW_MASKED_ENCOUNTER_EPIC_CSN %in% enc.0602 ~ "June2",
                              TRUE ~ "August5"),
  PRE_APRIL13 = NEW_MASKED_ENCOUNTER_EPIC_CSN %in% adm.46$NEW_MASKED_ENCOUNTER_EPIC_CSN,
  SEX = ifelse(SEX %in% c("FEMALE", "MALE"), as.character(SEX), NA), 
  COPD_ASTHMA = case_when(COPD == 1 | ASTHMA == 1 ~ 1,
                          COPD == 0 & ASTHMA == 0 ~ 0),
  ENC_ACUITY = factor(ENC_ACUITY, levels = c("ICU", "INPT", "ED", "OUTPT")),
  TEMPERATURE = case_when(TEMPERATURE < 50 ~ TEMPERATURE * 9/5 +32,
                          TRUE ~ TEMPERATURE),
  O2_LOW = case_when(O2_SAT<92 ~ 1, 
                     O2_SAT>=92 ~ 0),
  BMI_CLEAN = case_when(BMI>5 & BMI<60 ~ BMI),
  BMI_CAT = case_when(BMI_CLEAN < 18.5 ~ "<18.5",
                      BMI_CLEAN >=18.5 & BMI_CLEAN <25 ~ "18.5-25",
                      BMI_CLEAN >=25 & BMI_CLEAN <30 ~ "25-30", 
                      BMI_CLEAN >=30 & BMI_CLEAN <35 ~ "30-35",
                      BMI_CLEAN >=35 & BMI_CLEAN <40 ~ "35-40",
                      BMI_CLEAN >=40 ~ ">=40"),
  OBESITY_CLEAN = case_when(BMI_CLEAN >=30 ~ 1,
                            BMI_CLEAN <30 ~ 0,
                            is.na(BMI_CLEAN) & OBESITY == 1 ~ 1,
                            is.na(BMI_CLEAN) & OBESITY == 0 ~ 0),
  STEROID = case_when(NEW_MASKED_ENCOUNTER_EPIC_CSN %in% steroid_enc ~ 1,
                      !NEW_MASKED_ENCOUNTER_EPIC_CSN %in% steroid_enc ~ 0), 
  DEXAMETHASONE = case_when(NEW_MASKED_ENCOUNTER_EPIC_CSN %in% dexa_enc ~ 1,
                            !NEW_MASKED_ENCOUNTER_EPIC_CSN %in% dexa_enc ~ 0),
  CHRONIC_LIVER_DISEASE = case_when(CHRONIC_VIRAL_HEPATITIS == 1 | ALCOHOLIC_NONALCOHOLIC_LIVER_DISEASE == 1 ~ 1,
                                    CHRONIC_VIRAL_HEPATITIS == 0 & ALCOHOLIC_NONALCOHOLIC_LIVER_DISEASE == 0 ~ 0)
  ) 

all_enc$BMI_CAT <- factor(all_enc$BMI_CAT, levels = c("18.5-25", "<18.5", "25-30",
                                                      "30-35", "35-40", ">=40"))

fn <- "out/cohorts/0805_ALL_ENCOUNTERS.txt"
write.table(all_enc, fn, quote=F, sep="\t", row.names=F)
# all_enc <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F)

# Check 0805 All Encounters variables -------------------------------------
check_cat_var(all_enc$SEX)
check_cont_var(all_enc$AGE)
check_cat_var(all_enc$RACE_SIMPLE)
table(all_enc$RACE_ETHNICITY_COMBINED, all_enc$RACE_SIMPLE, useNA = "always")
table(all_enc$ETHNICITY, all_enc$RACE_SIMPLE, useNA = "always")
table(all_enc$RACE, all_enc$RACE_SIMPLE, useNA = "always")
table(all_enc$SMOKING_SIMPLE, all_enc$SMOKING_STATUS, useNA = "always")
table(all_enc$ICU, all_enc$INPATIENT_NON_ICU, useNA="always")
table(all_enc$ENC_ACUITY, all_enc$ICU, useNA = "always")
table(all_enc$ENC_ACUITY, all_enc$INPATIENT_NON_ICU, useNA = "always")
table(all_enc$ENC_ACUITY, all_enc$EMERGENCY_DEPARTMENT, useNA = "always")
check_cat_var(all_enc$ENC_TIME_PERIOD)
table(all_enc$ENC_TIME_PERIOD, all_enc$PRE_APRIL13, useNA = "always")
check_cat_var(all_enc$COPD_ASTHMA)
table(all_enc$COPD_ASTHMA, all_enc$COPD, useNA="always")
table(all_enc$COPD_ASTHMA, all_enc$ASTHMA, useNA="always")
check_cont_var(all_enc$TEMPERATURE)
check_cont_var(all_enc$HEART_RATE)
check_cont_var(all_enc$SYSTOLIC_BP)
check_cont_var(all_enc$O2_SAT)
check_cat_var(all_enc$O2_LOW)
table(all_enc$PATIENT_CLASS, all_enc$ENC_ACUITY, useNA="always")

# Prepare 0805 Outpatient Testing Cohort ----------------------------------
## This is a subset of 0805 All Encoutners
## Adults
## Outpatient encounters with COVID testing within 2 days of the encounter 
## Each row is one patient. for those with multiple encounters, choose the earliest encounter
## Baseline Labs at encounter, if available 

# table(all_enc$COVID_POS, useNA = "always")

# Vars to remove from the dataframe
out.08_exclude_vars <- c("MASKED_EXTERNAL_VISIT_ID", "RACE", "ETHNICITY", "RACE_ETHNICITY_COMBINED",
                         "PREFERREDLANGUAGE", "INFECTION_STATUS", "INFECTION_START_DAYS_SINCE_ENCOUNTER",
                         "COHORT_INCLUSION_CRITERIA", "ENCOUNTER_TYPE", "ADMISSION_TYPE",
                         "ADMISSION_TYPE", "PATIENT_CLASS", "DEPARTMENT_NAME", "CARE_AREA_TYPE", "EMERGENCY_DEPARTMENT", 
                         "INPATIENT_NON_ICU", "ICU", "DISCHARGE_DAYS_SINCE_ENCOUNTER", "DISCHARGE_LOCATION", "COVID_ORDER",
                         "COVID_RESULT", "ARDS", "ACUTE_KIDNEY_INJURY", "ACUTE_VENOUS_THROMBOEMBOLISM", "CEREBRAL_INFARCTION",
                         "INTRACEREBRAL_HEMORRHAGE", "ACUTE_MI", "TOCILIZUMAB", "DATE_OF_FIRST_TOCILIZUMAB",
                         "REMDESIVIR", "DATE_OF_FIRST_REMDESIVIR", "SARILUMAB", "DATE_OF_FIRST_SARILUMAB",
                         "ANAKINRA","DATE_OF_FIRST_ANAKINRA", "CONVALESCENT_PLASMA", "INITIAL_AIRWAY_TYPE",
                         "INITIAL_AIRWAY_DATE", "RUN_DATE", "STEROID", "DEXAMETHASONE")

out.08 <- all_enc %>% 
  filter(!is.na(COVID_POS)) %>% 
  filter(AGE >=18) %>% 
  filter(ENC_ACUITY == "OUTPT") %>% 
  filter(abs(COVID_ORDER_DAYS_SINCE_ENCOUNTER)<=2) %>% 
  select(-all_of(out.08_exclude_vars)) %>% 
  group_by(NEW_MASKED_MRN) %>% 
  arrange(ENC_SEQ_NUMBER) %>% 
  slice(1) %>% 
  left_join(., labs_wide, by=c("NEW_MASKED_MRN", "NEW_MASKED_ENCOUNTER_EPIC_CSN"))

# n_distinct(out.08$NEW_MASKED_MRN)
# n_distinct(out.08$NEW_MASKED_ENCOUNTER_EPIC_CSN)
# check_cont_var(out.08$COVID_ORDER_DAYS_SINCE_ENCOUNTER)
# map(out.08[55:length(out.08)], function(x) sum(is.na(x))/nrow(labs_wide)) ## outputs how many labs are missing 

fn <- "out/cohorts/0805_OUTPATIENT_ADULTS_TESTED.txt"
write.table(out.08, fn, quote=F, sep="\t", row.names=F)

# Prepare 0805 Admitted patients ------------------------------------------
## This a subset of 0805 All Encounters 
## COVID positive admitted adults. COVID test must have been ordered within 2 days of encounter  
## If a patient has multiple encounters, pick the most recent encounter 
## Variables to generate: 
## INHOSP_DEATH -- 1/0. Did the patient die in the hospital  
## DAYS_TO_INHOSPDEATH_DISCHARGE -- Number of days till in-hospital death or discharge 
## MANHATTAN -- 1/0. Whether the FACILITY was in Manhattan or not 
## AGE_CAT -- Factor. Age categories roughly corresponding to quartiles of the admitted patients' ages
## O2_CAT -- Factor. O2 sat at presentation categories corresponding to quartiles of admitted patients' initial O2
## Baseline Labs

adm.08_exclude_vars <- c("MASKED_EXTERNAL_VISIT_ID", "RACE", "ETHNICITY", "RACE_ETHNICITY_COMBINED",
                         "PREFERREDLANGUAGE", "INFECTION_STATUS", "INFECTION_START_DAYS_SINCE_ENCOUNTER",
                         "COHORT_INCLUSION_CRITERIA", "ENCOUNTER_TYPE", "ADMISSION_TYPE",
                         "ADMISSION_TYPE", "PATIENT_CLASS", "DEPARTMENT_NAME", "CARE_AREA_TYPE", "EMERGENCY_DEPARTMENT", 
                         "INPATIENT_NON_ICU", "COVID_ORDER","COVID_RESULT", "RUN_DATE")

adm.08 <- all_enc %>% 
  filter(AGE >=18) %>% 
  filter(ENC_ACUITY %in% c("INPT", "ICU")) %>% 
  filter(COVID_POS) %>% 
  filter(abs(COVID_ORDER_DAYS_SINCE_ENCOUNTER)<=2) %>% 
  mutate(
    MANHATTAN = case_when(FACILITY %in% c("THE MOUNT SINAI HOSPITAL", "MOUNT SINAI WEST", "MOUNT SINAI ST. LUKE'S", "MOUNT SINAI BI PETRIE", "MS 1160 5TH AVE") ~ 1,
                          FACILITY %in% c("MOUNT SINAI QUEENS HOSPITAL", "MOUNT SINAI BI BROOKLYN") ~ 0),
    INHOSP_DEATH = case_when(DECEASED_INDICATOR == 1 & DECEASED_DAYS_SINCE_ENCOUNTER <= DISCHARGE_DAYS_SINCE_ENCOUNTER ~ 1,
                             DECEASED_INDICATOR == 1 & DECEASED_DAYS_SINCE_ENCOUNTER > DISCHARGE_DAYS_SINCE_ENCOUNTER ~ 0,
                             DECEASED_INDICATOR == 0 & !is.na(DISCHARGE_DAYS_SINCE_ENCOUNTER) ~ 0),
    DAYS_TO_INHOSPDEATH_DISCHARGE = case_when(INHOSP_DEATH == 1 & DECEASED_DAYS_SINCE_ENCOUNTER >0 ~ DECEASED_DAYS_SINCE_ENCOUNTER,
                                              INHOSP_DEATH == 1 & DECEASED_DAYS_SINCE_ENCOUNTER <=0 ~ DISCHARGE_DAYS_SINCE_ENCOUNTER,
                                              INHOSP_DEATH == 1 & is.na(DISCHARGE_DAYS_SINCE_ENCOUNTER) ~ DECEASED_DAYS_SINCE_ENCOUNTER,
                                              !is.na(DISCHARGE_DAYS_SINCE_ENCOUNTER) ~ DISCHARGE_DAYS_SINCE_ENCOUNTER),
    AGE_CAT = case_when(AGE <55 ~ "<55", 
                        AGE >=55 & AGE <65 ~ "55-64", 
                        AGE >=65 & AGE <75 ~ "65-74", 
                        AGE >=75 ~ ">=75"),
    O2_CAT = case_when(O2_SAT < 92 ~ "<92",
                       O2_SAT >=92 & O2_SAT< 95 ~ "92-95",
                       O2_SAT >=95 & O2_SAT <98 ~ "95-98",
                       O2_SAT >=98 ~ ">=98"),
    FEBRILE = case_when(TEMPERATURE >= 100.4 ~ 1,
                        TEMPERATURE < 100.4 ~ 0),
    MAP = (1/3 * SYSTOLIC_BP) + (2/3 *DIASTOLIC_BP),
    HOTN = case_when(SYSTOLIC_BP<90 | MAP <65 ~ 1, 
                     SYSTOLIC_BP>=90 | MAP >=65 ~ 0),
    TACHYCARDIC = case_when(HEART_RATE > 100 ~ 1,
                       HEART_RATE <=100 ~ 0),
    TACHYPNEIC = case_when(RESPIRATORY_RATE >25 ~ 1,
                           RESPIRATORY_RATE <=25 ~ 0),
    INTUBATED = case_when(str_detect(INITIAL_AIRWAY_TYPE, "ETT|AIRWAY") ~ 1,
                          INITIAL_AIRWAY_TYPE=="" ~ 0),
    ICU_LVL = case_when(ICU == 1 | INTUBATED == 1 ~ 1,
                        ICU == 0 & INTUBATED == 0 ~ 0)
    ) %>% 
  select(-all_of(adm.08_exclude_vars)) %>% 
  left_join(., labs_wide, by=c("NEW_MASKED_MRN", "NEW_MASKED_ENCOUNTER_EPIC_CSN")) %>% 
  group_by(NEW_MASKED_MRN) %>% 
  arrange(desc(ENC_SEQ_NUMBER), .by_group=T) %>% 
  slice(1) %>% 
  mutate(
    LYMPHS_EST = WBC * LYMPHS_PCT/100,
    MONO_EST = WBC * MONO_PCT/100,
    NEUTRO_EST = WBC * NEUTRO_PCT/100,
    BASO_EST = WBC * BASO_PCT/100,
    EOS_EST = WBC * EOS_PCT/100,
    FERRITIN_ULN = case_when(SEX=="MALE" ~ FERRITIN/400,
                             SEX=="FEMALE" ~ FERRITIN/150)
  ) %>% 
  ungroup()

## Add standardized labs/vitals:
vars_to_std <- c("ALBUMIN","ALT","AST","BASO_PCT","BNP","BUN","CALCIUM",
              "CHLORIDE", "CREATININE", "EOS_PCT","HGB","INR","LYMPHS_PCT",
              "MONO_PCT","NEUTRO_PCT","PLT","POTASSIUM","PT","PTT","SODIUM","TBILI",
              "TROPONIN", "WBC","COVID_QUANT_AB","ANC","BASO","CRP","DDIMER","EOS",
              "ESR","FERRITIN","FIBRINOGEN","LDH","LYMPHS","MONO","PROCALCITONIN",
              "IL1B","IL6","IL8","TNFa", "LYMPHS_EST", "MONO_EST", "NEUTRO_EST",
              "BASO_EST", "EOS_EST", "FERRITIN_ULN",
              "TEMPERATURE", "HEART_RATE", "SYSTOLIC_BP", "RESPIRATORY_RATE", "O2_SAT")

std.vars <- map_dfr(adm.08 %>% select(all_of(vars_to_std)), scale)
colnames(std.vars) <- paste0(vars_to_std,"_STD")
adm.08 <- cbind(adm.08, std.vars)

# ## Duplicated patients
# duplicate_pts <- as.character(unique(adm.08$NEW_MASKED_MRN[duplicated(adm.08$NEW_MASKED_MRN)]))
# dd <- adm.08 %>% 
#   filter(NEW_MASKED_MRN %in% duplicate_pts)

## Check Variables
# n_distinct(adm.08$NEW_MASKED_MRN)
# check_cont_var(adm.08$AGE)
# check_cat_var(adm.08$COVID_POS)
# table(adm.08$RACE_SIMPLE, useNA="always")
# table(adm.08$ENC_TIME_PERIOD, useNA="always")
# table(adm.08$INHOSP_DEATH, useNA="always")
# table(adm.08$ENC_TIME_PERIOD, adm.08$INHOSP_DEATH, useNA="always")
# check_cont_var(adm.08$COVID_ORDER_DAYS_SINCE_ENCOUNTER)
# check_cont_var(adm.08$DAYS_TO_INHOSPDEATH_DISCHARGE) ## There are only 23 patients still admitted
# check_cont_var(adm.08$DISCHARGE_DAYS_SINCE_ENCOUNTER)
# check_cat_var(adm.08$MANHATTAN)
# table(adm.08$FACILITY, adm.08$MANHATTAN, useNA="always")
# check_cat_var(adm.08$REMDESIVIR)
# table(adm.08$ENC_TIME_PERIOD, adm.08$REMDESIVIR, useNA="always")
# table(adm.08$ENC_TIME_PERIOD, adm.08$O2_LOW, useNA="always")
# check_cat_var(adm.08$O2_LOW)
# check_cat_var(adm.08$AGE_CAT)
# check_cat_var(adm.08$TACHYCARDIC)
# check_cat_var(adm.08$TACHYPNEIC)
# check_cat_var(adm.08$HOTN)
# check_cat_var(adm.08$FEBRILE)


fn <- "out/cohorts/0805_ADMITTED_COVID_ADULTS.txt"
write.table(adm.08, fn, quote=F, sep="\t", row.names=F)
# adm.08 <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F)

## Make factors:
adm.08$RACE_SIMPLE <- factor(adm.08$RACE_SIMPLE, levels = c("NH White", "NH Black", "Hispanic", "Asian", "Other"))
adm.08$ENC_TIME_PERIOD <- factor(adm.08$ENC_TIME_PERIOD, levels = c("April13", "June2", "August5"))
adm.08$AGE_CAT <- factor(adm.08$AGE_CAT, levels = c("55-64", "<55", "65-74", ">=75"))
adm.08$FACILITY <- factor(adm.08$FACILITY, levels = unique(adm.08$FACILITY))
adm.08$SMOKING_SIMPLE <- factor(adm.08$SMOKING_SIMPLE, levels = c("NEVER", "FORMER", "CURRENT"))
adm.08$O2_CAT <- factor(adm.08$O2_CAT, levels = c(">=98", "95-98", "92-95", "<92"))
adm.08$BMI_CAT <- factor(adm.08$BMI_CAT, levels = c("18.5-25", "<18.5", "25-30",
                                                    "30-35", "35-40", ">=40"))

##Race cohort (excludes those without a known race, and Asians/Others)
race.08 <- adm.08 %>% 
  filter(!is.na(RACE_SIMPLE)) %>% 
  filter(RACE_SIMPLE %in% c("NH White", "NH Black", "Hispanic"))
fn <- "out/cohorts/0805_ADMITTED_COVID_WHITE_BLACK_HISP.txt"
write.table(race.08, fn, quote=F, sep="\t", row.names=F)
race.08$RACE_SIMPLE <- factor(race.08$RACE_SIMPLE, levels = c("NH White", "NH Black", "Hispanic"))


# check_cat_var(race.08$DECEASED_INDICATOR)
# check_cont_var(race.08$DAYS_TO_INHOSPDEATH_DISCHARGE)

# Prepare Longitudinal Labs for Inpatients -----------------------------------------------


# Prepare 0805 Pediatric Cohort -------------------------------------------



# Flow diagram numbers ----------------------------------------------------
## How many unique patients overall?
n_distinct(covid_main$NEW_MASKED_MRN) #144,518
## How many distinct encounters? 
n_distinct(covid_main$NEW_MASKED_ENCOUNTER_EPIC_CSN) #224,018
## How many patients younger than 18? 4,721
n_distinct(covid_main %>% filter(AGE < 18) %>% pull(NEW_MASKED_MRN))

## How many patients had at least one COVID test #75,996
n_distinct(all_enc %>% filter(!is.na(COVID_POS)) %>% pull(NEW_MASKED_MRN))
## I suspect that many of the encounters with no COVID testing are follow-ups or calls to inform patients of positive COVID results
## table(all_enc %>% filter(is.na(COVID_POS)) %>% pull(COHORT_INCLUSION_CRITERIA)) 
## How many are without any COVID tests and why are they in this cohort? #68,522 patients without a COVID swab but some other inclusion criteria
all_enc %>% group_by(NEW_MASKED_MRN) %>% arrange(COVID_POS) %>% slice(1) %>% filter(is.na(COVID_POS)) %>% pull(COHORT_INCLUSION_CRITERIA) %>% table() ## Mostly people getting tested for antibodies
all_enc %>% group_by(NEW_MASKED_MRN) %>% arrange(COVID_POS) %>% slice(1) %>% filter(is.na(COVID_POS)) %>% pull(NEW_MASKED_MRN) %>% n_distinct() #68,522 patients without a COVID swab but some other inclusion criteria

## How many tested patients actually tested positive?
n_distinct(all_enc %>% filter(COVID_POS)) ## 12,347

## How many hospitalized COVID patients?
n_distinct(adm.08$NEW_MASKED_MRN) #4,930

## How many discharged/dead/still admitted?
table(adm.08$INHOSP_DEATH, useNA="always") #1,198 deceased
sum(is.na(adm.08$DAYS_TO_INHOSPDEATH_DISCHARGE)) #23 still admitted or unknown

# Remove large files ------------------------------------------------------
# rm(covid_main)
rm(all.0413, all.0602)
rm(covid_meds, covid_labs, covid_vitals)
rm(simplified_labs, bl_labs, labs_wide)
