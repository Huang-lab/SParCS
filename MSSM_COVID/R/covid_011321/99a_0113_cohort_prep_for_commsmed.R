## 99a_0113_cohort_prep_for_commsmed.R
## Tomi Jun @ 2/2021
## Code to assemble a cohort from the beginning of the pandemic to 1/13/21 for CommsMed Revisions

library(tidyverse)


# Load data ---------------------------------------------------------------

data_dir = "../../../../../Huang_lab_data/MSSM_COVID/MSSM_COVID_20210113/" 

fn = paste(data_dir,"COVID_19_De-identified.zip",sep = "")
covid_main = read.table(header = T, sep="|", fill = T, file = unz(description = fn, filename = "COVID_19_De-identified.txt"), quote="")
fn = paste(data_dir,"COVID19_MEDICATIONS_DE-Identified.zip", sep = "")
covid_meds = read.table(header = T, sep="|", fill = T, file = unz(description = fn, filename = "COVID19_MEDICATIONS_De-Identified.txt"), quote="")
fn <- "../covid_0805/out/cohorts/0805_ADMITTED_COVID_ADULTS.txt"
adm.08 <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F)


# Prep variables ----------------------------------------------------------

## Make a list of all the encounters up to August 5
enc_list <- as.list(NULL)
time_periods <- c("April13", "June2", "August5")
for(i in 1:3){
  time_period <- time_periods[i]
  enc_list[[time_period]] <- adm.08 %>% 
    filter(ENC_TIME_PERIOD == time_period) %>% 
    pull(NEW_MASKED_ENCOUNTER_EPIC_CSN) %>% 
    unique()
}

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


# Build adm.113 df in two steps ----------------------------------------------

## First, build a df of all admissions (this includes repeated admissions for some patients)
all_adm <- covid_main %>% 
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
         ENC_TIME_PERIOD = case_when(NEW_MASKED_ENCOUNTER_EPIC_CSN %in% enc_list$April13 ~ "April",
                                     NEW_MASKED_ENCOUNTER_EPIC_CSN %in% enc_list$June2 ~ "June",
                                     NEW_MASKED_ENCOUNTER_EPIC_CSN %in% enc_list$August5 ~ "August",
                                     TRUE ~ "Jan"),
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
  ) %>% 
  filter(ENC_ACUITY %in% c("INPT", "ICU")) 
  
all_adm$BMI_CAT <- factor(all_adm$BMI_CAT, levels = c("18.5-25", "<18.5", "25-30",
                                                      "30-35", "35-40", ">=40"))


## Next, apply additional filters, and importantly, select only the earliest encounter of each patient

adm.113_exclude_vars <- c("MASKED_EXTERNAL_VISIT_ID", "RACE", "ETHNICITY", "RACE_ETHNICITY_COMBINED",
                         "PREFERREDLANGUAGE", "INFECTION_STATUS", "INFECTION_START_DAYS_SINCE_ENCOUNTER",
                         "COHORT_INCLUSION_CRITERIA", "ENCOUNTER_TYPE", "ADMISSION_TYPE",
                         "ADMISSION_TYPE", "PATIENT_CLASS", "DEPARTMENT_NAME", "CARE_AREA_TYPE", "EMERGENCY_DEPARTMENT", 
                         "INPATIENT_NON_ICU", "COVID_ORDER","COVID_RESULT", "RUN_DATE")
adm.113 <- all_adm %>% 
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
  select(-all_of(adm.113_exclude_vars)) %>% 
  group_by(NEW_MASKED_MRN) %>% 
  arrange(desc(ENC_SEQ_NUMBER), .by_group=T) %>% 
  slice(1) %>% 
  ungroup()

vars_to_std <- c("TEMPERATURE", "HEART_RATE", "SYSTOLIC_BP", "RESPIRATORY_RATE", "O2_SAT")
std.vars <- map_dfr(adm.113 %>% select(all_of(vars_to_std)), scale)
colnames(std.vars) <- paste0(vars_to_std,"_STD")
adm.113 <- cbind(adm.113, std.vars)

adm.113$RACE_SIMPLE <- factor(adm.113$RACE_SIMPLE, levels = c("NH White", "NH Black", "Hispanic", "Asian", "Other"))
adm.113$ENC_TIME_PERIOD <- factor(adm.113$ENC_TIME_PERIOD, levels = c("April", "June", "August", "Jan"))
adm.113$AGE_CAT <- factor(adm.113$AGE_CAT, levels = c("55-64", "<55", "65-74", ">=75"))
adm.113$FACILITY <- factor(adm.113$FACILITY, levels = unique(adm.113$FACILITY))
adm.113$SMOKING_SIMPLE <- factor(adm.113$SMOKING_SIMPLE, levels = c("NEVER", "FORMER", "CURRENT"))
adm.113$O2_CAT <- factor(adm.113$O2_CAT, levels = c(">=98", "95-98", "92-95", "<92"))
adm.113$BMI_CAT <- factor(adm.113$BMI_CAT, levels = c("18.5-25", "<18.5", "25-30",
                                                    "30-35", "35-40", ">=40"))

fn <- "out/cohorts/0113_ADMITTED_COVID_ADULTS.txt"
write.table(adm.113, fn, quote=F, sep="\t", row.names=F)

# Make a subcohort that is just the patients admitted since Aug 20 --------
adm.j13 <- adm.113 %>% 
  filter(ENC_TIME_PERIOD == "Jan")

fn <- "out/cohorts/ADMITTED_COVID_ADULTS_FROM_0805_TO_0113_FOR_COMMSMED_REVISIONS.txt"
write.table(adm.j13, fn, quote=F, sep="\t", row.names=F)


# Make a race subset including only white, black and hispanic -------------
race.113 <- adm.113 %>% 
  filter(!is.na(RACE_SIMPLE)) %>% 
  filter(RACE_SIMPLE %in% c("NH White", "NH Black", "Hispanic"))
fn <- "out/cohorts/0113_ADMITTED_COVID_WHITE_BLACK_HISP.txt"
write.table(race.113, fn, quote=F, sep="\t", row.names=F)
race.113$RACE_SIMPLE <- factor(race.113$RACE_SIMPLE, levels = c("NH White", "NH Black", "Hispanic"))

race.j13 <- race.113 %>% 
  filter(ENC_TIME_PERIOD == "Jan")

fn <- "out/cohorts/ADMITTED_COVID_ADULTS_0805_TO_1113_BLACK_WHITE_HISP.txt"
write.table(race.j13, fn, quote=F, sep="\t", row.names=F)

# Check numbers -----------------------------------------------------------

n_distinct(covid_main$NEW_MASKED_MRN)
n_distinct(covid_main$NEW_MASKED_ENCOUNTER_EPIC_CSN) 
## How many patients younger than 18?
n_distinct(covid_main %>% filter(AGE < 18) %>% pull(NEW_MASKED_MRN))

n_distinct(all_adm %>% filter(!is.na(COVID_POS)) %>% pull(NEW_MASKED_MRN))
## I suspect that many of the encounters with no COVID testing are follow-ups or calls to inform patients of positive COVID results
## table(all_adm %>% filter(is.na(COVID_POS)) %>% pull(COHORT_INCLUSION_CRITERIA)) 
## How many are without any COVID tests and why are they in this cohort? #68,522 patients without a COVID swab but some other inclusion criteria
all_adm %>% group_by(NEW_MASKED_MRN) %>% arrange(COVID_POS) %>% slice(1) %>% filter(is.na(COVID_POS)) %>% pull(COHORT_INCLUSION_CRITERIA) %>% table() ## Mostly people getting tested for antibodies
all_adm %>% group_by(NEW_MASKED_MRN) %>% arrange(COVID_POS) %>% slice(1) %>% filter(is.na(COVID_POS)) %>% pull(NEW_MASKED_MRN) %>% n_distinct() #68,522 patients without a COVID swab but some other inclusion criteria

## How many tested patients actually tested positive?


n_distinct(all_adm %>% filter(COVID_POS))

## How many hospitalized COVID patients?
n_distinct(adm.113$NEW_MASKED_MRN) 

## How many discharged/dead/still admitted?
table(adm.113$INHOSP_DEATH, useNA="always") 
sum(is.na(adm.113$DAYS_TO_INHOSPDEATH_DISCHARGE)) 

rm(all.0413, all.0602)
rm(covid_meds, covid_labs, covid_vitals)
rm(simplified_labs, bl_labs, labs_wide)

