#2_describe_0805.R
## Tomi Jun @ 8/2020

library(tidyverse)
source("../../../../../../../OneDrive/PROJECTS/CODING/R/tomi_functions.R")


# Data --------------------------------------------------------------------
# fn <- "out/cohorts/0805_ADMITTED_COVID_ADULTS.txt"
# adm.08 from 1_prepare_0805_cohort.R --> has all patients admitted up till August 5th
# race.08 from 1_prepare_0805_cohort.R --> has all white/black/hispanic pts admitted till August 5th 

fn <- "out/descriptive/t1_template.csv"
t1_temp <- read.csv(fn)

fn <- "out/descriptive/t1_temp_demo.csv"
t1_temp_demo <- read.csv(fn)

fn <- "out/descriptive/t1_temp_labs.csv"
t1_temp_labs <- read.csv(fn)


# Describe Demographics/ labs by Sex ---------------------------------------------------------
unique(t1_temp$var)[!unique(t1_temp$var) %in% colnames(adm.08)]

desc_sex_all <- describe_vars(data = adm.08, 
                          var_names = unique(t1_temp$var), 
                          cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                          cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                          cont_mode = "iqr") %>% 
  filter(level!="0") 
desc_sex_m <- describe_vars(data = adm.08 %>% filter(SEX=="MALE"), 
                            var_names = unique(t1_temp$var), 
                            cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                            cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                            cont_mode = "iqr") %>% 
  filter(level!="0")
desc_sex_f <- describe_vars(data = adm.08 %>% filter(SEX=="FEMALE"), 
                            var_names = unique(t1_temp$var), 
                            cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                            cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                            cont_mode = "iqr") %>% 
  filter(level!="0") 
comp_mf <- compare_desc_vars(df1 = adm.08 %>% filter(SEX=="MALE"), 
                             df2 = adm.08 %>% filter(SEX=="FEMALE"), 
                             var_names = unique(t1_temp$var),
                             cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(),
                             cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique()
                             ) 
## Stitch these together 
t1_sex <- full_join(desc_sex_m %>% select(variable, level, output_value), 
                    desc_sex_f %>% select(variable, level, output_value), 
                    by=c("variable", "level"), 
                    suffix = c(".m", ".f")) %>% 
  full_join(., 
            comp_mf %>% select(variable, level, pvalue), 
            by=c("variable", "level"),) %>% 
  full_join(., 
            desc_sex_all %>% select(variable, level, output_value, na_pct), 
            by=c("variable", "level"),) %>% 
  select(variable, level, male = output_value.m, female = output_value.f, pvalue, overall = output_value, na_pct) %>% 
  full_join(., 
             t1_temp %>% select(variable = var, level, var_label, order),
             by=c("variable", "level")) %>% 
  mutate(pvalue = as.numeric(pvalue),
         p_simple = case_when(pvalue<0.001 ~ "<0.001",
                              pvalue>=0.001 ~ as.character(signif(pvalue,1)))) %>% 
  arrange(order) %>% 
  select(variable, level, var_label, male, female, p_simple, overall, na_pct, order)

t1_demo <- inner_join(t1_sex %>% select(-order), t1_temp_demo %>% select(variable = var, level, order), by=c("variable", "level")) %>% arrange(order)
t1_labs <- inner_join(t1_sex %>% select(-order), t1_temp_labs %>% select(variable = var, level, order), by=c("variable", "level")) %>% arrange(order)

fn <- "out/descriptive/T1_bySex.txt"
write.table(t1_sex, fn, quote=F, sep="\t", row.names=F)

fn <- "out/descriptive/T1_Demographics_bySex.txt"
write.table(t1_demo, fn, quote=F, sep="\t", row.names=F)

## These labs are for all patients that have values, not limited to "routine" labs
fn <- "out/descriptive/T1_Labs_bySex.txt"
write.table(t1_labs, fn, quote=F, sep="\t", row.names=F)

#Density plot of age
p <- ggplot(data=adm.08, mapping=aes(x=AGE, fill=SEX)) +
  geom_density(alpha=0.8)
p

# Describe treatments -----------------------------------------------------
# Get chisq p values for manuscript text
#Remdeisivr
x <- as.matrix(table(adm.08$ENC_TIME_PERIOD, adm.08$REMDESIVIR))
chisq.test(x)

#Dexamethasone 
x <- as.matrix(table(adm.08$ENC_TIME_PERIOD, adm.08$DEXAMETHASONE))
chisq.test(x)

#Steroids
x <- as.matrix(table(adm.08$ENC_TIME_PERIOD, adm.08$STEROID))
chisq.test(x)


# Describe Outcomes by Sex ------------------------------------------------
# Death: INHOSP_DEATH
# ICU: ICU
# Intubation: INTUBATED
# Time to discharge
# Time to intubation
# Time to death

#Among those who died, what was the time to death
check_cont_var(adm.08 %>% filter(INHOSP_DEATH==1) %>% pull(DAYS_TO_INHOSPDEATH_DISCHARGE))

comp_mf_death <- compare_desc_vars(df1 = adm.08 %>% filter(INHOSP_DEATH==1 & SEX=="MALE"), 
                                   df2 = adm.08 %>% filter(INHOSP_DEATH==1 & SEX=="FEMALE"),
                                   var_names = unique(t1_temp$var),
                                   cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(),
                                   cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique()
) 
#IQR:
quantile(adm.08 %>% filter(INHOSP_DEATH==1 & SEX=="MALE") %>% pull(DAYS_TO_INHOSPDEATH_DISCHARGE), probs=c(0.25, 0.5, 0.75))
quantile(adm.08 %>% filter(INHOSP_DEATH==1 & SEX=="FEMALE") %>% pull(DAYS_TO_INHOSPDEATH_DISCHARGE), probs=c(0.25, 0.5, 0.75))


#Among those who did not die, what was the length of admission
check_cont_var(adm.08 %>% filter(INHOSP_DEATH==0) %>% pull(DAYS_TO_INHOSPDEATH_DISCHARGE))
check_cont_var(adm.08[adm.08$INHOSP_DEATH==0,"DAYS_TO_INHOSPDEATH_DISCHARGE"])

comp_mf_disch <- compare_desc_vars(df1 = adm.08 %>% filter(INHOSP_DEATH==0 & SEX=="MALE"), 
                                   df2 = adm.08 %>% filter(INHOSP_DEATH==0 & SEX=="FEMALE"),
                                   var_names = unique(t1_temp$var),
                                   cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(),
                                   cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique()
) 

#IQR:
quantile(adm.08 %>% filter(INHOSP_DEATH==0 & SEX=="MALE") %>% pull(DAYS_TO_INHOSPDEATH_DISCHARGE), probs=c(0.25, 0.5, 0.75))
quantile(adm.08 %>% filter(INHOSP_DEATH==0 & SEX=="FEMALE") %>% pull(DAYS_TO_INHOSPDEATH_DISCHARGE), probs=c(0.25, 0.5, 0.75))

#Compare deaths among intubated by sex
check_cont_var(adm.08$INITIAL_AIRWAY_DATE)
table(adm.08$INTUBATED, adm.08$INHOSP_DEATH, useNA="always")
comp_mf_intub <- compare_desc_vars(df1 = adm.08 %>% filter(INTUBATED==1 & SEX=="MALE"), 
                                   df2 = adm.08 %>% filter(INTUBATED==1 & SEX=="FEMALE"),
                                   var_names = unique(t1_temp$var),
                                   cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(),
                                   cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique()
) 


#Compare deaths in ICU by sex
table(adm.08$ICU_LVL, adm.08$INHOSP_DEATH, useNA="always")
comp_mf_icu <- compare_desc_vars(df1 = adm.08 %>% filter(ICU_LVL==1 & SEX=="MALE"), 
                                 df2 = adm.08 %>% filter(ICU_LVL==1 & SEX=="FEMALE"),
                                 var_names = unique(t1_temp$var),
                                 cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(),
                                 cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique()
) 


# Describe by Race/Ethnicity ----------------------------------------------
desc_race_all <- describe_vars(data = race.08, 
                          var_names = unique(t1_temp$var), 
                          cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                          cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                          cont_mode = "iqr") %>% 
  filter(level!=0)
desc_race_nhw <- describe_vars(data = race.08 %>% filter(RACE_SIMPLE=="NH White"), 
                               var_names = unique(t1_temp$var), 
                               cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                               cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                               cont_mode = "iqr") %>% 
  filter(level!=0)
desc_race_nhb <- describe_vars(data = race.08 %>% filter(RACE_SIMPLE=="NH Black"), 
                               var_names = unique(t1_temp$var), 
                               cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                               cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                               cont_mode = "iqr") %>% 
  filter(level!=0)
desc_race_his <- describe_vars(data = race.08 %>% filter(RACE_SIMPLE=="Hispanic"), 
                               var_names = unique(t1_temp$var), 
                               cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                               cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                               cont_mode = "iqr") %>% 
  filter(level!=0)
comp_wb <- compare_desc_vars(df1 = race.08 %>% filter(RACE_SIMPLE=="NH White"), 
                             df2 = race.08 %>% filter(RACE_SIMPLE=="NH Black"), 
                             var_names = unique(t1_temp$var),
                             cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(),
                             cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique()
                             )
comp_wh <- compare_desc_vars(df1 = race.08 %>% filter(RACE_SIMPLE=="NH White"), 
                             df2 = race.08 %>% filter(RACE_SIMPLE=="Hispanic"), 
                             var_names = unique(t1_temp$var),
                             cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(),
                             cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique()
                             )
## Stitch these together 
t1_race <- full_join(
  desc_race_nhw %>% select(variable, level, output_value),
  desc_race_nhb %>% select(variable, level, output_value),
  by=c("variable", "level"),
  suffix=c(".nhw", ".nhb")
) %>% 
  full_join(
    .,
    desc_race_his %>% select(variable, level, output_value),
    by=c("variable", "level"),
    ) %>% 
  select(variable, level, white = output_value.nhw, black = output_value.nhb, hispanic = output_value) %>% 
  full_join(
    .,
    desc_race_all %>% select(variable, level, output_value, na_pct),
    by=c("variable", "level")
  ) %>% 
  select(everything(), overall = output_value, na_pct) %>% 
  full_join(
    .,
    comp_wb %>% select(variable, level, pvalue),
    by=c("variable", "level")
  ) %>% 
  full_join(
    .,
    comp_wh %>% select(variable, level, pvalue),
    by=c("variable", "level"),
    suffix=c(".wb", ".wh")
  ) %>% 
  full_join(
    .,
    t1_temp %>% select(variable = var, level, var_label, order),
    by=c("variable", "level")
  ) %>% 
  select(variable, level, var_label, white, black, pvalue.wb, hispanic, pvalue.wh, overall, na_pct, order) %>% 
  arrange(order)

t1_race[,c(6,8)] <- map_dfr(t1_race[,c(6,8)], as.numeric)
t1_race_clean <- t1_race %>% 
  mutate(p.wb = ifelse(pvalue.wb<0.001, "<0.001", as.character(signif(pvalue.wb,2))),
         p.wh = ifelse(pvalue.wh<0.001, "<0.001", as.character(signif(pvalue.wh,2)))
         ) %>% 
  select(var_label, white, black, p.wb, hispanic, p.wh, overall)

fn <- "out/descriptive/T1_BaselineChar_byRace_raw.txt"
write.table(t1_race, fn, quote=F, sep="\t", row.names=F)

fn <- "out/descriptive/T1_BaselineChar_byRace_clean.txt"
write.table(t1_race_clean, fn, quote=F, sep="\t", row.names=F)

## Number of comorbidities by sex -- They aren't actually all that different
# comorb_vars <- c("HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
#                  "HEART_FAILURE", "ATRIAL_FIBRILLATION",
#                  "CHRONIC_KIDNEY_DISEASE", "COPD_ASTHMA",
#                  "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE",
#                  "OBSTRUCTIVE_SLEEP_APNEA", "HIV_FLAG")
# comorb <- adm.08 %>% 
#   select(SEX, all_of(comorb_vars)) %>% 
#   rowwise() %>% 
#   mutate(num_comorb = sum(c(HTN, DIABETES, CORONARY_ARTERY_DISEASE,
#                             HEART_FAILURE, ATRIAL_FIBRILLATION,
#                             CHRONIC_KIDNEY_DISEASE, COPD_ASTHMA,
#                             OBESITY_CLEAN, CANCER_FLAG, CHRONIC_LIVER_DISEASE,
#                             OBSTRUCTIVE_SLEEP_APNEA, HIV_FLAG),na.rm=T)) 
# 
# comorb %>%
#   ungroup() %>%
#   group_by(SEX) %>% 
#   summarise(median_comorbs = median(num_comorb, na.rm = T),
#             q25 = quantile(num_comorb, probs = 0.25),
#             q75 = quantile(num_comorb, probs = 0.75),
#             max = max(num_comorb, na.rm=T))



# Describe by time period -------------------------------------------------
desc_time_all <- describe_vars(data = adm.08, 
                               var_names = unique(t1_temp$var), 
                               cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                               cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                               cont_mode = "iqr") %>% 
  filter(level!=0)
desc_time_apr13 <- describe_vars(data = adm.08 %>% filter(ENC_TIME_PERIOD=="April13"), 
                               var_names = unique(t1_temp$var), 
                               cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                               cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                               cont_mode = "iqr") %>% 
  filter(level!=0)
desc_time_jun2 <- describe_vars(data = adm.08 %>% filter(ENC_TIME_PERIOD=="June2"), 
                                 var_names = unique(t1_temp$var), 
                                 cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                                 cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                                 cont_mode = "iqr") %>% 
  filter(level!=0)
desc_time_aug5 <- describe_vars(data = adm.08 %>% filter(ENC_TIME_PERIOD=="August5"), 
                                 var_names = unique(t1_temp$var), 
                                 cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                                 cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                                 cont_mode = "iqr") %>% 
  filter(level!=0)

comp_aj <- compare_desc_vars(df1 = adm.08 %>% filter(ENC_TIME_PERIOD=="April13"), 
                             df2 = adm.08 %>% filter(ENC_TIME_PERIOD=="June2"), 
                             var_names = unique(t1_temp$var),
                             cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(),
                             cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique()
)
comp_aa <- compare_desc_vars(df1 = adm.08 %>% filter(ENC_TIME_PERIOD=="April13"), 
                             df2 = adm.08 %>% filter(ENC_TIME_PERIOD=="August5"), 
                             var_names = unique(t1_temp$var),
                             cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(),
                             cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique()
)
t1_time <- full_join(
  desc_time_apr13 %>% select(variable, level, output_value),
  desc_time_jun2 %>% select(variable, level, output_value),
  by=c("variable", "level"),
  suffix = c(".apr", ".jun")
) %>% 
  full_join(
    .,
    desc_time_aug5 %>% select(variable, level, output_value),
    by=c("variable", "level")
  ) %>% 
  select(variable, level, april13 = output_value.apr, june2 = output_value.jun, august5 = output_value) %>% 
  full_join(
    .,
    desc_time_all %>% select(variable, level, output_value, na_pct),
    by=c("variable", "level")
  ) %>% 
  full_join(
    .,
    comp_aj %>% select(variable, level, pvalue),
    by=c("variable", "level")
  ) %>% 
  full_join(
    .,
    comp_aa %>% select(variable, level, pvalue),
    by=c("variable", "level"),
    suffix = c(".aj", ".aa")
  ) %>% 
  full_join(
    .,
    t1_temp %>% select(variable = var, level, var_label, order),
    by=c("variable", "level")
  ) %>% 
  select(variable, level, var_label, april13, june2, pvalue.aj, august5, pvalue.aa, overall = output_value, na_pct, order) %>% 
  arrange(order)

fn <- "out/descriptive/T1_BaselineChar_byTimePeriod.txt"
write.table(t1_time, fn, quote=F, sep="\t", row.names=F)

# Describe by hypoxia -----------------------------------------------------
desc_o2_all <- describe_vars(data = adm.08 %>% filter(!is.na(O2_LOW)), 
                              var_names = unique(t1_temp$var), 
                              cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                              cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                              cont_mode = "iqr") %>% 
  filter(level!="0") 
desc_o2_low <- describe_vars(data = adm.08 %>% filter(O2_LOW==1), 
                             var_names = unique(t1_temp$var), 
                             cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                             cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                             cont_mode = "iqr") %>% 
  filter(level!="0") 
desc_o2_wnl <- describe_vars(data = adm.08 %>% filter(O2_LOW==0), 
                             var_names = unique(t1_temp$var), 
                             cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                             cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                             cont_mode = "iqr") %>% 
  filter(level!="0") 
comp_o2 <- compare_desc_vars(
  df1= adm.08 %>% filter(O2_LOW==1),
  df2= adm.08 %>% filter(O2_LOW==0),
  var_names = unique(t1_temp$var),
  cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
  cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique() 
)
t1_o2 <- full_join(
  desc_o2_low %>% select(variable, level, output_value), 
  desc_o2_wnl %>% select(variable, level, output_value),
  by=c("variable", "level"),
  suffix=c(".low", ".wnl")
) %>% 
  full_join(
    .,
    desc_o2_all %>% select(variable, level, output_value, na_pct),
    by=c("variable", "level")
  ) %>% 
  full_join(
    .,
    comp_o2 %>% select(variable, level, pvalue),
    by=c("variable", "level")
  ) %>% 
  full_join(
    .,
    t1_temp %>% select(variable = var, level, var_label, order),
    by=c("variable", "level")
  ) %>% 
  select(variable, level, var_label, o2_low = output_value.low, o2_wnl = output_value.wnl, pvalue, overall = output_value, na_pct, order) %>% 
  arrange(order)

fn <- "out/descriptive/T1_BaselineChar_byO2.txt"
write.table(t1_o2, fn, quote=F, sep="\t", row.names=F)
