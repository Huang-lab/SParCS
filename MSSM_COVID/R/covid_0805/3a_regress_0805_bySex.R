# 3a_regress_0805_bySex.R
## Tomi Jun @ 8/2020
library(tidyverse)
library(car)
library(Hmisc)
source("../../../../../../../OneDrive/PROJECTS/CODING/R/tomi_functions.R")

# Data --------------------------------------------------------------------
#adm.08 from 1_prepare_0805_cohort.R

fn <- "out/regression/reg_temp.csv"
reg_temp <- read.csv(fn)


# Checks ------------------------------------------------------------------
deff(adm.08$INHOSP_DEATH, adm.08$MANHATTAN)
deff(adm.08$INHOSP_DEATH, adm.08$FACILITY)
deff(adm.08$ICU_LVL, adm.08$MANHATTAN)
deff(adm.08$INTUBATED, adm.08$MANHATTAN)


# Univariable regression for inhosp-death ---------------------------------

uni.all <- do_univar_logistic_reg(
  data = adm.08,
  vars = unique(reg_temp$var),
  outcome = "INHOSP_DEATH"
) %>% 
  full_join(
    .,
    reg_temp %>% select(var, level, var_label, order),
    by=c("var", "level")
  ) %>% 
  arrange(order) %>% 
  mutate(OR = case_when(is.na(OR) ~ 1,
                        !is.na(OR) ~ OR),
         or.ci.lo = case_when(is.na(or.ci.lo) ~ 1,
                              !is.na(or.ci.lo) ~ or.ci.lo),
         or.ci.hi = case_when(is.na(or.ci.hi) ~ 1,
                              !is.na(or.ci.hi) ~ or.ci.hi),
         clean_out1 = case_when(is.na(clean_out1) ~ "Ref",
                                !is.na(clean_out1) ~ clean_out1))

uni.m <- do_univar_logistic_reg(
  data = adm.08 %>% filter(SEX=="MALE"),
  vars = unique(reg_temp$var)[unique(reg_temp$var)!="SEX"],
  outcome = "INHOSP_DEATH"
) %>% 
  full_join(
    .,
    reg_temp %>% select(var, level, var_label, order),
    by=c("var", "level")
  ) %>% 
  arrange(order) %>% 
  mutate(OR = case_when(is.na(OR) ~ 1,
                        !is.na(OR) ~ OR),
         or.ci.lo = case_when(is.na(or.ci.lo) ~ 1,
                              !is.na(or.ci.lo) ~ or.ci.lo),
         or.ci.hi = case_when(is.na(or.ci.hi) ~ 1,
                              !is.na(or.ci.hi) ~ or.ci.hi),
         clean_out1 = case_when(is.na(clean_out1) ~ "Ref",
                                !is.na(clean_out1) ~ clean_out1))

uni.f <- do_univar_logistic_reg(
  data = adm.08 %>% filter(SEX=="FEMALE"),
  vars = unique(reg_temp$var)[unique(reg_temp$var)!="SEX"],
  outcome = "INHOSP_DEATH"
) %>% 
  full_join(
    .,
    reg_temp %>% select(var, level, var_label, order),
    by=c("var", "level")
  ) %>% 
  arrange(order) %>% 
  mutate(OR = case_when(is.na(OR) ~ 1,
                        !is.na(OR) ~ OR),
         or.ci.lo = case_when(is.na(or.ci.lo) ~ 1,
                              !is.na(or.ci.lo) ~ or.ci.lo),
         or.ci.hi = case_when(is.na(or.ci.hi) ~ 1,
                              !is.na(or.ci.hi) ~ or.ci.hi),
         clean_out1 = case_when(is.na(clean_out1) ~ "Ref",
                                !is.na(clean_out1) ~ clean_out1))

## Forest plot for univar overall 
make_forestplot(
  table_names = c("Variable", "Odds Ratio (95% CI)"),
  table_values = data.frame(variable = uni.all$var_label,
                            OR = uni.all$clean_out1),
  mean_values = uni.all$OR,
  ci.lo_values = uni.all$or.ci.lo,
  ci.hi_values = uni.all$or.ci.hi,
  num_groups = 1, 
  use_log_scale = T,
  smart_ticks = T,
  group_colors = "Black",
  filename = "out/regression/ForestPlot_Univar_All",
  filetype = "pdf",
  filewidth = 14,
  fileheight = 18
)

## Forest plot for univar by sex
make_forestplot(
  table_names = c("Variable", "Male OR (95% CI)", "Female OR (95% CI)"),
  table_values = data.frame(variable = uni.m$var_label,
                            male = uni.m$clean_out1,
                            female = uni.f$clean_out1),
  mean_values = data.frame(male = uni.m$OR,
                           female = uni.f$OR),
  ci.lo_values = data.frame(male=uni.m$or.ci.lo,
                            female=uni.f$or.ci.lo),
  ci.hi_values = data.frame(male=uni.m$or.ci.hi,
                            female=uni.f$or.ci.hi),
  num_groups = 2,
  legend_labels = c("Male", "Female"),
  use_log_scale = T,
  smart_ticks = T,
  group_colors = c("#377EB8", "#E41A1C"),
  filename = "out/regression/ForestPlot_Univar_bySex",
  filetype = "pdf",
  filewidth = 16,
  fileheight = 18
)


# Mutlivar for Death (Obesity/BMI) --------------------------------------------

##BMI
covars <- c("AGE_CAT", "SEX", "BMI_CLEAN", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
            "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

model <- formula(paste0("INHOSP_DEATH ~", paste0(covars, collapse = "+"), "+ BMI_CLEAN * SEX"))
mv <- glm(model, data = adm.08, family = "binomial")
summary(mv)

model <- formula(paste0("ICU_LVL ~", paste0(covars, collapse = "+"), "+ BMI_CLEAN * SEX"))
mv <- glm(model, data = adm.08, family = "binomial")
summary(mv)

## Obesity
covars <- c("AGE_CAT", "SEX", "OBESITY_CLEAN", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
            "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

model <- formula(paste0("INHOSP_DEATH ~", paste0(covars, collapse = "+"), "+ OBESITY_CLEAN * SEX"))
mv <- glm(model, data = adm.08, family = "binomial")
summary(mv)

model <- formula(paste0("ICU_LVL ~", paste0(covars, collapse = "+"), "+ OBESITY_CLEAN * SEX"))
mv <- glm(model, data = adm.08, family = "binomial")
summary(mv)


# Multivariable regression for inhosp-death -------------------------------
table(adm.08$INHOSP_DEATH)
fn <- "out/regression/multireg_temp.csv"
multireg_temp <- read.csv(fn)
multireg_sex <- multireg_temp %>% 
  filter(var!="SEX")

covars <- c("AGE_CAT", "SEX", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
            "OBESITY_CLEAN", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

model <- formula(paste0("INHOSP_DEATH ~", paste0(covars, collapse = "+")))
mv <- glm(model, data = adm.08, family = "binomial")
vif(mv)
output_logistic_reg_table(model=mv, fn="out/regression/LogisticMultiVar_DEATH_ALL.txt", sigfigs=3)

covars.sex <- c("AGE_CAT","RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
                "OBESITY_CLEAN", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
                "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
                "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")
model <- formula(paste0("INHOSP_DEATH ~", paste0(covars.sex, collapse = "+")))

mv.m <- glm(model, data = adm.08 %>% filter(SEX=="MALE"), family = "binomial")
output_logistic_reg_table(model=mv.m, fn="out/regression/LogisticMultiVar_DEATH_MALE.txt", sigfigs=3)

mv.f <- glm(model, data = adm.08 %>% filter(SEX=="FEMALE"), family = "binomial")
output_logistic_reg_table(model=mv.f, fn="out/regression/LogisticMultiVar_DEATH_FEMALE.txt", sigfigs=3)

## Interaction models 
int_df <- data.frame(var = character(0),
                        interaction = character(0),
                        p = numeric(0))
for(c in covars[covars != "SEX"]){
  model <-  formula(paste0("INHOSP_DEATH ~", paste0(covars, collapse = "+"), "+ SEX*", c))
  mv.int <- glm(model, data=adm.08, family = "binomial")
  output_logistic_reg_table(model=mv.int, fn=paste0("out/regression/interactions/Logistic_Interaction_Sex_", c, ".txt"), sigfigs=3)
  fn <- paste0("out/regression/interactions/Logistic_Interaction_Sex_", c, ".txt")
  multi.int <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
    filter(str_detect(var, ":")) %>%
    select(interaction = var, p) %>% 
    mutate(var = str_replace(interaction, ":SEXMALE|SEXMALE:", "")) %>% 
    select(var, interaction, p)
  int_df <- rbind(int_df, multi.int)
}

multi.int <- int_df %>% 
  full_join(
    .,
    multireg_sex %>% select(var=level, var_label, order),
    by=c("var")
  ) %>% 
  arrange(order) %>% 
  mutate(p = case_when(is.na(p) ~ "Ref",
                        !is.na(p) ~ as.character(p))) 



## Load and format the logistic regression results
fn <- "out/regression/LogisticMultiVar_ALL.txt" 
multi.all <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
  full_join(
    .,
    multireg_temp %>% select(var=level, var_label, order),
    by=c("var")
  ) %>% 
  arrange(order) %>% 
  mutate(OR = case_when(is.na(OR) ~ 1,
                        !is.na(OR) ~ OR),
         or.ci.lo = case_when(is.na(or.ci.lo) ~ 1,
                              !is.na(or.ci.lo) ~ or.ci.lo),
         or.ci.hi = case_when(is.na(or.ci.hi) ~ 1,
                              !is.na(or.ci.hi) ~ or.ci.hi),
         clean_out1 = case_when(is.na(clean_out1) ~ "Ref",
                                !is.na(clean_out1) ~ clean_out1)) %>% 
  filter(var != "(Intercept)")
  
  
fn <- "out/regression/LogisticMultiVar_MALE.txt"
multi.m <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
  full_join(
    .,
    multireg_sex %>% select(var=level, var_label, order),
    by=c("var")
  ) %>% 
  arrange(order) %>% 
  mutate(OR = case_when(is.na(OR) ~ 1,
                        !is.na(OR) ~ OR),
         or.ci.lo = case_when(is.na(or.ci.lo) ~ 1,
                              !is.na(or.ci.lo) ~ or.ci.lo),
         or.ci.hi = case_when(is.na(or.ci.hi) ~ 1,
                              !is.na(or.ci.hi) ~ or.ci.hi),
         clean_out1 = case_when(is.na(clean_out1) ~ "Ref",
                                !is.na(clean_out1) ~ clean_out1)) %>% 
  filter(var != "(Intercept)")

fn <- "out/regression/LogisticMultiVar_FEMALE.txt"
multi.f <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
  full_join(
    .,
    multireg_sex %>% select(var=level, var_label, order),
    by=c("var")
  ) %>% 
  arrange(order) %>% 
  mutate(OR = case_when(is.na(OR) ~ 1,
                        !is.na(OR) ~ OR),
         or.ci.lo = case_when(is.na(or.ci.lo) ~ 1,
                              !is.na(or.ci.lo) ~ or.ci.lo),
         or.ci.hi = case_when(is.na(or.ci.hi) ~ 1,
                              !is.na(or.ci.hi) ~ or.ci.hi),
         clean_out1 = case_when(is.na(clean_out1) ~ "Ref",
                                !is.na(clean_out1) ~ clean_out1)) %>% 
  filter(var != "(Intercept)")




## Forest plot for multivar overall 
make_forestplot(
  table_names = c("Variable", "Odds Ratio (95% CI)"),
  table_values = data.frame(variable = multi.all$var_label,
                            OR = multi.all$clean_out1),
  mean_values = multi.all$OR,
  ci.lo_values = multi.all$or.ci.lo,
  ci.hi_values = multi.all$or.ci.hi,
  num_groups = 1, 
  use_log_scale = T,
  smart_ticks = T,
  group_colors = "Black",
  filename = "out/regression/ForestPlot_Multivar_All",
  filetype = "pdf",
  filewidth = 11,
  fileheight = 11
)

## Forest plot for multivar by sex
make_forestplot(
  table_names = c("Variable", "Interaction P-value"),
  table_values = data.frame(variable = multi.m$var_label,
                            interaction = multi.int$p),
  mean_values = data.frame(male = multi.m$OR,
                           female = multi.f$OR),
  ci.lo_values = data.frame(male=multi.m$or.ci.lo,
                            female=multi.f$or.ci.lo),
  ci.hi_values = data.frame(male=multi.m$or.ci.hi,
                            female=multi.f$or.ci.hi),
  num_groups = 2,
  legend_labels = c("Male", "Female"),
  use_log_scale = T,
  smart_ticks = T,
  group_colors = c("#377EB8", "#E41A1C"),
  filename = "out/regression/ForestPlot_Multivar_bySex",
  filetype = "pdf",
  filewidth = 11,
  fileheight = 11
)


# Table of univariable and multivariable regressions overall --------------
#Use uni.all for univar and multi.all for multivar
usex <- uni.all %>% 
  mutate(OR=round(OR,2), 
         or.ci.lo = round(or.ci.lo, 2),
         or.ci.hi = round(or.ci.hi, 2),
         univar = case_when(clean_out1 == "Ref" ~ "Ref",
                            clean_out1 != "Ref" ~ paste0(OR, " (", or.ci.lo, "-", or.ci.hi,")"))
         ) %>% 
  select(var=level, var_label, univar, order)
msex <- multi.all %>% 
  mutate(OR=round(OR,2), 
         or.ci.lo = round(or.ci.lo, 2),
         or.ci.hi = round(or.ci.hi, 2),
         multivar = case_when(clean_out1 == "Ref" ~ "Ref",
                              clean_out1 != "Ref" ~ paste0(OR, " (", or.ci.lo, "-", or.ci.hi,")"))
  ) %>% 
  select(var, multivar)
uni_multi_sex <- full_join(usex, msex, by="var") %>% 
  arrange(order) %>% 
  select(-order, var)

fn <- "out/regression/table_univar_multivar_sex.txt"
write.table(uni_multi_sex, fn, quote=F, sep="\t", row.names=F)


# Multivar for death among those in ICU/inutbated ---------------------------------------
covars <- c("AGE", "SEX", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
            "OBESITY_CLEAN", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

model <- formula(paste0("INHOSP_DEATH ~", paste0(covars, collapse = "+")))
mv.icu <- glm(model, data = adm.08 %>% filter(ICU_LVL==1), family = "binomial")
vif(mv.icu)
output_logistic_reg_table(model=mv.icu, fn="out/regression/LogisticMultiVar_DEATH_ICU_PTS.txt", sigfigs=3)

mv.intub <- glm(model, data = adm.08 %>% filter(INTUBATED==1), family = "binomial")
vif(mv.intub)
output_logistic_reg_table(model=mv.intub, fn="out/regression/LogisticMultiVar_DEATH_INTUB_PTS.txt", sigfigs=3)


# Multivar for ICU --------------------------------------------------------
covars <- c("AGE_CAT", "SEX", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
            "OBESITY_CLEAN", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

model <- formula(paste0("ICU_LVL ~", paste0(covars, collapse = "+")))
mv <- glm(model, data = adm.08, family = "binomial")
vif(mv)
output_logistic_reg_table(model=mv, fn="out/regression/LogisticMultiVar_ICU_ALL.txt", sigfigs=3)

model <- formula(paste0("ICU_LVL ~", paste0(covars[covars!="SEX"], collapse = "+")))
mv.m <- glm(model, data = adm.08 %>% filter(SEX=="MALE"), family = "binomial")
vif(mv.m)
output_logistic_reg_table(model=mv.m, fn="out/regression/LogisticMultiVar_ICU_MALE.txt", sigfigs=3)

mv.f <- glm(model, data = adm.08 %>% filter(SEX=="FEMALE"), family = "binomial")
vif(mv.f)
output_logistic_reg_table(model=mv.f, fn="out/regression/LogisticMultiVar_ICU_FEMALE.txt", sigfigs=3)

  
# Multivar for Intubation -------------------------------------------------
covars <- c("AGE_CAT", "SEX", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
            "OBESITY_CLEAN", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

model <- formula(paste0("INTUBATED ~", paste0(covars, collapse = "+")))
mv <- glm(model, data = adm.08, family = "binomial")
vif(mv)
output_logistic_reg_table(model=mv, fn="out/regression/LogisticMultiVar_INTUB_ALL.txt", sigfigs=3)

model <- formula(paste0("INTUBATED ~", paste0(covars[covars!="SEX"], collapse = "+")))
mv.m <- glm(model, data = adm.08 %>% filter(SEX=="MALE"), family = "binomial")
vif(mv.m)
output_logistic_reg_table(model=mv.m, fn="out/regression/LogisticMultiVar_INTUB_MALE.txt", sigfigs=3)

mv.f <- glm(model, data = adm.08 %>% filter(SEX=="FEMALE"), family = "binomial")
vif(mv.f)
output_logistic_reg_table(model=mv.f, fn="out/regression/LogisticMultiVar_INTUB_FEMALE.txt", sigfigs=3)

# Subgroup Analysis using Publish -----------------------------------------
library(Publish)
fn <- "out/regression/subgroup_temp.csv"
subgroup_temp <- read.csv(fn)

## To use function subgroupAnalysis:
## 1: name the subgroup variables
## 2: Make sure all subgroup variablse are factors
## 3: set up the model
## 4: Run subgroupAanlysis 

covars <- c("AGE_CAT", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD", "O2_LOW",
            "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE",
            "COPD_ASTHMA", "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE")

subgroups <- c("AGE_CAT", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
        "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
        "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE",
        "COPD_ASTHMA", "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE",
        "O2_LOW")

int.subgroups <- c("MANHATTAN", "SMOKING_HX", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
                   "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE",
                   "COPD_ASTHMA", "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE",
                   "O2_LOW", "ICU_LVL", "INTUBATED") ## Vars that you need to turn into factors

outcomes <- c("INHOSP_DEATH", "ICU_LVL", "INTUBATED")
sub_list <- as.list(NULL)
for(o in outcomes){
  df <- adm.08 %>%
    mutate(MALE = case_when(SEX == "MALE" ~ 1,
                            SEX == "FEMALE" ~ 0)) # MALE will be the treatment variable 
  
  df[[o]] <- as.numeric(df[[o]]) #The outcome var must be numeric 
  df[,int.subgroups[int.subgroups!=o]] <- map_dfr(df[,int.subgroups[int.subgroups!=o]], as.factor) #make other vars into factors, except the outcome
  
  form <- formula(paste0(o, "~", paste0(c("MALE", covars), collapse = "+")))
  model <- glm(form, data = df, family = "binomial")
  
  sub.mf <- subgroupAnalysis(model, data=df, treatment="MALE", subgroups=subgroups[subgroups!=o])
  
  ## Prep table for Forest Plot based on sub.mf  
  sub.df <- left_join(subgroup_temp, sub.mf, by=c("subgroups", "level")) %>% 
    mutate(num_pts = sample_0 + sample_1,
           clean_out = case_when(!is.na(OddsRatio) ~ paste0(round(OddsRatio,2), " (", round(Lower,2),"-",round(Upper,2) ,")"),
                                 is.na(OddsRatio) ~ "") 
    ) 
  
  sub_list[[o]] <- sub.df
  
  fn <- paste0("out/regression/ForestPlot_Subgroups_bySEX_", o)
  
  make_forestplot(
    table_names = c("Subgroup", "N", "Odds ratio\n(95% CI)", "Interact.\nP-value"),
    table_values = data.frame(subgroup = c(sub.df$var_label),
                              N = c(sub.df$num_pts),
                              OR = c(sub.df$clean_out),
                              interaction = c(round(sub.df$pinteraction,2))),
    mean_values = c(sub.df$OddsRatio),
    ci.lo_values = c(sub.df$Lower),
    ci.hi_values = c(sub.df$Upper),
    use_log_scale = T,
    smart_ticks = T,
    xlab_text = "Odds Ratio (95% CI)\n<-- Better -- Male -- Worse -->",
    filename = fn,
    filetype = "pdf",
    filewidth = 12,
    fileheight = 14
  )
  
}

sub.df <- data.frame(Variable = sub_list[["INHOSP_DEATH"]]$var_label,
                     N = sub_list[["INHOSP_DEATH"]]$num_pts,
                     Death_p_interact = round(sub_list[["INHOSP_DEATH"]]$pinteraction,2),
                     Death_OR = sub_list[["INHOSP_DEATH"]]$clean_out,
                     Intubation_p_interact = round(sub_list[["INTUBATED"]]$pinteraction,2),
                     Intubation_OR = sub_list[["INTUBATED"]]$clean_out,
                     ICU_p_interact = round(sub_list[["ICU_LVL"]]$pinteraction,2),
                     ICU_OR = sub_list[["ICU_LVL"]]$clean_out
                     )

fn <- "out/regression/table_subgroups_or_values.txt"
write.table(sub.df, fn, quote=F, sep="\t", row.names=F)

# Assemble overall multivar models into table --------------------------------------
fn <- "out/regression/multireg_temp.csv"
multireg_temp <- read.csv(fn) %>% 
  select(var=level, var_label, order)

fn_outcomes <- c("DEATH", "INTUB", "ICU")

mv_list <- as.list(NULL)
or_list <- as.list(NULL)
for(o in fn_outcomes){
  fn <- paste0("out/regression/LogisticMultiVar_",o,"_ALL.txt")
  mv_list[[o]] <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
    mutate(OR = round(as.numeric(OR),2),
           or.ci.lo = round(as.numeric(or.ci.lo),2),
           or.ci.hi = round(as.numeric(or.ci.hi),2),
           clean_out = paste0(OR, " (", or.ci.lo, "-", or.ci.hi, ")")) %>% 
    select(var, clean_out)
  
  or_list[[o]] <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>%
    select(var, OR, or.ci.lo, or.ci.hi)
  
}

mv_df <- mv_list[[1]] 
or_df <- or_list[[1]]
cnames <- c("var", names(mv_list)[1])
colnames(mv_df) <- cnames
onames <- c("var", paste0(colnames(or_df[2:4]), ".DEATH"))
colnames(or_df) <- onames
for(i in 1:(length(mv_list)-1)){
  mv_df <- full_join(mv_df, mv_list[[i+1]], by="var")
  or_df <- full_join(or_df, or_list[[i+1]], by="var")
  cnames <- cnames %>% append(names(mv_list)[i+1])
  colnames(mv_df) <- cnames
  onames <- onames %>% append(paste0(c("OR.", "or.ci.lo.", "or.ci.hi."), fn_outcomes[i+1]))
  colnames(or_df) <- onames
}

mv_sex <- full_join(multireg_temp, mv_df %>% filter(var!="(Intercept)"), by="var") %>% 
  arrange(order) %>% 
  select(Variable = var_label, everything(), -c(var, order)) 
mv_sex <- map_dfr(mv_sex, function(x) case_when(is.na(x) ~ "Ref",
                                                !is.na(x) ~ x))


fn <- "out/regression/table_multivar_overall_combined.txt"
write.table(mv_sex, fn, quote=F, sep="\t", row.names=F)

or_sex <- full_join(multireg_temp, or_df %>% filter(var!="(Intercept)"), by="var") %>% 
  arrange(order)
or_sex[4:12] <- map_dfr(or_sex[4:12], function(x) case_when(is.na(x) ~ 1,
                                                            !is.na(x) ~ x))
## Make a forestplot
make_forestplot(
  table_names = c("Variable", "Death\nOR (95% CI)", "Intubation\nOR (95% CI)", "ICU\nOR (95% CI)"),
  table_values = data.frame(variable = c(mv_sex$Variable),
                            death = c(mv_sex$DEATH),
                            intub = c(mv_sex$INTUB),
                            icu = c(mv_sex$ICU)),
  mean_values = data.frame(or_sex$OR.DEATH, or_sex$OR.INTUB, or_sex$OR.ICU),
  ci.lo_values = data.frame(or_sex$or.ci.lo.DEATH, or_sex$or.ci.lo.INTUB, or_sex$or.ci.lo.ICU),
  ci.hi_values = data.frame(or_sex$or.ci.hi.DEATH, or_sex$or.ci.hi.INTUB, or_sex$or.ci.hi.ICU),
  use_log_scale = T,
  smart_ticks = T,
  xlab_text = "Odds Ratio (95% CI)",
  num_groups = 3, 
  legend_labels = c("Death", "Intubation", "ICU"),
  group_colors = c("#66C2A5", "#FC8D62", "#8DA0CB"),
  filename = "out/regression/ForestPlot_Multivar_DeathIntubICU_ALL",
  filetype = "pdf",
  filewidth = 16,
  fileheight = 12
)



# Assemble sex-stratified models into table -----------------------------
fn <- "out/regression/multireg_temp_sex_strat.csv"
multireg_temp2 <- read.csv(fn) %>% 
  select(var=level, var_label, order)

fn_outcomes <- c("DEATH", "INTUB", "ICU")
fn_sex <- c("MALE", "FEMALE")

mv_list <- as.list(NULL)
or_list <- as.list(NULL)
for(o in fn_outcomes){
  for(s in fn_sex){
    fn <- paste0("out/regression/LogisticMultiVar_",o,"_",s,".txt")
    mv_list[[paste0(o,"_",s)]] <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
      mutate(OR = round(as.numeric(OR),2),
             or.ci.lo = round(as.numeric(or.ci.lo),2),
             or.ci.hi = round(as.numeric(or.ci.hi),2),
             clean_out = paste0(OR, " (", or.ci.lo, "-", or.ci.hi, ")")) %>% 
      select(var, clean_out)
    
    or_list[[paste0(o,"_",s)]] <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
      select(var, OR, or.ci.lo, or.ci.hi)
  }
  or_list[[o]] <- full_join(or_list[[paste0(o,"_MALE")]], or_list[[paste0(o,"_FEMALE")]], by="var", suffix=c(".m", ".f"))
}

mv_df <- mv_list[[1]] 
cnames <- c("var", names(mv_list)[1])
colnames(mv_df) <- cnames
for(i in 1:(length(mv_list)-1)){
  mv_df <- full_join(mv_df, mv_list[[i+1]], by="var")
  cnames <- cnames %>% append(names(mv_list)[i+1])
  colnames(mv_df) <- cnames
}

mv_sex <- full_join(multireg_temp2, mv_df %>% filter(var!="(Intercept)"), by="var") %>% 
  arrange(order) %>% 
  select(Variable = var_label, everything(), -c(var, order)) 
mv_sex <- map_dfr(mv_sex, function(x) case_when(is.na(x) ~ "Ref",
                                                !is.na(x) ~ x))


fn <- "out/regression/table_multivar_sex_stratified.txt"
write.table(mv_sex, fn, quote=F, sep="\t", row.names=F)


## Make sex-stratified forestplots for each outcome

for(o in fn_outcomes){
  df <- full_join(multireg_temp2, or_list[[o]]%>% filter(var!="(Intercept)"), by="var") %>% 
    arrange(order) %>% 
    select(Variable = var_label, everything()) 
  
  df[4:9] <- map_dfr(df[4:9], function(x) case_when(is.na(x) ~ 1,
                                                    !is.na(x) ~ x))
  
  make_forestplot(
    table_names = c("Variable", "Male\nOR (95% CI)", "Female\nOR (95% CI)"),
    table_values = data.frame(variable = c(mv_sex$Variable),
                              male = c(mv_sex[[paste0(o,"_MALE")]]),
                              female = c(mv_sex[[paste0(o,"_FEMALE")]])
                              ),
    mean_values = data.frame(df$OR.m, df$OR.f),
    ci.lo_values = data.frame(df$or.ci.lo.m, df$or.ci.lo.f),
    ci.hi_values = data.frame(df$or.ci.hi.m, df$or.ci.hi.f),
    use_log_scale = T,
    smart_ticks = T,
    xlab_text = "Odds Ratio (95% CI)",
    num_groups = 2, 
    legend_labels = c("Male", "Female"),
    group_colors = c("#377EB8", "#E41A1C"),
    filename = paste0("out/regression/ForestPlot_Multivar_SexStrat_",o),
    filetype = "pdf",
    filewidth = 12,
    fileheight = 12
  )
  
}
