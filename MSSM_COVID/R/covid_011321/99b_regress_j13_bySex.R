## 99b_regress_j13_bySex.R
## Tomi Jun @ 2/2021
## Do regressions with Jan 13, 2021 cohort for commsmed revisions

library(tidyverse)
library(car)
library(Hmisc)
source("../../../../../../../OneDrive/PROJECTS/CODING/R/tomi_functions.R")


# load data ---------------------------------------------------------------
# adm.j13 from 99a 

fn <- "out/regression/no_labs_temp.csv"
no_labs_temp <- read.csv(fn)

fn <- "out/regression/multireg_temp.csv"
multireg_temp <- read.csv(fn)
multireg_sex <- multireg_temp %>% 
  filter(var!="SEX")



# Multivar for DEATH/ICU/INTUB --------------------------------------------------------
covars <- c("AGE_CAT", "SEX", "RACE_SIMPLE", "MANHATTAN",
            "OBESITY_CLEAN", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

outcomes <- c("INHOSP_DEATH", "ICU_LVL", "INTUBATED")

for(o in outcomes){
  model <- formula(paste0(o, "~", paste0(covars, collapse = "+")))
  
  mv <- glm(model, data = adm.j13, family = "binomial")
  # vif(mv)
  output_logistic_reg_table(model=mv, fn=paste0("out/regression/LogisticMultiVar_", o, "_ALL.txt"), sigfigs=3)
  
  model <- formula(paste0(o, "~", paste0(covars[covars!="SEX"], collapse = "+")))
  mv.m <- glm(model, data = adm.j13 %>% filter(SEX=="MALE"), family = "binomial")
  # vif(mv.m)
  output_logistic_reg_table(model=mv.m, fn=paste0("out/regression/LogisticMultiVar_", o, "_MALE.txt"), sigfigs=3)
  
  mv.f <- glm(model, data = adm.j13 %>% filter(SEX=="FEMALE"), family = "binomial")
  # vif(mv.f)
  output_logistic_reg_table(model=mv.f, fn=paste0("out/regression/LogisticMultiVar_", o, "_FEMALE.txt"), sigfigs=3)
  
}


# Interaction models for DEATH/ICU/INTUB --------------------------------------------
covars <- c("AGE_CAT", "SEX", "RACE_SIMPLE", "MANHATTAN",
            "OBESITY_CLEAN", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

outcomes <- c("INHOSP_DEATH", "ICU_LVL", "INTUBATED")

int_list <- as.list(NULL)

for(o in outcomes){
  
  int_df <- data.frame(var = character(0),
                       interaction = character(0),
                       p = numeric(0))
  
  for(c in covars[covars != "SEX"]){
    model <-  formula(paste0(o, "~", paste0(covars, collapse = "+"), "+ SEX*", c))
    mv.int <- glm(model, data=adm.j13, family = "binomial")
    fn <- paste0("out/regression/interactions/Logistic_Interaction_", o, "_Sex_", c, ".txt")
    output_logistic_reg_table(model=mv.int, fn= fn, sigfigs=3)
    
    multi.int <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
      filter(str_detect(var, ":")) %>%
      select(interaction = var, p) %>% 
      mutate(var = str_replace(interaction, ":SEXMALE|SEXMALE:", "")) %>% 
      select(var, interaction, p)
    int_df <- rbind(int_df, multi.int)
  }
  
  int_list[[o]] <- int_df %>% 
    full_join(
      .,
      multireg_sex %>% select(var=level, var_label, order),
      by=c("var")
    ) %>% 
    arrange(order) %>% 
    mutate(p = case_when(is.na(p) ~ "Ref",
                         !is.na(p) ~ as.character(p))) 
  
}

# Make Forest Plots For Multivar/Interaction Models for DEATH/ICU/Intub -----------------------------------

## Load and format the logistic regression results

cohort <- c("ALL", "MALE", "FEMALE")
outcomes <- c("INHOSP_DEATH", "ICU_LVL", "INTUBATED")
multi_list <- as.list(NULL)

for (o in outcomes) {
  for(c in cohort){
    fn <- paste0("out/regression/LogisticMultiVar_", o, "_", c, ".txt" )
    list_name <- paste0(o, ".", c)
    multi_list[[list_name]] <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
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
    
    if(c != "ALL"){
      multi_list[[list_name]] <- multi_list[[list_name]] %>% filter(var!="SEXMALE")
    }
    
  }
  
  ## Forest plot for multivar overall 
  list_name <- paste0(o,".ALL")
  make_forestplot(
    table_names = c("Variable", "Odds Ratio (95% CI)"),
    table_values = data.frame(variable = multi_list[[list_name]]$var_label,
                              OR = multi_list[[list_name]]$clean_out1),
    mean_values = multi_list[[list_name]]$OR,
    ci.lo_values = multi_list[[list_name]]$or.ci.lo,
    ci.hi_values = multi_list[[list_name]]$or.ci.hi,
    num_groups = 1, 
    use_log_scale = T,
    smart_ticks = T,
    group_colors = "Black",
    filename = paste0("out/regression/ForestPlot_Multivar_", o, "_All"),
    filetype = "pdf",
    filewidth = 11,
    fileheight = 11
  )
  
  ## Forest plot for multivar by sex
  list_male <- paste0(o, ".MALE")
  list_female <- paste0(o, ".FEMALE")
    
  make_forestplot(
    table_names = c("Variable", "Interaction P-value"),
    table_values = data.frame(variable = multi_list[[list_male]]$var_label,
                              interaction = int_list[[o]]$p),
    mean_values = data.frame(male = multi_list[[list_male]]$OR,
                             female = multi_list[[list_female]]$OR),
    ci.lo_values = data.frame(male=multi_list[[list_male]]$or.ci.lo,
                              female=multi_list[[list_female]]$or.ci.lo),
    ci.hi_values = data.frame(male=multi_list[[list_male]]$or.ci.hi,
                              female=multi_list[[list_female]]$or.ci.hi),
    num_groups = 2,
    legend_labels = c("Male", "Female"),
    use_log_scale = T,
    smart_ticks = T,
    group_colors = c("#377EB8", "#E41A1C"),
    filename = paste0("out/regression/ForestPlot_Multivar_", o, "_bySex"),
    filetype = "pdf",
    filewidth = 11,
    fileheight = 11
  )
  
}


# Output merged table of DEATH/ICU/INTUB results for ALL ------------------

suppT7 <- multi_list$INHOSP_DEATH.ALL %>% 
  select(var_label)
for(l in c("INHOSP_DEATH.ALL", "ICU_LVL.ALL", "INTUBATED.ALL")){
  df <- multi_list[[l]] %>% 
    select(var_label, clean_out1, p) %>% 
    mutate(p=format.pval(p, digits=1, na.form="-", eps=0.001))
  colnames(df) <- c("var_label", l, paste0("p.", l))
  suppT7 <- suppT7 %>% 
    full_join(., df, by="var_label")
}

fn <- "out/regression/SuppT7_LogisticMultiVar_Merged.txt"
write.table(suppT7, fn, quote=F, sep="\t", row.names=F)


# Output merged table of DEATH/ICU/INTUB results for sex-stratified Male/Female ------------------

suppT8 <- multi_list$INHOSP_DEATH.MALE %>% 
  select(var_label)
list_names <- c("INHOSP_DEATH.MALE", "INHOSP_DEATH.FEMALE",
                "ICU_LVL.MALE", "ICU_LVL.FEMALE",
                "INTUBATED.MALE", "INTUBATED.FEMALE")
for(l in list_names){
  df <- multi_list[[l]] %>% 
    select(var_label, clean_out1) 
  colnames(df) <- c("var_label", l)
  suppT8 <- suppT8 %>% 
    full_join(., df, by="var_label")
}

fn <- "out/regression/SuppT8_LogisticMultiVar_SexStrat_Merged.txt"
write.table(suppT8, fn, quote=F, sep="\t", row.names=F)


# Subgroup Analysis using Publish -----------------------------------------
library(Publish)
fn <- "out/regression/subgroup_temp.csv"
subgroup_temp <- read.csv(fn)

## To use function subgroupAnalysis:
## 1: name the subgroup variables
## 2: Make sure all subgroup variablse are factors
## 3: set up the model
## 4: Run subgroupAanlysis 

covars <- c("AGE_CAT", "RACE_SIMPLE", "MANHATTAN", "O2_LOW",
            "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE",
            "COPD_ASTHMA", "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE")

subgroups <- c("AGE_CAT", "RACE_SIMPLE", "MANHATTAN", 
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
  df <- adm.j13 %>%
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

