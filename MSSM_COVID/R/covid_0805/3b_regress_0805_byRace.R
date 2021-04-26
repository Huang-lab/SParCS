## 3b_regression_0805_byRace.R
## Tomi Jun @ 8/2020
library(tidyverse)
library(RColorBrewer)
library(Hmisc)
library(car)
source("../../../../../../../OneDrive/PROJECTS/CODING/R/tomi_functions.R")
source("0805_functions.R")

# Data --------------------------------------------------------------------
#race.08 from 1_prepare_0805_cohort.R

fn <- "out/regression/reg_temp.csv"
reg_temp <- read.csv(fn) %>% 
  filter(!level %in% c("RACE_SIMPLEAsian", "RACE_SIMPLEOther"))


# Univariable -------------------------------------------------------------

uni.race <- do_univar_logistic_reg(
  data = race.08,
  vars = unique(reg_temp$var),
  outcome = "INHOSP_DEATH"
) %>% 
  prep_logistic_for_forest(data =.,
                           template = reg_temp %>% select(var, level, var_label, order), 
                           by_vars = c("var", "level"))

uni.nhw <- do_univar_logistic_reg(
  data = race.08 %>% filter(RACE_SIMPLE == "NH White"),
  vars = unique(reg_temp$var)[unique(reg_temp$var) != "RACE_SIMPLE"],
  outcome = "INHOSP_DEATH"
) %>% 
  prep_logistic_for_forest(data =.,
                           template = reg_temp %>% select(var, level, var_label, order) %>% filter(var != "RACE_SIMPLE"), 
                           by_vars = c("var", "level"))

uni.nhb <- do_univar_logistic_reg(
  data = race.08 %>% filter(RACE_SIMPLE == "NH Black"),
  vars = unique(reg_temp$var)[unique(reg_temp$var) != "RACE_SIMPLE"],
  outcome = "INHOSP_DEATH"
) %>% 
  prep_logistic_for_forest(data =.,
                           template = reg_temp %>% select(var, level, var_label, order) %>% filter(var != "RACE_SIMPLE"), 
                           by_vars = c("var", "level"))

uni.his <- do_univar_logistic_reg(
  data = race.08 %>% filter(RACE_SIMPLE == "Hispanic"),
  vars = unique(reg_temp$var)[unique(reg_temp$var) != "RACE_SIMPLE"],
  outcome = "INHOSP_DEATH"
) %>% 
  prep_logistic_for_forest(data =.,
                           template = reg_temp %>% select(var, level, var_label, order) %>% filter(var != "RACE_SIMPLE"), 
                           by_vars = c("var", "level"))


## Forest plot for univar overall 
make_forestplot(
  table_names = c("Variable", "Odds Ratio (95% CI)"),
  table_values = data.frame(variable = uni.race$var_label,
                            OR = uni.race$clean_out1),
  mean_values = uni.race$OR,
  ci.lo_values = uni.race$or.ci.lo,
  ci.hi_values = uni.race$or.ci.hi,
  num_groups = 1, 
  use_log_scale = T,
  smart_ticks = T,
  group_colors = "Black",
  filename = "out/regression/ForestPlot_Univar_RACE_overall",
  filetype = "pdf",
  filewidth = 14,
  fileheight = 18
)

## Forest plot for univar White vs Black
make_forestplot(
  table_names = c("Variable", "White OR (95% CI)", "Black OR (95% CI)"),
  table_values = data.frame(variable = uni.nhw$var_label,
                            white = uni.nhw$clean_out1,
                            black = uni.nhb$clean_out1),
  mean_values = data.frame(white = uni.nhw$OR,
                           black = uni.nhb$OR),
  ci.lo_values = data.frame(white=uni.nhw$or.ci.lo,
                            black=uni.nhb$or.ci.lo),
  ci.hi_values = data.frame(white=uni.nhw$or.ci.hi,
                            black=uni.nhb$or.ci.hi),
  num_groups = 2,
  legend_labels = c("White", "Black"),
  use_log_scale = T,
  smart_ticks = T,
  group_colors = c("#1B9E77", "#D95F02"),
  filename = "out/regression/ForestPlot_Univar_RACE_NHW_NHB",
  filetype = "pdf",
  filewidth = 16,
  fileheight = 18
)
## Forest plot for univar White vs Hispanic
make_forestplot(
  table_names = c("Variable", "White OR (95% CI)", "Hispanic OR (95% CI)"),
  table_values = data.frame(variable = uni.nhw$var_label,
                            white = uni.nhw$clean_out1,
                            hispanic = uni.his$clean_out1),
  mean_values = data.frame(white = uni.nhw$OR,
                           hispanic = uni.his$OR),
  ci.lo_values = data.frame(white=uni.nhw$or.ci.lo,
                            hispanic=uni.his$or.ci.lo),
  ci.hi_values = data.frame(white=uni.nhw$or.ci.hi,
                            hispanic=uni.his$or.ci.hi),
  num_groups = 2,
  legend_labels = c("White", "Hispanic"),
  use_log_scale = T,
  smart_ticks = T,
  group_colors = c("#1B9E77", "#7570B3"),
  filename = "out/regression/ForestPlot_Univar_RACE_NHW_HIS",
  filetype = "pdf",
  filewidth = 16,
  fileheight = 18
)


# Multivariable regressions for Death/ICU/Intubation -----------------------------------------------------------
fn <- "out/regression/multireg_temp.csv"
multireg_temp <- read.csv(fn)
multireg_race <- multireg_temp %>% 
  filter(var!="RACE_SIMPLE")

outcomes <- c("INHOSP_DEATH", "ICU_LVL", "INTUBATED")

covars <- c("AGE_CAT", "SEX", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
            "OBESITY_CLEAN", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")
covars.race <- c("AGE_CAT","SEX", "MANHATTAN", "ENC_TIME_PERIOD",
                 "OBESITY_CLEAN", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
                 "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
                 "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

for(o in outcomes){
  # Overall model
  model <- formula(paste0(o, "~", paste0(covars, collapse = "+")))
  mv <- glm(model, data = race.08, family = "binomial")
  output_logistic_reg_table(model=mv, fn=paste0("out/regression/LogisticMultiVar_",o, "_RACE_OVERALL.txt"), sigfigs=3)

  # Race-stratified models
  model <- formula(paste0(o, "~", paste0(covars.race, collapse = "+")))
  mv.nhw <- glm(model, data = race.08 %>% filter(RACE_SIMPLE=="NH White"), family = "binomial")
  output_logistic_reg_table(model=mv.nhw, fn=paste0("out/regression/LogisticMultiVar_", o, "_RACE_NHW.txt"), sigfigs=3)
  mv.nhb <- glm(model, data = race.08 %>% filter(RACE_SIMPLE=="NH Black"), family = "binomial")
  output_logistic_reg_table(model=mv.nhb, fn=paste0("out/regression/LogisticMultiVar_", o, "_RACE_NHB.txt"), sigfigs=3)
  mv.his <- glm(model, data = race.08 %>% filter(RACE_SIMPLE=="Hispanic"), family = "binomial")
  output_logistic_reg_table(model=mv.his, fn=paste0("out/regression/LogisticMultiVar_", o, "_RACE_HIS.txt"), sigfigs=3)
}



## Interaction models -- use Publish package instead
# int_df <- data.frame(var = character(0),
#                      interaction = character(0),
#                      p = numeric(0))
# data_list <- list(WB = race.08 %>% filter(RACE_SIMPLE %in% c("NH White", "NH Black")),
#                   WH = race.08 %>% filter(RACE_SIMPLE %in% c("NH White", "Hispanic"))
#                   )
# int_list <- list(WB = int_df,
#                  WH = int_df)
# for(d in names(data_list)){
#   data <- data_list[[d]]
#   data$RACE_SIMPLE <- factor(data$RACE_SIMPLE, levels = c("NH White", as.character(unique(data$RACE_SIMPLE)[unique(data$RACE_SIMPLE)!="NH White"])))
#   for(c in covars[covars != "RACE_SIMPLE"]){
#     model <-  formula(paste0("INHOSP_DEATH ~", paste0(covars, collapse = "+"), "+ RACE_SIMPLE*", c))
#     mv.int <- glm(model, data=data, family = "binomial")
#     fn <- paste0("out/regression/interactions/Logistic_Interaction_Race_", d, "_", c, ".txt")
#     output_logistic_reg_table(model=mv.int, fn=fn, sigfigs=3)
#     multi.int <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
#       filter(str_detect(var, ":")) %>%
#       select(interaction = var, p) %>% 
#       mutate(var = str_replace(interaction, ":RACE_SIMPLE.*$|RACE_SIMPLE.*:", "")) %>% 
#       select(var, interaction, p)
#     int_list[[d]] <- rbind(int_list[[d]], multi.int)
#   }
# }
# 
# multi.int.wb <- int_list$WB %>% 
#   full_join(
#     .,
#     multireg_race %>% select(var=level, var_label, order),
#     by=c("var")
#   ) %>% 
#   arrange(order) %>% 
#   mutate(p = case_when(is.na(p) ~ "Ref",
#                        !is.na(p) ~ as.character(p))) 
# 
# multi.int.wh <- int_list$WH %>% 
#   full_join(
#     .,
#     multireg_race %>% select(var=level, var_label, order),
#     by=c("var")
#   ) %>% 
#   arrange(order) %>% 
#   mutate(p = case_when(is.na(p) ~ "Ref",
#                        !is.na(p) ~ as.character(p))) 


# Assemble race-stratified multivar models into tables ----------------------------
fn <- "out/regression/multireg_temp_race_strat.csv"
multireg_temp_race <- read.csv(fn) %>% 
  select(var=level, var_label, order)

fn_outcomes <- c("INHOSP_DEATH", "ICU_LVL", "INTUBATED")
fn_race <- c("NHW", "NHB", "HIS")


mv_list <- as.list(NULL)
or_list <- as.list(NULL)
for(o in fn_outcomes){
  for(s in fn_race){
    fn <- paste0("out/regression/LogisticMultiVar_",o,"_RACE_",s,".txt")
    mv_list[[paste0(o,"_",s)]] <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
      mutate(OR = round(as.numeric(OR),2),
             or.ci.lo = round(as.numeric(or.ci.lo),2),
             or.ci.hi = round(as.numeric(or.ci.hi),2),
             clean_out = paste0(OR, " (", or.ci.lo, "-", or.ci.hi, ")")) %>% 
      select(var, clean_out)
    
    or_list[[paste0(o,"_",s)]] <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
      select(var, OR, or.ci.lo, or.ci.hi)
    colnames(or_list[[paste0(o,"_",s)]]) <- c("var", paste0(c("OR", "or.ci.lo", "or.ci.hi"), ".", s))
  }
  or_list[[o]] <- full_join(or_list[[paste0(o,"_NHW")]], or_list[[paste0(o,"_NHB")]], by="var") %>% 
    full_join(or_list[[paste0(o,"_HIS")]], by ="var")
}

mv_df <- mv_list[[1]] 
cnames <- c("var", names(mv_list)[1])
colnames(mv_df) <- cnames
for(i in 1:(length(mv_list)-1)){
  mv_df <- full_join(mv_df, mv_list[[i+1]], by="var")
  cnames <- cnames %>% append(names(mv_list)[i+1])
  colnames(mv_df) <- cnames
}

mv_race <- full_join(multireg_temp_race, mv_df %>% filter(var!="(Intercept)"), by="var") %>% 
  arrange(order) %>% 
  select(Variable = var_label, everything(), -c(var, order)) 
mv_race <- map_dfr(mv_race, function(x) case_when(is.na(x) ~ "Ref",
                                                !is.na(x) ~ x))


fn <- "out/regression/table_multivar_race_stratified.txt"
write.table(mv_race, fn, quote=F, sep="\t", row.names=F)


# Race-stratified Forest Plot ---------------------------------------------
## Continuation of the section above
for(o in fn_outcomes){
  df <- full_join(multireg_temp_race, or_list[[o]]%>% filter(var!="(Intercept)"), by="var") %>% 
    arrange(order) %>% 
    select(Variable = var_label, everything()) 
  
  df[4:12] <- map_dfr(df[4:12], function(x) case_when(is.na(x) ~ 1,
                                                    !is.na(x) ~ x))
  
  make_forestplot(
    table_names = c("Variable"),
    table_values = data.frame(variable = c(mv_race$Variable)),
    mean_values = data.frame(df$OR.NHW, df$OR.NHB, df$OR.HIS),
    ci.lo_values = data.frame(df$or.ci.lo.NHW, df$or.ci.lo.NHB, df$or.ci.lo.HIS),
    ci.hi_values = data.frame(df$or.ci.hi.NHW, df$or.ci.hi.NHB, df$or.ci.hi.HIS),
    use_log_scale = T,
    smart_ticks = T,
    xlab_text = "Odds Ratio (95% CI)",
    num_groups = 3, 
    legend_labels = c("White", "Black", "Hispanic"),
    group_colors = c("#1B9E77", "#D95F02", "#7570B3"),
    filename = paste0("out/regression/ForestPlot_Multivar_RaceStrat_",o),
    filetype = "pdf",
    filewidth = 6,
    fileheight = 12
  )
  
}

# Subgroup Analysis using Publish -----------------------------------------
library(Publish)
fn <- "out/regression/subgroup_temp_race.csv"
subgroup_temp_race <- read.csv(fn)

## To use function subgroupAnalysis:
## 1: name the subgroup variables
## 2: Make sure all subgroup variablse are factors
## 3: set up the model
## 4: Run subgroupAanlysis 

covars <- c("AGE_CAT", "SEX", "MANHATTAN", "ENC_TIME_PERIOD", "O2_LOW",
            "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE",
            "COPD_ASTHMA", "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE")

subgroups <- c("AGE_CAT", "SEX", "MANHATTAN", "ENC_TIME_PERIOD",
               "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
               "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE",
               "COPD_ASTHMA", "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE",
               "O2_LOW")

int.subgroups <- c("MANHATTAN", "SMOKING_HX", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
                   "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE",
                   "COPD_ASTHMA", "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE",
                   "O2_LOW", "ICU_LVL", "INTUBATED") ## Vars that you need to turn into factors

outcomes <- c("INHOSP_DEATH", "ICU_LVL", "INTUBATED")
race <- c("WHITE", "BLACK", "HISPANIC")
sub_list <- as.list(NULL)
for(o in outcomes){
  df <- race.08 %>%
    mutate(BLACK = case_when(RACE_SIMPLE == "NH Black" ~ 1,
                             RACE_SIMPLE != "NH Black" ~ 0),
           WHITE = case_when(RACE_SIMPLE == "NH White" ~ 1, 
                              RACE_SIMPLE != "NH White" ~ 0),
           HISPANIC = case_when(RACE_SIMPLE == "Hispanic" ~ 1,
                                RACE_SIMPLE != "Hispanic" ~ 0)) # Make the treatment variables 
  
  df[[o]] <- as.numeric(df[[o]]) #The outcome var must be numeric 
  df[,int.subgroups[int.subgroups!=o]] <- map_dfr(df[,int.subgroups[int.subgroups!=o]], as.factor) #make other vars into factors, except the outcome
  
  for(r in race){
    form <- formula(paste0(o, "~", paste0(c(r, covars), collapse = "+")))
    model <- glm(form, data = df, family = "binomial")
    sub.race <- subgroupAnalysis(model, data=df, treatment=r, subgroups=subgroups[subgroups!=o])
    
    sub.df <- left_join(subgroup_temp_race, sub.race, by=c("subgroups", "level")) %>% 
      mutate(num_pts = sample_0 + sample_1,
             clean_out = case_when(!is.na(OddsRatio) ~ paste0(round(OddsRatio,2), " (", round(Lower,2),"-",round(Upper,2) ,")"),
                                   is.na(OddsRatio) ~ "") 
             ) 
    sub_list[[paste0(o, "_", r)]] <- sub.df
    fn <- paste0("out/regression/ForestPlot_Subgroups_", r, "vsOTHERS_", o)
   
    
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
      xlab_text = paste0("Odds Ratio (95% CI)\n<-- Better --", r, "-- Worse -->"),
      filename = fn,
      filetype = "pdf",
      filewidth = 12,
      fileheight = 14
    ) 
    
  }
}


for(r in race){
  sub.df <- data.frame(Variable = sub_list[[paste0("INHOSP_DEATH_",r)]]$var_label,
                       N = sub_list[[paste0("INHOSP_DEATH_",r)]]$num_pts,
                       Death_p_interact = round(sub_list[[paste0("INHOSP_DEATH_",r)]]$pinteraction,2),
                       Death_OR = sub_list[[paste0("INHOSP_DEATH_",r)]]$clean_out,
                       Intubation_p_interact = round(sub_list[[paste0("INTUBATED_", r)]]$pinteraction,2),
                       Intubation_OR = sub_list[[paste0("INTUBATED_", r)]]$clean_out,
                       ICU_p_interact = round(sub_list[[paste0("ICU_LVL_", r)]]$pinteraction,2),
                       ICU_OR = sub_list[[paste0("ICU_LVL_", r)]]$clean_out
  )
  
  fn <- paste0("out/regression/table_subgroups_or_values_", r, ".txt")
  write.table(sub.df, fn, quote=F, sep="\t", row.names=F)
}


# Archive -----------------------------------------------------------------
###############

## Load and format the logistic regression results
fn <- "out/regression/LogisticMultiVar_RACE_OVERALL.txt" 
multi.race <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
  full_join(
    .,
    multireg_temp %>% select(var=level, var_label, order) %>% filter(!var %in% c("RACE_SIMPLEAsian", "RACE_SIMPLEOther")),
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

fn <- "out/regression/LogisticMultiVar_RACE_NHW.txt"
multi.nhw <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
  full_join(
    .,
    multireg_race %>% select(var=level, var_label, order),
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

fn <- "out/regression/LogisticMultiVar_RACE_NHB.txt"
multi.nhb <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
  full_join(
    .,
    multireg_race %>% select(var=level, var_label, order),
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

fn <- "out/regression/LogisticMultiVar_RACE_HIS.txt"
multi.his <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
  full_join(
    .,
    multireg_race %>% select(var=level, var_label, order),
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
  table_values = data.frame(variable = multi.race$var_label,
                            OR = multi.race$clean_out1),
  mean_values = multi.race$OR,
  ci.lo_values = multi.race$or.ci.lo,
  ci.hi_values = multi.race$or.ci.hi,
  num_groups = 1, 
  use_log_scale = T,
  smart_ticks = T,
  group_colors = "Black",
  filename = "out/regression/ForestPlot_Multivar_RACE_OVERALL",
  filetype = "pdf",
  filewidth = 11,
  fileheight = 11
)

## Forest plot for multivar by white-black
make_forestplot(
  table_names = c("Variable", "Interaction P-value"),
  table_values = data.frame(variable = multi.nhw$var_label,
                            interaction = multi.int.wb$p),
  mean_values = data.frame(white = multi.nhw$OR,
                           black = multi.nhb$OR),
  ci.lo_values = data.frame(white=multi.nhw$or.ci.lo,
                            black=multi.nhb$or.ci.lo),
  ci.hi_values = data.frame(white=multi.nhw$or.ci.hi,
                            black=multi.nhb$or.ci.hi),
  num_groups = 2,
  legend_labels = c("White", "Black"),
  use_log_scale = T,
  smart_ticks = T,
  group_colors = c("#1B9E77", "#D95F02"),
  filename = "out/regression/ForestPlot_Multivar_RACE_WB",
  filetype = "pdf",
  filewidth = 11,
  fileheight = 11
)

## Forest plot for multivar by white-hispanic
make_forestplot(
  table_names = c("Variable", "Interaction P-value"),
  table_values = data.frame(variable = multi.nhw$var_label,
                            interaction = multi.int.wh$p),
  mean_values = data.frame(white = multi.nhw$OR,
                           hispanic = multi.his$OR),
  ci.lo_values = data.frame(white=multi.nhw$or.ci.lo,
                            hispanic=multi.his$or.ci.lo),
  ci.hi_values = data.frame(white=multi.nhw$or.ci.hi,
                            hispanic=multi.his$or.ci.hi),
  num_groups = 2,
  legend_labels = c("White", "Hispanic"),
  use_log_scale = T,
  smart_ticks = T,
  group_colors = c("#1B9E77", "#7570B3"),
  filename = "out/regression/ForestPlot_Multivar_RACE_WH",
  filetype = "pdf",
  filewidth = 11,
  fileheight = 11
)

