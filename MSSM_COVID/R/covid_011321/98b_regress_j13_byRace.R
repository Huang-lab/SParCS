##98b_regress_j13_byRace.R
##Tomi Jun @ 3/2021

library(tidyverse)


# Load data ---------------------------------------------------------------

# fn <- "out/cohorts/ADMITTED_COVID_ADULTS_0805_TO_1113_BLACK_WHITE_HISP.txt"
# race.j13 <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F)


fn <- "out/regression/multireg_temp_race_j13.csv"
multireg_race <- read.csv(fn)



# Regression --------------------------------------------------------------
## Will use AGE instead of AGE_CAT because there were very few deaths in certain AGE_CAT when stratifying by race

covars <- c("AGE", "SEX", "RACE_SIMPLE", "MANHATTAN",
            "OBESITY_CLEAN", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

outcomes <- c("INHOSP_DEATH", "ICU_LVL", "INTUBATED")

races <- c("NH White", "NH Black", "Hispanic")

mv <- as.list(NULL)
for(o in outcomes){
  
  model <- formula(paste0(o, "~", paste0(covars, collapse = "+")))
  mv[["ALL"]] <- glm(model, data = race.j13, family = "binomial")
  output_logistic_reg_table(model=mv[["ALL"]], fn=paste0("out/regression/race/LogisticMultiVarRace_", o, "_ALL.txt"), sigfigs=3)
  
  for(r in races){
    r_abrv <- str_replace(r, "\\s", "") %>% str_extract(., "\\w\\w\\w") %>% str_to_upper
    model <- formula(paste0(o, "~", paste0(covars[covars!="RACE_SIMPLE"], collapse="+")))
    data <- race.j13 %>% 
      filter(RACE_SIMPLE==r)
    
    mv[[r_abrv]] <- glm(model, data = data, family = "binomial")
    output_logistic_reg_table(model=mv[[r_abrv]], fn=paste0("out/regression/race/LogisticMultiVarRace_", o, "_", r_abrv,".txt"), sigfigs=3)

  }
}


# Assemble race-stratified multivar models into tables ----------------------------

fn_races <- c("NHW", "NHB", "HIS", "ALL")

mv_list <- as.list(NULL)
or_list <- as.list(NULL)
for(o in outcomes){
  for(r in fn_races){
    o_r <- paste0(o,"_",r)
    
   fn <- paste0("out/regression/race/LogisticMultiVarRace_", o_r,".txt")
   
   mv_list[[o_r]] <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
     select(var, clean_out1)
   
   or_list[[o_r]] <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
     select(var, OR, or.ci.lo, or.ci.hi)
   colnames(or_list[[o_r]]) <- c("var", paste0(c("OR", "or.ci.lo", "or.ci.hi"), ".", r))
   
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

mv_race <- full_join(multireg_race %>% select(var=level, var_label, order), 
                     mv_df %>% filter(var!="(Intercept)"), by="var") %>% 
  arrange(order) %>% 
  select(Variable = var_label, everything(), -c(var, order)) 
mv_race <- map_dfr(mv_race, function(x) case_when(is.na(x) ~ "Ref",
                                                  !is.na(x) ~ x))


fn <- "out/regression/race/table_multivar_race_stratified_j13.txt"
write.table(mv_race, fn, quote=F, sep="\t", row.names=F)


# Race-stratified Forest Plot ---------------------------------------------
## Continuation of the section above

multireg_race_sans <- multireg_race %>% 
  filter(var!="RACE_SIMPLE")

for(o in outcomes){
  df <- full_join(multireg_race_sans %>% select(var=level, var_label, order), 
                  or_list[[o]]%>% filter(var!="(Intercept)"), by="var") %>% 
    arrange(order) %>% 
    select(Variable = var_label, everything()) 
  
  df[4:12] <- map_dfr(df[4:12], function(x) case_when(is.na(x) ~ 1,
                                                      !is.na(x) ~ x))
  
  
  make_forestplot(
    table_names = c("Variable"),
    table_values = data.frame(variable = c(df$Variable)),
    mean_values = data.frame(df$OR.NHW, df$OR.NHB, df$OR.HIS),
    ci.lo_values = data.frame(df$or.ci.lo.NHW, df$or.ci.lo.NHB, df$or.ci.lo.HIS),
    ci.hi_values = data.frame(df$or.ci.hi.NHW, df$or.ci.hi.NHB, df$or.ci.hi.HIS),
    use_log_scale = T,
    smart_ticks = T,
    xlab_text = "Odds Ratio (95% CI)",
    num_groups = 3, 
    legend_labels = c("White", "Black", "Hispanic"),
    group_colors = c("#1B9E77", "#D95F02", "#7570B3"),
    filename = paste0("out/regression/race/ForestPlot_Multivar_RaceStrat_j13_",o),
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


covars <- c("AGE_65", "SEX", "MANHATTAN", "O2_LOW",
            "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE",
            "COPD_ASTHMA", "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE")

subgroups <- c("AGE_65", "SEX", "MANHATTAN", 
               "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
               "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE",
               "COPD_ASTHMA", "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE",
               "O2_LOW")

int.subgroups <- c("AGE_65", "MANHATTAN", "SMOKING_HX", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
                   "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE",
                   "COPD_ASTHMA", "OBESITY_CLEAN", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE",
                   "O2_LOW", "ICU_LVL", "INTUBATED") ## Vars that you need to turn into factors

outcomes <- c("INHOSP_DEATH", "ICU_LVL", "INTUBATED")
race <- c("WHITE", "BLACK", "HISPANIC")
sub_list <- as.list(NULL)
for(o in outcomes){
  df <- race.j13 %>%
    mutate(AGE_65 = case_when(AGE>65 ~ 1, 
                              AGE<=65 ~ 0),
           BLACK = case_when(RACE_SIMPLE == "NH Black" ~ 1,
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
    fn <- paste0("out/regression/race/ForestPlot_Subgroups_", r, "vsOTHERS_", o)
    
    
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
  
  fn <- paste0("out/regression/race/table_subgroups_or_values_", r, ".txt")
  write.table(sub.df, fn, quote=F, sep="\t", row.names=F)
}


