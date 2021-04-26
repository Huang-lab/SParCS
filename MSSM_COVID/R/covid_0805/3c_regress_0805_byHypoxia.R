#3c_regress_0805_byHypoxia.R
## Tomi Jun @ 8/2020
library(tidyverse)
library(RColorBrewer)
source("../../../../../../../OneDrive/PROJECTS/CODING/R/tomi_functions.R")
source("0805_functions.R")

# Data --------------------------------------------------------------------
#adm.08 from 1_prepare_0805_cohort.R

o2.08 <- adm.08 %>% 
  filter(!is.na(O2_SAT))

fn <- "out/regression/reg_temp.csv"
reg_temp <- read.csv(fn) 

# Univariable regression for inhosp-death ---------------------------------

uni.o2 <- do_univar_logistic_reg(
  data = o2.08,
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

uni.low <- do_univar_logistic_reg(
  data = o2.08 %>% filter(O2_LOW==1),
  vars = unique(reg_temp$var)[!unique(reg_temp$var)%in% c("O2_LOW", "O2_CAT")],
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

uni.wnl <- do_univar_logistic_reg(
  data = o2.08 %>% filter(O2_LOW==0),
  vars = unique(reg_temp$var)[!unique(reg_temp$var)%in% c("O2_LOW", "O2_CAT")],
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
  table_values = data.frame(variable = uni.o2$var_label,
                            OR = uni.o2$clean_out1),
  mean_values = uni.o2$OR,
  ci.lo_values = uni.o2$or.ci.lo,
  ci.hi_values = uni.o2$or.ci.hi,
  num_groups = 1, 
  use_log_scale = T,
  smart_ticks = T,
  group_colors = "Black",
  filename = "out/regression/ForestPlot_Univar_O2_All",
  filetype = "pdf",
  filewidth = 14,
  fileheight = 18
)

## Forest plot for univar by O2
make_forestplot(
  table_names = c("Variable", "Hypoxic OR (95% CI)", "Normoxic OR (95% CI)"),
  table_values = data.frame(variable = uni.low$var_label,
                            hypoxic = uni.low$clean_out1,
                            normoxic = uni.wnl$clean_out1),
  mean_values = data.frame(hypoxic = uni.low$OR,
                           normoxic = uni.wnl$OR),
  ci.lo_values = data.frame(hypoxic=uni.low$or.ci.lo,
                            normoxic=uni.wnl$or.ci.lo),
  ci.hi_values = data.frame(hypoxic=uni.low$or.ci.hi,
                            normoxic=uni.wnl$or.ci.hi),
  num_groups = 2,
  legend_labels = c("Hypoxic", "Normoxic"),
  use_log_scale = T,
  smart_ticks = T,
  group_colors = c("#A6CEE3", "#1F78B4"),
  filename = "out/regression/ForestPlot_Univar_byO2",
  filetype = "pdf",
  filewidth = 16,
  fileheight = 18
)



# Multivariable regression for inhosp-death -------------------------------
fn <- "out/regression/multireg_temp.csv"
multireg_temp <- read.csv(fn)
multireg_o2 <- multireg_temp %>% 
  filter(var!="O2_CAT")

covars <- c("AGE_CAT", "SEX", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
            "SMOKING_SIMPLE","BMI_CAT", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
            "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
            "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE", "O2_LOW")

model <- formula(paste0("INHOSP_DEATH ~", paste0(covars, collapse = "+")))
mv <- glm(model, data = o2.08, family = "binomial")
output_logistic_reg_table(model=mv, fn="out/regression/LogisticMultiVar_O2_ALL.txt", sigfigs=3)

covars.o2 <- c("AGE_CAT", "SEX", "RACE_SIMPLE", "MANHATTAN", "ENC_TIME_PERIOD",
                "SMOKING_SIMPLE", "BMI_CAT", "HTN", "DIABETES", "CORONARY_ARTERY_DISEASE",
                "HEART_FAILURE", "ATRIAL_FIBRILLATION", "CHRONIC_KIDNEY_DISEASE", 
                "COPD_ASTHMA", "CANCER_FLAG", "CHRONIC_LIVER_DISEASE")
model <- formula(paste0("INHOSP_DEATH ~", paste0(covars.o2, collapse = "+")))

mv.low <- glm(model, data = o2.08 %>% filter(O2_LOW==1), family = "binomial")
output_logistic_reg_table(model=mv.low, fn="out/regression/LogisticMultiVar_O2_LOW.txt", sigfigs=3)

mv.wnl <- glm(model, data = o2.08 %>% filter(O2_LOW==0), family = "binomial")
output_logistic_reg_table(model=mv.wnl, fn="out/regression/LogisticMultiVar_O2_WNL.txt", sigfigs=3)

## Interaction models 
int_df <- data.frame(var = character(0),
                     interaction = character(0),
                     p = numeric(0))
for(c in covars[covars != "O2_LOW"]){
  model <-  formula(paste0("INHOSP_DEATH ~", paste0(covars, collapse = "+"), "+ O2_LOW*", c))
  mv.int <- glm(model, data=o2.08, family = "binomial")
  output_logistic_reg_table(model=mv.int, fn=paste0("out/regression/interactions/Logistic_Interaction_O2_", c, ".txt"), sigfigs=3)
  fn <- paste0("out/regression/interactions/Logistic_Interaction_O2_", c, ".txt")
  multi.int <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
    filter(str_detect(var, ":")) %>%
    select(interaction = var, p) %>% 
    mutate(var = str_replace(interaction, ":O2_LOW|O2_LOW:", "")) %>% 
    select(var, interaction, p)
  int_df <- rbind(int_df, multi.int)
}

multi.int <- int_df %>% 
  full_join(
    .,
    multireg_o2 %>% select(var=level, var_label, order),
    by=c("var")
  ) %>% 
  arrange(order) %>% 
  mutate(p = case_when(is.na(p) ~ "Ref",
                       !is.na(p) ~ as.character(p))) 



## Load and format the logistic regression results
fn <- "out/regression/LogisticMultiVar_O2_ALL.txt" 
multi.o2 <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
  full_join(
    .,
    multireg_o2 %>% select(var=level, var_label, order),
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


fn <- "out/regression/LogisticMultiVar_O2_LOW.txt"
multi.low <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
  full_join(
    .,
    multireg_o2 %>% select(var=level, var_label, order),
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

fn <- "out/regression/LogisticMultiVar_O2_WNL.txt"
multi.wnl <- read.table(fn, header=T, sep="\t", quote="", stringsAsFactors = F) %>% 
  full_join(
    .,
    multireg_o2 %>% select(var=level, var_label, order),
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
  table_values = data.frame(variable = multi.o2$var_label,
                            OR = multi.o2$clean_out1),
  mean_values = multi.o2$OR,
  ci.lo_values = multi.o2$or.ci.lo,
  ci.hi_values = multi.o2$or.ci.hi,
  num_groups = 1, 
  use_log_scale = T,
  smart_ticks = T,
  group_colors = "Black",
  filename = "out/regression/ForestPlot_Multivar_O2_ALL",
  filetype = "pdf",
  filewidth = 11,
  fileheight = 11
)

## Forest plot for multivar by O2
make_forestplot(
  table_names = c("Variable", "Interaction P-value"),
  table_values = data.frame(variable = multi.low$var_label,
                            interaction = multi.int$p),
  mean_values = data.frame(hypoxic = multi.low$OR,
                           normoxic = multi.wnl$OR),
  ci.lo_values = data.frame(hypoxic=multi.low$or.ci.lo,
                            normoxic=multi.wnl$or.ci.lo),
  ci.hi_values = data.frame(hypoxic=multi.low$or.ci.hi,
                            normoxic=multi.wnl$or.ci.hi),
  num_groups = 2,
  legend_labels = c("Hypoxic", "Normoxic"),
  use_log_scale = T,
  smart_ticks = T,
  group_colors = c("#A6CEE3", "#1F78B4"),
  filename = "out/regression/ForestPlot_Multivar_byO2",
  filetype = "pdf",
  filewidth = 11,
  fileheight = 11
)

