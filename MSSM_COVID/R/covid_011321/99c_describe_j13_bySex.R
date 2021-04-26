## 99c_describe_j13_bySex.R
## Tomi Jun @ 2/2021

library(tidyverse)
source("../../../../../../../OneDrive/PROJECTS/CODING/R/tomi_functions.R")

# Data --------------------------------------------------------------------
# adm.j13 from 99a -- This is the dataframe from Aug 2020 to Jan 2021
# adm.113 from 99a -- This is the dataframe from the beginning of the pandemic to Jan 2021


fn <- "out/descriptive/t1_template.csv"
t1_temp <- read.csv(fn)

fn <- "out/descriptive/t1_temp_demo.csv"
t1_temp_demo <- read.csv(fn)


# Describe j13 Demographics by Sex ---------------------------------------------------------
unique(t1_temp$var)[!unique(t1_temp$var) %in% colnames(adm.j13)]

desc_sex_all <- describe_vars(data = adm.j13, 
                              var_names = unique(t1_temp$var), 
                              cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                              cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                              cont_mode = "iqr") %>% 
  filter(level!="0") 
desc_sex_m <- describe_vars(data = adm.j13 %>% filter(SEX=="MALE"), 
                            var_names = unique(t1_temp$var), 
                            cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                            cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                            cont_mode = "iqr") %>% 
  filter(level!="0")
desc_sex_f <- describe_vars(data = adm.j13 %>% filter(SEX=="FEMALE"), 
                            var_names = unique(t1_temp$var), 
                            cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                            cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                            cont_mode = "iqr") %>% 
  filter(level!="0") 
comp_mf <- compare_desc_vars(df1 = adm.j13 %>% filter(SEX=="MALE"), 
                             df2 = adm.j13 %>% filter(SEX=="FEMALE"), 
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

fn <- "out/descriptive/T1_bySex.txt"
write.table(t1_sex, fn, quote=F, sep="\t", row.names=F)

fn <- "out/descriptive/T1_Demographics_bySex.txt"
write.table(t1_demo, fn, quote=F, sep="\t", row.names=F)

# Describe j13 vs 113 Demographics ---------------------------------------------------------
unique(t1_temp$var)[!unique(t1_temp$var) %in% colnames(adm.113)]

desc_113_all <- describe_vars(data = adm.113, 
                              var_names = unique(t1_temp$var), 
                              cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                              cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                              cont_mode = "iqr") %>% 
  filter(level!="0") 
desc_pre_j13 <- describe_vars(data = adm.113 %>% filter(ENC_TIME_PERIOD != "Jan"), 
                            var_names = unique(t1_temp$var), 
                            cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                            cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                            cont_mode = "iqr") %>% 
  filter(level!="0")
desc_post_j13 <- describe_vars(data = adm.113 %>% filter(ENC_TIME_PERIOD=="Jan"), 
                            var_names = unique(t1_temp$var), 
                            cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique(), 
                            cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(), 
                            cont_mode = "iqr") %>% 
  filter(level!="0") 
comp_j13 <- compare_desc_vars(df1 = adm.113 %>% filter(ENC_TIME_PERIOD != "Jan"), 
                             df2 = adm.113 %>% filter(ENC_TIME_PERIOD == "Jan"), 
                             var_names = unique(t1_temp$var),
                             cat_vars = t1_temp %>% filter(var_type=="CAT") %>% pull(var) %>% unique(),
                             cont_vars = t1_temp %>% filter(var_type=="CONT") %>% pull(var) %>% unique()
) 
## Stitch these together 
t1_time <- full_join(desc_pre_j13 %>% select(variable, level, output_value), 
                    desc_post_j13 %>% select(variable, level, output_value), 
                    by=c("variable", "level"), 
                    suffix = c(".pre", ".post")) %>% 
  full_join(., 
            comp_j13 %>% select(variable, level, pvalue), 
            by=c("variable", "level"),) %>% 
  full_join(., 
            desc_113_all %>% select(variable, level, output_value, na_pct), 
            by=c("variable", "level"),) %>% 
  select(variable, level, Aug = output_value.pre, Jan = output_value.post, pvalue, overall = output_value, na_pct) %>% 
  full_join(., 
            t1_temp %>% select(variable = var, level, var_label, order),
            by=c("variable", "level")) %>% 
  mutate(pvalue = as.numeric(pvalue),
         p_simple = case_when(pvalue<0.001 ~ "<0.001",
                              pvalue>=0.001 ~ as.character(signif(pvalue,1)))) %>% 
  arrange(order) %>% 
  select(variable, level, var_label, Aug, Jan, p_simple, overall, na_pct, order)

t1_j13 <- inner_join(t1_time %>% select(-order), t1_temp_demo %>% select(variable = var, level, order), by=c("variable", "level")) %>% arrange(order)

fn <- "out/descriptive/T1_AllVars_by_0805_0113_Cohorts.txt"
write.table(t1_time, fn, quote=F, sep="\t", row.names=F)

fn <- "out/descriptive/T1_Demographics_by_0805_0113_cohorts.txt"
write.table(t1_j13, fn, quote=F, sep="\t", row.names=F)

