## 4a_labs_0805_bySex.R
## Tomi Jun @ 9/2020
## Examine labs by sex and their association with outcomes 

library(tidyverse)
library(ggpubr)
source("../../../../../../../OneDrive/PROJECTS/CODING/R/tomi_functions.R")

# Data --------------------------------------------------------------------
#adm.08 from 1_prepare_0805_cohort.R
#t1_temp loaded in 2_describe_0805.R

lab_vars <- c("WBC", "HGB", "PLT", "NEUTRO_EST", "LYMPHS_EST", "MONO_EST",
              "BASO_EST", "EOS_EST", "SODIUM", "POTASSIUM", "CHLORIDE", 
              "BUN", "CREATININE", "AST", "ALT", "TBILI", "ALBUMIN", "PT", "PTT",
              "INR", "FIBRINOGEN", "DDIMER", "FERRITIN", "FERRITIN_ULN", 
              "PROCALCITONIN", "LDH", "CRP", "BNP", "TROPONIN", "ESR", "IL1B", 
              "IL6", "IL8", "TNFa")

inflamm_labs <- c("FIBRINOGEN", "DDIMER", "FERRITIN", "FERRITIN_ULN", 
                  "PROCALCITONIN", "LDH", "CRP")

# Set-up missingness dataframe ----------------------------------------------------------
## The key thing is to set up the dataframe mll
## mll has the missingness of each lab by facility

## Which sites routinely checked labs? 
missing_labs_list <- as.list(NULL)
for(f in unique(adm.08$FACILITY)){
  missing_labs_list[[f]] <- adm.08 %>% 
    filter(FACILITY== f) %>% 
    select(all_of(lab_vars)) %>%  
    mutate(num_pts = n()) %>% 
    pivot_longer(cols = all_of(lab_vars), names_to = "lab", values_to = "value") %>% 
    group_by(lab) %>% 
    summarise(num_pts = unique(num_pts),
              missing = sum(is.na(value)),
              missing_pct = missing/num_pts) %>% 
    arrange(missing_pct)
}

mll <- bind_rows(missing_labs_list, .id="FACILITY") %>% 
  filter(!FACILITY %in% c("MS 1160 5TH AVE", "MOUNT SINAI BI PETRIE"))
lab_lvls <- mll %>% 
  group_by(lab) %>% 
  arrange(desc(missing_pct), .by_group = T) %>% 
  slice(1) %>% 
  arrange(missing_pct) %>% 
  pull(lab)
#Visualize missingness of labs by facility
p <- ggplot(data = mll, mapping = aes(x=factor(lab, levels=lab_lvls), fill=FACILITY, y=missing_pct)) +
  geom_col(position=position_dodge()) +
  theme_bw() +
  theme_readable(legend_pos = "top", x_angle=45)
p


# Male Female Boxplots ----------------------------------------------------
for(l in lab_vars){
  cat(paste0(l,"."))
  sites <- NULL
  sites <- mll %>% 
    filter(missing_pct<0.2) %>% 
    filter(lab==l) %>% 
    pull(FACILITY)
  if(l %in% c("IL1B", "IL6", "IL8", "TNFa")){sites <- "THE MOUNT SINAI HOSPITAL"}
  if(is_empty(sites)){next}
  df <- adm.08 %>% 
    filter(FACILITY %in% sites) %>% 
    filter(!is.na(get(l))) 
  
  form <- formula(paste0(l, "~SEX"))
  wilcox <- wilcox.test(form, data=df)

  ## Output boxplots
  var_label <- t1_temp %>% 
    filter(var==l) %>% 
    pull(var_label)
  
  p <- ggplot(data=df, mapping=aes(x=SEX, y=get(l), fill=SEX)) +
    geom_violin()+
    geom_boxplot(width =0.2)+
    labs(x="", y=var_label) +
    stat_compare_means(comparisons=list(c("FEMALE", "MALE")),
                       hide.ns=F, label="p.signif"
    ) +
    theme_bw()+
    theme_readable(no_legend = T, x_angle=45) +
    scale_fill_manual(values=c("#E31A1C", "#1F78B4"))
  
  if(max(df[l], na.rm=T)/min(df[l], na.rm=T) > 10){
    p <- p + scale_y_log10()
  }
  
  p
  
  if(wilcox$p.value<0.05){
    fn <- paste0("out/labs/sex/sig_difference/Boxplot_Sex_", l, ".png")
  }else{
    fn <- paste0("out/labs/sex/Boxplot_Sex_", l, ".png")
  }
  ggsave(fn, h=5, w=4)
}


# Linear regression: Sex * Hypoxia --------------------------------------------
## The question is whether Sex and Hypoxia interact wrt any of the baseline labs
## For labs where there is an interaciton, it may be worth examining whether that lab associates with outcome
## Use a threshold of 20% missingness (only use facilities that had <20% of the lab value missing)
## Make boxplots too (why not)

results_list <- as.list(NULL)
i=1
for(l in lab_vars){
  cat(paste0(l,"."))
  l_std <- paste0(l,"_STD") 
  sites <- NULL
  sites <- mll %>% 
    filter(missing_pct<0.2) %>% 
    filter(lab==l) %>% 
    pull(FACILITY)
  if(l %in% c("IL1B", "IL6", "IL8", "TNFa")){sites <- "THE MOUNT SINAI HOSPITAL"}
  if(is_empty(sites)){next}
  df <- adm.08 %>% 
    filter(FACILITY %in% sites) %>% 
    filter(!is.na(get(l))) %>%
    filter(!is.na(O2_LOW)) %>% 
    mutate(SEX_O2 = case_when(SEX=="MALE" & O2_LOW == 1 ~ "Male, <92%", 
                              SEX=="MALE" & O2_LOW == 0 ~ "Male, >=92%",
                              SEX=="FEMALE" & O2_LOW == 1 ~ "Female, <92%",
                              SEX=="FEMALE" & O2_LOW == 0 ~ "Female, >=92%"))
  
  ## Output linear regression results -- Use Continuouse O2_SAT instead of O2_LOW?
  num_pts <- nrow(df)
  form <- formula(paste0(l_std, "~ SEX * O2_SAT"))
  model <- lm(form, data=df)
  summ <- as.data.frame(summary(model)$coefficients)
  colnames(summ) <- c("coef", "se", "t", "p")
  summ <- summ %>% 
    select(coef, p)
  interaction_p <- summ["SEXMALE:O2_SAT", "p"]
  results_list[[i]] <- unlist(c(lab=l_std, n=num_pts, summ["SEXMALE", ], 
                         summ["O2_SAT", ], summ["SEXMALE:O2_SAT", ]))
  i=i+1
  
  ## Output boxplots
  var_label <- t1_temp %>% 
    filter(var==l) %>% 
    pull(var_label)
  
  p <- ggplot(data=df, mapping=aes(x=SEX_O2, y=get(l), fill=SEX_O2)) +
    geom_violin()+
    geom_boxplot(width =0.2)+
    labs(x="", y=var_label) +
    stat_compare_means(comparisons=list(c("Female, <92%", "Female, >=92%"),
                                        c("Male, <92%", "Male, >=92%"),
                                        c("Female, <92%", "Male, <92%"),
                                        c("Female, >=92%", "Male, >=92%")),
                       hide.ns=T, label="p.signif"
                       ) +
    theme_bw()+
    theme_readable(no_legend = T, x_angle=45) +
    scale_fill_manual(values=c("#FB9A99", "#E31A1C", "#A6CEE3", "#1F78B4"))
  
  if(max(df[l], na.rm=T)/min(df[l], na.rm=T) > 10){
    p <- p + scale_y_log10()
  }
  
  p
  
  if(interaction_p<0.05){
    fn <- paste0("out/labs/sex_o2/sig_interaction/Boxplot_Sex_O2_", l, ".png")
  }else{
    fn <- paste0("out/labs/sex_o2/Boxplot_Sex_O2_", l, ".png")
  }
  ggsave(fn, h=5, w=4)
}

linreg_df <- as.data.frame(do.call(rbind, results_list), stringsAsFactors = F)
colnames(linreg_df) <- c("lab", "n", "coef.male", "p.male", "coef.o2", "p.o2", "coef.int", "p.int")
linreg_df[3:8] <- map_dfr(linreg_df[3:8], as.numeric)
linreg_df[3:8] <- map_dfr(linreg_df[3:8], signif, 3)


# Pearson correlation matrix of inflammatory labs ------------------------
library(Hmisc)
# infl <- adm.08 %>% select(all_of(inflamm_labs), ALBUMIN, WBC)
# infl <- adm.08 %>% select(all_of(lab_vars))
infl <- adm.08 %>% filter(FACILITY == "THE MOUNT SINAI HOSPITAL") %>% select(WBC, CRP, FERRITIN, DDIMER, LDH, PROCALCITONIN)
cor <- rcorr(as.matrix(infl))
cor_df <- flattenCorrMatrix(cor$r, cor$P) 


