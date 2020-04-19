#### Conduct single-variable regression on MSSM covid data ####

source("load_mssm_covid_data.R")
source("stat_function.R")
source("aesthetics_function.R")

# A quick logistic regression test, COVID_RESULT ~ each single variable

tt=NULL
for (variable in colnames(covid_main_20200406wResults)[-c(1,2)]){
  results = run_glm(data = covid_main_20200406wResults, xi = variable, yi="COVID_RESULT_BINARY", ytype = "Binary")
  tt = rbind(tt, results)
}

tt$FDR = p.adjust(tt[,"Pr(>Chi)"], method="fdr") 
tt=tt[order(tt$FDR, decreasing=FALSE),]


write.table(tt, quote=F, sep="\t", file = "out/covid_main_20200406wResults_logisticRegSingleVar.tsv", row.names = F)


# Quick peak into the results
cat("Top 10 associated variables in data:\n")
head(tt[1:10,])

p = ggplot(data=tt, aes(y=-log10(FDR), x=coeff, color = coeff>0))
p = p + geom_point(alpha=0.4) + theme_bw()
p = p + geom_text_repel(aes(label=ifelse(-log(FDR)>100,NA,as.character(xi))),size=2)
p = p + scale_y_log10()
p
ggsave(file = "out/20200406_COVID19_results_logisticRegSingleVar_volcano.pdf",useDingbat=F)