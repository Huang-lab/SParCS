# stat_functions.R

##### multivariate regression wrapper #####

run_glm = function(data=NULL, xi = "", yi = "", covi=NULL, ytype="Continuous") {
  row_stat=NULL
  
  ## Check data input
  cat(paste("Processing: yi =", yi, " xi =", xi, " covi =", covi, "\n") )
  
  ## determine the formula, ie covariates to be included
  covars = ""
  # only add covariates if they have more than one level in the data subset
  for (covar in covi){ 
    if (sum(!is.na(unique(data[,covar])))>1){
      covars = paste(covars,covar,sep="+")
    } 
  }
  if (covars != ""){
    model=formula(paste(yi,"~",xi,covars))
  } else{
    model=formula(paste(yi,"~",xi))
  }
  
  test = ""
  ## fit the model
  if (ytype=="Continuous") {
    glmfit= try(glm(formula=model,data=data,family=gaussian(link = "identity")))
    test = "F" # F test
  } else if (ytype=="Binary") {
    glmfit= try(glm(formula=model,data=data,family=binomial(link = "logit")))
    test = "Chisq" # Chi Square test
  } else {
    stop("Unknown model ytype ", ytype)
  }
  
  if(class(glmfit)[1] == "try-error") {
    cat(paste("    Error caught fitting glm, continuing.  yi =", yi, " xi =", xi, " covi =", covi, "\n") )
    next
  }
  
  # retrieve coefficient
  if (length(names(coefficients(glmfit)))>1 & xi %in% names(coefficients(glmfit))){ 
    coeff = coefficients(glmfit)[[xi]]
  } else if (length(names(coefficients(glmfit)))>1) { # for binary the names become [xi][level1ofxi]
    coeff = coefficients(glmfit)[[2]]
  } else {
    coeff = NA
  }
  
  ## ANOVA
  fit = try(anova(glmfit,test=test))
  if(class(fit)[1] == "try-error") {
    cat(paste("    Error caught conducting ANOVA, continuing.  yi =", yi, " xi =", xi, " covi =", covi, "\n") )
    next
  } else {
    fit=as.matrix(fit)
    if (xi %in% rownames(fit)) (row_stat = cbind(yi,ytype,xi,as.data.frame(t(fit[xi,])),coeff,covars))
  }
  return(row_stat)
  
}