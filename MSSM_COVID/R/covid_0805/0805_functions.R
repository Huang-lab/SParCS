##0805_functions.R
## Tomi Jun @ 08/2020
## Utility functions

prep_logistic_for_forest <- function(data = NULL, template = NULL, by_vars = NULL){
  ## Convenience function to prep logistic regression tables for forest plotting
  ## data: A dataframe 
  ## template: A template to organize the dataframe
  ## by_vars: A vector of the variables used to merge the data and the template 
  df <- data %>% 
    full_join(
      .,
      template %>% select(var, level, var_label, order),
      by=by_vars
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
  
  
  
}