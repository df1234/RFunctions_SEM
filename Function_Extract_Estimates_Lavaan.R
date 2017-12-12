################################################################################
##################### FUNCTION FOR EXTRACTING ESTIMATES  #######################
######################### FROM FITTED LAVAAN OBJECT ############################
################################################################################

# Input: x - fitted lavaan object, e.g. fit <- cfa(HS.model, data=HolzingerSwineford1939)
#        component - "=~" (LVs), "~" (regressions), "~~" (variances) or "~1" (intercepts)
#        name - string used in name of output file , e.g. "HS.model"
# Output: a .txt file containing a table of relevant values.
#         Can be copied and pasted into Word -> use convert text to table function
#         Btw. Excel will destory the formatting.
# Invoked as "table.estimates(x, component, name)"


table.estimates <- function(x, component, name) {
  
  options("scipen"=100, "digits"=8) # prevent exponential notation
  
  # extract estimates
  estall = data.frame(parameterEstimates(x, standardized=T))
  
  # subset component specified by user
  est = subset(estall, op == component)
  
  # format values
  est$est = sprintf("%.2f", round(est$est, 2)) # round 2 to digits
  est$se = sprintf("%.2f", round(est$se, 2)) # round 2 to digits
  est$z = sprintf("%.2f", round(est$z, 2)) # round 2 to digits
  est$std.all = sprintf("%.2f", round(est$std.all, 2))
  est$pvalue = rd(est$pvalue, digits = 3) # round 3 to digits, remove leading 0
  est$pvalue[grepl(".000", est$pvalue, ignore.case=F)] <- "< .001" # if smaller than .001 write "< .001"
  
  # remove unneccessary columns
  est$ci.lower = NULL
  est$ci.upper = NULL
  est$std.lv = NULL
  est$std.nox = NULL

  # get file name, using userinput
  file.name = paste(c("table_estimates_", name,".txt"),
                    collapse = "")
  
  # write table in current directory
  write.table(est, file = file.name, sep = ",", quote = F, row.names = F)
  
  return("Table of estimates written in current directory")
}
