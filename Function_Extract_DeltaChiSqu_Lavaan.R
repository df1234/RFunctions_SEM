################################################################################
##################### FUNCTION FOR EXTRACTING DELTA CHI SQU ####################
############################ FOR MODEL COMPARISONS #############################
################################################################################

# Input: x1, x2 - fitted lavaan objects to be compared, e.g. fit <- cfa(HS.model, data=HolzingerSwineford1939)
#        component - "=~" (LVs), "~" (regressions), "~~" (variances) or "~1" (intercepts)
#        name - string used in name of output file , e.g. "HS.model"
# Output: a characters tring containing ????2 statisics
# Invoked as "modelcomp.deltachisqu(x1, x2)"

modelcomp.deltachisqu <- function(x1, x2) {
  
  # check whether weights is installed
  is.installed <- function(mypkg){
    is.element(mypkg, installed.packages()[,1])
  } 
  
  if (!is.installed("weights")){
    install.packages("weights")
  }
  
  require(weights)
  
  options("scipen"=100, "digits"=8) # prevent exponential notation
  
  # Compare models
  ano = data.frame(anova(x1, x2))[2,]
  
  # determine whether p smaller than .001
  if (ano$Pr..Chisq. < 0.001) {
    sign = "< "
  } else {
    sign = "= "
  }
  
  # determine whether p is 1
  if (ano$Pr..Chisq. > 0.9994) {
    sign = "> "
  }
  
  # format values
  ano$Chisq.diff = sprintf("%.2f", round(ano$Chisq.diff, 2)) # round 2 to digits
  ano$Pr..Chisq. = rd(ano$Pr..Chisq., digits = 3) # round 3 to digits, remove leading 0
  if (ano$Pr..Chisq. == "1.000") {
    ano$Pr..Chisq. <- ".999"
  }
  ano$Pr..Chisq.[grepl(".000", ano$Pr..Chisq., ignore.case=F)] <- ".001" # if smaller than .001 write "< .001"

  # coerce into character
  out = paste(c("dX2(", ano$Df.diff, ") = ", ano$Chisq.diff,
                ", p ", sign, ano$Pr..Chisq.),
              collapse = "")
  
  return(out)
  
}
