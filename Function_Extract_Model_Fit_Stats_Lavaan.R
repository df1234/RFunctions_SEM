################################################################################
################### FUNCTION FOR EXTRACTING KEY MODEL FIT  #####################
#################### STATISTICS FROM FITTED LAVAAN OBJECT ######################
################################################################################

# Input: x - fitted lavaan object, e.g. fit <- cfa(HS.model, data=HolzingerSwineford1939)
# Output: a character string containing key model fit statistics
# Invoked as "model.fit.stats(x)"

model.fit.stats <- function(x) {
  
  options("scipen"=100, "digits"=8) # prevent exponential notation
  
  # extract relevant statistics
  table = data.frame(t(data.frame(fitMeasures(x, fit.measures = 
                                                c("chisq.scaled","df.scaled","pvalue.scaled",
                                                  "rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
                                                  "cfi.scaled","srmr")))))
  
  # determine whether p smaller than .001
  if (table$pvalue.scaled < 0.001) {
    sign = "<"
  } else {
    sign = "="
  }
  
  # format values
  table$chisq.scaled = sprintf("%.2f", round(table$chisq.scaled, 2)) # round 2 to digits
  table$pvalue.scaled = rd(table$pvalue.scaled, digits = 3) # round 3 to digits, remove leading 0
  table$pvalue.scaled[grepl(".000", table$pvalue.scaled, ignore.case=F)] <- ".001" # if smaller than .001 write "< .001"
  table$rmsea.ci.lower.scaled = rd(table$rmsea.ci.lower.scaled, digits = 3) # round 3 to digits, remove leading 0
  table$rmsea.ci.upper.scaled = rd(table$rmsea.ci.upper.scaled, digits = 3) # round 3 to digits, remove leading 0
  table$rmsea.scaled = rd(table$rmsea.scaled, digits = 3) # round 3 to digits, remove leading 0
  table$cfi.scaled = rd(table$cfi.scaled, digits = 3) # round 3 to digits, remove leading 0
  table$srmr = rd(table$srmr, digits = 3) # round 3 to digits, remove leading 0
  
  # coerce into character
  out = paste(c("X2(", table$df.scaled, ") = ", table$chisq.scaled,
                ", p ", sign, " ", table$pvalue.scaled, "; RMSEA = ",
                table$rmsea.scaled, " [", table$rmsea.ci.lower.scaled, " ",
                table$rmsea.ci.upper.scaled, "]; CFI = ", table$cfi.scaled,
                " ; SRMR = ", table$srmr),
              collapse = "")
  
  return(out)
  
}
