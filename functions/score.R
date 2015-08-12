library(plyr)
library(dplyr)
score <- function(dat) {
  dat$Probe.ERR = 1-dat$Probe.ACC
  dat$Probe.ACC = NULL
  dat[dat$Probe.ERR == 1, 'Probe.RT'] = NA
  return(dat)
}

mean_trans <- function(dat, cols, ll=c(2:8), tt=c("NP","NN","RN")) {
  #Params:
  #dat: dataframe in the specified format, with outliers already removed
  #cols: a vector listing out the column names to calculate the scores by these groups
  #ll: use if you want to select only certain list lengths
  #tt: use if you want to select only certain trial types
  #returns a dataframe with average Error and RT across specified conditions
  
  # melt data, so RT and ACC in same col
  mdat <- melt(dat, measure.vars=c('Probe.ERR','Probe.RT'), variable_name='measure') %>%
    filter(ListLength %in% ll, TrialType %in% tt)
  mdat.mean <- ddply(mdat, cols, summarize, dv = mean(value, na.rm=TRUE))
}