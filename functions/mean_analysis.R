library(plyr)
library(dplyr)

t.test.cog = function(dat, dv_name, within = FALSE){
  # Makes all t-test comparisons comparing the cognitive control condtions
  # Parameters:
  # dat: The dataframe in ... format
  # dv_name: name of the dependent variable e.g.(Probe.ERR)
  # within: T or F, should it be a within t-test? Defaults to FALSE if not specified.
  out <- list()
  dat_spread <- dat %>% select(Subject, Cog:dv) %>%
    spread(measure, dv) 
  
  wide <- dat_spread %>% select(Subject, Cog, Probe.ERR) %>%
    spread(Cog, Probe.ERR)
  out$ERR$react_base   = t.test(wide$Reactive, wide$Baseline, paired=within)        
  out$ERR$proact_base  = t.test(wide$Proactive, wide$Baseline, paired=within)
  out$ERR$proact_react = t.test(wide$Proactive, wide$Reactive, paired=within)
  
  wide <- dat_spread %>% select(Subject, Cog, Probe.RT) %>%
    spread(Cog, Probe.RT)
  out$RT$react_base   = t.test(wide$Reactive, wide$Baseline, paired=within)        
  out$RT$proact_base  = t.test(wide$Proactive, wide$Baseline, paired=within)
  out$RT$proact_react = t.test(wide$Proactive, wide$Reactive, paired=within)
  return(out)
}

t.summary = function(ttest){
  data.frame(t=ttest$statistic, df = ttest$parameter,
             CI_low = ttest$conf.int[1], CI_high = ttest$conf.int[2], 
             p = ttest$p.value)
}
