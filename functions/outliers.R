library(plyr)
library(ggplot2)
source('functions/rel.funcs.r')

set_jitter = position_jitter(width=.1, height=0)

meanit = colwise(mean, .cols=c('Probe.RT', 'Probe.ACC', 'NoResp'))

outlier_detection <- function(dat) {
  means = ddply(dat, .(Subject, Cond, Block, FirstBlock), meanit)
  N = ddply(dat, .(Subject, Cond, Block), nrow)
  N = unique(N[,c('Cond', 'Block', 'V1')])
  
  # Exclude participants who..
  # 1. Score below chance for either list length
  # 2. Do not score above chance for short lists
  # Speer et al used 80 trials per block, gave break every 40 trials
  # Why are our trials so close together? (speer gave an average of 2,500ms between trials)
  # TODO need response bias measure..
  # e.g. NP vs NN (d')
  #      old vs new in delay (d')
  
  ###Is this essentially a 90% confidence interval around chance?
  thresh.low = qbinom(.05, 120, .5) / 120  
  thresh.high = 1 - thresh.low
  
  # Remove subjects who scored below chance on Short list lengths
  reject_short = subset(means, Probe.ACC < thresh.high & Block %in% 'Short')
  
  # Those who did not score above chance on either short or long
  below_chance     = subset(means, Probe.ACC < thresh.low)
  
  # Reject if they were at chance for both lengths
  reject_chance = ddply(below_chance, .(Subject), function(df) df[nrow(df) == 1,])
  
  # Additionally exclude participants who.. 
  # had a NoResp rate greater than 20% on any Session
  reject_lowResp = subset(means, NoResp > .2)
  
  # OR reject NoResp rate greater than 20% on any Session X TrialType
  #means_tt = ddply(dat, .(Subject, Cond, Block, TrialType), meanit)
  #reject_lowResp = subset(means_tt, NoResp > .2)
  
  # Subject numbers to reject
  reject_all = unique(c(reject_short$Subject, reject_chance$Subject, reject_lowResp$Subject))
  
  return(reject_all)
}

outlier_plot <- function(dat, reject_all) {
  means = ddply(dat, .(Subject, Cond, Block, FirstBlock), meanit)
  p.outlier = qplot(Block, Probe.ACC, data=means, facets=Cond~FirstBlock, 
                    position=set_jitter, alpha=.7, 
                    color=ifelse(Subject %in% reject_all, 'drop', 'keep'))
  return(p.outlier)
} 

outlier_plot_tt <- function(dat, reject_all) {
  #Outlier Plot broken down by Trial Type
  newTrialType = gsub("-.*", "", dat$TrialType)
  newTrialType = factor(newTrialType, levels=c('NP', 'NR', 'RN', 'NN'))
  means.TT = ddply(dat, .(Subject, Cog, newTrialType), meanit)
  dat$newTrialType = newTrialType 
  p.out = qplot(newTrialType, Probe.ACC, data=means.TT, facets=.~Cog, position=set_jitter, alpha=.7,
                color=ifelse(Subject %in% reject_all, 'drop', 'keep'))
  return(p.out)
} 

clean <- function(dat, reject_all) {
  clean_dat = subset(dat, (!Subject %in% reject_all))  #remove outliers
  return(clean_dat)
}


