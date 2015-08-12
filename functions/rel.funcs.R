library(plyr)
library(reshape)

#Function for splitting data in half
#e.g. ddply(data, .(block1, block2), half)
# splits data into each unique combination of block1, block2, 
# and adds a half column
half = function(df){
  df$half = rep(c('A', 'B'), nrow(df))[1:nrow(df)]        #repeat A,B for each entry in col
  return(df)
}

#Function for calculating split half reliability
split.rel = function(df, dvcol){
  aligned = cast(df, Subject ~ half, value=dvcol)
  out = c()
  out[dvcol] = with(aligned, cor(A, B))
  N = 2      # inflate with SB-prophecy formula
  out = (N * out) / (1 + (N-1)*out)
  out
}

d.fix1 = function(dv, N){
  # Common correction found from Wikipedia (find original source)
  correction = 2^(-1/N)
  dv = ifelse(dv == 1, correction, dv)
  dv = ifelse(dv == 0, 1 - correction, dv)
  dv
}

d.fix2 = function(dv, N){
  # Correction suggested by Todd
  (dv*N + .5) / (N + 1)               #e.g  (Total_Hits + .5) / (N + 1)
}

d.prime = function(H, FA, numAX=FALSE, numBX=FALSE, fix=d.fix2, measure=c('dprime', 'bias', 'pbi')[1]){
  # Takes either raw accuracies and number of items, or raw scores (0 or 1)
  if (class(numAX) == "logical") {
    numAX = length(H)
    H = mean(H)
  }
  if (class(numBX) == "logical") {
    numBX = length(FA)
    FA = 1 - mean(FA)             #REVERSE SCORED
  }
  H = fix(H, numAX)
  FA = fix(FA, numBX)
  
  stopifnot(!any(c(H, FA) %in% 0:1))
  if (measure == 'dprime') return(qnorm(H) - qnorm(FA))
  if (measure == 'bias') return((qnorm(H) + qnorm(FA)) / 2)
  if (measure == 'pbi') return((1-H - FA) / (1-H + FA))  #pbi on errors of both
}
  
#TODO since we only take RT on trials with correct probe response, subs with no correct responses
#     for a TrialType will return NaN during scoring. This can be remedied by having the cor function
#     drop those subs.
split.axcpt = function(data, retdata=FALSE, fix=d.fix2, makewhole=FALSE){
  # Args:
  #   retdata:    whether to return the raw calculations for each half
  #   fix:        function to correct for perfect accuracy when calculating dprime
  #   makewhole:  if TRUE, runs calculations without splitting halves
  #               e.g. to get the scored data back..
  #                  split.axcpt(data, retdata=TRUE, makewhole=TRUE)
  #
  # Requires the following columns in the data.frame (data)..
  #   Subject:    Participant IDs
  #   TrialType:  one of {AX, AY, BX, BY}
  #   Probe.ACC:  trial-by-trial accuracy (not averaged)
  #   Probe.RT:   trial-by-trial response time

  # Make new column for split half block
  if (!makewhole){
    data = ddply(data, .(Subject, TrialType),  half)
    data.rt = ddply(subset(data, Probe.ACC == 1), .(Subject, TrialType),  half)
  }
  else {
    data$half = "A"
    data.rt = transform(subset(data, Probe.ACC == 1), half="A")       # FIX
  }
  
  # Get number of trials for first subject        TODO (MC): remove?
  trial.table = table(data[, c('Subject', 'TrialType')])[1,]

  # Score halves
  means.acc = cast(data, Subject + half ~ TrialType, mean, value='Probe.ACC')
  means.rt     = cast(data.rt, Subject + half ~ TrialType, mean, value='Probe.RT')
  
  means.rt$pbi  =  with(means.rt, (AY - BX) / (AY + BX))

  sub.dprime = ddply(data, .(Subject, half), function(df){
                      resps = dlply(df, .(TrialType))
                      c(dprime = d.prime(resps$AX$Probe.ACC, resps$BX$Probe.ACC),
                        bias = d.prime(resps$AX$Probe.ACC, resps$AY$Probe.ACC, measure='bias'),
                        pbi = d.prime(resps$AY$Probe.ACC, resps$BX$Probe.ACC, measure='pbi'))
  })
  
  # merge dprime estimates into scored accuracy
  means.acc = merge(means.acc, sub.dprime)
  
  if (retdata) return(list(means.rt=means.rt, means.acc=means.acc, trial.info=trial.table))
  
  # Split-half reliabilities
  measures = c('pbi', 'AX', 'AY', 'BX', 'BY', 'dprime', 'bias')
  names(measures) = measures
  out = list()
  out[['acc']] = llply(measures, function(m) split.rel(means.acc, m))
  out[['rt']]  = llply(measures[1:5], function(m) split.rel(means.rt, m))   #change indexing hack
  out 
}