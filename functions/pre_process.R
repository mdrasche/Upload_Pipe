library(plyr)

#TODO Function to check if data is in correct form?
#

preprocess <- function(upload_dat) {
  #Select the columns needed
  wordstims = paste0('w', 1:8)
  keep = c('Subject', 'ExperimentName', 'Running', wordstims, 'Probe.ACC',
           "Probe.RT", "Block", "ListLen", "probe", "Top.N", "TrialType", "Probe.Pos") 
  exp.conds = (upload_dat$Procedure %in% "main") #Keep "main" Procedure--includes "PracticeList"
  dat = subset(upload_dat, exp.conds, select=keep)
  
  #Fix RT if response not give within limit
  noresp = dat$Probe.RT == 0     # if RT 0, then no response was given
  stopifnot(!all(is.na(noresp))) # make sure no NAs
  dat[noresp, 'Probe.RT'] = 2000 #Max RT
  #Create Column indicating No Response
  dat$NoResp = noresp
  
  # Split "Running" column into condition "Cog" and "BlockNo"
  for (i in 1:nrow(dat)) {
    if (dat[i, "Running"] !="PracticeList") {
      words_arr = unlist(dat[i, "Running"])
      dat[i, 'Cog'] = gsub("Block.", "", words_arr)
      dat[i, 'BlockNo'] = unlist(regmatches(words_arr, gregexpr("[[:digit:]]+", words_arr)))
    }
    else{
      dat[i,'Cog'] = "Practice"
      dat[i,'BlockNo'] = "Practice"
    }
  }
  
  # Get block order
  #TODO will need general method
  order2 = dat$ExperimentName == "Conway005_FullOrder2"
  dat[,"FirstBlock"] <- "Base->Re->Pro"
  dat[order2,"FirstBlock"] <- "Base->Pro->Re"
  
  dat$TrialNo <- dat$Block #Unique number for each trial
  
  #Change Block to indicate short v. long list length
  dat$Block <- "Long"
  practice = dat$Cog == "Practice"
  proactive = dat$Cog == "Proactive"
  dat[practice, "Block"] <- "Practice"
  dat[proactive, "Block"] <- "Short"
  
  #Add Cond col to indicate high v. low RN
  dat$Cond <- "Low"
  reactive = dat$Cog == "Reactive"
  dat[practice, "Cond"] <- "Practice"
  dat[reactive, "Cond"] <- "High"
  
  #Change 'ListLen' to 'ListLength'
  colnames(dat)[which(names(dat) == "ListLen")] <- "ListLength"
  
  #Create OldLL ---TODO Is this needed?
  for (i in 1:nrow(dat)) {
    dat[i,"OldLL"] <- paste0(dat[i,"Top.N"], dat[i,"ListLength"] - dat[i,"Top.N"])
  }
  
  dat <- subset(dat, Cog !="Practice")
  
  return(dat)
}

