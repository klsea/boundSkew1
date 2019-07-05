isolate_measure <- function(data, term, datadict) {
  # input: data = data table
  # term = character 
  # datadict = data dictionary with only 2 columns *UPDATE WHEN DATA DICT IS UPDATED
  data[,c(9,grep(term, datadict[,2]))]
}

add_correct_avi_labels <- function(avi, term, datadict) {
  # input: avi 
  # term = character
  # datadict = data dictionary with only 2 columns *UPDATE WHEN DATA DICT IS UPDATED
  d0 <- as.character(datadict[grep(term, datadict[,2]),2])
  d1 <- d0[1:6]
  d1 <- gsub('Listed below are a number of words that describe feelings. Some of the feelings are very similar to\neach other, whereas others are very different from each other. Read each word and then select how\noften YOU ACTUALLY HAVE that feeling over the course of a typical week:\nOver the course of a typical week, I ACTUALLY feel… - ', '', d1)
  d2 <- d0[7:24]
  d2 <- gsub("Over the course of a typical week, I ACTUALLY feel… - ", "", d2)
  d3 <- c(d1,d2)
  colnames(avi) <- c('SubID', d3)
  return(avi)
}

score_avi <- function(avi) {
  avi$hap <- (avi$Enthusiastic + avi$Excited + avi$Strong) / 3 
  avi$lap <- (avi$Calm + avi$Relaxed + avi$Peaceful) / 3
  avi$la <- (avi$Quiet + avi$Passive + avi$Still) / 3
  #avi$pos <- (avi$Euphoric + avi$Elated + avi$Rested ) /3 # We don't think this is correct
  avi$lan <- (avi$Dull + avi$Sleepy + avi$Sluggish) / 3
  avi$han <- (avi$Fearful + avi$Hostile + avi$Nervous) /3
  avi$ha <- (avi$Aroused + avi$Surprised + avi$Astonished) / 3
  #avi$neg <- (avi$Lonely + avi$Inactive + avi$Idle ) /3 # We don't think this is correct
  return(avi[,c(1,26:31)])
}
