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

isolate_measure2 <- function(data, first_term, last_term, datadict) {
  #input:
  #data = data table 
  #first_term = first term in questionnaire section (character)
  #last_term = last term in questionnaire section (character)
  #datadict = data dictionary with only 2 columns *UPDATE WHEN DATA DICT IS UPDATED
  first <- as.data.frame(grep(first_term, datadict[,2]))
  last <- as.data.frame(grep(last_term, datadict[,2]))
  f1 <- first[1,]
  l1 <- last[length(last),]
  data[,c(9, f1:l1)]
}

add_measure_labels <- function(measure, first_term, last_term, datadict ) {
  #isolate_measure2(graph_lit)# would be great to use this line if function can be auto input rather than typing out again
  fst <- as.data.frame(grep(first_term, datadict[,2]))
  lst <- as.data.frame(grep(last_term, datadict[,2]))
  f1 <- fst[1,]
  l1 <- lst[length(lst),]
  ratings <- as.character(dd[f1:l1,2])
  colnames(measure) <- c('SubID', ratings)
  return(measure)
}
  
score_graph_lit <- function(data) {
  # input: data = data table of graph literacy only
  dt <- cbind.data.frame(data$SubID, rowSums(data[2:11]))
  colnames(dt) <- c('SubID', 'graph_lit')
  return(dt)
}
