pariedttable <- function(data, var.names, output){
  # data is a data frame in wide format
  # var.names is a vector with the names of the columns you want to compare
  dt <- matrix(nrow = length(var.names), ncol = length(var.names)-1)
  d2 <- matrix(nrow = length(var.names), ncol = length(var.names)-1)
  count = 0
  for (l1 in 1:(length(var.names)-1)){
    #print(l1)
    var1 = var.names[l1]
    #print(var1)
    for (l2 in (l1+1):length(var.names)) {
      #print(l2)
      var2 = var.names[l2]
      #print(var2)
      t <- t.test(d1[,l1+2], d1[,l2+2], paired = TRUE)
      #print(t)
      dt[l1, l2-1] <- paste0(round(t$statistic,2), '[', round(t$conf.int[1],2), ',', round(t$conf.int[2],2), ']')
      d2[l1, l2-1] <- round(t$p.value, 4)
      count = count+1
    }
  }
  criticalp = 0.05/count
  row.names(dt) <- var.names
  colnames(dt) <- var.names[2:length(var.names)]
  row.names(d2) <- var.names
  colnames(d2) <- var.names[2:length(var.names)]
  ifelse(output == 1, return(dt), return(d2))
}


#data <- d1
#var.names <- colnames(d1)[3:8]
#ttable(d3,colnames(d1)[3:8])
