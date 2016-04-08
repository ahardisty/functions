baseline <- rep(1100, 10)
summer <- sample(x = 1:560, 10)
summer <- summer*10.4
# demoDF <- dframe
# demoDF <- rbind(demoDF, dframe[,1:6])
whichMin <- function(x, y, mult, comp = min){
  x <- x*mult[1]
  y <- y*mult[2]
  comp(x,y)
}

dframe <- data.frame(baseline = baseline,summer = summer)

# determine tier 1
dframe$tier1 <- apply(dframe[,c("summer","baseline")], 
                      1, function(x,y) FUN = whichMin(x= x[1], y = x[2],mult = c(1,1)))

# dframe$tier2 <- apply(dframe[,c("summer","baseline")], 1, 
#                       function(x,y) FUN = whichMin(x= x[1], y = x[2],mult = c(1,2),comp = min))
# 
# dframe$tier2 <- dframe$tier2 - dframe$tier1

dframe$tier2 <- apply(dframe[,c("summer","baseline")], 1, 
                      function(x,y) whichMin(x = x[1], y = x[2],mult = c(1,2),comp = min)) - dframe$tier1


# dframe$tier3 <- apply(dframe[,c("summer","baseline")], 1, 
#                       function(x,y) FUN = whichMin(x= x[1], y = x[2],mult = c(1,2),comp = min))
# 
# dframe$tier3 <- dframe$summer - dframe$tier3

dframe$tier3 <- dframe$summer - apply(dframe[,c("summer","baseline")], 1, 
                      function(x,y) FUN = whichMin(x = x[1], y = x[2],mult = c(1,2),comp = min))


dframe$total <- apply(dframe[,c("tier1","tier2","tier3")], 1, sum)
dframe$totalb <- apply(dframe[,c("tier1","tier2b","tier3b")], 1, sum)

dframe$match <- identical(dframe$total, dframe$summer)

dframe
