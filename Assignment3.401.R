
## Katie Melhuish
## Assignment 3
### 1 ### 
##A
move.avg <- function(values, n) {
  new.vec = c()
  for(i in 1:length(values)){
    if(i<n){
      new.vec[i] = NA
    } else if (i == n){
      new.vec[i] = NA
    }else{
      new.vec[i] = mean(values[(i-n+1):i-1])
    }
  }
  new.vec
  }

##B
move.avg.summary <- function(values, n) {
  avgs = c()
  for(i in 1:length(values)){
    if(i<n){
      avgs[i] = NA
    } else if (i == n){
      avgs[i] = NA
    }else{
      avgs[i] = mean(values[(i-n+1):i-1])
    }
  }
  e = values - avgs
  list(avgs = avgs, errors = e )
}

### 2 ###
np.percentile.finder <- function(V, P) {
  V <- sort(V)
  pPer <- 100/length(V)
  p <- (P * 100) 
  index <- (p/pPer)
  V[index]
}

##nums <- c(5,4,3,2,1,10,9,8,7,6)
##np.percentile.finder(nums, .9)
##np.percentile.finder(nums, .2)


### 3 ###
df.summarize <- function(df){
  n = ncol(df)
  for(i in 1:n){
    col <- df[,i]
    if(is.numeric(col)){
      count = i
      toString(count)
      avg <- mean(col, na.rm=TRUE)
      toString(avg)
      min <- which.min(col)
      min <- col[min]
      toString(min)
      max <- which.max(col)
      max <- col[max]
      toString(max)
      sd <- sd(col, na.rm=TRUE)
      toString(sd)
      print(paste0("Column ",count,": mean= ",avg,", min = ",min,", max = ",max,", sd = ",sd))
    }else{
      count = i
      toString(i)
      print(paste("Column ",count," is non-numeric"))
    }
  }
}

#n = c(2, NA, 5) 
#s = c("aa", "bb", "cc") 
#b = c(TRUE, FALSE, TRUE) 
#o = c(9, 9, 9)
#df = data.frame(n, s, b, o) 
#df.summarize(df)

