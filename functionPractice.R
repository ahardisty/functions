# Basic Structure of a Function ------------------------------------------------------------

# this is a function
set.seed(100)
d <- rpois(25, 8)

GetCI <- function(x, level = 0.95){ # allow for input in confidence level
  if(level <= 0 || level >= 1) {
    stop("The 'level' arg must be >0 and <1") # eliminate upper lower violations
  }
  if(level <0.5) {
    warning("Conf Intervals are usually close to 1, e.g 0.95")
  }
  m <- mean(x)
  n <- length(x)
  SE <- sd(x) / sqrt(n)
  upper <- 1 - (1-level)/2
  ci <- m + c(-1,1) * qt(upper, n-1)*SE
  # return(c(m, SE)
  return(list(data=x, mean=m, se=SE, CI=ci)) # return a list for multiple objects; give names
}

exT <- function(n = 10000) {
  # Purpose: Test if system.time works ok;   n: loop size
  system.time(for(i in 1:n) x <- mean(rt(1000, df = 4)))
}
system.time(exT())

timing <- 1:100
monkey <- lapply(timing, function(x) list((x)[[1]]*mean(x)))
str(monkey)

fnc <- GetCI(d, .75)
unlist(fnc)
fnc$mean[1]
fnc[[3]][1:2]
use.switch <- function(x){switch(x, letters[1:5] = "first", letters[10:20] = "z", "other")} 
use.switch(" a")

# Lander, Jared P. (2013-12-20). R for Everyone: Advanced Analytics and Graphics (Addison-Wesley Data & Analytics Series) (Kindle Locations 2610-2618). Pearson Education. Kindle Edition. 

# revenue function
revenue <- matrix(E, ncol = 5)

RevSummary <- function(x, ...) {
  if(!is.matrix(x) && !is.data.frame(x)) {
    stop("'x' must be a matrix or data frame")
  }

  # ellipsis.args <- list(...)
  
  rev.per.company <- colMeans(x, ...)
  rev.per.day <- rowMeans(x, ...)
  
  return(list(AvgRevPerCompany = rev.per.company,
              AvgRevPerDay = rev.per.day)) # return an list
}

RevSummary(revenue, na.rm = TRUE)
RevSummary(revenue, ... = TRUE)

# make output invisible
annoying <- function(x, nums){
  invisible(rep(x, ...))
}
tmp <- annoying(1:4, 10)

# recursive functions
# function that calls itself
# use Recall()

# custom function with apply()
x <- matrix(rpois(1e4, 8), 1000)
apply(x, 2, mean)
apply(x, 1, mean)

# Datacamp Functions ------------------------------------------------------
# Define the function hello()
hello <- function() {
  print("Hi there!")
  TRUE
}

# Call the function hello()
hello()

# Define the function my_filter()
my_filter <- function(x) {
  if(x > 0) {
    return(x)
  } else {
    return(NULL)
  }
}

# Call the function my_filter() twice
my_filter(5)
my_filter(-5)



# R passes arguments by value ---------------------------------------------

increment <- function(x, inc = 1) {
  x <- x + inc
  x
}
count <- 5
a <- increment(count, 2)
b <- increment(count)
count <- increment(count, 2)



# R You Functional --------------------------------------------------------

# The linkedin and facebook vectors have already been created for you

# Define the interpret function
# The linkedin and facebook vectors have already been created for you

# Define the interpret function
interpret <- function(num_views) {
  if(num_views > 15) {
    print("You're popular!")
    return(num_views)
  } else {
    print("Try to be more visible!")
    return(0)
  }
}

# Call the interpret function twice
linkedin <- as.integer(runif(15, 0, 100))
facebook <- as.integer(runif(15, 0, 100))
interpret(linkedin[1])
interpret(facebook[2])


# R You Functional 2 --------------------------------------------------------
# Define the interpret_all() function
interpret_all <- function(views, return_sum = TRUE) {
  count <- 0
  for(v in views) {
    count <- count + interpret(v)
  }
  if(return_sum) {
    return(count)
  } else {
    return(NULL)
  }
}

# Call the interpret_all() function on both linkedin and facebook
interpret_all(linkedin)
interpret_all(facebook)


x <- 5
f <- function() {
        y <- 10
        c(x = x, y = y)
}
f()

x <- 5
g <- function() {
        x <- 20
        y <- 10
        c(x = x, y = y)
}
g()

l <- 5
h <- function(x,y,z,l=2) {
        i <- function(l) {
                c(x = l*x, y = l+y, z = z*l)
        }
        i(l)
}
h(2)
h(x = 2,4,40)
h(x = 3, y = 3, z = 40, l = 1)


f2 <- function(a, b) {
  a * 10
}
f2(10,b = 4)
, stop("This is an error!"))

system.time(read.csv())



