dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
patient_1 <- dat[1, ]
max(patient_1)
dim(dat)
n <- dim(dat)[1]
n <- nrow(dat)

plot_one <- function(x){
  x + 1
}

plot_one(2)

fahrenheit_to_kelvin <- function(temp_F) {
  temp_K <- ((temp_F - 32) * (5 / 9)) + 273.15
  return(temp_K)
}

#fahrenheit_to_kelvin(temp_F=32)

kelvin_to_celsius <- function(temp_K) {
  temp_C <- temp_K - 273.15
  return(temp_C)
}

fahrenheit_to_celsius <- function(temp_F) {
  temp_K <- fahrenheit_to_kelvin(temp_F)
  temp_C <- kelvin_to_celsius(temp_K)
  return(temp_C)
}

fahrenheit_to_celsius (32)

fence <- function(original, wrapper){
  ## I want to concatenate wrapper - orignial -wrapper
  c(wrapper, original,wrapper)
}

best_practice <- c("Write", "programs", "for", "people", "not", "computers")
asterisk <- "***"  # R interprets a variable with a single value as a vector
# with one element.
fence(best_practice, asterisk)

##
outside <- function(x){
  c(x[1],x[length(x)])
}

dry_principle <- c("Don't", "repeat", "yourself", "or", "others")
outside(dry_principle)

outside2 <- function(x){
  c(head(x,1),tail(x,1))
}
outside2(dry_principle)

## 
center <- function(data, desired) {
  (data - mean(data)) + desired
}
z <- c(0,0,0,0)
z
center(z,3)

center2 <- function(data, desired) {
  data <- (data - mean(data)) + desired # data <<-
  return(data)
}

## loop
best_practice <- c("Let", "the", "computer", "do", "the", "work")
print_words <- function(sentence) {
  for (word in sentence) {
    print(word)
  }
}

print_words(best_practice)

##
len <- 0
vowels <- c("a", "e", "i", "o", "u")
for (v in vowels) {
  len <- len + 1
}
# Number of vowels
len  

expo <- function(val,N){
  res <- 1
  for (i in 1:N){
    res <- res * val
      }
  res
}

expo(2,4)

##
analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

analyze_all <- function(pattern) {
  # Runs the function analyze for each file in the current working directory
  # that contains the given pattern.
  filenames <- list.files(path = "data", pattern = pattern, full.names = TRUE)
  for (f in filenames) {
    analyze(f)
  }
}

pdf("inflammation-01.pdf")
analyze("data/inflammation-01.csv")
dev.off()

num <- 37
if (num > 100) {
  print("greater")
} else {
  print("not greater")
}
print("done")

##
sign <- function(num) {
  if (num > 0) {
    return(1)
  } else if (num == 0) {
    return(0)
  } else {
    return(-1)
  }
}

sign(-3)

##
plot_dist <- function(x, threshold) {
  if (length(x) > threshold) {
    boxplot(x)
  } else {
    stripchart(x)
  }
}
dat <- read.csv("data/inflammation-01.csv", header = FALSE)
plot_dist(dat[, 10], threshold = 10)     # day (column) 10

if (6){
  print("OK")
} else {
  print("NOK")
}

dat <- read.csv(file="data/sample.csv")
logicals <- c(TRUE,TRUE, FALSE, FALSE, TRUE, T,T,F,F)
head(dat[,logicals])

dat <- read.csv("data/car-speeds.csv",stringsAsFactors = TRUE)
dat$Color <- ifelse(dat$Color == "Red", "Green",dat$Color)
dat$Color

dat <- read.csv("data/car-speeds.csv",stringsAsFactors = FALSE)
dat$Color <- ifelse(dat$Color == "Red", "Green",dat$Color)
dat$Color

dat <- read.csv("data/car-speeds.csv",stringsAsFactors = FALSE,strip.white = TRUE)
unique(dat$Color)

# write.csv(dat,"data/car-speeds-cleaned.csv",row.names = FALSE)


# 
sex <- factor(c("male","female","female","male"))
food <- factor(c("low","high","medium","low","high"),levels = c("low","medium","high"),ordered = TRUE)
levels(food)
min(food)

dat <-read.csv(file="data/sample.csv",stringsAsFactors = TRUE)
dat$Gender[dat$Gender=="f"] <- "F"
dat$Gender[dat$Gender=="m"] <- "M"
# dat$Gender <- droplevels(dat$Gender)
plot(x=dat$Gender,y=dat$BloodPressure)

##make a function that takes x and y (integers) and returns a list containing
#the sum(integer) and the sum coded as a string
hello <- function(x,y) {
  res <- list(sum=x+y,sumstring=as.character(x+y))
  return(res)
}

hello(3,4)[1]
hello(3,4)$sumstring

# read the file and compute the mean of each column
dat <- read.csv(file="data/inflammation-01.csv",header = FALSE)
apply(dat,margin=2, mean)

dry_principle <- c("don't","repeat","yourself","or","others")
outside <- function(x){
  x <- c(x[1],x[length(x)])
  return(x)
}
outside(dry_principle)


display <- function(a = 1, b = 2, c = 3) {
  result <- c(a, b, c)
  names(result) <- c("a", "b", "c")  # This names each element of the vector
  return(result)
}

display(12,34,56)

rescale2 <- function(v,lower=0,upper=1){
  L <- min(v)
  H <- max(v)
  result <- (v - L) / (H - L) * (upper - lower) + lower
  return(result)
}
rescale2(c(1,5,8),lower=2,upper=5)


rescale <- function(v) {
  # Rescales a vector, v, to lie in the range 0 to 1.
  L <- min(v)
  H <- max(v)
  result <- (v - L) / (H - L)
  return(result)
}
rescale(c(1,2,3))

best_practice <- c("Let", "the", "computer", "do", "the", "work")
print_hi <- function(sentence) { 
  for (word in sentence) {
    print(word)
  }
}

print_hi(best_practice)

len <- 0
vowels <- c("a", "e", "i", "o", "u")
for (v in vowels) {
  len <- len + 1
}

len

# or 
vowels <- c("a", "e", "i", "o", "u")
length(vowels)

print_N <- function(i){
  for (j in seq(i)){
    print(j)
  } 
}

print_N(3)

df <- c(4, 8, 15, 16, 23, 42)
total <- function(v){
  v_sum <- 0
  for (i in v){
    v_sum <- v_sum+i
  }
  return(v_sum)
}
total(df)

expo <- function(base,power){
  result <- 1
  for (i in seq(power)){
    result <- result*base
  }
  return(result)
}

expo(2,4)