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
h <- 2
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