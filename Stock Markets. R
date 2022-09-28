df <- read.csv("https://people.bu.edu/kalathur/datasets/myPrimes.csv")
head(df)

# Question 1

# Question 1/A 

par(mfrow=c(1,2))

last_digit <- table(df$LastDigit)


barplot(last_digit,
        col = "salmon", ylim=c(0,350),
        xlab = "Last Digit", ylab = "Frequency")


# Question 1/B

first_digit <- table(df$FirstDigit)

barplot(first_digit, 
        col = 'wheat', ylim = c(0,200),
        xlab = 'First Digit', ylab = 'Frequency')
 
# Question 1/C 

#  There was only one even digit (2) in all last digits but it appeared infrequently.
#  Of all odd numbers in the data set, five(5) appeared the least out of odd numbers.

# The range of data in the first digit database was evenly spread
# The frequency gradually decreased as the number increased.


# Question 2

us_quarters <- read.csv("https://people.bu.edu/kalathur/datasets/us_quarters.csv")


# Question 2/A

# max...
us_quarters[us_quarters$DenverMint == max(us_quarters$DenverMint), "State"]
# Connecticut

us_quarters[us_quarters$PhillyMint == max(us_quarters$PhillyMint), "State"]
# Virginia 

# min...

us_quarters[us_quarters$DenverMint == min(us_quarters$DenverMint), "State"]
#Oklahoma

us_quarters[us_quarters$PhillyMint == min(us_quarters$PhillyMint), "State"]
#Iowa

# Question 2/B


((sum(us_quarters$DenverMint) + sum(us_quarters$PhillyMint))*1000) / 4
# $8699400000


# Question 2/C

par(mfrow=c(1,1))

quarter_matrix <- as.matrix(us_quarters[, c("DenverMint","PhillyMint")])

barplot(t(quarter_matrix), names.arg = us_quarters$State, legend.text = TRUE,
        ylim = c(0,1000000),col = c("blue", "grey"), beside = TRUE, las = 2)
# bar plot of data 

# Inference = The further east you go, the more coins are minted
# Midwest countries mint the least amount of coins

# Question 2/D

# Scatter plot...

plot(us_quarters$DenverMint, us_quarters$PhillyMint, col = 'red', 
xlab = 'Denver Mint', ylab = 'Philly Mint')

# A high concentration of minted coins at the lower end. As the numbers increase
# there are less data points with one State far exceeding the others.

# Question 2/E

boxplot(quarter_matrix, col = c('blue','red'))

# DenverMint - bigger IQR than Philly Mint
# Denver Mint - Two outliers

# PhillyMint - Smaller IQR than DenverMnt with smaller range and lower median.
# PhillyMint - A great deal of outliers in this daataset, skewing the boxplot. 


# Question 2/F


third_quarter_denver <- fivenum(us_quarters$PhillyMint)[4]

third_quarter_philly <- fivenum(us_quarters$PhillyMint)[4]

iqr_philly <- IQR(us_quarters$PhillyMint)

iqr_denver <- IQR(us_quarters$DenverMint)

Tmax_denver <- third_quarter_denver+(1.5*iqr_denver)

Tmax_philly <- third_quarter_philly+(1.5*iqr_philly)

us_quarters$State[which(us_quarters$DenverMint > Tmax_denver)]

# "Connecticut" "Virginia" "New York" 

us_quarters$State[which(us_quarters$PhillyMint > Tmax_philly)]

# "Connecticut" "Massachusetts"  "Maryland" "South Carolina" "New Hampshire" 
# "Virginia"  "New York" "North Carolina"

# Question 3

stocks <- read.csv("https://people.bu.edu/kalathur/datasets/stocks.csv")

# Question 3/A

head(stocks)

stocks_matrix <- as.matrix(stocks[ ,c(2:6)])

pairs(stocks_matrix, col = 'red', pch = 16)

# Question 3/B
round(cor(stocks_matrix),2)

# Question 3/C

# There is a very strong correlation between MSFT, AAPL and GOOG
# FB and AAPL have the least correlation
# All have positive correlation
# The strongest correlation is between MSFT and GOOG

# Question 3/D


cm <- round(cor(stocks_matrix),2)

for (i in 1:nrow(cm)) {
  res <- sort(cm[i, ],decreasing = TRUE)
  print(paste("Top three for stock", rownames(cm)[i]))
  print(res[2:4])
}

# Question 4 

par(mfrow=c(1,1))

scores <- read.csv("https://people.bu.edu/kalathur/datasets/scores.csv")

head(scores)

# Question 4/A

x <- hist(scores$Score, main = 'Scores')

for (i in 1:length(x)){
  cat(x$counts[i], "are in range (",x$breaks[i],',', x$breaks[i+1], "]", '\n')
}

# Question 4/B
x <- hist(scores$Score, breaks = c(30,50,70,90))
grades <- c('C', 'B', 'A')
x$counts
x$breaks

for (i in 1:length(x$counts)) {
  cat(x$counts[i],"students in ", grades[i], "grade range (", x$breaks[i],',',
      x$breaks[i+1], ']', '\n')
  
}

