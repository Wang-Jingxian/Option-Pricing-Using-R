setwd("C:/Users/Thinkpad/Desktop/FE")
data1 <- read.csv("AAPL.csv", stringsAsFactors = FALSE) # Import Apple(AAPL)
data2 <- read.csv("FB2A.BE.csv", stringsAsFactors = FALSE) # Import Facebook(FB)
data3 <- read.csv("WMT.csv", stringsAsFactors = FALSE) # Import Wamart(WMT)
n1 <- nrow(data1) # The last row number of AAPL data
n2 <- nrow(data2) # The last row number of FB data
n3 <- nrow(data3) # The last row number of WMT data
AAPL <- data1[, 6]                   # Adjusted stock price of AAPL
FB <- data2[, 6]                     # Adjusted stock price of FB
WMT <- data3[, 6]                    # Adjusted stock price of WMT

AAPL.r <- diff(log(AAPL))            # Stock daily return for AAPL
FB.r <- diff(log(FB))                # Stock daily return for FB
WMT.r <- diff(log(WMT))              # Stock daily return for WMT
AAPL.vol <- sd(AAPL.r)/sqrt(1/252)   # Stock daily volatility for AAPL
FB.vol <- sd(FB.r)/sqrt(1/252)       # Stock daily volatility for FB
WMT.vol <- sd(WMT.r)/sqrt(1/252)     # Stock daily volatility for WMT

set.seed(5)

d <- 10000          # Simulation trials
T1 <- 1             # Time until 1st record of best performing stock (in years)
T2 <- 2             # Time until expiration, also the 2nd record (in years)
r <- 0.02904        # risk-free rate (U.S. 10 Year Treasury)
K <- 1              # Strike for return: 100%
principal <- 1000   # Principal

m <- T1 * 252             # Number of subintervals
m2 <- T2 * 252
delta.t <- T1 / m         # Time per subinterval (in years)

f <- rep(0, d)      # Discounted payoff

s1 <- rep(0, m2 + 1)   # s1, s2, s3 are used to stock the stock price
s2 <- rep(0, m2 + 1) 
s3 <- rep(0, m2 + 1)
s1[1] <- AAPL[n1]     # initial stock price AAPL
s2[1] <- FB[n2]       # initial stock price FB
s3[1] <- WMT[n3]      # initial stock price WMT

ret.t1 <- rep(0, d)    # The return of the 1st stock(AAPL) in the 1st year
ret.t2 <- rep(0, d)    # The return of the 2nd stock(FB) in the 1st year
ret.t3 <- rep(0, d)    # The return of the 3RD stock(WMT) in the 1st year
ret.t11 <- rep(0, d)   # The return of the 1st stock in the 2nd year
ret.t21 <- rep(0, d)   # The return of the 2nd stock in the 2nd year
ret.t31 <- rep(0, d)   # The return of the 3rd stock in the 2nd year
maxi <- rep(0, d)  # The 1st year's maximum return
maxi2 <- rep(0, d) # The 2nd year's maximum return
a <- rep(0, d)     # a is the average of the 1st and 2nd year's maximum return



for (j in 1:d) {   # d times simulation
  # Simulate stock price path in the 1st year period: time 2 to (m + 1)
  for (i in 2:m) {      
    W1   <- sqrt(delta.t) * rnorm(1)  # Draw from normal distribution
    ret1 <- (r-(1/2)*AAPL.vol^2) * delta.t + AAPL.vol * W1 # AAPL return
    s1[i] <- s1[i - 1] * exp(ret1) # Stock Price prediction for APPL
    
    W2   <- sqrt(delta.t) * rnorm(1)
    ret2 <- (r-(1/2)*FB.vol^2) * delta.t + FB.vol * W2
    s2[i] <- s2[i - 1] * exp(ret2) # Stock Price prediction for FB
    
    W3  <- sqrt(delta.t) * rnorm(1)
    ret3  <- (r-(1/2)*WMT.vol^2) * delta.t + WMT.vol * W3
    s3[i] <- s3[i - 1] * exp(ret3) # Stock Price prediction for WMT
  }
  ret.t1[j] <- s1[m]/s1[1] # Return for AAPL for each simulation (1st year)
  ret.t2[j] <- s2[m]/s2[1] # Return for FB for each simulation (1st year)
  ret.t3[j] <- s3[m]/s3[1] # Return for WMT for each simulation (1st year)
  
  # maxi is the highest return in the 1st year for d times simulations
  maxi[j] <- max(ret.t1[j], ret.t2[j], ret.t3[j]) 
  
  ############################################################################
  # If the return for the 3rd stock is the highest in time j's simulation in 
  # the 1st year, then simulate the two other stocks for d times and denote 
  # the 1st stock's time j's return as ret.t11[j], 2nd stock's time j's return  
  # as ret.t21[j], and 3rd stock's return as 0. The 2nd year's maximum stock 
  # value at j time's simulation is represented by maxi2[j].
  # The 2nd year's simulation period is (m + 2)(day 253) to (m2 + 1)(day 505)
  ############################################################################
  if (ret.t3[j] == maxi[j]){     #If the 3rd stock performs best in the 1st year
    for (i in (m + 1):(m2)){ #Simulate 1st & 2nd stocks for the 2nd year
      W1.2   <- sqrt(delta.t) * rnorm(1)
      ret1.2 <- (r-(1/2)*AAPL.vol^2) * delta.t + AAPL.vol * W1.2
      s1[i] <- s1[i - 1] * exp(ret1.2) #Simulation of 1st stock in the 2nd year
      
      W2.2   <- sqrt(delta.t) * rnorm(1)
      ret2.2 <- (r-(1/2)*FB.vol^2) * delta.t + FB.vol * W2.2
      s2[i] <- s2[i - 1] * exp(ret2.2) #Simulation of 2nd stock in the 2nd year
    }
    ret.t11[j] <- s1[m2]/s1[m + 1] #Return of the 1st stock in the 2nd year
    ret.t21[j] <- s2[m2]/s2[m + 1] #Return of the 2nd stock in the 2nd year
    ret.t31[j] <- 0                    #Return of the 3rd stock in the 2nd year
    # maxi2 is the highest return in the 2st year for d times simulations
    # with the 1st year's highest return stock eliminated in the 2nd year
    maxi2[j] <- max(ret.t11[j], ret.t21[j])
  }
  
  if (ret.t2[j] == maxi[j]){     #If the 2nd stock performs best in the 1st year
    for (i in (m + 1):(m2)){ #Simulate 1st & 3rd stocks for the 2nd year
      W1.2   <- sqrt(delta.t) * rnorm(1)
      ret1.2 <- (r-(1/2)*AAPL.vol^2) * delta.t + AAPL.vol * W1.2
      s1[i] <- s1[i - 1] * exp(ret1.2)
      
      W3.2   <- sqrt(delta.t) * rnorm(1)
      ret3.2 <- (r-(1/2)*WMT.vol^2) * delta.t + WMT.vol * W3.2
      s3[i] <- s3[i - 1] * exp(ret3.2)
    }
    ret.t11[j] <- s1[m2]/s1[m + 1]
    ret.t21[j] <- 0
    ret.t31[j] <- s3[m2]/s3[m + 1]
    maxi2[j] <- max(ret.t11[j], ret.t31[j])
  }
  
  if (ret.t1[j] == maxi[j]){     #If the 1st stock performs best in the 1st year
    for (i in (m + 1):(m2)){ #Simulate 2nd & 3rd stocks for the 2nd year
      W2.2   <- sqrt(delta.t) * rnorm(1)
      ret2.2 <- (r-(1/2)*FB.vol^2) * delta.t + FB.vol * W2.2
      s2[i] <- s2[i - 1] * exp(ret2.2)
      
      W3.2   <- sqrt(delta.t) * rnorm(1)
      ret3.2 <- (r-(1/2)*WMT.vol^2) * delta.t + WMT.vol * W3.2
      s3[i] <- s3[i - 1] * exp(ret3.2)
    }
    ret.t11[j] <- 0
    ret.t21[j] <- s2[m2]/s2[m + 1]
    ret.t31[j] <- s3[m2]/s3[m + 1]
    maxi2[j] <- max(ret.t21[j], ret.t31[j]) 
  }
  ################################################################
  # a[j] is the average of the 1st and 2nd year's best return
  # for each simulation with the 1st year's highest return stock
  # eliminated in the 2nd year
  ################################################################
  a[j] <- (maxi[j] + maxi2[j])/2 
  
  # f[j] is the option payoff for each simulation
  f[j] <- max(a[j] - K, 0) * principal
}

Option_P = mean(f) * exp(-r * T2)  #Discounted Option Payoff
Bond_P = principal * exp(-r * T2)  #PV of Bond
participation = (1000 - principal * exp(-r * T2))/Option_P  #Participation Rate

#The Option price is the discounted average payoff
cat("Discounted Option Payoff:", round(Option_P, 4), "\n")
cat("Standard Error:", round(sd(f) / sqrt(d), 4), "\n")
cat("Participation Rate:", round(participation, 4))
cat("Initial Investment:", round(mean(f) * participation 
                                 * exp(-r * T2) + Bond_P, 4), "\n")
cat("Option Price:", Option_P * participation)

q <- rep(0, d)  # Option payoff with participation rate
for(j in 1:d){
  q[j] = f[j] * participation
}

library("ggplot2") #Plot the discounted payoff as histogram
b <- ggplot(as.data.frame(f), aes(x = f)) 
b + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(f)),
             linetype = "dashed", size = 0.6)

library(psych)
describe(q)
summary(q)





