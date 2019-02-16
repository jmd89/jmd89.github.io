rm(list = ls())

# User input
salary <- 36261
monthly_deductions <- 241.74
monthly_bonus <- 11.25
true_salary <- salary - monthly_deductions*12 + monthly_bonus*12
loan <-  4915.56
gradYear <- 2011
country <- NULL
savings <- 2500
homeowner <- NULL
loan_interestRate <- 1.015

# Basic calculations
payment <- if (true_salary > 18330){
  .09*(true_salary-18330)
} else{
  0
}

nYears <- 30 - (as.numeric(format(Sys.Date(), "%Y")) - gradYear)
allYears <- seq(as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(Sys.Date(), "%Y")) + nYears, 1)

# Student loan calculation (PAYE)
loan_PAYE <- numeric()
x <- loan
paid_PAYE <- 0
for (i in 1:nYears) {
  if (x > payment) {
    x <- loan_interestRate*(x - payment)
    paid_PAYE <- paid_PAYE + payment
    loan_PAYE <- c(loan_PAYE, x)
  }else{
    paid_PAYE <- paid_PAYE + x
    x <- 0
    if (x > 0) {
    loan_PAYE <- c(loan_PAYE, x)
    }
  }
}

paidYears_PAYE <- seq(as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(Sys.Date(), "%Y"))+length(loan_PAYE)-1, 1)
plot(paidYears_PAYE, loan_PAYE)
length(loan_PAYE)
tail(loan_PAYE, 1)
paid_PAYE
loan_PAYE_interestPaid <- paid_PAYE - loan
loan_PAYE_interestPaid


# Pay off loan early
loan_early <- numeric()
x <- loan - savings
paid_early <- 0
for (i in 1:nYears) {
  if (x > payment) {
    x <- loan_interestRate*(x - payment)
    paid_early <- paid_early + payment
    loan_early <- c(loan_early, x)
  }else{
    paid_early <- paid_early + x
    x <- 0
    if (x > 0) {
      loan_early <- c(loan_early, x)
    }
  }
}

paidYears_early <- seq(as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(Sys.Date(), "%Y"))+length(loan_early)-1, 1)

loan_early
plot(paidYears_early, loan_early)
tail(loan_early, 1)
paid_early <- paid_early + savings
loan_early_interestPaid <- paid_early - loan
loan_early_interestSaved <- loan_PAYE_interestPaid - loan_early_interestPaid
loan_early_interestSaved

# S&S ISA 
isa_interestRate <- 1.07
isa <- as.numeric()
x <- savings
for (i in 1:length(loan_PAYE)){
  x <- x*isa_interestRate
  isa <- c(isa, x)
}

isa_profit <- tail(isa, 1) - savings
isa_profit
loan_early_interestSaved
isa_profit - loan_early_interestSaved

# Marcus
marcus_interestRate_intro <- 1.0165
marcus_interestRate <- 1.015
marcus <- numeric()
x <- savings
for (i in 1:length(loan_PAYE)){
  if (i == 1){
    x <- x*marcus_interestRate_intro
    marcus <- c(marcus, x)
  }else{
    x <- x*marcus_interestRate
    marcus <- c(marcus, x)
  }
}

marcus_profit <- tail(marcus, 1) - savings
marcus_profit
loan_early_interestSaved
marcus_profit - loan_early_interestSaved
