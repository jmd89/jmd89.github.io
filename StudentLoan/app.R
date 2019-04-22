library(shiny)
library(shinythemes)
#setwd('~/Documents/jmd89.github.io/StudentLoan/')
#source('websiteFunctions.R')

# Initial values
age = 24
salary = 30000
deductions = 0
loan = 30000
loanInterest = 0
startYear = 2015
gradYear = 2018
savings = 0
country = 'England/Wales'

# Functions

########## Functions
# Estimate loan interest rate
est_intRate <- function(startYear, salary, country){
  if (startYear < 2012 | country == 'Scotland'){ # if Plan 1 loan
    return(1.75)
  }else{
    if (salary >= 45000){
      return(6.3)
    }else{
      if (salary < 25000){
        return(3.3)
      }else{
        return(3.3 + ((salary - 25000)/20000*3))
      }
    }
  }
}

payments_calc <- function(salary, country, startYear){
  if (country == 'Scotland' | startYear < 2012){ # if Plan 1 loan
    if (salary > 18935){
      return(.09*(salary-18935))
    }else{
      return(0)
    }
  }else{
    if (salary > 25725){
      return(.09*(salary-25725))
    }else{
      return(0)
    }
  }
}

# Calculate the number of remaining years left to pay
# nYears_calc <- function(gradYear){ # this isn't used?
#   return(30 - (as.numeric(format(Sys.Date(), "%Y")) - gradYear))
# }

paymentYears_calc <- function(startYear, gradYear, country, age){
  if (startYear < 2007 && country == 'England/Wales'){ # only 65+ forgiveness
    nYears <- 64 - age
  }else if (startYear > 2006 && startYear < 2012 && country == 'England/Wales'){
    nYears <- 25 - (as.numeric(format(Sys.Date(), "%Y")) - gradYear) # 25 year forgiveness
  }else if (startYear >= 2012 && country == 'England/Wales'){
    nYears <- 30 - (as.numeric(format(Sys.Date(), "%Y")) - gradYear) # 25 year forgiveness
  }else if (startYear < 2007 && country == 'Scotland'){
    if (65 - age < (30 - (as.numeric(format(Sys.Date(), "%Y")) - gradYear))){
      nYears <-  65 - age
    }else{
      nYears <- 30 - (as.numeric(format(Sys.Date(), "%Y")) - gradYear)
    }
  }else if (startYear >= 2007 && country == 'Scotland'){
    nYears <- 30 - (as.numeric(format(Sys.Date(), "%Y")) - gradYear)
  }
  return(seq(as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(Sys.Date(), "%Y")) + nYears, 1))
}

# Calculate balances
PAYEbalances_calc <- function(loan, salary, country, startYear, nYears, interest, lumpSum = 0){
  payment <- payments_calc(salary, country, startYear)
  balances <- numeric()
  x <- loan - lumpSum
  for (i in 1:nYears){
    if (x > payment){
      x <- (1+interest/100)*(x - payment)
      balances <- c(balances, x)
    }else{
      balances <- c(balances, 0)
      return(balances)
    }
  }
  return(balances)
}

# Calculate total paid
PAYEpaid_calc <- function(loan, salary, country, startYear, nYears, interest, lumpSum = 0){
  payment <- payments_calc(salary, country, startYear)
  balances <- numeric()
  x <- loan - lumpSum
  paid <- 0
  for (i in 1:nYears){
    if (x > payment){
      x <- (1+interest/100)*(x - payment)
      paid <- paid + payment
      balances <- c(balances, x)
    }else{
      balances <- c(balances, 0)
      paid <- paid + x
      return(paid)
    }
  }
  return(paid)
}

trim <- function(x){
  formatC(x, format = "d", big.mark = ',')
}

runValidations <- function(x, text = TRUE){
  if (text){
    validate(
      need(x$startYear < x$gradYear, paste('Graduation year must be after starting year.', sep = "\n")),
      need(x$salary, paste('Enter your annual salary. Do not type the £ symbol.', ' ', sep = "\n")),
      need(x$deductions, paste('Enter monthly salary deductions. Put zero (0) if  you do not have any deductions. Do not type the £ symbol.', ' ', sep = "\n")),
      need(x$loan, paste('Enter loan balance. Do not type the £ symbol.', ' ', sep = "\n")),
      need(x$loanInterest, paste('Enter loan interest rate. Put zero (0) and the calculator will estimate your interest rate based on the values given. Do not type the % symbol.', ' ', sep = "\n")),
      need(x$gradYear, paste('Enter your graduation year.', ' ', sep = "\n")),
      need(x$savings, paste('Enter your savings. Put zero (0) if you do not have any savings. Do not type the £ symbol', ' ', sep = "\n")),
      need(x$savings <= x$loan, paste('Only enter how much you are planning on using to pay down your loan, not your total savings.'))
      
    )
  }else{
    validate(
      need(x$startYear < x$gradYear, paste(" ")),
      need(x$salary, " "),
      need(x$deductions, " "),
      need(x$loan, " "),
      need(x$loanInterest, " "),
      need(x$gradYear, " "),
      need(x$savings, " "),
      need(x$savings <= x$loan, paste(' '))
      
    )
  }
}

########## Define UI for dataset viewer app ----
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # App title ----
                #titlePanel("Should I Pay Off My Student Loan?"),
                
                # Sidebar layout with a input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Age
                    numericInput(inputId = 'age',
                                 label = 'Your age:',
                                 value = age),
                    
                    # Year Started
                    numericInput(inputId = 'startYear',
                                 label = 'Year you started your studies:',
                                 value = startYear),
                    
                    # Input: Graduation Year
                    numericInput(inputId = "gradYear",
                                 label = "Graduation year",
                                 value = gradYear),
                    
                    # Input: Country of Residence
                    selectInput(inputId = 'country',
                                label = 'Country of residence when loan was taken:',
                                choices = c('England/Wales', 'Scotland'),
                                selected = country),
                    
                    # Input: Loan
                    numericInput(inputId = "loan",
                                 label = "Current Student Loan Balance:",
                                 value = loan),
                    
                    # Input: Interest Rate
                    numericInput(inputId = "loanInterest",
                                 label = "If you have been put on a non-standard interest rate enter it here, otherwise leave it as zero and it will be calculated using the details given above:",
                                 value = loanInterest),
                    
                    numericInput(inputId = "salary",
                                 label = "Annual Salary (Before Tax):",
                                 value = salary),
                    # Deductions
                    numericInput(inputId = "deductions",
                                 label = "Monthly salary sacrifice deductions shown on your paycheque (e.g. pension payments, cycle to work scheme, etc.):",
                                 value = deductions),
                    
                    # Input: Savings
                    numericInput(inputId = "savings",
                                 label = "Savings (enter how much you are planning on using to pay down your student loan, put zero otherwise):",
                                 value = savings,
                                 min = 0),
                    
                    # Button
                    actionButton("generate", "Generate Report", icon = icon("arrow-right"), width = 200)
                    
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # PAYE
                    htmlOutput("info"),
                    plotOutput("PAYEPlot"),
                    htmlOutput("PAYEPlotText"),
                    htmlOutput("PAYEStats")
                  )
                )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  observe({
    req(input$generate)
    # Updates goButton's label and icon
    updateActionButton(session, "generate",
                       label = "Update Report",
                       icon = icon("refresh"))
  })
  
  # Estimate interest rate if needed
  temp_loanInterest <- eventReactive(input$generate,{
    input$loanInterest
  })
  
  loanInterest_calc <- eventReactive(input$generate,{
    if (temp_loanInterest() == 0){
      est_intRate(input$startYear, input$salary, input$country)
    }else{
      temp_loanInterest()
    }
  })
  
  paymentYears <- eventReactive(input$generate, {
    paymentYears_calc(input$startYear, input$gradYear, input$country, input$age)
  })
  
  PAYEbalances <- eventReactive(input$generate, {
    PAYEbalances_calc(input$loan, input$salary-(input$deductions*12), input$country, input$startYear, length(paymentYears()), loanInterest_calc())
  })
  
  PAYEpaid <- eventReactive(input$generate,{
    PAYEpaid_calc(input$loan, input$salary-(input$deductions*12), input$country, input$startYear, length(paymentYears()), loanInterest_calc())
  })
  
  repayBalances <- eventReactive(input$generate, {
    PAYEbalances_calc(input$loan, input$salary-(input$deductions*12), input$country, input$startYear, length(paymentYears()), loanInterest_calc(), input$savings)
  })
  
  repayPaid <- eventReactive(input$generate, {
    PAYEpaid_calc(input$loan, input$salary-(input$deductions*12), input$country, input$startYear, length(paymentYears()), loanInterest_calc(), input$savings)
  })
  
  paymentAmount <- eventReactive(input$generate,{
    payments_calc(input$salary-(input$deductions*12), input$country, input$startYear)
  })
  
  interestInfo_text <- eventReactive(input$generate, {
    if (input$loanInterest == 0){
      paste('An interest rate of ', loanInterest_calc(), '% has been used for the calculations. This is based on the country you lived in when you took out your loan, the year you started university, and your current salary.', sep = '')
    }else{
      return()
    }
  })
  
  forgiveText <- eventReactive(input$generate,{
    if (tail(PAYEbalances(), n=1) > 0){
      if (input$country == 'England/Wales'){
        if (input$startYear < 2007){ # Only 65+ forgiveness
          paste('Based on the information provided your loan will be forgiven once you reach 65 years of age. The remaining years until this date have been used in the calculations.')
        }else if (input$startYear < 2012){ # if English Plan 1, 25 year forgiveness
          paste('Based on the information provided your loan will be forgiven 25 years after graduation. The remaining years until this date have been used in the calculations.')
        }else{
          paste('Based on the information provided your loan will be forgiven 30 years after graduation. The remaining years until this date have been used in the calculations.')
        }
      }else if (input$startYear < 2007){ # else if Scottish before 2007
        if (65 - input$age < 30 - (as.numeric(format(Sys.Date(), "%Y")) - input$gradYear)){ # if age forgiveness is sooner
          paste('Based on the information provided, you are going to turn 65 before the 30 year forgiveness period ends. The remaining years until you turn 65 have been used in the calculations.')
        }else{
          paste('Based on the information provided your loan will be forgiven 30 years after graduation. The remaining years until this date have been used in the calculations.')
        }
      }else{
        paste('Based on the information provided your loan will be forgiven 30 years after graduation. The remaining years until this date have been used in the calculations.')
      }
    }else{
      return()
    }
  })
  
  PAYEStats_text <- eventReactive(input$generate, {
    runValidations(input, text = FALSE)
    if (length(PAYEbalances()) > 1){
      if (tail(PAYEbalances(), n=1) > 0) {
        paste('At your current salary you are not set to pay off your student loan from salary deductions. You will pay £', trim(paymentAmount()/12), ' a month from your paycheque and will pay back £', trim(round(PAYEpaid(),0)), ' before your loan is forgiven.
              However, at the end of the repayment period your outstanding balance of £', trim(round(tail(PAYEbalances(), 1), 0)), ' will be written off, meaning you will never have to pay this back.', sep = '')
      }else if (length(PAYEbalances()) == 2){
        paste('You will pay off your student loan in ', length(PAYEbalances())-1, ' year from salary deductions alone. Over this time period you will pay back £', trim(round(PAYEpaid(), 0)),
              ', meaning you will pay £', trim(round(PAYEpaid()-input$loan, 0)), ' in interest',
              ' (or ', trim(round(PAYEpaid()/input$loan*100-100, 2)), '% of the current loan balance).', sep = '')
      }else{
        paste('You will pay off your student loan in ', length(PAYEbalances())-1, ' years from salary deductions alone. You will pay £', trim(paymentAmount()/12), ' a month from your paycheque and will pay back £', trim(round(PAYEpaid(), 0)),
              ' in total, meaning you will pay £', trim(round(PAYEpaid()-input$loan, 0)), ' in interest',
              ' (or ', trim(round(PAYEpaid()/input$loan*100-100, 2)), '% of the current loan balance).', sep = '')
      }
    }else{
      'It looks like you will pay off your student loan this year! Congratulations! You should contact the student loans company to set up a direct debit and avoid overpayment through the automatic salary deduction system.'
    }
  })
  
  repayEarly_text <- eventReactive(input$generate, {
    runValidations(input, text = FALSE)
    if (input$savings == input$loan) {
      if (PAYEpaid() < input$loan){
        paste('If you pay off your student loan completely using your savings you will pay £', trim(round(input$loan-PAYEpaid(), 0)),
              ' more than if you keep paying through salary deductions, plus you\'ll have spent all of your savings!', sep = '')
      }else{
        paste('If you pay off your student loan completely using your savings you will save £', trim(round(PAYEpaid()-input$loan, 0)),
              ' by not paying interest, but you might lose out if you could save at a higher interest rate than your loan is currently at.', sep = '')
      }
    }else{
      if (0 < input$savings && input$savings < input$loan && PAYEpaid() < repayPaid() + input$savings){ # if using savings RESULTS IN PAYING MORE
        if (tail(repayBalances(), n = 1) == 0){ # if paying down loan results in loan being paid off
          if (length(repayBalances()) == 2){
            paste('Paying down your student loan means you will pay it off completely in ', length(repayBalances())-1, ' year. However, you will pay a total of £', trim(repayPaid() + input$savings), ', which is £', trim(repayPaid() + input$savings - PAYEpaid()), ' more than if you keep your savings and keep paying from your paycheque!', sep = '')
          }else{
            paste('Paying down your student loan means you will pay it off completely in ', length(repayBalances())-1, ' year. However, you will pay a total of £', trim(repayPaid() + input$savings), ', which is £', trim(repayPaid() + input$savings - PAYEpaid()), ' more than if you keep your savings and keep paying from your paycheque!', sep = '')
          }
        }else{ # if paying down loan does NOT result in the loan being paid off
          paste('If you pay down your loan using your savings you will still not pay it off from salary deductions, and at the end of the repayment period your outstanding balance of £', trim(tail(repayBalances(), n = 1)), ' will be forgiven. You will pay a total of £', trim(repayPaid() + input$savings), ' which is more than if you keep your savings and keep paying from your paycheque! You simply spent £', trim(input$savings), ' from your savings and then will pay the same amount of money over the repayment period as you would have otherwise.', sep = '')
        }
      }else{ # If paying your loan off SAVES MONEY
        if (tail(repayBalances(), n=1) == 0){ # if paying down loan results in loan being paid off
          if (length(repayBalances()) == 2){
            paste('Paying down your student loan means you will pay it off completely in ', length(repayBalances())-1, ' year. You will pay a total of £', trim(repayPaid() + input$savings), ', which is £', trim(PAYEpaid() - (repayPaid() + input$savings)), ' less than if you keep your savings and keep paying from your paycheque. You should think to yourself whether this saving is worth the risk of not having money put away for an emergency, or whether you could put these savings towards another goal such as buying a house or investing early in a pension.', sep = '')
          }else{
            paste('Paying down your student loan means you will pay it off completely in ', length(repayBalances())-1, ' years. You will pay a total of £', trim(repayPaid() + input$savings), ', which is £', trim(PAYEpaid() - (repayPaid() + input$savings)), ' less than if you keep your savings and keep paying from your paycheque. You should think to yourself whether this saving is worth the risk of not having money put away for an emergency, or whether you could put these savings towards another goal such as buying a house or investing early in a pension.', sep = '')
          }
        }else{
          paste('If you pay down your loan using your savings you will still not pay it off within thirty years of graduation, and at the end of this period your outstanding balance of £', trim(tail(repayBalances(), n = 1)), ' will be forgiven. You will pay a total of £', trim(repayPaid() + input$savings), ' which is £', trim(PAYEpaid() - (repayPaid() + input$savings)), ' less than if you keep your savings and keep paying from your paycheque. You should think to yourself whether this saving is worth the risk of not having money put away for an emergency, or whether you could put these savings towards another goal such as buying a house or investing early in a pension.', sep = '')
        }
      }
    }
  })
  
  
  #Output
  
  output$info <- eventReactive(input$generate, {
    runValidations(input, text = TRUE)
    if (input$startYear < input$gradYear){
      HTML(paste(interestInfo_text(), forgiveText(), sep = '<br><br>'))
    }
  })
  
  PAYE_plotData <- eventReactive(input$generate,{
    runValidations(input, text = FALSE)
    data.frame(paymentYears()[0:length(PAYEbalances())], PAYEbalances())
  })

  repay_plotData <- eventReactive(input$generate,{
    runValidations(input, text = FALSE)
    data.frame(paymentYears()[0:length(repayBalances())], repayBalances())
  })

  PAYE_plot <- eventReactive(input$generate,{
    if (input$savings == 0){
      plot(PAYE_plotData(), xlab = "Year", ylab = "Loan Balance (£)", ylim = c(0, ceiling(max(PAYEbalances())/2500)*2500), type = "b")
    }else{
      plot(PAYE_plotData(), xlab = "Year", ylab = "Loan Balance (£)", ylim = c(0, ceiling(max(PAYEbalances())/2500)*2500), type = "b")
      lines(repay_plotData(), col = "red", type = "b")
    }
  })

  output$PAYEPlot <- renderPlot({
    #runValidations(input, text = FALSE)
    if (length(PAYEbalances()) == 1){
      return()
    }else{
      PAYE_plot()
    }
  })
  
  output$PAYEStats <- eventReactive(input$generate, {
    runValidations(input, text = FALSE)
    if (input$savings == 0 | input$savings == input$loan){
      plotText = 'The plot above shows the balances of your loan as you pay if off from your paycheque, including the effect of interest on your balance.'
    }else{
      plotText = 'The black points in the plot above show the balances of your loan as you pay if off from your paycheque, including the effect of interest on your balance. The red points show your balances if you use your savings to pay down your loan early.'
    }
    if (input$savings == 0 && length(PAYEbalances()) > 1) {
      HTML(paste(plotText, PAYEStats_text(), sep='<br/><br/>'))
    }else if (input$savings == 0){
      HTML(paste(PAYEStats_text(), sep='<br/><br/>'))
    }else if (length(PAYEbalances()) == 1){
      HTML(paste(PAYEStats_text(), 'At this stage, so long as you have a suitable savings pot for emergencies, you could use your savings to pay down your student loan and save on any added interest.', sep="<br/><br/>"))
    }else if (input$savings > 0){
      HTML(paste(plotText, PAYEStats_text(), paste(repayEarly_text()), sep="<br/><br/>"))
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)