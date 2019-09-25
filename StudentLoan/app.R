library(shiny)
library(shinythemes)
options(scipen=999)

# Initial values
age = 24
salary = 30000
deductions = 0
loan = 30000
loanInterest = 0
startYear = 2014
gradYear = 2017
savings = 0
country = 'England/Wales'
plan1Threshold = 18935
plan2LowerThreshold = 25725
plan2UpperThreshold = 46305

########## Functions
# Estimate loan interest rate
intRate_calc <- function(startYear, salary, country){
  if (startYear < 2012 | country == 'Scotland'){ # if Plan 1 loan
    return(1.75)
  }else{ # if Plan 2 loan
    if (salary >= plan2UpperThreshold){ # if salary is above upper threshold
      return(5.4) # max interest rate
    }else{
      if (salary < plan2LowerThreshold){ # if salar is below lower threshold
        return(2.4) # min interest rate
      }else{
        return(2.4 + ((salary - plan2LowerThreshold)/(plan2UpperThreshold-plan2LowerThreshold) * 3)) # salary minus the threshold as a percentage of the 3% extra
      }
    }
  }
}

# Calculate payments
payments_calc <- function(salaries, country, startYear){ #increases payments with salary increases
  paymentList <- numeric()
  if (country == 'Scotland' | startYear < 2012){ # if Plan 1 loan
    for (s in salaries){
      if (s > plan1Threshold){ # if salary is larger than threshold
        paymentList <- c(paymentList, .09*(s-plan1Threshold)) # payment is 9% of salary above threshold
      }else{
        paymentList <- c(paymentList, 0) # otherwise payment is zero
      }
    }
  }else{ # if Plan 2 loan
    for (s in salaries){
      if (s > plan2LowerThreshold){ # if salary is larger than threshold
        paymentList <- c(paymentList, .09*(s-plan2LowerThreshold)) # payment is 9% of salary above threshold
      }else{
        paymentList <- c(paymentList, 0) # otherwise payment is zero
      }
    }
  }
  return(paymentList)
}

# Calculate salaries
salaryIncreases <- function(salary, deductions, increase, nYears){
  salaryList <- c(salary-deductions*12) # create list of post-deduction salaries
  currentSalary <- salary # current annual salary without deductions
  deduct <- deductions*12 # calculate annual salary deductions
  for (i in 1:nYears){
    deduct <- (1+increase/100)*(deduct) # calculate increase in deductions
    currentSalary <- (1+increase/100)*(currentSalary) # calculate increase in salary
    salaryList <- c(salaryList, currentSalary-deduct) # add post-deduction salary to list
  }
  return(salaryList)
}

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

PAYEbalances_calc <- function(loan, salaries, country, startYear, nYears, interest, customInterest = 'No', lumpSum = 0){
  payments <- payments_calc(salaries, country, startYear)
  balances <- numeric()
  x <- loan - lumpSum
  for (i in 1:nYears){
    if (x > payments[i]){
      if (interest == 0 | customInterest == 'No'){
        intRate <- intRate_calc(startYear, salaries[i], country)
      }else{
        intRate <- interest
      }
      x <- (1+intRate/100)*(x - payments[i])
      balances <- c(balances, x)
    }else{
      balances <- c(balances, 0)
      return(balances)
    }
  }
  return(balances)
}


# Calculate total paid
PAYEpaid_calc <- function(loan, salaries, country, startYear, nYears, interest, customInterest = 'No', lumpSum = 0){
  payments <- payments_calc(salaries, country, startYear)
  balances <- numeric()
  x <- loan - lumpSum
  paid <- 0
  for (i in 1:nYears){
    if (x > payments[i]){
      if (interest == 0 | customInterest == 'No'){
        intRate <- intRate_calc(startYear, salaries[i], country)
      }else{
        intRate <- interest
      }
      x <- (1+intRate/100)*(x - payments[i])
      paid <- paid + payments[i]
    }else{
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
      need(x$salaryIncrease, paste('Enter your expected annual salary increase as a percentage. Do not type the % symbol.')),
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
      need(x$salaryIncrease, paste(' ')),
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
                    selectInput(inputId = 'customInterest',
                                label = 'Have you been put on a non-standard interest rate (e.g. due to non payment while living abroad)?:',
                                choices = c('Yes', 'No'),
                                selected = 'No'),
                    
                    conditionalPanel(
                      condition = "input.customInterest ==  'Yes'",
                      numericInput(inputId = "loanInterest",
                                   label = "Enter interest rate:",
                                   value = loanInterest)
                    ),
                    
                    # Input: Salary
                    numericInput(inputId = "salary",
                                 label = "Annual Salary (Before Tax):",
                                 value = salary),
                    
                    # Deductions
                    numericInput(inputId = "deductions",
                                 label = "Monthly salary sacrifice deductions shown on your paycheque (e.g. pension payments, cycle to work scheme, etc.):",
                                 value = deductions),
                    
                    # Input: Salary increase percentage
                    numericInput(inputId = "salaryIncrease",
                                 label = "Enter your expected percentage annual payrise (your deductions will increase at the same rate):",
                                 value = 2.0),
                    
                    conditionalPanel(
                      condition = "input.salaryIncrease > 0",
                      checkboxInput(inputId = "plotSalary",
                                    label = "Plot your predicted salary increases?:",
                                    value = TRUE,
                                    width = '300px')
                    ),
                    
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
                    htmlOutput("PAYEStats"),
                    
                    textOutput("testText")
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
    if (temp_loanInterest() == 0 | input$customInterest == 'No'){
      intRate_calc(input$startYear, input$salary, input$country)
    }else{
      temp_loanInterest()
    }
  })
  
  paymentYears <- eventReactive(input$generate, {
    paymentYears_calc(input$startYear, input$gradYear, input$country, input$age)
  })
  
  salaries <- eventReactive(input$generate,{
    salaryIncreases(input$salary, input$deductions, input$salaryIncrease, length(paymentYears()))
  })
  
  PAYEbalances <- eventReactive(input$generate, {
    PAYEbalances_calc(input$loan, salaries(), input$country, input$startYear, length(paymentYears()), input$loanInterest, customInterest = input$customInterest, lumpSum = 0)
  })
  
  PAYEpaid <- eventReactive(input$generate,{
    PAYEpaid_calc(input$loan, salaries(), input$country, input$startYear, length(paymentYears()), input$loanInterest, customInterest = input$customInterest, lumpSum = 0)
  })
  
  repayBalances <- eventReactive(input$generate, {
    PAYEbalances_calc(input$loan, salaries(), input$country, input$startYear, length(paymentYears()), input$loanInterest, customInterest = input$customInterest, input$savings)
  })
  
  repayPaid <- eventReactive(input$generate, {
    PAYEpaid_calc(input$loan, salaries(), input$country, input$startYear, length(paymentYears()), input$loanInterest, customInterest = input$customInterest, input$savings)
  })
  
  disclaimer_text <- 'This calculator is a work in progress. The code is available on <a href="https://github.com/jmd89/jmd89.github.io">GitHub</a> if you would like to see how the calculations are made, and/or if you want to contribute to the project.'
  
  interestInfo_text <- eventReactive(input$generate, {
    if (input$salaryIncrease > 0){
      if (input$loanInterest == 0 | input$customInterest == 'No'){
        if (input$country == 'Scotland' | input$startYear < 2012 | input$salary > plan2UpperThreshold){
          paste('A loan interest rate of ', round(loanInterest_calc(), 2), '% has been used for the calculations. This is based on the country you lived in when you took out your loan, the year you started university, and your current salary.', sep = '')
        }else if (input$salary < plan2LowerThreshold && tail(salaries(), 1) > plan2LowerThreshold){
          paste('Your current loan interest rate is ', round(loanInterest_calc(), 2), '%, but this will increase as you earn more. Your interest rate is calculated based on the country you lived in when you took out your loan, the year you started university, and your current salary.', sep = '')
        }else if (input$salary > plan2LowerThreshold && input$salary < plan2UpperThreshold){
          paste('Your current loan interest rate is ', round(loanInterest_calc(), 2), '%, but this will increase as you earn more. Your interest rate is calculated based on the country you lived in when you took out your loan, the year you started university, and your current salary.', sep = '')
        }
      }
    }else{
      paste('An interest rate of ', round(loanInterest_calc(), 2), '% has been used for the calculations. This is based on the country you lived in when you took out your loan, the year you started university, and your current salary.', sep = '')
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
        paste('At your current salary you are not set to pay off your student loan from salary deductions. You will pay back £', trim(round(PAYEpaid(),0)), ' before your loan is forgiven.
              However, at the end of the repayment period your outstanding balance of £', trim(round(tail(PAYEbalances(), 1), 0)), ' will be written off, meaning you will never have to pay this back.', sep = '')
      }else if (length(PAYEbalances()) == 2){
        paste('You will pay off your student loan in ', length(PAYEbalances())-1, ' year from salary deductions alone. Over this time period you will pay back £', trim(round(PAYEpaid(), 0)),
              ', meaning you will pay £', trim(round(PAYEpaid()-input$loan, 0)), ' in interest',
              ' (or ', trim(round(PAYEpaid()/input$loan*100-100, 2)), '% of the current loan balance).', sep = '')
      }else{
        paste('You will pay off your student loan in ', length(PAYEbalances())-1, ' years from salary deductions alone. You will pay back £', trim(round(PAYEpaid(), 0)),
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
            paste('Paying down your student loan means you will pay it off completely in ', length(repayBalances())-1, ' years. However, you will pay a total of £', trim(repayPaid() + input$savings), ', which is £', trim(repayPaid() + input$savings - PAYEpaid()), ' more than if you keep your savings and keep paying from your paycheque!', sep = '')
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
    HTML(paste(disclaimer_text, interestInfo_text(), forgiveText(), sep = '<br><br>'))
  })
  
  PAYE_plotData <- eventReactive(input$generate,{
    runValidations(input, text = FALSE)
    data.frame(paymentYears()[0:length(PAYEbalances())], PAYEbalances())
  })
  
  repay_plotData <- eventReactive(input$generate,{
    runValidations(input, text = FALSE)
    data.frame(paymentYears()[0:length(repayBalances())], repayBalances())
  })
  
  salary_plotData <- eventReactive(input$generate,{
    runValidations(input, text = FALSE)
    data.frame(paymentYears()[0:length(salaries())], salaries())
  })
  
  
  PAYE_plot <- eventReactive(input$generate,{
    if (input$salaryIncrease > 0 && input$plotSalary == TRUE){
      if (input$savings == 0){
        ymax <- (ceiling(max(max(PAYEbalances()), max(salaries())))/2500*2500)
        plot(PAYE_plotData(), xlab = "Year", ylab = "£", ylim = c(0, ymax*1.3), type = "b", pch = 15)
        lines(salary_plotData(), col = "blue", type = "b", pch = 16)
        legend(as.numeric(format(Sys.Date(), "%Y")), ymax*1.3, legend=c("Loan Balance", "Salary"),
               col=c("black", "blue"), lty=1:2, cex=0.8, pch = c(15, 16))
      }else{
        ymax <- (ceiling(max(max(PAYEbalances()), max(salaries())))/2500*2500)
        plot(PAYE_plotData(), xlab = "Year", ylab = "£", ylim = c(0, ymax*1.3), type = "b", pch = 15)
        lines(repay_plotData(), col = "red", type = "b", pch = 17)
        lines(salary_plotData(), col = "blue", type = "b", pch = 16)
        legend(as.numeric(format(Sys.Date(), "%Y")), ymax*1.25, legend=c("Loan Balance (repayments only)", "Loan Balance (after lump sum)", "Salary"),
               col=c("black", "red", "blue"), lty=1:2, cex=0.8, pch = c(15, 17, 16))
      }
    }else if (input$savings == 0){
      plot(PAYE_plotData(), xlab = "Year", ylab = "£", ylim = c(0, ceiling(max(PAYEbalances())/2500)*2500*1.3), type = "b", pch = 15)
    }else{
      ymax <- ceiling(max(PAYEbalances()))/2500*2500
      plot(PAYE_plotData(), xlab = "Year", ylab = "£", ylim = c(0,ymax*1.3), type = "b", pch = 15)
      lines(repay_plotData(), col = "red", type = "b", pch = 17)
      legend(as.numeric(format(Sys.Date(), "%Y")), ymax*1.3, legend=c("Loan Balance (repayments only)", "Loan Balance (after lump sum)"),
             col=c("black", "red"), lty=1:2, cex=0.8, pch = c(15, 17))
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
    if (input$plotSalary == FALSE | input$salaryIncrease == 0){
      if (input$savings == 0 | input$savings == input$loan){
        plotText = 'The plot above shows the balances of your loan as you pay if off from your paycheque, including the effect of interest on your balance.'
      }else{
        plotText = 'The black squares in the plot above show the balances of your loan as you pay if off from your paycheque, including the effect of interest on your balance. The red triangles show your balances if you use your savings to pay down your loan early.'
      }
    }else{
      if (input$savings == 0 | input$savings == input$loan){
        plotText = 'The black squares in the plot above show the balances of your loan as you pay if off from your paycheque, including the effect of interest on your balance. The blue circles show your predicted salary as it increases over the years.'
      }else{
        plotText = 'The black squares in the plot above show the balances of your loan as you pay if off from your paycheque, including the effect of interest on your balance. The red triangles show your balances if you use your savings to pay down your loan early. The blue circles show your predicted salary as it increases over the years.'
      }
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