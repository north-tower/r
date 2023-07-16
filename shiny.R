library(shiny)



Lxs = c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
        9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
        6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
        0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000)
Lx = Lxs[-(1:3)]

#X
Wholelife_annuity = function(age,ir){
  lx = Lx[-(1:(age-20))]; if (age==20) lx = Lx
  V = 1/(1+ir)
  epv = sum( (V^(0:(120-age)))*lx )/Lx[age-20+1]
  return(epv)}

Wholelife_assurance = function(age,ir){
  DD = ir/(1+ir)
  epv = 1 - DD*Wholelife_annuity(age,ir)
  return(epv)}

#Y
Wholelife_annuity_y = function(age,ir){
  lx = Lx[-(1:(age-20))]; if (age==20) lx = Lx
  V = 1/(1+ir)
  epv = sum( (V^(0:(120-age)))*lx )/Lx[age-20-3]
  return(epv)}

Wholelife_assurance_y = function(age,ir){
  DD = ir/(1+ir)
  epv = 1 - DD*Wholelife_annuity_y(age,ir)
  return(epv)}



ui = fluidPage(
  titlePanel("Insurance Benefit Valuation Tool"),
  sidebarPanel(
    sliderInput(inputId="Age",label="Age", value = 30,min=20,max=90),
    numericInput("IR",label="Interest Rate (in %)",value=4,min=0,max =15),
    radioButtons("PremiumPayment",label="Premium payment", 
                 choices = list("Single","Level"), selected = "Level"),
    radioButtons("InsuranceBenefit",label="Insurance Benefit", 
                 choices = list("Pure endowment","Term assurance","Endowment"), selected = "Pure endowment"),
    radioButtons("BenefitPayment",label="Insurance Benefit payment", 
                 choices = list("End of year of death","Immediately on death"), selected = "End of year of death"),
    numericInput(inputId="AssuredSum",label="Assured Sum",value=1,min=0,max=100000000),
    numericInput(inputId="BenefitTerm",label="Benefit Term",value=10,min=0,max=100000000),
  ),
  mainPanel( textOutput("prem"), textOutput("OutputText")
              , textOutput("claim"),textOutput("text")
              ,plotOutput("chart")
              )
)


server = function(input,output) {
  output$OutputText = renderText( {
    age = input$Age
    InterestRate = input$IR/100
    
    if (input$BenefitPayment=="End of year of death") {EPVB = Wholelife_assurance(age,InterestRate) }
    if (input$BenefitPayment=="Immediately on death") {EPVB = ((1+InterestRate)^0.5)*Wholelife_assurance(age,InterestRate) }
    
    Prem = input$AssuredSum*EPVB
    IntialExpenses = Prem*100
    
    
    paste0("Intial Expenses:" , IntialExpenses)
    
  } )
  output$prem = renderText({
    age = input$Age
    InterestRate = input$IR/100
    
    if (input$BenefitPayment=="End of year of death") {EPVB = Wholelife_assurance(age,InterestRate) }
    if (input$BenefitPayment=="Immediately on death") {EPVB = ((1+InterestRate)^0.5)*Wholelife_assurance(age,InterestRate) }
    
    Prem = input$AssuredSum*EPVB
    paste0("Premium:", Prem)
  })
  output$claim = renderText({
    ClaimExpenses = input$BenefitTerm*100
    paste0("Claim Expenses:" , ClaimExpenses)
  })
  output$text = renderText({
    age = input$Age
    InterestRate = input$IR/100
    
    if (input$BenefitPayment=="End of year of death") {EPVB = Wholelife_assurance(age,InterestRate) }
    if (input$BenefitPayment=="Immediately on death") {EPVB = ((1+InterestRate)^0.5)*Wholelife_assurance(age,InterestRate) }
    
    Prem = input$AssuredSum*EPVB
    PremiumExpenses = Prem*100
    if (input$PremiumPayment=="Level") {paste0("Premium Expenses:" , PremiumExpenses)}
    
  })
  
  output$chart = renderPlot({
    age = input$Age
    InterestRate = 0:15
    
    if (input$BenefitPayment=="End of year of death") {EPVB = Wholelife_assurance(age,InterestRate) }
    if (input$BenefitPayment=="Immediately on death") {EPVB = ((1+InterestRate)^0.5)*Wholelife_assurance(age,InterestRate) }
    
    Prem = input$AssuredSum*EPVB
    
    plot(Prem,InterestRate)
  })
}


shinyApp(ui = ui , server = server)