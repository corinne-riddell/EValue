#TO-DO
#1. Make it work for all types of effect estimates.
#2. Also calculate the bias factor.


library(shiny)
library(tidyverse)
library(plotly)

ui <- fluidPage(
  
  titlePanel("E-value calculator for sensitivity analysis in observational studies"),
  tabsetPanel(
    tabPanel("Calculator",
             fluidRow(
               column(4, HTML("<b/>What is your estimated risk ratio?</b>"),
                      numericInput(inputId = "est.RR", label = NULL, value = 3.9, min = 0.001, max = 999, step = 1, width = '80px')),
               column(4, HTML("<b/>What is the lower bound on its confidence interval?</b>"),
                      uiOutput("lower_CI"),
                      uiOutput("Warning.lcl")),
               column(4, HTML("<b/>What is the upper bound?</b>"),
                      uiOutput("upper_CI"),
                      uiOutput("Warning.ucl"))
             ),
             hr(),
             uiOutput("e.value"),
             uiOutput("e.val.cl.near1"),
             hr(),
             HTML(paste("The e-value is the minimum strength required for both the exposure-confounder and exposure-disease relationships that is required to 'explain away' the estimated relationship between exposure and disease.",
                  " If one of the two parameters is smaller than the e-value, the other must be larger, as defined by the curve below.",
                  " All points along the curve define joint relationships that explain away the estimated effect, including points to the right of the curve.")),
             plotlyOutput("curveOfExplainAway", width = "400px", height = "400px")
    ),
    tabPanel("About",
             HTML(paste0(
               "<br><br>The e-value was introduced by VanderWeele and Ding in 2017 in a publication in the Annals of Internal Medicine.<sup>1, 2</sup>",
               " This calculator was created by Corinne Riddell to facilitate easy e-value computation. Please submit a pull request if you've uncovered a technical error or would like to suggest a stylistic improvement.<br><br>",
               "<b>References</b><br>",
               "1. VanderWeele TJ, Ding P. Sensitivity Analysis in Observational Research: Introducing the E-Value.<i> Annals of Internal Medicine</i>. 2017. 167(4):268-274.<br>", 
               "2. Ding P, VanderWeele TJ. Sensitivity Analysis without assumptions. <i>Epidemiology</i>. 2016. 27:368-77."
             )
             )
    )
    
  )
)

server <- function(input, output) {
  
  output$lower_CI <- renderUI({
    numericInput(inputId = "LCL.RR", label = NULL, value = 1.8, min = 0.001, max = input$est.RR, step = 1, width = '80px')
  }) 
  
  output$upper_CI <- renderUI({  
    numericInput(inputId = "UCL.RR", label = NULL, value = 8.7, min = input$est.RR, max = 999, step = 1, width = '80px')
  })
  
  output$Warning.lcl <- renderUI({
    if(input$est.RR < input$LCL.RR){
      HTML("<font color='red'><b>Error:</b> Please update the lower confidence level so it is smaller than the estimated risk ratio.</font>")
    }
  })

  output$Warning.ucl <- renderUI({
    if(input$est.RR > input$UCL.RR){
      HTML("<font color='red'><b>Error:</b> Please update the upper confidence level so it is greater than the estimated risk ratio.</font>")
    }
  })
  
  e.val = reactive({
    if(input$est.RR > 1){
      input$est.RR + sqrt(input$est.RR*(input$est.RR-1))
    } else{
      (1/input$est.RR) + sqrt((1/input$est.RR)*((1/input$est.RR)-1))
    }
    
  })

  e.val.cl.near.1 = reactive({
    e.val.limit <- NA
    if(input$LCL.RR < 1 & input$UCL.RR > 1){
      e.val.limit <- 1
    }else if(input$LCL.RR < 1 & input$UCL.RR < 1){
      e.val.limit <- (1/input$UCL.RR) + sqrt((1/input$UCL.RR)*((1/input$UCL.RR)-1))
    }else{
      e.val.limit <- input$LCL.RR + sqrt(input$est.RR*(input$LCL.RR-1))
    }
    return(e.val.limit)
  })
  
  which.bound <- reactive({
    which.one <- NA
    if(input$LCL.RR < 1 & input$UCL.RR > 1){
      which.one <- "either"
    }else if(input$LCL.RR < 1 & input$UCL.RR < 1){
      which.one <- "the upper"
    }else{
      which.one <- "the lower"
    }
    return(which.one)
  })
  
   output$e.value <- renderUI({
     HTML(paste0("The e-value for the estimate of the risk ratio is ", round(e.val(), 2), ". The observed risk ratio of ", input$est.RR, " could be explained away by an unmeasured confounder that was associated with both the treatment and the outcome by a risk ratio of ", round(e.val(), 2),"-fold each, above and beyond the measured confounders, but weaker confounding cound not do so." ))
     })
   
   output$e.val.cl.near1 <- renderUI({
     if(input$est.RR >= input$LCL.RR & input$est.RR <= input$UCL.RR){ # estimated RR in range of CI:
     HTML(paste0("<br>The e-value for ", which.bound()," confidence bound is ", round(e.val.cl.near.1(), 2), "."))
     }
   })
   
   output$curveOfExplainAway <- renderPlotly({
     
     rr.ud <- function(rr.eu) {
       if(input$est.RR > 1){
         (input$est.RR*(1 - rr.eu))/(input$est.RR - rr.eu)
       }else{
         ((1/input$est.RR)*(1 - rr.eu))/((1/input$est.RR) - rr.eu)
       }
     }
     
     ggplotly(
       ggplot(data.frame(rr.eu = c(0, 20)), aes(rr.eu)) + 
         stat_function(fun = rr.ud) + 
          scale_y_continuous(limits = c(1, e.val()*3)) + 
          scale_x_continuous(limits = c(1, e.val()*3)) +
         xlab("Risk ratio for exposure-confounder relationship") + ylab("Risk ratio for confounder-disease relationship") + 
         geom_point(dat = data.frame(rr.eu = e.val(), rr.ud = e.val()), aes(rr.eu, rr.ud)) +
          geom_text(dat = data.frame(rr.eu = e.val(), rr.ud = e.val()), 
                     aes(rr.eu, rr.ud), 
                    label = paste0("E-value:\n (", round(e.val(), 2), ",", round(e.val(), 2),")"),
                    nudge_x = e.val()*(3/5), size = 3) + 
         theme_minimal()
     )
   })       

}

# Run the application 
shinyApp(ui = ui, server = server)

