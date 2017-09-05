#TO-DO
#1. Make it work for all types of effect estimates.
#2. Also calculate the bias factor.


library(shiny)
library(tidyverse)
library(plotly)

ui <- fluidPage(
  
  titlePanel("E-value calculator for sensitivity analysis in observational studies"),
  tabsetPanel(
    tabPanel("E-Value Calculator",
             fluidRow(
               column(3, HTML("<b>What effect measure did you estimate?</b>"),
                      selectInput(inputId = "selected_measure", label = NULL, 
                                  choices = c("Risk Ratio", "Rate Ratio", "Odds Ratio", "Hazard Ratio"),
                                 width = '160px')),
               conditionalPanel(condition = "(input.selected_measure == 'Odds Ratio') || (input.selected_measure == 'Hazard Ratio')",
                                column(2, HTML("<b>Is the outcome rare (<15%)?</b>"),
                                          checkboxInput(inputId = "Rare.outcome", label = "Yes", value = T))), 
               column(3, uiOutput("label_text"),
                      numericInput(inputId = "est.effect", label = NULL, value = 3.9, min = 0.001, max = 999, step = 1, width = '80px')),
               column(2, HTML("<b>What is the lower bound on its confidence interval?</b>"),
                      uiOutput("lower_CI"),
                      uiOutput("Warning.lcl")),
               column(2, HTML("<b>What is the upper bound?</b>"),
                      uiOutput("upper_CI"),
                      uiOutput("Warning.ucl"))
             ),
             hr(),
             HTML(paste("<b>What is the e-value?</b><br>The e-value is the minimum strength required for both the exposure-confounder and exposure-disease relationships that is required to 'explain away' the estimated relationship between exposure and disease.",
                  " If one of the two parameters is smaller than the e-value, the other must be larger, as defined by the curve below.",
                  " All points along the curve define joint relationships that explain away the estimated effect, including points to the right of the curve.")),
             plotlyOutput("curveOfExplainAway", width = "400px", height = "400px"),
             hr(),
             HTML("<b>Interpretation of the e-value</b>"),
             uiOutput("e.value"),
             uiOutput("e.val.cl.near1")

    ),
    tabPanel("Bias Factor Calculator",
             HTML("<br>If you have substantive knowledge on the strength of the relationships between the unmeasured confounder",
                  " and the exposure and outcome you can use these numbers to calculate the bias factor. As stated by VanderWeele and Ding,",
                  " Let RR<sub>UD</sub> denote the maximum risk ratio for the outcome comparing any two categories of the unmeasured",
                  " confounders, within either treatment group, conditional on the observed covariates. Let RR<sub>EU</sub> denote",
                  " the maximum risk ratio for any specific level of the unmeasured confounders comparing those with and without treatment, with",
                  " adjustment already made for the measured covariates.<br><br>"),
             fluidRow(
               column(4, 
                      HTML("<b>Specify the effect estimate:</b>"),
                      numericInput(inputId = "effect.estimate.page2", label = NULL, value = 3.9, min = 1.02, max = 999, width = '80px')),
               column(4, 
                      HTML("<b>Specify RR<sub>EU</sub>:</b>"),
                      numericInput(inputId = "RR_EU", label = NULL, value = 2, min = 1.02, max = 999, width = '80px')),
               column(4, 
                      HTML("<b>Specify RR<sub>UD</sub>:</b>"),
                      numericInput(inputId = "RR_UD", label = NULL, value = 4, min = 1.02, max = 999, width = '80px'))
             ),
             hr(),
             uiOutput("Bias_Factor")
             ),
    tabPanel("About",
             HTML(paste0(
               "<br><br>The e-value was introduced by VanderWeele and Ding in 2017 in a publication in the Annals of Internal Medicine.<sup>1, 2</sup>",
               " This calculator was created by <a href='http://corinne-riddell.github.io/'>Corinne Riddell</a> to facilitate easy e-value computation. All of the code to create the app is available on <a href = 'https://github.com/corinne-riddell/EValue'>GitHub</a>.",
               " Please open an issue or submit a pull request if you've uncovered a technical error or would like to suggest a stylistic improvement.<br><br>",
               "<b>References</b><br>",
               "1. VanderWeele TJ, Ding P. Sensitivity Analysis in Observational Research: Introducing the E-Value.<i> Annals of Internal Medicine</i>. 2017. 167(4):268-274.<br>", 
               "2. Ding P, VanderWeele TJ. Sensitivity Analysis without assumptions. <i>Epidemiology</i>. 2016. 27:368-77."
             )
             )
    )
    
  )
)

server <- function(input, output) {
  
  output$label_text <- renderUI({
    HTML(paste0("<b>What is your estimated ", input$selected_measure, "?</b>"))
  })
  
  output$lower_CI <- renderUI({
    numericInput(inputId = "LCL.effect", label = NULL, value = 1.8, min = 0.001, max = input$est.effect, step = 1, width = '80px')
  }) 
  
  output$upper_CI <- renderUI({  
    numericInput(inputId = "UCL.effect", label = NULL, value = 8.7, min = input$est.effect, max = 999, step = 1, width = '80px')
  })
  
  output$Warning.lcl <- renderUI({
    if(input$est.effect < input$LCL.effect){
      HTML(paste0("<font color='red'><b>Error:</b> Please update the lower confidence level so it is smaller than the estimated ", 
                  input$selected_measure, 
                  ".</font>"))
    }
  })

  output$Warning.ucl <- renderUI({
    if(input$est.effect > input$UCL.effect){
      HTML(paste0("<font color='red'><b>Error:</b> Please update the upper confidence level so it is greater than the estimated ", 
                  input$selected_measure, 
                  ".</font>"))
    }
  })
  
  e.val = reactive({
    value <- input$est.effect

    if(input$selected_measure == "Odds Ratio" & input$Rare.outcome == F){
      value <- sqrt(input$est.effect) #use the square root of the OR if the outcome is common.
    }
    
    if(input$selected_measure == "Hazard Ratio" & input$Rare.outcome == F){
      value <- (1 - (0.5^sqrt(input$est.effect)))/(1 - (0.5^sqrt(1/input$est.effect))) #use a function of the HR if the outcome is common
    }

    if(value > 1){
      value + sqrt(value*(value - 1))
    } else{
      (1/value) + sqrt((1/value)*((1/value) - 1))
    }
    
  })

  e.val.cl.near.1 = reactive({
    e.val.limit <- NA
    if(input$LCL.effect < 1 & input$UCL.effect > 1){ #if the CI (RR, HR, OR) includes 1 then the e-value for the CI is 1.
      e.val.limit <- 1
    }else if(input$LCL.effect < 1 & input$UCL.effect < 1){ #upper bound is closest to 1 and both upper bound and lower bound are < 1
      value2 <- input$UCL.effect
      
      if(input$selected_measure == "Odds Ratio" & input$Rare.outcome == F){
        value2 <- sqrt(input$UCL.effect)
      }
      
      if(input$selected_measure == "Hazard Ratio" & input$Rare.outcome == F){
        value2 <- (1 - (0.5^sqrt(input$UCL.effect)))/(1 - (0.5^sqrt(1/input$UCL.effect))) 
      }
      
      e.val.limit <- (1/value2) + sqrt((1/value2)*((1/value2)-1))
    }else{ # lower bound is closest to 1 and both upper and lower bound are > 1
      value3 <- input$LCL.effect
      
      if(input$selected_measure == "Odds Ratio" & input$Rare.outcome == F){
        value3 <- sqrt(input$LCL.effect)
      }
      
      if(input$selected_measure == "Hazard Ratio" & input$Rare.outcome == F){
        value3 <- (1 - (0.5^sqrt(input$LCL.effect)))/(1 - (0.5^sqrt(1/input$LCL.effect))) 
      }
      
      e.val.limit <- value3 + sqrt(value3*(value3 - 1))
    }
    return(e.val.limit)
  })
  
  which.bound <- reactive({
    which.one <- NA
    if(input$LCL.effect < 1 & input$UCL.effect > 1){
      which.one <- "either"
    }else if(input$LCL.effect < 1 & input$UCL.effect < 1){
      which.one <- "the upper"
    }else{
      which.one <- "the lower"
    }
    return(which.one)
  })
  
   output$e.value <- renderUI({
     HTML(paste0("The e-value for the estimate of the ", input$selected_measure, " is ", round(e.val(), 2), ". The observed ", input$selected_measure, " of ", input$est.effect, " could be explained away by an unmeasured confounder that was associated with both the treatment and the outcome by a ", input$selected_measure, " of ", round(e.val(), 2),"-fold each, above and beyond the measured confounders, but weaker confounding could not do so." ))
     })
   
   output$e.val.cl.near1 <- renderUI({
     if(input$est.effect >= input$LCL.effect & input$est.effect <= input$UCL.effect){ # estimated effect in range of CI:
     HTML(paste0("<br>The e-value for ", which.bound()," confidence bound is ", round(e.val.cl.near.1(), 2), "."))
     }
   })
   
   output$curveOfExplainAway <- renderPlotly({
     
     rr.ud <- function(rr.eu) {
       value4 <- input$est.effect
       if(input$selected_measure == "Odds Ratio" & input$Rare.outcome == F){
         value4 <- sqrt(input$est.effect)
       }
       
       if(input$selected_measure == "Hazard Ratio" & input$Rare.outcome == F){
         value4 <- (1 - (0.5^sqrt(input$est.effect)))/(1 - (0.5^sqrt(1/input$est.effect))) 
       }
       
       if(value4 > 1){
         (value4*(1 - rr.eu))/(value4 - rr.eu)
       }else{
         ((1/value4)*(1 - rr.eu))/((1/value4) - rr.eu)
       }
     }
     
     g <- ggplotly(
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
     
     g$x$data[[2]]$text <- "E-value"
     g$x$data[[1]]$text <- gsub("y", "RR_UD", g$x$data[[1]]$text)
     g$x$data[[1]]$text <- gsub("rr.eu", "RR_EU", g$x$data[[1]]$text)
     
     g
   }) 
   
   bias.factor <- reactive({
     input$RR_UD*input$RR_EU/(input$RR_UD + input$RR_EU - 1)  
   })
   
   adjusted.effect <- reactive({
     adjust.effect <- ifelse(input$effect.estimate.page2 > 1, input$effect.estimate.page2/bias.factor(), input$effect.estimate.page2*bias.factor())
   })
   
   output$Bias_Factor <- renderUI({
     HTML(paste0("The bias factor is ", round(bias.factor(), 2), ". At most, this bias factor could alter the risk ratio to become ",
                 round(adjusted.effect(), 2), "."))
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

