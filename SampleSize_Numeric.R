library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(dplyr)
library(openxlsx)
options("nwarnings")

Calc.MOE <- function(n, SD, conf.level, corr=alg.cor){
  Z <- qnorm(1-(1-conf.level)/2)  
  var.diff <- 2*SD^2 - 2*corr*SD*SD
  moe <- Z*sqrt(var.diff/n)
  return(moe)
}

Diff_SS <- function(margin, SD, ci.level=conf.level, corr=alg.cor){
  # This function yields the sample size needed to estimate the mean difference between the algorithm and the chart review truth within a certain margin.
  # This function assumes that the standard deviation of the difference needs to be calculated
  # SD is the standard deviation of the algorithm asset value
  # This function could be set up to calculate the SD from a vector of values is desired
  # margin is desired margin of error in terms of the absolute difference 
  # corr is the assumed correlation between the true value of the asset and the algorithm estimate
  # ci.level is the two sided confidence limit
  # Calculate variance of the difference
  # Assume algorithm and true data asset have the same variance
  var.diff <- 2*SD^2 - 2*corr*SD*SD
  z <-  qnorm((1-ci.level)/2) 
  calc.N <- (z^2*var.diff)/margin^2 
  N <- ceiling(calc.N)
  return(N) 
}

ui <- dashboardPage(
  dashboardHeader(title = "Numeric Variables: Sample Size Estimator", titleWidth = 410),
  dashboardSidebar(
    tags$div(
      tags$img(src = "https://health.ucdavis.edu/data/includes/images/Img-cdev/icons/hac-logo.png", height = "100px"),
      style = "text-align: center; padding: 10px;"
    ),
    sidebarMenu(
      menuItem("Introduction", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Defined Sample Size", tabName = "definen", icon = icon("th")),
      menuItem("Defined Margin of Error", tabName = "definemoe", icon = icon("th")),
      menuItem("Bias and Error Estimation", tabName = "estimation", icon = icon("th"))
      #menuItem("Upload Data for Bias and Error Estimation", tabName = "upload", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
        h1("INTRODUCTION"),
        div(
          p(HTML("<span style='font-size: 20px;'>This tool helps you decide how many records to review to evaluate the performance of a numeric data asset algorithm. It also estimates the algorithm’s bias and error rate for a numeric variable. For best results, it’s helpful to know the mean and standard deviation of your data, which you might have from earlier stages of algorithm development and data review.</span>")),
          p(HTML("<span style='font-size: 20px;'>Use this tool for variables that are numbers such as heart rate, weight, length of stay, or count data like number of transfusions.  These are variables for which you can calculate an average (mean) and a standard deviation.  </span>")),
          p(HTML("<span style='font-size: 20px;'>There are three main functions in the tool:</span>")),
          tags$ol(
            tags$li(
              HTML("<span style='font-size: 20px;'><strong>Defined Sample Size tab:</strong></span>"),    
              tags$ul(tags$li(
                HTML("<span style='font-size: 20px;'>Use this tab if you know the number of records you can review. This tab will show you how precisely you will be able to estimate the margin of error.</span>")
              ))
            ),
            tags$li(
              HTML("<span style='font-size: 20px;'><strong>Defined Margin of Error Tab: </strong></span>"),    
              tags$ul(tags$li(
                HTML("<span style='font-size: 20px;'>Use this tab if you need to determine if the algorithm’s margin of error (i.e., how close is the estimated mean is to the true mean) is within a specific amount and want a high level of certainty about your results. </span>")
              ))
            ),
            tags$li(
              HTML("<span style='font-size: 20px;'><strong>Bias and Error Estimation Tab: </strong></span>"),    
              tags$ul(tags$li(
                HTML("<span style='font-size: 20px;'>Once you have reviewed a sample of records, use this tab to estimate the algorithm’s bias and error. You can enter the information manually or upload the data.</span>")))
            )
          )
        ),
        fluidRow(
          column(12, h2("Key Terms:") ),
          column(12, HTML("<span style='font-size: 20px;'><u><p>Margin of Error</u> - The difference between the estimated mean from the algorithm and the true mean.</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><u><p>True Mean </u> - The actual average value of the data asset in the full dataset. This is known only if all records are reviewed.</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><u><p>Estimated Mean </u> - The average value estimated from the sample you reviewed. </p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><u><p>Anticipated Mean </u> - Your informed estimate of the mean value before reviewing any records. This guess helps calculate the sample size and can be based on expert input or prior data reviews. </p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><u><p>95% Confidence interval</u> - The range where the true mean is expected to fall 95% of the time. It provides upper and lower limits for your estimate, helping you understand how precise your results are.</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><u><p>Bias</u> - The average difference between the estimated mean and the true mean. A positive bias means the estimated mean is higher than the true mean.</p></span>")),
          column(12, h2("Note on Sampling and Margin of Error")),
          column(12, HTML("<span style='font-size: 20px;'><p>Because it's not practical to examine every single record, you look at a smaller group (called a sample). The average (or mean) from this sample can change slightly depending on which records are included. To account for this variability, we calculate a confidence interval—a range that shows where we expect the true average to fall most of the time. For example, if the confidence interval is 50 to 60, we can say we're 95% sure the true average lies somewhere in that range. Using larger samples helps reduce this uncertainty, making the range smaller and giving us more confidence in the result.</p></span>")),
          column(12, h2("Why Sample Size Matters?")),
          column(12, HTML("<span style='font-size: 20px;'><p>The size of your sample affects the precision of your results:</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><p>•	Smaller sample sizes: More variation in error rates, leading to wider confidence intervals and less certainty.</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><p>•	Larger sample sizes: Less variation, narrower confidence intervals, and greater confidence in your results.</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><p>By choosing the right sample size, you can get reliable estimates without needing to review every record.</p></span>"))
        )
      ),
      
      # third tab content
      tabItem(tabName = "definemoe",
        h1("Defined Margin of Error"),
        p(HTML("<span style='font-size: 20px;'> Use this tab to determine the sample necessary to estimate the uncertainty in the algorithm, i.e., how close is the estimated mean to the true mean. </span>")),
        box(
          title = "Input Parameters",
          width = 12,
          numericInput("alg.mean", HTML("<span style='font-weight: normal;'> <b>What is the anticipated mean of your data asset?</b><br><small>* Get this from the Data Profile output generated after algorithm logic is set.</small>"), 5, min = -10000, max = 10000),
          numericInput("alg.sd", HTML("<span style='font-weight: normal;'> <b>What is the standard deviation of your data asset?</b><br><small>* Get this from the Data Profile output generated after algorithm logic is set.</small>"), 1, min = 0, max = 10000),
          numericInput("alg.cor", HTML("<span style='font-weight: normal;'> <b>How well do you think the values from the algorithm logic will match to the values from the record review?</b><br><small>* Meaning how well will the estimate match the truth (correlation). Enter a number from 0 to 1, and 1 means 100% match.</small>"), 0.9, min = -1, max = 1),
          selectInput("conf.level", HTML("<span style='font-weight: normal;'> <b>What confidence level do you want?</b><br><small>* The closer the number is to 1, the more confident you can be that the true value lies within a certain margin of error.</small>"), choices = c(0.90, 0.95, 0.99), selected = 0.95),
          textInput("Units", HTML("<span style='font-weight: normal;'>(Optional) Enter the text of measurement units for report generating purpose, eg. lbs or days"), value = "", width = NULL, placeholder = NULL),
          HTML("<span style='font-weight: normal;'> <b>Now, please indicate the desired precision as either the percentage difference <u>OR</u> absolute difference. </b>Precision is how close you want your estimated mean to be to the true mean. For example, if the true mean is 10 days and it is acceptable if your estimated mean is greater or less than the true been by 2 days, then the desired precision is 2.<br><small>* This is the maximum acceptable difference between the true value and algorithm value difference</small>"),
          # absolute.diff
          box(width = 6,
              numericInput("abs.diff", "Enter the desired precision for the mean difference between the algorithm and the true mean (#)?", 0.25, min = 0, max = 10000),
              actionButton("goButtonAbs", "Use Absolute Difference"),
          ),
          # perc.diff
          box(width = 6,
              numericInput("perc.diff", "Enter the desired precision for the mean difference between the algorithm and the true mean (%)?", 5, min = 0, max = 10000),
              actionButton("goButtonPerc", "Use Percentage Difference"),
          )
        ),
        fluidRow(
          column(12, p(htmlOutput("tab2_absdiff"))),
          column(12, p(htmlOutput("tab2_text1"))),
          box(
            width = 12,
            fluidRow(
              column(12, h2(htmlOutput("tab2_text4"))),
              column(12, plotOutput("tab2_plot2")),
              column(12, plotOutput("tab2_plot3")),
              column(6, numericInput("smallest.n", "Enter the smallest sample size to display", 3, min = 1, max = 9999)),
              column(6, numericInput("largest.n", "Enter the largest sample size to display", 100, min = 2, max = 10000)),
              column(12, DT::dataTableOutput("tab2_table1"))
            )
          )
        )
      ),
      # second tab content
      tabItem(tabName = "definen",
        h1("Defined Sample Size"),
        p(HTML("<span style='font-size: 20px;'> Use this tab if you know the number of records you can review. This tab will show you how precisely you will be able to estimate the margin of error.</span>")),
        box(
          title = "Input Parameters",
          width = 12,
          numericInput("exp.n2", "How many charts will be reviewed?", 10, min = 1, max = 1000),
          numericInput("exp.mean2", HTML("<span style='font-weight: normal;'> <b>What is the anticipated mean of your data asset?</b><br><small>* Get this from the Data Profile output generated after algorithm logic is set.</small>"), 5, min = -10000, max = 10000),
          numericInput("sd2", HTML("<span style='font-weight: normal;'> <b>What is the standard deviation of your data asset?</b><br><small>* Get this from the Data Profile output generated after algorithm logic is set.</small>"), 1, min = 0, max = 10000),
          numericInput("corr2", HTML("<span style='font-weight: normal;'> <b>How well do you think the values from the algorithm logic will match to the values from the record review?</b><br><small>* Meaning how well will the estimate match the truth (correlation). Enter a number from 0 to 1, and 1 means 100% match.</small>")
                       , 0.9, min = 0, max = 1),
          selectInput("conf.level2", HTML("<span style='font-weight: normal;'> <b>What confidence level do you want?</b><br><small>* Default is set to 0.95. The closer the number is to 1, the more confident you can be that the true value lies within a certain margin of error.</small>")
                      , choices = c(0.90, 0.95, 0.99), selected = 0.95),
          textInput("Units2", HTML("<span style='font-weight: normal;'>(Optional) Enter the text of measurement units for report generating purpose, eg. lbs or days</span>"), value = "", width = NULL, placeholder = NULL),
          column(12, p(htmlOutput("tab3_text1")))#,
          #column(12, p(htmlOutput("tab3_text2")))
        ),
        fluidRow(
          column(12, p(htmlOutput("tab3_text3"))),
          column(12, p(htmlOutput("tab3_text4"))),
          column(12, box(
            width = 12,
            fluidRow(
              column(12, p(htmlOutput("tab3_text5"))),
              column(12, plotOutput("tab3_plot1")),
              column(12, plotOutput("tab3_plot2")),
              column(6, numericInput("smallest.n2", "Enter the smallest sample size to display", 3, min = 3, max = 9999)),
              column(6, numericInput("largest.n2", "Enter the largest sample size to display", 100, min = 4, max = 10000))
            )
          ))
        )
      ),
      # Fourth tab content
      tabItem(tabName = "estimation",
        h1("Bias and Error Estimation"),
        fluidRow(
          column(12, HTML("<span style='font-size: 20px;'>Once you have reviewed a sample of records, use this tab to estimate the algorithm’s bias and error. You can enter the information manually <strong>OR</strong> upload the data. <span>"))
        ),
        box(width = 12,
            selectInput("conf.level4", "What confidence level do you want?", choices = c(0.90, 0.95, 0.99), selected = 0.95),
          box(
            title = "Input Parameters",
            width = 8,
            numericInput("chart.n4", "Enter the number of records reviewed.", 70, min = 1, max = 100000),
            numericInput("chart.mean4", "What is the estimated chart review mean of your data asset?", 100, min = -10000, max = 10000),
            numericInput("chart.sd4", "Enter the standard deviation of the difference between the chart review values and the algorithm values.", 5, min = 0, max = 100000),
            numericInput("chart.corr4", "Enter the correlation between the chart reviewed and algorithm values.", 0.9, min = -1, max = 1),
            HTML("<p> <strong>Now, please either enter the mean obtained from the algorithm <u>OR</u> the percent of error for the differnce between the algorithm and the chart review means to obtain the confidence interval. </strong></p>"),
            # perc.diff
            box(width = 6,
                numericInput("algo.mean4", "Enter the algorithm estimate (#)", 110, min = -10000, max = 10000),
                actionButton("goButtonAbs4", "Use Algorithm Mean")
            ),
            # absolute.diff
            box(width = 6,
                numericInput("perc.error4", "Enter the percentage of error estimation (%)", 10, min = 10000, max = 10000),
                actionButton("goButtonPerc4", "Use Chart Review Error Percentage")
            )
          ),
          box(width = 4,
              title = "Upload Data",
              fileInput("upload", NULL,
                        accept = c(".csv", ".xlsx", ".xls", ".sas7bdat", ".sav", ".dta", ".rds")),
              selectInput('sheet', "Choose Sheet",  NULL),
              selectInput("variable.algo", "Choose Chart Review Variable ", NULL),
              selectInput("variable.chart", "Choose Algorithm Estimate Variable", NULL),
              actionButton("goButtonData4", "Use Uploaded Data")
          ),
        ),
        fluidRow(
          column(12, p(htmlOutput("tab4_text1")))
        )
      )
    )
  )
)






server <- function(input, output,session) {
  # Reactive value to store the last button clicked
  lastButtonClicked  <- reactiveVal("")
  observeEvent(input$goButtonPerc, {
    lastButtonClicked("perc")
  })
  observeEvent(input$goButtonAbs, {
    lastButtonClicked("abs")
  })

  
  tab2_absdiff <- reactive({
    if(lastButtonClicked() == "perc") {
      return(input$perc.diff*input$alg.mean/100)
    } else if(lastButtonClicked() == "abs") {
      return(input$abs.diff)
    } else {
      return(NA)  # Default case if no button has been clicked yet
    }
  })

  lastButtonClicked4  <- reactiveVal("")
  observeEvent(input$goButtonPerc4, {
    lastButtonClicked4("perc")
  })
  observeEvent(input$goButtonAbs4, {
    lastButtonClicked4("abs")
  })
  observeEvent(input$goButtonData4, {
    lastButtonClicked4("data")
  })
  tab4_absdiff <- reactive({
    if(lastButtonClicked4() == "abs") {
      return(input$algo.mean4-input$chart.mean4)
    } else if(lastButtonClicked4() == "perc") {
      return(input$chart.mean4*input$perc.error4/100)
    } else if(lastButtonClicked4() == "data") {
      return(mean(data()[,input$variable.algo])- mean(data()[,input$variable.chart]))
    } else {
      return(NA)  # Default case if no button has been clicked yet
    }
  })
  tab4_estimatedmean <- reactive({
    if(lastButtonClicked4() == "abs") {
      return(input$chart.mean4)
    } else if(lastButtonClicked4() == "perc") {
      return(input$chart.mean4)
    } else if(lastButtonClicked4() == "data") {
      return(mean(data()[,input$variable.chart]))
    } else {
      return(NA)  # Default case if no button has been clicked yet
    }
  })
  tab4_chartn <- reactive({
    if(lastButtonClicked4() == "abs" | lastButtonClicked4() == "perc") {
      return(input$chart.n4)
    } else if(lastButtonClicked4() == "data") {
      return(nrow(data()))
    } else {
      return(ncol(data()))
    }
  })
  tab4_sd <- reactive({
    if(lastButtonClicked4() == "abs" | lastButtonClicked4() == "perc") {
      return(input$chart.sd4)
    } else if(lastButtonClicked4() == "data") {
      return(sd(data()[,input$variable.algo]-data()[,input$variable.chart]))
    } else {
      return(NA)
    }
  })
  tab4_corr <- reactive({
    if(lastButtonClicked4() == "abs" | lastButtonClicked4() == "perc") {
      return(input$chart.corr4)
    } else if(lastButtonClicked4() == "data") {
      return(cor(data()[,input$variable.algo],data()[,input$variable.chart]))
    } else {
      return(NA)
    }
  })
  
  
  tab2_text1 <- reactive({
    alg.mean <- input$alg.mean
    #perc.diff <- input$perc.diff
    alg.sd <- input$alg.sd
    alg.cor <- input$alg.cor
    conf.level <- as.numeric(input$conf.level)
    Units <- input$Units
    smallest.n <- input$smallest.n
    largest.n <- input$largest.n
    
    absolute.diff = tab2_absdiff()
    N.MOE <- Diff_SS(absolute.diff, SD=alg.sd, ci.level=conf.level, corr=alg.cor)
    
    text <- paste("Assuming a standard deviation of ",
                  alg.sd, ", and that the correlation between the algorithm values and the true values is ",
                  alg.cor, ", a sample size of ",
                  N.MOE, " would be required such that the estimated mean difference between the algorithm and the true value was ",
                  absolute.diff, " ",
                  Units, " or less with ",
                  conf.level*100, "% confidence. This corresponds to a ",
                  round(100*absolute.diff/alg.mean,1),"% margin of error.")
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab2_text1 <- renderUI({
    tab2_text1()
  })
  
  tab2_text4 <- reactive({
    alg.mean <- input$alg.mean
    perc.diff <- input$perc.diff
    alg.sd <- input$alg.sd
    alg.cor <- input$alg.cor
    conf.level <- as.numeric(input$conf.level)
    Units <- input$Units
    smallest.n <- input$smallest.n
    largest.n <- input$largest.n
    
    absolute.diff = perc.diff*alg.mean/100
    N.MOE <- Diff_SS(absolute.diff, SD=alg.sd, ci.level=conf.level, corr=alg.cor)
    
    text <- paste0("<u>Figures 3 and 4</u> show the confidence interval limits for the mean (Figure 3) and the bias (i.e., the difference between the estimated mean and the true mean; Figure 4) over a range of sample sizes for the given standard deviation of ",  alg.sd, " at a ", conf.level*100, "% confidence level.")
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab2_text4 <- renderUI({
    tab2_text4()
  })

  
  tab2_plot2 <- reactive({
    alg.mean <- input$alg.mean
    perc.diff <- input$perc.diff
    alg.sd <- input$alg.sd
    alg.cor <- input$alg.cor
    conf.level <- as.numeric(input$conf.level)
    Units <- input$Units
    smallest.n <- input$smallest.n
    largest.n <- input$largest.n
    
    MOE.Est <- Calc.MOE(n=smallest.n:largest.n, SD=alg.sd, conf.level, corr=alg.cor)
    MOE.Plot <- data.frame(N=smallest.n:largest.n, MOE.LCL=alg.mean-MOE.Est, MOE.UCL=alg.mean+MOE.Est)

    title <- paste0("Figure 3: ",conf.level * 100, "% Confidence Intervals for Mean = ", alg.mean)
    
    ggplot(MOE.Plot, aes(x=N, y=alg.mean)) + ylim(min(MOE.Plot$MOE.LCL), max(MOE.Plot$MOE.UCL)) + 
      geom_line(aes(x = N, y = MOE.LCL,colour = "95% CI"),lwd=1.5) +
      geom_line(aes(x = N, y = MOE.UCL,colour = "95% CI"),lwd=1.5) +
      geom_line(aes(x = N, y = alg.mean,colour = "Mean"),lwd=1.5) +
      ylab("Anticipated Mean of Data Asset") + xlab("Sample Size") +
      labs(title = title) + 
      theme(legend.title = element_blank())
  })
  # Render the plots based on the new dataset
  output$tab2_plot2 <- renderPlot({
    tab2_plot2()
  })  
  
  tab2_plot3 <- reactive({
    alg.mean <- input$alg.mean
    perc.diff <- input$perc.diff
    alg.sd <- input$alg.sd
    alg.cor <- input$alg.cor
    conf.level <- as.numeric(input$conf.level)
    Units <- input$Units
    smallest.n <- input$smallest.n
    largest.n <- input$largest.n
    
    MOE.Est <- Calc.MOE(n=smallest.n:largest.n, SD=alg.sd, conf.level, corr=alg.cor)
    MOE.Plot <- data.frame(N=smallest.n:largest.n, MOE.LCL=alg.mean-MOE.Est, MOE.UCL=alg.mean+MOE.Est)
    
    title <- paste0("Figure 4: ",conf.level * 100, "% Confidence Intervals for the bias for a true mean = ", alg.mean)
    
    ggplot(MOE.Plot, aes(x=N, y=(alg.mean-alg.mean))) + 
      ylim(min((MOE.Plot$MOE.LCL-alg.mean)), max((MOE.Plot$MOE.UCL-alg.mean))) + 
      geom_line(aes(x = N, y = (MOE.LCL-alg.mean),colour = "95% CI"),lwd=1.5) +
      geom_line(aes(x = N, y = (MOE.UCL-alg.mean),colour = "95% CI"),lwd=1.5) +
      geom_line(aes(x = N, y = (alg.mean-alg.mean),colour = "Mean"),lwd=1.5) +
      ylab("Anticipated Difference Between Algorithm and True Value") + xlab("Sample Size") +
      labs(title = title) + 
      theme(legend.title = element_blank())
  })
  # Render the plots based on the new dataset
  output$tab2_plot3 <- renderPlot({
    tab2_plot3()
  })  
  
  tab2_table1 <- reactive({
    # Create a new dataset based on the selected parameters
    alg.mean <- input$alg.mean
    perc.diff <- input$perc.diff
    alg.sd <- input$alg.sd
    alg.cor <- input$alg.cor
    conf.level <- as.numeric(input$conf.level)
    Units <- input$Units
    smallest.n <- input$smallest.n
    largest.n <- input$largest.n
    
    MOE.Est <- Calc.MOE(n=smallest.n:largest.n, SD=alg.sd, conf.level, corr=alg.cor)
    data.frame(N=smallest.n:largest.n, LCL=round(alg.mean-MOE.Est,2), UCL=round(alg.mean+MOE.Est,2),`CI Width`=round(MOE.Est*2,2),`Margin of error (%)` = round(MOE.Est/alg.mean*100,2),check.names = F)
  })
  # Render the plots based on the new dataset
  output$tab2_table1 <- DT::renderDataTable(
    datatable(
      tab2_table1(),
      rownames = FALSE,
      options=list(pageLength = 10),
      caption = htmltools::tags$caption(paste0("Table 1. Parametric ",
                                               as.numeric(input$conf.level)*100, "% confidence intervals for anticipated mean of ",
                                               round(as.numeric(input$lower.range),1),
                                               "to",
                                               round(as.numeric(input$upper.range),1),
                                               " based on a sample size of ",
                                               input$n.charts, " charts. CI Width is the difference between the upper (UCL) and lower (LCL) confidence limits."), style="color:black;font-size:14px")
      )
  )

  
  
  tab3_text1 <- reactive({
    exp.mean <- input$exp.mean2
    exp.n <- input$exp.n2
    sd <- input$sd2
    corr <- input$corr2
    conf.level <- as.numeric(input$conf.level2)
    Units <- input$Units2
    MOE.GivenN <- Calc.MOE(n=exp.n, SD=sd, conf.level, corr=corr)
    
    text <- paste0("If ",
                   exp.n, " charts are reviewed, the margin of error would be ",
                   round(MOE.GivenN,1), " with ",
                   conf.level*100, "% confidence. This means that with ",
                   conf.level*100, "% confidence the mean estimated from the algorithm would be within ",
                   round(MOE.GivenN,1), " ", Units , " of the true mean."
                   )
    HTML(paste0("<span style='font-size: 20px;'><strong>", text, "</strong></span>"))
  })
  output$tab3_text1 <- renderUI({
    tab3_text1()
  })
  
  tab3_text2 <- reactive({
    exp.mean <- input$exp.mean2
    exp.n <- input$exp.n2
    sd <- input$sd2
    corr <- input$corr2
    conf.level <- as.numeric(input$conf.level2)
    Units <- input$Units2
    MOE.GivenN <- Calc.MOE(n=exp.n, SD=sd, conf.level, corr=corr)
    
    text <- paste0("<u>For the anticipated mean of ",
                     exp.mean, " ", Units, ", this margin of error represents a ",
                     round(100*MOE.GivenN/exp.mean,1), "% error.</u>")
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab3_text2 <- renderUI({
    tab3_text2()
  })
  
  tab3_text3 <- reactive({
    exp.mean <- input$exp.mean2
    exp.n <- input$exp.n2
    sd <- input$sd2
    corr <- input$corr2
    conf.level <- as.numeric(input$conf.level2)
    Units <- input$Units2
    
    MOE.GivenN <- Calc.MOE(n=exp.n, SD=sd, conf.level, corr=corr)
    
    text <- paste0("Is a margin of error of ",
                     round(MOE.GivenN,1), " ", Units, #" representing a ", round(100*MOE.GivenN/exp.mean,1), " % error",  
                   "acceptable? ", 
                   "In other words, would it be acceptable if the algorithm yielded a mean estimate ",
                   round(MOE.GivenN,1), " ", Units , " higher or lower than the true mean value?")
    HTML(paste0("<span style='font-size: 30px;'>", text, "</span>"))
  })
  output$tab3_text3 <- renderUI({
    tab3_text3()
  })
  
  tab3_text4 <- reactive({
    exp.mean <- input$exp.mean2
    exp.n <- input$exp.n2
    sd <- input$sd2
    exp.mer <- input$exp.mer2
    corr <- input$corr2
    conf.level <- as.numeric(input$conf.level2)
    Units <- input$Units2
    
    MOE.GivenN <- Calc.MOE(n=exp.n, SD=sd, conf.level, corr=corr)
    
    text <- paste0("If ", 
                     round(MOE.GivenN,1), " ", Units, 
                     " is too large of a margin of error, you can use the figure or this application to interactively investigate how the margin of error based on ",
                     100*conf.level, "% confidence limits changes with the number of charts reviewed for the assumed standard deviation. The margin of error is one-half the total width of the confidence interval of the difference between the anticipated mean and the true mean.")

    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab3_text4 <- renderUI({
    tab3_text4()
  })
  
tab3_text5 <- reactive({
    exp.mean <- input$exp.mean2
    exp.n <- input$exp.n2
    sd <- input$sd2
    exp.mer <- input$exp.mer2
    corr <- input$corr2
    conf.level <- as.numeric(input$conf.level2)
    Units <- input$Units2
    
    MOE.GivenN <- Calc.MOE(n=exp.n, SD=sd, conf.level, corr=corr)
    
    text <- paste0("<u>Figures 1 and 2</u> show how the margin of error changes for a range of sample sizes for the given standard deviation of ",
                   sd, " at a ",
                   100*conf.level, "% confidence level.")
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab3_text5 <- renderUI({
    tab3_text5()
  })
  
  
  tab3_plot1 <- reactive({
    exp.mean <- input$exp.mean2
    exp.n <- input$exp.n2
    sd <- input$sd2
    corr <- input$corr2
    conf.level <- as.numeric(input$conf.level2)
    Units <- input$Units2
    smallest.n <- input$smallest.n2
    largest.n <- input$largest.n2
    
    MOE.Est <- Calc.MOE(n=smallest.n:largest.n, SD=sd, conf.level, corr=corr)
    MOE.Plot <- data.frame(N=smallest.n:largest.n, MOE=MOE.Est)
    
    Estimate <- data.frame(exp.value=exp.mean, curve_n=exp.n)

    title <- paste0("Figure 1: Changes in margin of error (absolute value) with number of charts reviewed")
    g <- ggplot(MOE.Plot, aes(x=N, y=MOE.Plot$MOE)) + ylim(0, max(MOE.Plot$MOE)) + 
      geom_line(lwd=2) +
      ylab("Margin of Error (absolute value)") + xlab("Sample Size")  + labs(title = title)

    g
  })
  output$tab3_plot1 <- renderPlot({
    tab3_plot1()
  })
  
  tab3_plot2 <- reactive({
    exp.mean <- input$exp.mean2
    exp.n <- input$exp.n2
    sd <- input$sd2
    corr <- input$corr2
    conf.level <- as.numeric(input$conf.level2)
    Units <- input$Units2
    smallest.n <- input$smallest.n2
    largest.n <- input$largest.n2
    
    MOE.Est <- Calc.MOE(n=smallest.n:largest.n, SD=sd, conf.level, corr=corr)
    MOE.Plot <- data.frame(N=smallest.n:largest.n, MOE=MOE.Est)
    
    Estimate <- data.frame(exp.value=exp.mean, curve_n=exp.n)
    
    title <- paste0("Figure 2: Changes in margin of error (percentage) with number of charts reviewed")
    g <- ggplot(MOE.Plot, aes(x=N, y=MOE.Plot$MOE/exp.mean*100)) + ylim(0, max(MOE.Plot$MOE/exp.mean*100)) + 
      geom_line(lwd=2) +
      ylab("Margin of Error (%)") + xlab("Sample Size")  + labs(title = title)
    
    g
  })
  output$tab3_plot2 <- renderPlot({
    tab3_plot2()
  })
  
  
  # Begining of the server function for tab 4
  tab4_text1 <- reactive({
    chart.rev <- input$chart.n4
    obs.sd <- input$chart.sd4
    obs.corr <- input$chart.corr4
    conf.level <- as.numeric(input$conf.level4)
    
    obs.diff <- tab4_absdiff()
    chart.mean <- tab4_estimatedmean()
    
    MOE.Est <- Calc.MOE(n=chart.rev, SD=obs.sd, conf.level, corr=obs.corr)
    
    if (0<=conf.level){
      text <- paste0("The estimated bias of the algorithm is ",
                     round(obs.diff,3), " with a ",
                     conf.level*100, "% confidence interval of [",
                     round(obs.diff-MOE.Est,3), ", ",
                     round(obs.diff+MOE.Est,3), "] OR ",
                     round(obs.diff/chart.mean*100,2), "% [",
                     round((obs.diff-MOE.Est)/chart.mean*100,2), "%, ",
                     round((obs.diff+MOE.Est)/chart.mean*100,2), "%] in percentage change.",
                     " This means that the average difference between the <u>mean estimated by the algorithm</u> and the <u>true mean</u> value of data asset is within this range with 95% confidence.")
    }
    else{
      text <- paste0("Number of errors observed must be not greater than number of chart viewed!!!!")
    }
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab4_text1 <- renderUI({
    tab4_text1()
  })
  
  # import data using appropriate function
  data <- reactive({
    if((!is.null(input$upload)) && (input$sheet != "")){
      ext <- tools::file_ext(input$upload$name)
      # import data using appropriate function based on file type
      switch(
        ext,
        csv = read.csv(input$upload$datapath),
        xls = read_xls(input$upload$datapath, sheet = input$sheet),
        xlsx = read.xlsx(input$upload$datapath, sheet = input$sheet),
        sas7bdat = read_sas(input$upload$datapath),
        sav = read_spss(input$upload$datapath),
        dta = read_dta(input$upload$datapath),
        rds = readRDS(input$upload$datapath),
        validate(paste0("Invalid file; Please upload a file of the following",
                        " types: .csv, .xls, .xlsx, .sas7bdat, .sav, .dta, .rds"))
      )
    } else {
      NULL
    }
  })
  
  sheetNames <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    if(ext == 'xls' | ext == "xlsx"){
      readxl::excel_sheets(input$upload$datapath)
    } else {
      "No Sheets"
    }
  })
  # update dropdown menu after data is imported
  observe({
    updateSelectInput(
      session, "sheet", choices = sheetNames()
    )
  })
  
  variable.algo <- reactive({
    if((!is.null(data()))){
      colnames(data())
    }
  })
  # update dropdown menu after data is imported
  observe({
    updateSelectInput(
      session, "variable.algo", choices = variable.algo()
    )
  })

  variable.chart <- reactive({
    if((!is.null(data()))){
      colnames(data())
    }
  })
  # update dropdown menu after data is imported
  observe({
    updateSelectInput(
      session, "variable.chart", choices = variable.chart()
    )
  })
}

shinyApp(ui, server)
