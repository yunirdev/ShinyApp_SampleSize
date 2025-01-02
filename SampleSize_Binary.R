library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Binary variable: Sample Size Estimator", titleWidth = 400),
  dashboardSidebar(
    tags$div(
      tags$img(src = "https://health.ucdavis.edu/data/includes/images/Img-cdev/icons/hac-logo.png", height = "100px"),
      style = "text-align: center; padding: 10px;"
    ),
    sidebarMenu(
      menuItem("Introduction", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Plan Sample Size", tabName = "sample_size", icon = icon("th")),
      menuItem("Error Rate Confidence", tabName = "confidence", icon = icon("th")),
      menuItem("Error Rate Estimation", tabName = "estimation", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
        h1("Introduction"),
        div(
          p(HTML("<span style='font-size: 20px;'>This tool helps you decide how many records to review to evaluate the performance of a binary data asset algorithm. It also estimates the algorithm’s error rate for a binary variable, providing a range (called a confidence interval) to show how precise the estimate is.</span>")),
          p(HTML("<span style='font-size: 20px;'>Use this tool for binary variables which are variables that take on one of two values (e.g., yes/no, lived/died, female/male).</span>")),
          p(HTML("<span style='font-size: 20px;'>There are three main functions in the tool:</span>")),
          tags$ol(
            tags$li(
              HTML("<span style='font-size: 20px;'><strong>Defined Sample Size Tab</strong></span>"),    
              tags$ul(tags$li(
                HTML("<span style='font-size: 20px;'>Use this tab if you know the number of records you can review. This tab will show you how precisely you will be able to estimate your error rate with this sample size.</span>")
              ))
            ),
            tags$li(
              HTML("<span style='font-size: 20px;'><strong>Error Rate Confidence Tab</strong></span>"),    
              tags$ul(tags$li(
                HTML("<span style='font-size: 20px;'>Use this tab if you need to determine if the algorithm’s error rate is below a specific limit and want a high level of certainty about your results.</span>")
              ))
            ),
            tags$li(
              HTML("<span style='font-size: 20px;'><strong>Error Rate Estimation Tab</strong></span>"),    
              tags$ul(tags$li(
                HTML("<span style='font-size: 20px;'>Once you have reviewed a sample of records, use this tab to estimate the algorithm's error rate. You'll enter the number of records you checked and how many errors you found to get an estimate of the error rate.</span>")
              ))
            )
          )
        ),
        fluidRow(
          column(12, h2("Key Terms") ),
          column(12, HTML("<span style='font-size: 20px;'><u><p>True error rate</u>: The actual percentage of errors in the entire dataset. You can only know this for sure if you check every single record, which is usually impractical.</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><u><p>Estimated error rate</u>: The percentage of errors found in the sample you reviewed. </p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><u><p>Anticipated error rate</u>: Your informed estimate of what the error rate might be before reviewing any records. This guess helps calculate the sample size and can be based on expert input or prior reviews.</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><u><p>95% Confidence interval</u>: The range where the true error rate is expected to fall 95% of the time. It provides upper and lower limits for your estimate, helping you understand how precise your results are.</p></span>")),
          column(12, h2("Note on Sampling and Estimating the True Error Rate")),
          column(12, HTML("<span style='font-size: 20px;'><p>Since it's usually not possible to review every record, you select a smaller group (sample) to examine. The percentage of errors in your sample may differ slightly each time you draw a new sample. This variation happens naturally and means the sample’s error rate (i.e., estimated error rate) might not perfectly match the actual error rate of the full dataset (i.e., true error rate). </p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><p>To address this, the tool calculates a confidence interval—a range where the true error rate is likely to be. For example, if the confidence interval is 2%-5%, you can be 95% sure the true error rate falls within this range.</p></span>")),
          column(12, h2("Why Sample Size Matters")),
          column(12, HTML("<span style='font-size: 20px;'><p>The size of your sample affects the precision of your results:</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><p>•	Smaller sample sizes: More variation in error rates, leading to wider confidence intervals and less certainty.</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><p>•	Larger sample sizes: Less variation, narrower confidence intervals, and greater confidence in your results.</p></span>")),
          column(12, HTML("<span style='font-size: 20px;'><p>By choosing the right sample size, you can get reliable estimates without needing to review every record.</p></span>"))
        )
      ),
      
      # Second tab content
      tabItem(tabName = "sample_size",
        h1("Defined Sample Size"),
        p(HTML("<span style='font-size: 20px;'>Use this tab if you know the number of records you plan to review. This tab will show you how precisely you will be able to estimate your error rate with this sample size.</span>")),
        p(HTML("<span style='font-size: 20px;'>We don't know what the true error rate of the algorithm is, and we also don't know how many errors we will find in a sample. Therefore, we need to make an educated guess about the error rate we expect. This is the <strong>anticipated error rate</strong>. Information from the TL2 phase of algorithm development may help inform what error rate is reasonable to expect.</span>")),
        box(
          title = "Input Parameters",
          width = 6,
          numericInput("n.charts", "How many charts will be reviewed? (Integer > 1)", 10, min = 1, max = 10000),
          selectInput("conf.level", HTML("<span style='font-weight: normal;'> <b>What confidence level do you want?</b> <br> <small>* Default is 95%. The closer the number is to 1, the more confidence there is that the true error rate lies within the calculated range. A higher confidence level needs a larger sample.</small> </span>"), choices = c(0.90, 0.95, 0.99), selected = 0.95)
          # actionButton("update_plots", "Update Plots and Table")
        ),
        p(htmlOutput("tab2_text1")),
        fluidRow(
          column(12, plotOutput("tab2_plot1")),
          column(12, h2("What This Means")),
          column(12, p(htmlOutput("tab2_text2"))),
          column(12, HTML("<span style='font-size: 20px;'>Note: The true error rate can be anywhere in this interval and even be outside this range although with low probability.</span>")),
          column(12, DT::dataTableOutput("tab2_table1")),
          column(12, h2("How to Use the Results")),
          column(12, HTML("<span style='font-size: 20px;'><p>We don't know what the true error rate of the algorithm is and we also don't know how many errors we will find in a sample. Therefore, we need to make an educated guess about the error rate we expect. Information from the TL2 phase of algorithm development may help inform what error rate is reasonable to expect.<p></span>")),
          column(8, numericInput("guess.err", "What's your best guess for the anticipated error rate (%)?", 10, min = 0, max = 100)),
          column(12, p(htmlOutput("tab2_text3"))),
          column(12, h2(htmlOutput("tab2_text4"))),
          column(12, p(htmlOutput("tab2_text5"))),
          box(
            title = "Input parameters for showing how the confidence limits change as the sample size changes for an anticipated error rate:",
            width = 4,
            numericInput("smallest.n", "Smallest number of records to review", 1, min = 1, max = 9999),
            numericInput("largest.n", "Largest number of records to review", 200, min = 2, max = 10000)
          ),
          column(8, plotOutput("tab2_plot2"))
        )
      ),
      # Third tab content
      tabItem(tabName = "confidence",
        h1("Error Rate Confidence"),
        p(HTML("<span style='font-size: 20px;'>Use this tab if you need to determine if the algorithm’s error rate is below a specific limit and want a high level of certainty about your results.</span>")),
        box(
          title = "Input Parameters",
          width = 8,
          numericInput("exp.mer", "What is the maximum acceptable error rate (%) ?", 10, min = 0, max = 100),
          numericInput("exp.er3", HTML("<span style='font-weight: normal;'> <b>What is the anticipated error rate (%)? </b> <br>
                                       <small>* The anticipated error rate is your informed estimate of what the error rate might be before reviewing any records. This value MUST be lower than than maximum acceptable error rate</small> </span>") 
                       , 5, min = 0, max = 100),
          selectInput("conf.level3", HTML("<span style='font-weight: normal;'> <b>What confidence level do you want?</b> <br> <small>* Default is 95%. The closer the number is to 1, the more confidence there is that the true error rate lies within the calculated range. A higher confidence level needs a larger sample.</small> </span>"), choices = c(0.90, 0.95, 0.99), selected = 0.95)
        ),
        fluidRow(
          column(12, p(htmlOutput("tab3_text1"))),
          column(12, p(htmlOutput("tab3_text2"))),
          column(12, plotOutput("tab3_plot1")),
          column(12, p(htmlOutput("tab3_text3")))
        )
      ),
      # Fourth tab content
      tabItem(tabName = "estimation",
        h1("Error Rate Estimation"),
        fluidRow(
          column(12, HTML("<span style='font-size: 20px;'>Once you have conducted a record review to validate the algorithm, you can use this page to estimate the confidence interval for the true error rate.<span>"))
        ),
        box(
          title = "Input Parameters",
          width = 8,
          numericInput("chart.n.4", "Enter the number of records reviewed.", 70, min = 1, max = 100000),
          numericInput("err.n.4", "Enter the number of records where the algorithm was found to be erroneous.", 4, min = 0, max = 100000),
          selectInput("conf.level4", HTML("<span style='font-weight: normal;'> <b>What confidence level do you want?</b> <br> <small>* Default is 95%. The closer the number is to 1, the more confidence there is that the true error rate lies within the calculated range. A higher confidence level needs a larger sample.</small> </span>"), choices = c(0.90, 0.95, 0.99), selected = 0.95)
        ),
        fluidRow(
          column(12, p(htmlOutput("tab4_text1")))
        )
      )
    )
  )
)

server <- function(input, output,session) {
  tab2_text1 <- reactive({
    n_charts <- input$n.charts
    conf_level <- input$conf.level
    text <- paste("If", n_charts, "records are reviewed, Figure 1 shows the", 
                  as.character(as.numeric(conf_level) * 100), 
                  "% confidence intervals for an anticipated error rate ranging from 1% to 50%. This information is also displayed in Table 1.")
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab2_text1 <- renderUI({
    tab2_text1()
  })
  
  tab2_text2 <- reactive({
    n_charts <- as.numeric(input$n.charts)
    conf_level <- as.numeric(input$conf.level)
    err_n <- round(n_charts/10)
    text <- paste("This figure shows the upper and lower confidence limits for the given sample size for an anticipated error rate. For example, if the anticipated error rate in ", 
                  n_charts, "records is going to be",
                  round(err_n/n_charts*100,1), 
                  " %, then we are",
                  as.character(as.numeric(conf_level) * 100), "% confident that the true error rate for the algorithm is between ",
                  max(round((err_n/n_charts-qnorm(1-(1-conf_level)/2)*sqrt((err_n/n_charts*(1-err_n/n_charts))/n_charts))*100,1),0), " % and ",
                  round((err_n/n_charts+qnorm(1-(1-conf_level)/2)*sqrt((err_n/n_charts*(1-err_n/n_charts))/n_charts))*100,1), " %.",
                  "The width of the confidence interval changes depending on the anticipated error rate.")
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab2_text2 <- renderUI({
    tab2_text2()
  })
  
  tab2_text3 <- reactive({
    n_charts <- as.numeric(input$n.charts)
    conf_level <- as.numeric(input$conf.level)
    guess.err <- as.numeric(input$guess.err)/100
    text <- paste("Based on your anticipated error rate of ",
                  guess.err*100, "%, if we review ", 
                  n_charts, "charts and find ",
                  round(guess.err*n_charts,1), "errors, the upper ",
                  as.character(as.numeric(conf_level) * 100), "% confidence limit of the error rate will be ",
                  round((guess.err+qnorm(1-(1-conf_level)/2)*sqrt((guess.err*(1-guess.err))/n_charts))*100,1), " %.")
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab2_text3 <- renderUI({
    tab2_text3()
  })
  
  tab2_text4 <- reactive({
    n_charts <- as.numeric(input$n.charts)
    conf_level <- as.numeric(input$conf.level)
    guess.err <- as.numeric(input$guess.err)/100
    
    text <- paste("Would it be acceptable if the true error rate for the algorithm was as high as ",
                  round((guess.err+qnorm(1-(1-conf_level)/2)*sqrt((guess.err*(1-guess.err))/n_charts))*100,1), " %?")
    text
  })
  output$tab2_text4 <- renderUI({
    tab2_text4()
  })
  
  tab2_text5 <- reactive({
    n_charts <- as.numeric(input$n.charts)
    conf_level <- as.numeric(input$conf.level)
    guess.err <- as.numeric(input$guess.err)/100
    text <- paste0("If ",
                  round((guess.err+qnorm(1-(1-conf_level)/2)*sqrt((guess.err*(1-guess.err))/n_charts))*100,1),"% is too high, you can interactively investigate how the ",
                  conf_level*100, "% confidence limits change with the number of charts reviewed for your anticipated error rate of ",
                  round(guess.err*100,1), "%.")
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab2_text5 <- renderUI({
    tab2_text5()
  })
  
  tab2_plot1 <- reactive({
    # Create a new dataset based on the selected parameters
    n.charts <- input$n.charts
    conf.level <- as.numeric(input$conf.level)
    E.rate <- seq(0.01, 0.50, 0.01)
    # estimate confidence intervals for n.charts for varying error rates (Use parametric confidence intervals)
    Z <- qnorm(1-(1-conf.level)/2)
    half.width <- Z*sqrt((E.rate*(1-E.rate))/n.charts)
    LCL <- E.rate-half.width
    LCL[LCL < 0] <- 0    # Setting negative values to 0
    UCL <- E.rate+half.width
    UCL[UCL > 1] <- 1    # Setting values greater than 1 to 1
    UCL <- round(100*UCL, 1)
    LCL <- round(100*LCL, 1)
    # Generate a new dataset using parameter1 and parameter2
    Estimate <- data.frame(Error.Rate=100*E.rate, LCL=LCL, UCL=UCL, CI.Width=UCL-LCL)
    title <- paste0("Figure 1: ",conf.level * 100, "% Confidence Bands, N = ", n.charts)
    ggplot(Estimate, aes(x=Error.Rate, y=Error.Rate)) + ylim(0, max(UCL)) + 
      geom_ribbon(aes(ymin=LCL, ymax=UCL), alpha=0.1, fill = "navy", color = "navy", lwd=2) +
      geom_line() +
      #geom_hline(yintercept=seq(0,max(UCL),10), col="gray", lwd=1, lty=2) +
      #geom_vline(xintercept=seq(0,50,10), col="gray", lwd=1, lty=2) +
      ylab("Confidence Limits (%)") + xlab("Anticipated Error Rate (%)") + labs(title = title)
  })
  # Render the plots based on the new dataset
  output$tab2_plot1 <- renderPlot({
    tab2_plot1()
  })
  
  tab2_plot2 <- reactive({
    exp.er = input$guess.err
    smallest.n = input$smallest.n
    largest.n = input$largest.n
    conf.level = as.numeric(input$conf.level)
    
    Z <- qnorm(1-(1-conf.level)/2)
    plot.er <- exp.er/100
    n <- smallest.n:largest.n
    lcl <- plot.er-Z*sqrt((plot.er*(1-plot.er))/n)
    lcl <- 100*lcl
    lcl[lcl < 0] <- 0
    ucl <- plot.er+Z*sqrt((plot.er*(1-plot.er))/n)
    ucl <- 100*ucl
    Estimate <- data.frame(Sample_Size=n, LCL=lcl, UCL=ucl)
    title <- paste0("Figure 2: ",conf.level * 100, "% Confidence Intervals, Anticipated Error Rate = ", exp.er, "%")
    ggplot(data.frame(n,lcl,ucl), aes(x=n, y=lcl)) + 
      ylim(min(lcl), max(ucl)) + #xlim(smallest.n, largest.n) +
      geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.1, fill = "navy", color = "navy", lwd=2) +
      geom_line() +
     # geom_hline(yintercept=seq(0,max(ucl),10), col="gray", lwd=1, lty=2) +
      geom_hline(yintercept=exp.er, col="black", lwd=1) +
      #geom_vline(xintercept=seq(smallest.n,largest.n,10), col="gray", lwd=1, lty=2) +
      ylab("Confidence Limits (%)") + xlab("Sample Size") + labs(title = title)
  })
  # Render the plots based on the new dataset
  output$tab2_plot2 <- renderPlot({
    tab2_plot2()
  })  
  
  
  
  
  
  tab2_table1 <- reactive({
    # Create a new dataset based on the selected parameters
    n.charts <- input$n.charts
    conf.level <- as.numeric(input$conf.level)
    E.rate <- seq(0.01, 0.50, 0.01)
    # estimate confidence intervals for n.charts for varying error rates (Use parametric confidence intervals)
    Z <- qnorm(1-(1-conf.level)/2)
    half.width <- Z*sqrt((E.rate*(1-E.rate))/n.charts)
    LCL <- E.rate-half.width
    LCL[LCL < 0] <- 0    # Setting negative values to 0
    UCL <- E.rate+half.width
    UCL[UCL > 1] <- 1    # Setting values greater than 1 to 1
    UCL <- round(100*UCL, 1)
    LCL <- round(100*LCL, 1)
    DIFF <- round(UCL-LCL, 1)
    E.rate <- round(E.rate*100,0)
    # Generate a new dataset using parameter1 and parameter2
    data.frame(`Error Rate`=E.rate, `Lower Limit`=LCL, `Upper Limit`=UCL, `Range`=DIFF,check.names = F)
  })
  # Render the plots based on the new dataset
  output$tab2_table1 <- DT::renderDataTable(
    datatable(
      tab2_table1(),
      rownames = FALSE,
      options=list(pageLength = 10),
      caption = htmltools::tags$caption(paste0("Table 1. Parametric (normal approximation) ",
                                               as.numeric(input$conf.level)*100, "% confidence intervals for anticipated error rates of 1 to 50% based on a sample size of ",
                                               input$n.charts, " charts. Range is the difference between the upper and lower limits."), style="color:black;font-size:14px")
    )
  )

  
  ## Starting the server for tab 3
  tab3_text1 <- reactive({
    exp.mer <- input$exp.mer
    exp.er3 <- input$exp.er3
    conf.level <- as.numeric(input$conf.level3)
    
    Z <- qnorm(1-(1-conf.level)/2)
    n <- ceiling(Z^2*(exp.er3/100)*(1-exp.er3/100)/((exp.mer-exp.er3)/100)^2)

    
    if (exp.er3<exp.mer){
      text <- paste0("A sample size of ",
                     n, " will be needed to provide an upper ",
                     conf.level*100, "% confidence limit less than ",
                     exp.mer, "% for an anticipated error rate up to ",
                     exp.er3, "%.")
    }
    else{
      text <- paste0("The best guess of the error rate has to be less than the maximum acceptable error rate!!!")
    }

    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab3_text1 <- renderUI({
    tab3_text1()
  })

  tab3_text2 <- reactive({
    exp.mer <- input$exp.mer
    exp.er3 <- input$exp.er3
    conf.level <- as.numeric(input$conf.level3)
    
    Z <- qnorm(1-(1-conf.level)/2)
    n <- ceiling(Z^2*(exp.er3/100)*(1-exp.er3/100)/((exp.mer-exp.er3)/100)^2)
    half.width <- Z*sqrt((exp.er3/100*(1-exp.er3/100))/n)
    
    if (exp.er3<exp.mer){
      text <- paste0("Figure 1 demonstrates that for a sample size of ",
                     n," and an anticipated error rate of ",
                     exp.er3, "%, the ",
                     conf.level*100, "% confidence interval would be [",
                     max(round((exp.er3/100-half.width)*100,1),0), ", ",
                     min(round((exp.er3/100+half.width)*100,1),100),"]%. If the anticipated error rate increases, a larger sample size would be needed to ensure that the upper confidence limit remains below ",
                     exp.mer, "%.")
    }
    else{
      text <- paste0("")
    }
    
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab3_text2 <- renderUI({
    tab3_text2()
  })
  
  tab3_text3 <- reactive({
    exp.mer <- input$exp.mer/100
    exp.er3 <- input$exp.er3/100
    conf.level <- as.numeric(input$conf.level3)
    
    Z <- qnorm(1-(1-conf.level)/2)
    n <- ceiling(Z^2*(exp.er3/100)*(1-exp.er3/100)/((exp.mer-exp.er3)/100)^2)
    half.width <- Z*sqrt((exp.er3*(1-exp.er3))/n)
    
    if (exp.er3<exp.mer){
      text <- paste0("Note: The true error rate can be anywhere in this interval and even be outside this range although with low probability.")
    }
    else{
      text <- paste0("")
    }
    
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab3_text3 <- renderUI({
    tab3_text3()
  })
  
  
  tab3_plot1 <- reactive({
    exp.mer <- input$exp.mer/100
    exp.er3 <- input$exp.er3/100
    conf.level <- as.numeric(input$conf.level3)
    
    Z <- qnorm(1-(1-conf.level)/2)
    n <- Z^2*(exp.er3)*(1-exp.er3)/((exp.mer-exp.er3))^2
    
    
    exp.er3_ = seq(0,exp.mer,0.0001)
    n_ <- Z^2*(exp.er3_)*(1-exp.er3_)/((exp.mer-exp.er3_))^2
    exp.er3_ = exp.er3_[which(n_<ceiling(n)*3)]
    exp.er3_ = seq(0,max(exp.er3_),0.0001)
    n_ <- Z^2*(exp.er3_)*(1-exp.er3_)/((exp.mer-exp.er3_))^2
    half.width <- Z*sqrt((exp.er3_*(1-exp.er3_))/n_)*100
    
    exp.er3_ = exp.er3_*100
    exp.mer = exp.mer*100
    exp.miner = pmax(exp.er3_ - half.width,0)
    exp.maxer = pmin(exp.er3_ + half.width,100)
    
    Estimate <- data.frame(curve_err=exp.er3_, curve_n=n_, exp.mer_=exp.mer, exp.miner_ = exp.miner,exp.maxer_=exp.maxer)

    title <- paste0("Figure 1: Number of charts needed to review for ",conf.level * 100, "% Confidence Level, Maximum Acceptable Error Rate = ", exp.mer, "%")
    
    g <- ggplot(Estimate, aes(x=curve_n, y=exp.er3_)) +
      geom_line() +
      geom_ribbon(aes(ymin=exp.miner_, ymax=exp.maxer_), alpha=0.1, fill = "navy", color = "navy", lwd=2) +
      xlab("Sample Size") + ylab("Anticipated Error Rate (%)") + labs(title = title) +
      geom_segment(
        aes(
          x = 0,
          xend = n,
          y = exp.er3*100, yend = exp.er3*100
        ),
        linetype = 'dashed'
      ) +
      geom_segment(
        aes(
          x = n,
          xend = n,
          y = 0,
          yend = exp.er3*100
        ),
        linetype = 'dashed'
      ) + 
      annotate("text", x = n*2, y=exp.mer*19/20, label = paste0("Maximum Acceptable Error Rate = ", exp.mer, "%"))
    
    if (exp.er3*100<exp.mer){
      g
    }
  })
  output$tab3_plot1 <- renderPlot({
    tab3_plot1()
  })

  
  
  # Begining of the server function for tab 4
  
  tab4_text1 <- reactive({
    chart.n.4 <- input$chart.n.4
    err.n.4 <- input$err.n.4
    conf.level <- as.numeric(input$conf.level4)
    
    error_r = err.n.4/chart.n.4
    
    Z <- qnorm(1-(1-conf.level)/2)
    half.width <- Z*sqrt((error_r*(1-error_r))/chart.n.4)
    
    if (err.n.4<=chart.n.4){
      text <- paste0("Given ",
                     err.n.4, " errors observed in the ",
                     chart.n.4, " charts reviewed, the estimated error rate is ",
                     round(error_r*100,1), "% with a ",
                     conf.level*100, "% confidence interval of [",
                     max(0,round(error_r-half.width,3)*100),", ",
                     min(100,round(error_r+half.width,3)*100), "]%.")
    }
    else{
      text <- paste0("Number of errors observed must be not greater than number of chart viewed!!!!")
    }
    
    HTML(paste0("<span style='font-size: 20px;'>", text, "</span>"))
  })
  output$tab4_text1 <- renderUI({
    tab4_text1()
  })

}

shinyApp(ui, server)
