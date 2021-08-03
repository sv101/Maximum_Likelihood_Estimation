# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(shinycssloaders)
library(plotly)
library(tidyverse)
library(lubridate)
library(data.table)
library(dplyr)
library(ggpmisc)
library(EnvStats)
library(lattice)

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Maximum Likelihood Estimation", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "App_Template")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        #menuItem("Challenge", tabName = "challenge", icon = icon("gears")),
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
        #menuItem("Wizard", tabName = "wizard", icon = icon("hat-wizard")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          tags$a(href = 'http://stat.psu.edu/',tags$img(src = 'psu_icon.jpg', align = "left", width = 180)),
          br(),br(),br(),
          h3(tags$b("About:")),
          
          withMathJax(),
          h1("Maximum Likelihood Estimation "), # This should be the full name.
          p("In this app you will explore the maximum likelihood estimation with some
            plots. You will also test your knowledge with the escape room game."),
          h2("Instructions"),
          p("This information will change depending on what you want to do."),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the Exploration Tab."),
            #tags$li("Challenge yourself."),
            tags$li("Play the escape room game to test how far you've come.")
          ),
          ##### Go Button--location will depend on your goals ----
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "Prerequisites !",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space ----
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield  and Robert P. Carey, III.",
            br(),
            "We would like to extend a special thanks to the Shiny Program
            Students.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 5/19/2021 by NJH.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Background"),
          p("In order to get the most out of this app, please review the
            following:"),
          box(
            title = strong("What is maximum likelihood estimation?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Maximum likelihood estimation is a statistical method for 
            estimating the parameters of a model. The parameter values are found 
            such that they maximise the likelihood that the process described 
            by the model produced the data that were actually observed."
          ),
          box(
            title = strong("Simple MLE Procedure"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("1. An independent and identically distributed sample of data:"),
            p("$$y_1,y_2,...,y_N$$"),
            p("2. Assume that the data is generated from some parametric density function. 
              In particular, let the data be generated from a known density function (or probability mass function): "),
            p("$$y_i \\sim f(y|\\theta)$$"),
            p("3. Exploiting the fact that the data are iid, you can construct a likelihood function:"),
            p("$$L(\\theta) = \\prod^{N}_{i=1}f(y_i|\\theta)\\\\log(L(\\theta)) = \\sum^{N}_{i=1} \\log f(y_i |\\theta)$$"),
            p("4. Last step, we could use R or calculate by hand to find the value of \\theta which maximize the function")
          ),
          box(
            title = strong("Invariance Property"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("Theorem:"),
            br(),
            p("If \\(\\hat \\theta\\) is the maximum likelihood estimator of \\(\\theta\\) and if f is a function,
              then \\(f(\\hat \\theta\\)) is a maximum likelihood estimator of \\(f(\\theta\\))") 
            #p("then \\(f(\\hat \\theta\\)) is a maximum likelihood estimator of \\(f(\\theta\\))")
          ),
          box(
            title = strong("Large Sample Property"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("Asymptotic Normality:"),
            p("Let \\(\\ {Y_1,Y_2,...,Y_n} \\) be a sequence of i.i.d observations where"),
            p("$$ Y_k \\sim f(y|\\theta)$$"),
            br(),
            p("\\(\\hat \\theta\\) is the MLE of \\(\\theta\\), then"),
            p("$$ \\sqrt n\\ (\\hat \\theta\\ - \\theta) \\ -> \\ N(0,{1\\over I(\\theta)})$$")
          ),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go2",
              label = "Explore !",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),

        #### Set up the Explore Page ----
        
        tabItem(
          tabName = "explore",
          withMathJax(),
          #h2("Explore the Concept"),
          #p("Common elements include graphs, sliders, buttons, etc."),
          #p("The following comes from the NHST Caveats App:"),
          tabsetPanel(id = "exp",
                      tabPanel(
                        title = 'One parameter',
                        br(),
                  
                        h4(strong("One parameter")),
                        h6(tags$li("Adjust the sliders to change the sample size 
                                   and corresponding parameters.")),
                        h6(tags$li("Click 'New Sample' button to generate plot.")),
                                 br(),
                                 #width = 25
                        
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              inputId = "distribution",
                              label = "Choose distribution: ",
                              choices = c("Poisson Distribution", "Exponential Distribution")
                            ),
                            
                            checkboxInput(inputId = "checkbox1",
                                          label = "Show the data table",
                                          value = FALSE),
                            
                            
                            conditionalPanel(
                              condition = ("input.distribution == 'Poisson Distribution'"),
                              
                              sliderInput(
                                inputId = "n1",
                                label = "Choose Sample Size: ",
                                min = 0,
                                max = 200,
                                value = 10,
                                step = 2
                              ), #end tab of sidebarInput
                              
                              sliderInput(
                                inputId = "l1",
                                label = "Choose true parameter value for Poisson: \\(\\lambda\\) ",
                                min = 0,
                                max = 100,
                                value = 1,
                                step = 2
                              )#end tab of sidebarInput

                            ),
                            

                            conditionalPanel(
                              condition = ("input.distribution == 'Exponential Distribution'"),
                              
                              sliderInput(
                                inputId = "n2",
                                label = "Choose Sample Size: ",
                                min = 0,
                                max = 200,
                                value = 10,
                                step = 2
                              ), #end tab of sidebarInput
                              
                              sliderInput(
                                inputId = "l2",
                                label = "Choose true parameter value for Exponential: \\(\\lambda\\)",
                                min = 0,
                                max = 5,
                                value = 0.1,
                                step = 0.1
                              ) #end tab of sidebarInput
                            ),
                            
                            
                            br()
                          ),#end tab of sidebarPanel
                          
                          fluidRow(column(width = 7,
                                          fluidRow(
                                            conditionalPanel(
                                              condition = "input.distribution == 'Poisson Distribution",
                                              fluidRow(
                                                column(12, plotOutput("poissonplot",width = "98%")#plotlyOutput
                                                         ), #Green for true, black for estimate
                                                br(),
                                                column(12, tableOutput("diffValues"))
                                              )
                                            ),
                                            conditionalPanel(
                                              condition = "input.distribution == 'Exponential Distribution",
                                              fluidRow(
                                                column(12, plotOutput("expplot",width = "98%")#plotlyOutput
                                                ), #Green for true, black for estimate
                                                br(),
                                                column(12, tableOutput("diffValues2"))
                                              )
                                            )
                                            
                                          ))) #end of fluid page
                        )
                               ),
                      
                      tabPanel(
                        title = 'Two parameters',
                        br(),
                        h4(strong("Two parameters")),
                        h6(tags$li("Adjust the sliders to change the sample size 
                                   and corresponding parameters.")),
                        h6(tags$li("Click 'New Sample' button to generate plot.")),
                               br(),
                        
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              inputId = "distribution1",
                              label = "Choose distribution: ",
                              choices = c("Gamma Distribution", "Multinomial Distribution")
                            ),
                            
                            checkboxInput(inputId = "checkbox2",
                                          label = list("Show the data table"),
                                          value = FALSE),
                            
                            conditionalPanel(
                              condition = ("input.distribution == 'Gamma Distribution'"),
                              sliderInput(
                                inputId = "n3",
                                label = "Choose Sample Size: ",
                                min = 0,
                                max = 200,
                                value = 10,
                                step = 2
                              ), #end tab of sidebarInput
                              
                              sliderInput(
                                inputId = "a3",
                                label = "Choose true parameter value for Gamma: \\(\\alpha\\) (Shape) ",
                                min = 1,
                                max = 20,
                                value = 1,
                                step = 0.5
                              ),#end tab of sidebarInput
                              
                              sliderInput(
                                inputId = "b3",
                                label = "Choose true parameter value for Gamma: \\(\\beta\\) (Rate)",
                                min = 0,
                                max = 20,
                                value = 1,
                                step = 0.1
                              )#end tab of sidebarInput
                            )
                            
                          ),#end tab of sidebarPanel
                          fluidRow(column(width = 7,
                                          fluidRow(
                                            conditionalPanel(
                                              condition = "input.distribution == 'Gamma Distribution",
                                              fluidRow(
                                                #column(12, plotOutput("poissonplot",width = "98%")#plotlyOutput
                                                #), #Green for true, black for estimate
                                                #br(),
                                                #column(12, tableOutput("table3")),
                                                #br(),
                                                column(12, plotOutput("gamma_plot", height = 600)#plotlyOutput
                                                )
                                              )
                                            )
                                            
                                          ))) #end of fluid page
                        ),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                               div(
                                 style = "text-align: center",
                                 bsButton(
                                   inputId = "go3",
                                   label = "Play !",
                                   size = "large",
                                   icon = icon("bolt"),
                                   style = "default"
                                 )
                               )
                        
                      )
            
            
          ),
          br(),
          br(),
          br()
          ),
        #### Set up a Challenge Page ----
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2("Challenge Yourself"),
          p("The general intent of a Challenge page is to have the user take
            what they learned in an Exploration and apply that knowledge in new
            contexts/situations. In essence, to have them challenge their
            understanding by testing themselves."),
          p("What this page looks like will be up to you. Something you might
            consider is to re-create the tools of the Exploration page and then
            a list of questions for the user to then answer.")
        ),
        #### Set up a Game Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Test Yourself with Escape Room Game"),
          p(" "),
          br(),
          br(),
          br(),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go4",
              label = "Continue !",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),

        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App Template will help you get started building your own app"
      )
    }
  )
  
  observeEvent(input$go1,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "prerequisites")
  })
  
  observeEvent(input$go2,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "explore")
  })
  
  observeEvent(input$go3,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "game")
  })
  
  observeEvent(input$go4,{
    updateTabItems(
      session = session,
      inputId = "pages",
      selected = "references")
  })
  
  ############Plots output########
  #collect inputs to a dataframe
  #loglikeliood of poisson
  #n represents sample size, l would be the lambda

  
  #loglikelihood for poisson function
  poissonLik <- function(lambda1,y1){
    n1 <- length(y1)
    logl <- (log(lambda1) * sum(y1)) - (n1 * lambda1) - sum(lfactorial(y1))
    #logl<- -(sum(y)*log(lambda)-n*lambda)
    #return(-logl)
  }
  
  #########Poisson distribution########
  observeEvent(
    eventExpr = c(as.numeric(input$n1), as.numeric(input$l1)),
    handlerExpr = {
      #lambda <- seq(1,100,length.out = input$n)
      data1 <- data.table(
        y1 = rpois(input$n1,input$l1),
        lambda1 = seq(1, by = 2,length.out = input$n1)
      )
      
      
      lambda1 <- data1$lambda1
      y1 <- data1$y1
      est.l1 <- epois(y1)$parameters
      str(est.l1)
      #str(data)
      #Logl <- poissonLik(data$lambda,data$y) #sapply
      #m <- matrix(c("loglik", df))
      
      Logl <- lapply(lambda1,FUN = poissonLik,y1)
      Logl <- unlist(Logl) #delete the first row(sample size)
      #print(Logl)
      
      table1 <-  data.table(lambda1,Logl) #combine lambda and loglikihood value
      #print(table)
      #table <- as.data.frame(table)
      #str(table)
      
      #output$diffValues <- renderTable(
        if (input$checkbox1 == "TRUE") {
          output$diffValues <- renderTable(table1)
        }
      
      else if (input$checkbox1 == "FALSE") {
        output$diffValues = NULL
      }
        
      if (input$distribution == 'Poisson Distribution') {
        output$poissonplot <- renderPlot({
          y_max <- table1$lambda1[which.max(table1$Logl)]
          ggplot(table1, aes(x = lambda1, y = Logl)) +
            geom_line(colour = "blue") +
            xlab("lambda") + ylab("Log-likelihood") +
            ggtitle("Log Likelihood Estimation for Poisson Distribution") +
            geom_vline(xintercept = y_max, colour = "green", size = 0.8) +
            geom_vline(xintercept = est.l1, colour = "black", size = 0.8)
          #stat_peaks(col = "green")
        })
      }#end of if
    }
  )
  
  #########Exponential Distribution########
  expoLik <- function(lambda2,y2){
    n2 <- length(y2)
    logl_exp <- (log(lambda2) * n2) - sum(y2) * lambda2
  }
  
  observeEvent(
    eventExpr = c(as.numeric(input$n2), as.numeric(input$l2)),
    handlerExpr = {
      #lambda <- seq(1,100,length.out = input$n)
      data2 <- data.table(
        y2 = rexp(input$n2,input$l2),
        lambda2 = seq(1, by = 0.5, length.out = input$n2)
      )
      
      lambda2 <- data2$lambda2
      y2 <- data2$y2
      est.l2 <- eexp(y2)$parameters
      #str(data)
      #Logl <- poissonLik(data$lambda,data$y) #sapply
      #m <- matrix(c("loglik", df))
      
      Logl_exp <- lapply(lambda2,FUN = expoLik, y2)
      Logl_exp <- unlist(Logl_exp) #delete the first row(sample size)
      #print(Logl)
      
      table2 <-  data.table(lambda2,Logl_exp) #combine lambda and loglikihood value
      #print(table)
      #table <- as.data.frame(table)
      #str(table)
      
      #output$diffValues <- renderTable(
      if (input$checkbox1 == "TRUE") {
        output$diffValues2 <- renderTable(table2)
      }
      
      else if (input$checkbox1 == "FALSE") {
        output$diffValues2 = NULL
      }
      
      if (input$distribution == 'Exponential Distribution') {
        output$expplot <- renderPlot({
          y_max <- table2$lambda2[which.max(table2$Logl_exp)]
          ggplot(table2, aes(x = lambda2, y = Logl_exp)) +
            geom_line(colour = "blue") +
            ggtitle("Log Likelihood Estimation for Exponential Distribution") +
            xlab("lambda") + ylab("Log-likelihood") +
            geom_vline(xintercept = y_max, colour = "green", size = 0.8)+
            geom_vline(xintercept = est.l2, colour = "black", size = 0.8)
          #stat_peaks(col = "green")
        })
      }#end of if
      
    }
  )
  
  ########Gamma Distribution#########
  #Log likelihood function for Gamma Distribution
  gamLik <- function(alpha, beta, x) {
    n <- length(x)
    alpha <- alpha
    beta <- beta #seperate inputs
    sum_x <- sum(x)
    sum_logx <- sum(log(x))
    Logl_gamma = vector("numeric", length(beta))
    for (i in 1:length(beta)) {
      Logl_gamma[i] = n * alpha[i] * log(beta[i]) + n * lgamma(alpha[i]) + sum_x / beta[i] - (alpha[i] - 1) * sum_logx 
    }
    #lgamma() gives the natural logarithm of alpha function
    return(-Logl_gamma)
  }
  
  observeEvent(
    eventExpr = c(as.numeric(input$n3), as.numeric(input$a3), as.numeric(input$b3)),
    handlerExpr = {
      data_gamma <- data.table(
        x = rgamma(input$n3, shape = input$a3, rate = input$b3),
        #rgamma() generate random sample for gamma distribution
        alpha = seq(1, by = 0.5, length.out = input$n3), #generate
        beta = seq(0, by = 0.1, length.out = input$n3)
      ) #end of data.table
      
      x <- data_gamma$x
      alpha <- data_gamma$alpha
      names(alpha) <- alpha
      beta <- data_gamma$beta
      names(beta) <- beta
      est_gamma <- egamma(x)$parameters
      
      Logl_gamma <- lapply(alpha, beta, FUN = gamLik, x)
      Logl_gamma <- unlist(Logl_gamma)
      str(Logl_gamma)
      
      table_gamma <- data.table(alpha, beta, Logl_gamma)
      str(table_gamma)
      
      output$table3 <- renderTable(
        if (input$checkbox2 == "TRUE") {
          output$table3 <- renderTable(table_gamma)
        }
        else if (input$checkbox2 == "FALSE") {
          output$table3 = NULL
        }
      )
      
      output$gamma_plot <- renderPlot({
        z <- outer(X = alpha, Y = beta, FUN = gamLik, x = x)
        wireframe(z, drape = T, xlab = "alpha", ylab = "beta", zlab = "Log Likelihood",
                  main = "Log-Likelihood for gamma distribution",
                  scales = list(z.ticks=5, arrows=FALSE, col="black", font=3, tck=1))
      })
    }
  )
  
  
  #else if (input$checkbox1 == "FALSE") {
    #output$diffValues2 = NULL
  #}
  #exponentialLik <- function(lambda,y){
    #n<-length(y)
    #n*log(lambda) - lambda * sum(y)
  #}
  #y = rexp(input$n,input$l)
  #lambda = seq(1,50,length.out = input$n)
  #Output plot
  #(log(lambda) * sum(x)) - (n * lambda)
  #lambda <- seq(from = 0.05, to = 10 ,by = 0.05)
  # Data simulation: Poisson with lambda = 5
  #y <- rpois(n, lambda1)
  #values_for_mu<- seq(from=0.05, to = 10 ,by =0.05 )
  #new loglikelihood (only depends on mu)
  #Evaluate the loglikelihood at different values of mu
  #values_log_like <- unlist(lapply(values_for_mu, 
                                   #FUN = log_like_poissson2))
  #generate a dataframe to ggplot2
  #df <- data.frame(values_for_mu, values_log_like)
  # Plot
  

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)




