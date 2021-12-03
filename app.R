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
library(rgl)
library(rsm)
library(fields)

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
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
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
          withMathJax(),
          h1("Maximum Likelihood Estimation "), 
          p("In this app you will explore the maximum likelihood estimation with some
            plots. You will also test your knowledge with the escape room game."),
          h2("Instructions"),
          p("This information will change depending on what you want to do."),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the Exploration Tab."),
            tags$li("Play the escape room game to test how far you've come.")
          ),
          ##### Go Button--location will depend on your goals ----
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "Prerequisites",
              size = "large",
              icon = icon("book"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield and Robert P. Carey, III, updated in 2021 by Jiayue
            He and Yudan Zhang",
            br(),
            "We would like to extend a special thanks to the Shiny Program
            Students.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 5/19/2021 by NJH.")
          )
        ),
        
        #### Prerequisites Page ----
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
            collapsed = FALSE,
            width = '100%',
            "Maximum likelihood estimation is a statistical method for
            estimating the parameters of a model. The parameter values are found
            such that they maximize the likelihood that the process described
            by the model produced the data that were actually observed."
          ),
          
          box(
            title = strong("Simple MLE Procedure"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            tags$ol(
              tags$li("An independent and identically distributed sample of data:
                      \\(y_1,y_2,...,y_N\\)"),
              tags$li("Assume that the data is generated from some parametric
                      density function. In particular, let the data be generated
                      from a known density function (or probability mass
                      function): \\(y_i \\sim f\\left(y\\big|\\theta\\right)\\)"),
              tags$li("Exploiting the fact that the data are identically and
                      independently distributed ('i.i.d.'), you can construct a
                      likelihood function: \\[L(\\theta) = \\prod^{N}_{i=1}
                      f\\left(y_i\\big|\\theta\\right)\\\\log(L(\\theta)) =
                      \\sum^{N}_{i=1} log \\left(f\\left(y_i \\big|\\theta\\right)\\right)\\]"),
              tags$li("We use optimization techniques either by hand or using
                      software such as R to find the value of \\(\\theta\\) which
                      maximizes the function.")
            )
          ),
          
          box(
            title = strong("Invariance Property"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p(strong("Theorem: "), "If \\(\\widehat{\\theta}\\) is the maximum likelihood estimator of
              \\(\\theta\\) and \\(f\\) is a function, then
              \\(f(\\widehat{\\theta}\\)) is a maximum likelihood estimator of
              \\(f(\\theta)\\).")
            #p("then \\(f(\\hat \\theta\\)) is a maximum likelihood estimator of \\(f(\\theta\\))")
          ),
          
          box(
            title = strong("Large Sample Property"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p("Asymptotic Normality:"),
            p("Let \\(\\ {Y_1,Y_2,...,Y_n} \\) be a sequence of i.i.d observations where"),
            p("$$ Y_k \\sim f(y|\\theta)$$"),
            br(),
            p("\\(\\hat \\theta\\) is the MLE of \\(\\theta\\), then"),
            p("$$ \\sqrt n\\ (\\hat \\theta\\ - \\theta) \\rightarrow \\ N(0,{1\\over I(\\theta)})$$")
          ),
          br(),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go2",
              label = "Explore!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        

        ### Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore Maximum Likelihood Estimation with interactive plots!"),
          h4(tags$li("Adjust the sliders to change the sample size
                                   and corresponding parameters.")),
          h4(tags$li("Click 'New Sample' button to generate plot.")),
          br(),
          
          tabsetPanel(
            id = "exp",
            
            ### One Parameter Tab ----
            tabPanel(
              title = 'One Parameter',
              br(),
              
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    selectInput(
                      inputId = "distribution",
                      label = "Choose distribution: ",
                      choices = c("Poisson Distribution", "Exponential Distribution")
                    ), #end of selectInput
                    
                    #conditional controls
                    conditionalPanel(
                      condition = ("input.distribution == 'Poisson Distribution'"),
                      sliderInput(
                        inputId = "n1",
                        label = "Choose Sample Size: ",
                        min = 0,
                        max = 200,
                        value = 10,
                        step = 2
                      ), #end of selectInput
                      sliderInput(
                        inputId = "l1",
                        label = "Choose true parameter value for Poisson: \\(\\lambda\\) ",
                        min = 0,
                        max = 100,
                        value = 1,
                        step = 2
                      ) #end tab of sliderInput
                    ), #end of conditionalPanel
                    
                    conditionalPanel(
                      condition = ("input.distribution == 'Exponential Distribution'"),
                      sliderInput(
                        inputId = "n2",
                        label = "Choose Sample Size: ",
                        min = 0,
                        max = 200,
                        value = 10,
                        step = 2
                      ), #end of selectInput
                      sliderInput(
                        inputId = "l2",
                        label = "Choose true parameter value for Exponential: \\(\\lambda\\) ",
                        min = 0,
                        max = 100,
                        value = 1,
                        step = 2
                      ) #end tab of sliderInput
                    ) #end of conditionalPanel
                  )
                  
                ), #end of column
                
                column(
                  width = 8,
                  offset = 0,
                  # conditional plots
                  conditionalPanel(
                    condition = ("input.distribution == 'Poisson Distribution'"),
                    p("Poisson Distribution",style = "text-align: center"),
                    plotOutput("poisson", width = "60%")
                  ) #end of conditionalPanel
                )
              ),
                  
                  ### Poisson Distribution ----
            
              
                
                ### Exponential Distribution ----
                ),#end of tabpanel
            
            tabPanel(
              title = "Two parameters",
              br(),
              
              fluidRow(
                column(width = 6,
                       selectInput(
                         inputId = "distribution1",
                         label = "Choose distribution: ",
                         choices = c("Gamma Distribution")
                       ) #end of selectInput
                )#end of column
              ), 
              
              fluidRow(
                conditionalPanel(
                  condition = ("input.distribution1 == 'Gamma Distribution'"),
                  column(width = 6,
                         sliderInput(
                           inputId = "n3",
                           label = "Choose Sample Size: ",
                           min = 0,
                           max = 200,
                           value = 10,
                           step = 2
                         ), #end of selectInput
                         
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
                  ), #end of column
                  p("Gamme Distribution",style = "text-align: center"),
                  plotOutput("", width = "60%")
                ) #end of conditionalPanel
              ) # end of fluidrow
            )
            
            )
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
            "Attali, D. (2020), shinyjs: Easily Improve the User Experience of 
            Your Shiny Apps in Seconds, R package. Available from  
            https://CRAN.R-project.org/package=shinyjs"
          ), 
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, 
            R package. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R package. Available 
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent", 
            "Neudecker, A. (2019), shinyMatrix: Shiny Matrix Input Field, R 
            package, R package. Available from 
            https://CRAN.R-project.org/package=shinyMatrix"
          ), 
          p(
            class = "hangingindent",
            "Novomestky, F. (2012), matrixcalc: Collection of functions for 
            matrix calculations., R package. Available from
            https://CRAN.R-project.org/package=matrixcalc"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets: Custom 
            Inputs Widgets for Shiny, R package. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. and Lionel, H. (2020), tidyr: Tidy Messy Data, R package.
            Available from https://CRAN.R-project.org/package=tidyr"
          ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016), ggplot2: Elegant graphics for data analysis,
            R Package. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
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
      selected = "references")
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

  #log-likelihood for poisson function
  poissonLik <- function(lambda1,y1){
    n1 <- length(y1)
    logl <- (log(lambda1) * sum(y1)) - (n1 * lambda1) - sum(lfactorial(y1))
    #logl<- -(sum(y)*log(lambda)-n*lambda)
    #return(-logl)
  }
  
  output$poisson <- renderPlot(
    expr = {
      y1 <- rpois(input$n1,input$l1)
      ggplot() +
        #xlab("$\lambda")+
        #ylab("Log_likelihood")+
        xlim(c(0,100)) +
        stat_function(
          fun = poissonLik,
          args = list(y1 = y1),
          color = "blue",
          size = 2
        ) +
        geom_vline(
          xintercept = input$l1,
          color = boastPalette[3],
          size = 1
        ) +
        geom_vline(
          xintercept = mean(y1),
          color = "black",
          size = 1,
          linetype = "dashed"
        ) +
        theme_bw() +
        labs(x = "Lambda", y = "Log-likelihood")
    },
    alt = "Maximum Likelihood plot for Poisson"
  )
  
  ## Example Plot using stat_function ----

  
  ## Example Plot using stat_function ----


  #########Poisson distribution########


  #########Exponential Distribution########


  ########Gamma Distribution#########
  #Log likelihood function for Gamma Distribution



  ## Example RGL plot ----

      
  




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




