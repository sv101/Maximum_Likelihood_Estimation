# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(fields)

# Load additional dependencies and setup functions ----
source('coastal-room.R')

## Likelihood functions ----
### Poisson function
poissonLik <- function(lambda, y){
  n <- length(y)
  return(-n * lambda - sum(lfactorial(y)) + log(lambda) * sum(y))
}

### Exponential
expoLik <- function(lambda, y){
  n <- length(y)
  return(n * log(lambda) - lambda * sum(y))
}

### Gamma Distribution
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

# Load Data ----
roomItems <- read.csv(file = "roomItems.csv", header = TRUE )
questionBank <- read.csv(file = "mleQuestions.csv", header = TRUE)

maxParts <- max(table(questionBank$Index))

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "blue",
    ## Header ----
    dashboardHeader(
      title = "Max Likelihood Est.",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Maximum_Likelihood_Estimation")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Escape Room", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Maximum Likelihood Estimation"),
          p("In this app you will explore the maximum likelihood estimation with
            some plots. You will also test your knowledge with the escape room
            game."),
          h2("Instructions"),
          h3("Explore Page"),
          tags$ol(
            tags$li("Choose to explore plots for either one or two parameters."),
            tags$li("Then choose which distribution you want to explore."),
            tags$li("Examine the plots and see what happens when you change the
                    sample size and the true parameter value.")
          ),
          h3("Escape Room"),
          tags$ol(
            tags$li("Move the mouse around the scene to see objects you can
                    interact with get highlighted. Click on an object and then
                    press the Interact button. This will use an Action Point."),
            tags$li("Some objects will have items others may require keys. Found
                    items will automatically be added to your backpack."),
            tags$li("You can combine items in your backpack together by selecting
                    a matching pair and clicking the Combine button."),
            tags$li("To earn more Action Points, answer the questions below. You
                    can get new context/scenarios by pressing the button at the
                    bottom.")
          ),
          p("Note: this app tends to work best in a maximzed window."),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "Prerequisites",
              icon = icon("book"),
              size = "large"
            )),
          h2("Acknowledgements"),
          p("This app was originally created by Jiayue He and Yudan Zhang in 2021.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 12/7/2021 by NJH.")
          )
        ),
        ### Prerequisites Page ----
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
            "Maximum likelihood estimation is a statistical method for estimating
            the parameters of a model. The parameter values are found such that
            they maximise the likelihood that the process described by the model
            produced the data that were actually observed."
          ),
          box(
            title = strong("Simple MLE Procedure"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
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
                      \\sum^{N}_{i=1} log \\left(f\\left(y_i \\big|\\theta
                      \\right)\\right)\\]"),
              tags$li("We use optimization techniques either by hand or using
                      software such as R to find the value of \\(\\theta\\) which
                      maximizes the function.")
            )
          ),
          box(
            title = strong("Invariance Property"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(strong("Theorem: "), "If \\(\\widehat{\\theta}\\) is the maximum
              likelihood estimator of \\(\\theta\\) and \\(f\\) is a function,
              then \\(f(\\widehat{\\theta}\\)) is a maximum likelihood estimator
              of \\(f(\\theta)\\).")
          ),
          box(
            title = strong("Large Sample Property"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("Asymptotic Normality: Let \\(\\ {Y_1,Y_2,...,Y_n} \\) be a
              sequence of i.i.d observations where \\(Y_k \\sim f(y|\\theta)\\)
              and \\(\\widehat{\\theta}\\) is the MLE of \\(\\theta\\), then
              \\[\\sqrt{n} \\left(\\widehat{\\theta} - \\theta\\right)
              \\rightarrow N\\left(0,\\frac{1}{I(\\theta)}\\right)\\]")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go2",
              label = "Explore",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        ### Explore page ----
        tabItem(
          tabName =  "explore",
          withMathJax(),
          h2("Explore Maximum Likelihood Estimation Plots"),
          p("Select whether you want to estimate one or two parameters and then
            set options for type of distribution. Adjust the sliders for the
            sample size and the true values of the parameters to see how the plots
            change."),
          br(),
          tabsetPanel(
            id = "exp",
            type = "tabs",
            #### One parameter tab ----
            tabPanel(
              title = 'One parameter',
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    selectInput(
                      inputId = "oneParamDist",
                      label = "Choose a distribution",
                      choices = c(
                        "Poisson Distribution",
                        "Exponential Distribution"
                      )
                    ),
                    sliderInput(
                      inputId = "oneParamSize",
                      label = "Set sample size",
                      min = 1,
                      max = 200,
                      step = 1,
                      value = 10
                    ),
                    sliderInput(
                      inputId = "singleParameter",
                      label = "Set true parameter value, \\(\\lambda\\)",
                      min = 0,
                      max = 100,
                      step = 1,
                      value = 1
                    )
                  )
                ),
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("oneParamPlot"),
                  p("The blue curve represents the log-likelihood function at each
                    possible value of the parameter (\\(\\lambda\\)) on the
                    horizontal axis, given a data collection. The solid green
                    vertical line represents the true value of the parameter
                    while the dashed black vertical line represents the estimate
                    based on the sample data.")
                )
              )
            ),
            #### Two parameter tab ----
            tabPanel(
              title = "Two parameters",
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    selectInput(
                      inputId = "twoParamDist",
                      label = "Select a distribution",
                      choices = c("Gamma Distribution"),
                    ),
                    sliderInput(
                      inputId = "twoParamSize",
                      label = "Set sample size",
                      min = 1,
                      max = 200,
                      step = 1,
                      value = 10
                    ),
                    sliderInput(
                      inputId = "twoParamAlpha",
                      label = "Set true value for \\(\\alpha\\)",
                      min = 1,
                      max = 20,
                      step = 0.5,
                      value = 1
                    ),
                    sliderInput(
                      inputId = "twoParamBeta",
                      label = "Set true value for \\(\\beta\\)",
                      min = 0.1,
                      max = 20,
                      step = 0.1,
                      value = 1
                    )
                  )
                ),
                column(
                  width = 1,
                  noUiSliderInput(
                    inputId = "twoParamVertTilt",
                    label = "Tilt the plot",
                    min = 0,
                    max = 90,
                    step = 5,
                    value = 0,
                    orientation = "vertical",
                    direction = "rtl",
                    behaviour = "snap",
                    color = "#009CDE",
                    height = "300px",
                    format = wNumbFormat(decimals = 0, suffix = "º")
                  ),
                ),
                column(
                  width = 7,
                  plotOutput("twoParamPlot"),
                  sliderInput(
                    inputId = "twoParamHorizSpin",
                    label = "Spin the plot",
                    min = 0,
                    max = 360,
                    step = 1,
                    value = 180,
                    width = "100%",
                    animate = animationOptions(
                      interval = 500,
                      loop = TRUE
                    )
                  ),
                  p("This plot shows the log-likelihood of values for the alpha
                    parameter and the beta parameter, given sample data. The log-
                    likelihood values appear on the vertical axis and as the
                    coloring of the surface. You can use the spin slider to
                    rotate the plot horizontally (left and right) and the tilt
                    slider to rotate the plot vertically (top to bottom).")
                )
              )
            )
          ),
          br(),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go3",
              label = "Escape Room",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        ### Escape Room Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Escape from a Coastal Living Room"),
          fluidPage(
            tabsetPanel(
              #### Escape Room ----
              tabPanel(
                title = "Escape Room",
                br(),
                p("Click on different parts of the scene and click the Interact button
                   to explore the room. To earn more action points, answer the questions
                   below the scene."),
                br(),
                fluidRow(
                  column(
                    width = 7,
                    offset = 0,
                    coastalRoom,
                    tags$script(HTML(
                      "document.getElementById('coastalRoom').focus();
                $('#objects').on('click', '.scene-object', (ev) => {
                  Shiny.setInputValue('object', ev.target.id);
                })"
                ))
              ),
              ##### Scene Info Column ----
              column(
                width = 5,
                uiOutput("clickedObject", class = "largerFont"),
                bsButton(
                  inputId = "interactObject",
                  label = "Interact with object",
                  icon = icon("hand-point-up"),
                  size = "large",
                  style = "success",
                  disabled = TRUE
                ),
                br(),
                br(),
                uiOutput("actionPointReport", class = "largerFont"),
                p("Out of action points? Answer questions below to gain action
                points to interact with the scene and objects."),
                hr(),
                h3("Collected Items"),
                p("Your backpack contains:"),
                uiOutput("backpackContents"),
                h4("Combine Items"),
                p("Select two items from your backpack to use an action point to
                combine."),
                selectInput(
                  inputId = "selectedItems",
                  label = "Items to combine",
                  choices = "nothing",
                  selected = NULL,
                  multiple = TRUE
                ),
                bsButton(
                  inputId = "combineItems",
                  label = "Combine items",
                  icon = icon("object-group"),
                  style = "warning",
                  size = "large"
                ),
                hr(),
                bsButton(
                  inputId = "resetGame",
                  label = "Reset Game",
                  style = "danger",
                  size = "large"
                    )
                  )
                )
              )
            )
          ),
          p("Click on different parts of the scene and click the Interact button
            to explore the room. To earn more action points, answer the questions
            below the scene."),
          br(),
          fluidRow(
            column(
              width = 7,
              offset = 0,
              coastalRoom,
              tags$script(HTML(
                "document.getElementById('coastalRoom').focus();
                $('#objects').on('click', '.scene-object', (ev) => {
                  Shiny.setInputValue('object', ev.target.id);
                })"
              ))
            ),
            #### Scene Info Column ----
            column(
              width = 5,
              uiOutput("clickedObject", class = "largerFont"),
              bsButton(
                inputId = "interactObject",
                label = "Interact with object",
                icon = icon("hand-point-up"),
                size = "large",
                style = "success",
                disabled = TRUE
              ),
              br(),
              br(),
              uiOutput("actionPointReport", class = "largerFont"),
              p("Out of action points? Answer questions below to gain action
                points to interact with the scene and objects."),
              hr(),
              h3("Collected Items"),
              p("Your backpack contains:"),
              uiOutput("backpackContents"),
              h4("Combine Items"),
              p("Select two items from your backpack to use an action point to
                combine."),
              selectInput(
                inputId = "selectedItems",
                label = "Items to combine",
                choices = "nothing",
                selected = NULL,
                multiple = TRUE
              ),
              bsButton(
                inputId = "combineItems",
                label = "Combine items",
                icon = icon("object-group"),
                style = "warning",
                size = "large"
              ),
              hr(),
              bsButton(
                inputId = "resetGame",
                label = "Reset Game",
                style = "danger",
                size = "large"
              )
            )
          ),
          hr(),
          h3("Earn Action Points"),
          p("Use the context to answer questions to earn more action points."),
          #### Questions and Answers area ----
          uiOutput("questionAnswer"),
          br(),
          bsButton(
            inputId = 'nextQuestion',
            label = "New Context and Questions",
            style = "warning",
            size = "large"
          )
        ),
        ### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2021), boastUtils: BOAST Utilities.
            (v. 0.1.11.1), [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2021). shinydashboard: Create
            dashboards with ‘Shiny’. (v. 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny:
            Web application framework for R. (v. 1.7.1) [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Nychka, D., Furrer, R., Paige, J., and Sain, S. (2021). fields: Tools
            for spatial data (v. 13.3) [R package]. Available from
            https://github.com/dnychka/fieldsRPackage"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2021). shinyWidgets: Custom
            inputs widgets for shiny (v. 0.6.2). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
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
  ## Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      messageText <- switch(
        EXPR = input$pages,
        overview = "Use this app to explore maximum likelihood plots and to test
            your knowledge with an escape room.",
        prerequisites = "Use this app to explore maximum likelihood plots and to test
            your knowledge with an escape room.",
        explore = "Explore the log-likelihood plot for different distributions.
            See what happens when you change the sample size and the true value.",
        game = "Click on different parts of the scene to interact. Answer
        questions to earn more action points.",
        references = "Use this app to explore maximum likelihood plots and to test
            your knowledge with an escape room."
      )
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = messageText,
        type = "info"
      )
    })

  ## Go buttons ----
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )

  observeEvent(
    eventExpr = input$go2,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    }
  )

  observeEvent(
    eventExpr = input$go3,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "game"
      )
    }
  )

  ## Explore Page Code ----
  oneParamData <- reactiveVal(0)

  ### One parameter case ----
  observeEvent(
    eventExpr = c(input$oneParamDist, input$oneParamSize, input$singleParameter),
    handlerExpr = {
      if (input$oneParamDist == "Poisson Distribution" &
          input$singleParameter > 0) {
        oneParamData(rpois(n = input$oneParamSize, lambda = input$singleParameter))
      } else if (input$oneParamDist == "Exponential Distribution" &
                 input$singleParameter > 0) {
        oneParamData(rexp(n = input$oneParamSize, rate = input$singleParameter))
      } else {
        print("Error in one parameter data")
      }
    }
  )

  output$oneParamPlot <- renderPlot(
    expr = {
      validate(
        need(
          expr = input$oneParamSize > 0,
          message = "Sample size must be greater than zero"
        ),
        need(
          expr = input$singleParameter > 0,
          message = "True value must be greater than zero"
        )
      )
      ggplot() +
        xlim(c(0, max(100, mean(oneParamData()),
                      input$singleParameter, 1/mean(oneParamData())))) +
        stat_function(
          fun = ifelse(
            test = input$oneParamDist == "Poisson Distribution",
            yes = poissonLik,
            no = expoLik
          ),
          args = list(y = oneParamData()),
          color = "blue",
          size = 1
        ) +
        geom_vline(
          xintercept = input$singleParameter,
          color = boastPalette[3],
          size = 1
        ) +
        geom_vline(
          xintercept = ifelse(
            input$oneParamDist == "Poisson Distribution",
            yes = mean(oneParamData()),
            no = 1/mean(oneParamData())
          ),
          color = "black",
          size = 1,
          linetype = "dashed"
        ) +
        theme_bw() +
        theme(
          text = element_text(size = 18)
        ) +
        ylab("Log-likelihood") +
        xlab("Lambda") +
        labs(
          title = paste(
            "Maximum Likelihood Plot for",
            input$oneParamDist
          )
        )
    },
    alt = reactive(
      paste("Log-likelihood plot for the", input$oneParamDist, "the dashed
            vertical line shows the value of the sample mean while the green
            vertical line shows the true value of the parameter from the slider.")
    )
  )

  ### Two parameter case ----
  ### Currently only for gamma distribution
  twoParamData <- reactiveVal(0)
  observeEvent(
    eventExpr = c(input$twoParamDist, input$twoParamSize,
                  input$twoParamAlpha, input$twoParamBeta),
    handlerExpr = {
      if (input$twoParamDist == "Gamma Distribution" &
          input$twoParamAlpha > 0 &
          input$twoParamBeta > 0) {
        twoParamData(
          rgamma(
            n = input$twoParamSize,
            shape = input$twoParamAlpha,
            rate = input$twoParamBeta
          )
        )
      } else {
        print("Error in two parameter data")
      }
    }
  )

  observeEvent(
    eventExpr = twoParamData(),
    handlerExpr = {
      alphas <- seq(0.01, 20, length = 40)
      betas <- alphas
      likelihoods <- outer(
        X = alphas,
        Y = betas,
        FUN = gamLik,
        x = twoParamData()
      )
      output$twoParamPlot <- renderPlot(
        expr = {
          validate(
            need(
              expr = input$twoParamSize > 0,
              message = "Sample size must be greater than zero"
            )
          )
          fields::drape.plot(
            x = alphas,
            y = betas,
            z = likelihoods,
            col = rainbow(50), #rainbow documentation
            ticktype = "simple",
            theta = input$twoParamHorizSpin,
            phi = input$twoParamVertTilt,
            xlab = "Alpha parameter",
            ylab = "Beta parameter",
            zlab = "Log-likelihood"
          )
        },
        alt = reactive(
          paste("Log-likelihood plot for the", input$twoParamDist, "with the
                alpha and beta parameters forming two of the axes and the
                log-likelihood of those alpha and beta values given data the
                vertical axis and coloring.")
        )
      )
    }
  )

  ## Escape Room Code ----
  ## Debugging ----
  observeEvent(
    eventExpr = input$debug,
    handlerExpr = {
      print(mapping())
      actionPoints(10)
    }
  )

  ## User/game reactive variables ----
  gameInProgress <- reactiveVal(FALSE)
  interactedList <- reactiveVal("start")
  actionPoints <- reactiveVal(1)
  backpackNew <- reactiveVal(NULL)


  ## Scene related tasks ----
  ### Hide items behind objects ----
  mapping <- reactiveVal({
    places <- objects$name[which(objects$assignable != "no")]
    places <- sample(places, length(places), replace = FALSE)
    mappings <- roomItems
    for (i in 1:nrow(mappings)) {
      if (mappings$location[i] == "") {
        mappings$location[i] <- places[i]
      }
    }
    mappings
  })

  ### Watch and report selected scene object ----
  observeEvent(
    eventExpr = input$object,
    handlerExpr = {
      if (is.null(input$object)) {
        message <- "Select an object in the scene."
      } else {
        cleanObject <- gsub(
          pattern = "_",
          replacement = " ",
          x = input$object
        )
        message <- paste0(
          "You've clicked on the ",
          cleanObject,
          "."
        )
        updateButton(
          session = session,
          inputId = "interactObject",
          label = paste(" Interact with the", cleanObject),
          disabled = FALSE
        )
      }
      output$clickedObject <- renderUI({
        p(message)
      })
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  ### Interact with selected object ----
  observeEvent(
    eventExpr = input$interactObject,
    handlerExpr = {
      if (input$object == "exit" && "exitKey" %in% backpackNew()) {
        sendSweetAlert(
          session = session,
          title = "Winner!",
          type = "success",
          html = TRUE,
          text = tags$div(
            tags$p("Congrats! You have escaped from the room; enjoy the beach."),
            tags$img(src = "beach.png", alt = "a beach scene", width = "100%")
          )
        )
        updateButton(
          session = session,
          inputId = "interactObject",
          disabled = TRUE
        )
        updateButton(
          session = session,
          inputId = "combineItems",
          disabled = TRUE
        )
        updateButton(
          session = session,
          inputId = "nextQuestion",
          disabled = TRUE
        )
      } else if ((input$object == "exit" && !("exitKey" %in% backpackNew())) ||
                 (input$object == "closet" && !("closetKey" %in% backpackNew()))) {
        sendSweetAlert(
          session = session,
          title = "Key Needed",
          type = "warning",
          text = "A key is needed to interact further with this object."
        )
      } else if (actionPoints() < 1) {
        sendSweetAlert(
          session = session,
          title = "Out of Action Points",
          type = "warning",
          text = "You're out of action points. Correctly answer questions
            below to earn more points to spend on actions."
        )
      } else if (input$object %in% interactedList()) {
        sendSweetAlert(
          session = session,
          title = "Already Interacted",
          type = "info",
          text = "You've already interacted with this object; there is nothing
          more for you to do with it."
        )
      } else {
        actionPoints(actionPoints() - 1)
        interactedList(c(interactedList(), input$object))
        if (!(input$object %in% mapping()$location)) {
          sendSweetAlert(
            session = session,
            title = "Nothing Found",
            type = "info",
            text = "There is nothing here."
          )
        } else {
          foundItem <- mapping()$itemName[which(mapping()$location == input$object)]
          foundItemDesp <- mapping()$description[which(mapping()$location == input$object)]
          sendSweetAlert(
            session = session,
            title = "Found Item!",
            type = "info",
            text = paste0("You have found ", foundItemDesp,". It's been added to
                          your backpack.")
          )
          backpackNew(c(backpackNew(), foundItem))
          updateSelectInput(
            session = session,
            inputId = "selectedItems",
            choices = backpackNew()
          )
        }
      }
    }
  )

  ### Combine items ----
  observeEvent(
    eventExpr = input$combineItems,
    handlerExpr = {
      if (length(input$selectedItems) < 2 || is.null(input$selectedItems)) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Too few items selected; please select two items to combine.",
          type = "error"
        )
      } else if (length(input$selectedItems) > 2) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Too many items selected; please select two items to combine.",
          type = "error"
        )
      } else if (actionPoints() <= 0) {
        sendSweetAlert(
          session = session,
          title = "Out of Action Points",
          type = "warning",
          text = "You're out of action points. Correctly answer questions
            below to earn more points to spend on actions."
        )
      } else {
        box <- input$selectedItems[grepl("Box", input$selectedItems)]
        key <- input$selectedItems[grepl("Key", input$selectedItems)]
        metalBox <- gsub(pattern = "Box", replacement = "", x = box)
        metalKey <- gsub(pattern = "Key", replacement = "", x = key)

        if (length(box) != 0 & length(key) != 0 &
            (metalBox == "copper" | metalKey == "copper")) {
          newItem <- "none"
        } else if (length(box) != 0 & length(key) != 0) {
          newItem <- roomItems$itemName[which(roomItems$location == box &
                                                roomItems$required == key)]
          newDescp <- roomItems$description[which(roomItems$location == box &
                                                    roomItems$required == key)]
        } else {
          newItem <- "none"
        }

        if (grepl("Key", newItem)) {
          sendSweetAlert(
            session = session,
            title = "New Item!",
            type = "info",
            text = paste0(
              "You successfully combined the items and have found ",
              newDescp,
              ". This has been added to your backpack."
            )
          )
          backpackNew(c(backpackNew(), newItem))
          actionPoints(actionPoints() - 1)
          updateSelectInput(
            session = session,
            inputId = "selectedItems",
            choices = backpackNew()
          )
        } else {
          sendSweetAlert(
            session = session,
            title = "Nothing Happened",
            type = "info",
            text = "Nothing happened when you combined the objects."
          )
        }
      }
    }
  )

  ## Earning action points and related tasks ----
  ### Question part ----
  # This only triggers the first time the game page is accessed

  # create shuffled index vector:
  scenario <- reactiveVal(
    sample(x = 1:max(questionBank$Index),
           size = max(questionBank$Index),
           replace = F)
  )

  index <- reactiveVal(0)
  observeEvent(
    eventExpr = input$pages,
    handlerExpr = {
      if (input$pages == "game" && !gameInProgress()) {
        index(1)
        gameInProgress(TRUE)
      }
    }
  )

  ### new scenario button ----
  observeEvent(
    eventExpr = input$nextQuestion,
    handlerExpr = {
      if (index() == length(scenario())) {
        scenario(
          sample(
            x = 1:max(questionBank$Index),
            size = max(questionBank$Index),
            replace = FALSE
          )
        )
        index(1)
      } else {
        index(index() + 1)
      }
      # clear hints, clear feedback
      lapply(
        X = 1:nrow(subsetQB()),
        FUN = function(x){
      output[[paste0("questionFeedback-", x)]] <- renderIcon()
      output[[paste0("hintText-", x)]] <- renderUI(NULL)
        })
    }
  )
  subsetQB <- reactiveVal(0)

  ### questionAnswer block----
  observeEvent(
    eventExpr = index(),
    handlerExpr = {
      # subset questions:
      subsetQB(questionBank[which(questionBank$Index == scenario()[index()]), ])
      output$questionAnswer <- renderUI({
        if (index() > 0 && nrow(subsetQB()) > 0) {
          tagList(
            h4('Scenario'),
            withMathJax(p(subsetQB()[1,'scenario'])),
            lapply(
              X = 1:nrow(subsetQB()),
              FUN = function(x){

                #### display questions ----
                tagList(
                  h4(paste('Question ', x)),
                  withMathJax(p(subsetQB()[x, 'question'])),
                  radioGroupButtons(
                    inputId = paste0("answer-", x),
                    label = "Choose your answer",
                    choices = as.vector(
                      as.matrix(
                        subsetQB()[x, LETTERS[1:subsetQB()[x,'optionCount']]]
                      )
                    ),
                    selected = character(0),
                    size = "lg",
                    direction = "vertical",
                    individual = FALSE,
                    checkIcon = list(
                      yes = icon("check-square"),
                      no = fontawesome::fa(name = "far fa-square")
                    ),
                    status = "game"
                  ),

                  #### hint button ----
                  fluidRow(
                    column(
                      width = 2,
                      bsButton(
                        inputId = paste0("showHint-", x),
                        label = "Show hint",
                        icon = NULL,
                        size = "large",
                        type = "toggle",
                        value = FALSE
                      )
                    ),
                    column(
                      width = 10,
                      uiOutput(paste0("hintText-", x))
                    )
                  )
                  ,

                  #### submitAnswer button ----
                  fluidRow(
                    column(
                      width = 2,
                      offset = 0,
                      bsButton(
                        inputId = paste0("submit-", x),
                        label = "Submit answer",
                        style = "success",
                        size = "large"
                      )
                    ),
                    column(
                      width = 10,
                      offset = 0,
                      uiOutput(paste0("questionFeedback-", x))
                    )
                  ),
                  br()
                )
              }
            )
          )
        }
      })
    }
  )

  ## Display of hint text ----
  sapply(
    X = 1:maxParts,
    FUN = function(x) {
      observeEvent(
        eventExpr = input[[paste0("showHint-", x)]],
        handlerExpr = {
          if (input[[paste0("showHint-", x)]]) {
            output[[paste0("hintText-", x)]] <- renderUI(
              withMathJax(p(subsetQB()[x, "hint"]))
            )
            updateButton(
              session = session,
              inputId = paste0("showHint-", x),
              label = "Hide hint"
            )
          } else {
            output[[paste0("hintText-", x)]] <- renderUI(NULL)
            updateButton(
              session = session,
              inputId = paste0("showHint-", x),
              label = "Show hint"
            )
          }
        }
      )
    }
  )

  ## Answer checking ----
  sapply(
    X = 1:maxParts,
    FUN = function(x) {
      observeEvent(
        eventExpr = input[[paste0("submit-", x)]],
        handlerExpr = {
          if (is.null(input[[paste0("answer-", x)]])) {
            sendSweetAlert(
              session = session,
              title = "Select an Answer",
              text = paste("You need to select answer for Question", x),
              type = "warning"
            )
          } else {
            output[[paste0("questionFeedback-", x)]] <- renderIcon(
              icon = ifelse(
                test = input[[paste0("answer-", x)]] == subsetQB()[x, "answer"],
                yes = "correct",
                no = "incorrect"
              )
            )
            # add score for correct answer
            if (input[[paste0("answer-", x)]] == subsetQB()[x, "answer"]) {
              actionPoints(actionPoints() + 1)
            }
          }
        }
      )
    }
  )

  # clear questionFeedback when changing choice:
  sapply(
    X = 1:maxParts,
    FUN = function(x) {
      observeEvent(
        eventExpr = input[[paste0("answer-", x)]],
        handlerExpr = {
          output[[paste0("questionFeedback-", x)]] <- renderIcon()
        }
      )
    }
  )

  ## Reset Button for all ----
  observeEvent(
    eventExpr = input$resetGame,
    handlerExpr = {
      interactedList("start")
      actionPoints(1)
      backpackNew(NULL)

      #### Shuffled scenarios ----
      scenario(
        sample(
          x = 1:max(questionBank$Index),
          size = max(questionBank$Index),
          replace = FALSE
        )
      )
      #### index change ----
      index(1)

      #### clear questionFeedback ----
      subsetQB(questionBank[which(questionBank$Index == scenario()[index()]), ])
      lapply(
        X = 1:nrow(subsetQB()),
        FUN = function(x){
          output[[paste0("questionFeedback-", x)]] <- renderIcon()
        })

      #### Reset objects and places ----
      places <- objects$name[which(objects$assignable != "no")]
      places <- sample(places, length(places), replace = FALSE)
      mappings <- roomItems
      for (i in 1:nrow(mappings)) {
        if (mappings$location[i] == "") {
          mappings$location[i] <- places[i]
        }
      }
      mapping(mappings)
      updateButton(
        session = session,
        inputId = "interactObject",
        label = " Interact with object",
        disabled = TRUE
      )
      updateButton(
        session = session,
        inputId = "combineItems",
        disabled = FALSE
      )
      updateButton(
        session = session,
        inputId = "nextQuestion",
        disabled = FALSE
      )
      updateSelectInput(
        session = session,
        inputId = "selectedItems",
        choices = "Collect items first"
      )
    }
  )

  ## Display Elements ----
  ### Display remaining action points ----
  output$actionPointReport <- renderUI({
    paste("You have", actionPoints(), "action point(s) remaining.")
  })

  ### Display backpack contents ----
  output$backpackContents <- renderUI({
    paste(backpackNew(), collapse = ", ")
  })

  ### Code for Re-rendering mathematics ----
  typesetMath(session = session)

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
