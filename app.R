# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(shinyalert)

# Load additional dependencies and setup functions ----
source('coastal-room.R')

# Load Data ----
roomItems <- read.csv(file = "roomItems.csv", header = TRUE )
questionBank <- read.csv(file = "mleQuestions.csv", header = TRUE)

maxParts <- max(table(questionBank$Index))

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
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
    ### Create the sidebar/left navigation menu ----
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
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Maximum Likelihood Estimation"),
          p("In this app you will explore the maximum likelihood estimation with
            some plots. You will also test your knowledge with the escape room
            game."),
          h2("Instructions"),
          tags$ol(
            tags$li("The app works best in a maximized window."),
            tags$li("Answer questions to earn action points."),
            tags$li("Use action points to interact with objects in the scene
                    and gain items which will be stored in your backpack."),
            tags$li("Items in the backpack may need to be combined to be useful.
                    Such as using a key or password to open a box in your backpack.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go",
              label = "Go",
              icon = icon("bolt"),
              size = "large"
            )),
          h2("Acknowledgements"),
          p("This app was originally created by Jiayue He and Yudan Zhang.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 12/6/2021 by NJH.")
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
            such that they maximise the likelihood that the process described
            by the model produced the data that were actually observed."
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
              label = "Explore!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        #### Explore page ----
        tabItem(
          tabName =  "explore",
          withMathJax(),
          
        ),
        #### Escape Room Page ----
        tabItem(
          tabName = "game",
          withMathJax(),
          h2("Escape from a Coastal Living Room"),
          p("Click on different parts of the scene and click the Interact button
            to explore the room. To earn more action points, answer the questions
            below the scene."),
          br(),
          bsButton(
            inputId = "debug",
            label = "debug",
            icon = icon("bug"),
            size = "large"
          ),
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
          ),
          hr(),
          h3("Earn Action Points"),
          p("Use the context to answer questions to earn more action points."),
          ##### Questions and Answers area ----
          uiOutput("questionAnswer"),
          br(),
          bsButton(
            inputId = 'nextQuestion',
            label = "New Context and Questions",
            style = "warning",
            size = "large"
          )
        ),
        #### Set up the References Page ----
        ##### Needs Completing ----
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
  ## Debugging ----
  observeEvent(
    eventExpr = input$debug,
    handlerExpr = {
      print(mapping())
      actionPoints(10)
    }
  )

  ## Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = "Click on different parts of the scene to interact. Answer
        questions to earn more action points.",
        type = "info"
      )
    })

  ## Go button ----
  observeEvent(
    eventExpr = input$go,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "game"
      )
    })

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
