library(shiny)
library(shinyjs)


labels_5 <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", 
              "Agree", "Strongly agree")

labels_6 <- c("Strongly disagree", "Disagree", "Slightly disagree",
              "Slightly agree", "Agree", "Strongly agree")

labels_7 <- c("Strongly disagree", "Disagree", "Slightly disagree",
              "Neither agree nor disagree", "Slightly agree", "Agree",
              "Strongly agree")

likert_matrix <- function(questions, labels, title = NULL) {
  
  n <- length(labels)
  
  tagList(
    if (!is.null(title)) h4(title),
    
    tags$div(
      class = "likert-scroll",
      
      tags$div(
        class = "likert-matrix",
        
        style = sprintf(
          "grid-template-columns: minmax(260px, 2fr) repeat(%d, minmax(105px, 1fr));",
          n
        ),
        
        tags$div(class = "likert-header"),
        
        lapply(labels, function(level) {
          tags$div(
            class = "likert-header likert-level",
            level
          )
        }),
        
        lapply(seq_along(questions), function(i) {
          
          tagList(
            tags$div(
              class = "likert-question",
              questions[i]
            ),
            
            radioButtons(
              inputId = names(questions)[i],
              label = NULL,
              choiceNames = lapply(labels, function(level) {
                tags$span(
                  class = "sr-only",
                  paste("Select", level)
                )
              }),
              choiceValues = seq_along(labels),
              selected = character(0),
              inline = TRUE
            )
          )
        })
      )
    )
  )
}

navbarPage(
  "Graphical Narratives",
  id = "inNavBar",
  
  header = tagList(
    useShinyjs(),
    
    tags$head(
      tags$script(HTML("
        window.addEventListener('message', function(e) {
          if (e.origin !== window.location.origin) return;

          if (e.data && e.data.type === 'tracking') {
            Shiny.setInputValue(
              'tracking',
              JSON.stringify(e.data),
              {priority: 'event'}
            );
          }
        });
      ")),
    ),
    
    tags$style(HTML("
      .likert-scroll {
      width: 100%;
      overflow-x: auto;
      margin-bottom: 25px;
    }
      
      .likert-matrix {
      display: grid;
      align-items: center;
      column-gap: 10px;
      row-gap: 0;
      min-width: 760px;
      width: 100%;
    }
  
    .likert-header {
      text-align: center;
      display: flex;
      justify-content: center;
      align-items: center;
    }
  
    .likert-question-header {
      text-align: left;
    }
  
    .likert-level {
      font-size: 13px;
    }
  
    .likert-question {
      padding: 14px 4px;
      border-bottom: 1px solid #E5E7EB;
    }
      
     .likert-matrix .shiny-input-container {
      display: contents;
    }
    
    .likert-matrix .shiny-options-group {
      display: contents;
    }
    
    .likert-matrix .radio-inline {
      display: flex;
      justify-content: center;
      align-items: center;
      width: 100%;
      margin: 0;
      padding: 14px 0;
      box-sizing: border-box;
    }
    
    .likert-matrix .radio-inline input[type='radio'] {
      position: static;
      float: none;
      margin: 0;
    }

"))
  ),
  
  # ---------------- #
  # Informed Consent #
  # ---------------- #
  
  tabPanel(
    "Informed Consent",
    value = "informed-consent-tab",
    
    fluidRow(
      
      column(
        id = "informed_consent_sidebar",
        width = 3,
        
        helpText(
          "Your response is voluntary and any information we collect from you will be kept confidential.",
          "Please read the informed consent document (click the button below) before you decide whether to participate."
        ),
        
        a(
          "Show Informed Consent Document",
          href = "informed_consent.html",
          target = "_blank"
        ),
        
        checkboxInput(
          "consent",
          "I have read the informed consent document and agree.",
          width = "100%"
        ),
        
        actionButton("begin_exp", "Begin Experiment", class = "btn btn-info")
      ),
      
      column(
        id = "informed_consent_main",
        width = 9,
        
        h3("Welcome to a Survey on Graphical Narratives"),
        HTML("hi :)")
      )
    )
  ),
  
  # ------------ #
  # Demographics #
  # ------------ #
  
  tabPanel(
    "Demographics",
    value = "demographics-tab",
    
    textInput("prolific_id", "Prolific ID"),
    
    selectizeInput(
      "age",
      "Age Range",
      choices = c( "", "Under 19", "19-25", "26-30", "31-35", 
                   "36-40", "41-45", "46-50", "51-55", "56-60", 
                   "Over 60", "Prefer not to answer")
    ),
    
    radioButtons(
      "gender",
      "Gender Identity",
      choices = c(
        "Female", "Male", "Variant/Nonconforming", "Prefer not to answer"
      ),
      selected = character(0)
    ),
    
    selectizeInput(
      "education",
      "Highest Education Level",
      choices = c("",
                  "High School or Less",
                  "Some Undergraduate Courses",
                  "Undergraduate Degree",
                  "Some Graduate Courses",
                  "Graduate Degree",
                  "Prefer not to answer")),
    
    likert_matrix(
      title = "Need for Cognition",
      questions = c(
        nfc_1 = "I prefer complex to simple problems.",
        nfc_2 = "I like to have the responsibility of handling a situation that requires a lot of thinking.",
        nfc_3 = "Thinking is not my idea of fun.",
        nfc_4 = "I would rather do something that requires little thought than something that is sure to challenge my thinking abilities.",
        nfc_5 = "I really enjoy a task that involves coming up with new solutions to problems.",
        nfc_6 = "I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought."
      ),
      labels = labels_5
    ),
    
    likert_matrix(
      title = "General Trust Disposition",
      questions = c(
        gtd_1 = "Have a good word for everyone.",
        gtd_2 = "Retreat from others.",
        gtd_3 = "Avoid contacts with others.",
        gtd_4 = "Respect others."
      ),
      labels = labels_6
    ),
    
    actionButton("submit_demo", "Submit", class = "btn btn-info")
  ),
  
  # ----- #
  # Study #
  # ----- #
  
  tabPanel(
    "Study",
    value = "study-tab",
    
    tabsetPanel(
      id = "study_subtabs",
      
      tabPanel("Explore",       value = "explore-tab"),
      tabPanel("Takeaways",     value = "takeaways-tab"),
      tabPanel("Comprehension", value = "comprehension-tab"),
      
      tabPanel(
        "Trust",
        value = "trust-tab",
        
        likert_matrix(
          title = "Trust",
          questions = c(
            tr_1 = "I am familiar with the topic or data this visualization presents.",
            tr_2 = "I understand what this visualization is trying to tell me.",
            tr_3 = "I believe the visualization shows real data.",
            tr_4 = "I would rely on the facts in this visualization.",
            tr_5 = "I would feel confident using the information to make a decision."
          ),
          labels = labels_7
        ),
        
        shinyjs::disabled(
          actionButton("finish_exp", "Submit", class = "btn btn-info")
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.study_subtabs != 'trust-tab'",
      
      fluidRow(
        column(width = 8, uiOutput("narrative")),
        column(width = 4, uiOutput("tasks"))
      )
    )
  ),
  
  # ---- #
  # Done #
  # ---- #
  
  tabPanel(
    "Done",
    value = "done-tab",
    
    h4("Thank you for participating!")
  )
)