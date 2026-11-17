library(shiny)
library(shinyjs)


labels_5 <- c(
  "Strongly disagree", "Disagree", "Neither agree nor disagree",
  "Agree", "Strongly agree"
)

labels_6 <- c(
  "Strongly disagree", "Disagree", "Slightly disagree",
  "Slightly agree", "Agree", "Strongly agree"
)

labels_7 <- c(
  "Strongly disagree", "Disagree", "Slightly disagree",
  "Neither agree nor disagree", "Slightly agree", "Agree",
  "Strongly agree"
)

likert <- function(id, label, labels) {
  radioButtons(
    id,
    label,
    choiceNames  = labels,
    choiceValues = seq_along(labels),
    selected     = character(0),
    inline       = TRUE
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
      
      # hide the Study subtab navigation
      tags$style(HTML("
        #study_subtabs > .nav-tabs {
          display: none;
        }
      "))
    )
  ),
  
  # ---------------- #
  # Informed Consent #
  # ---------------- #
  
  tabPanel(
    "Informed Consent",
    value = "informed-consent-tab",
    
    fluidRow(
      column(
        id    = "informed_consent_sidebar",
        width = 3,
        
        helpText(
          "Your response is voluntary and any information we collect from you will be kept confidential.",
          "Please read the informed consent document (click the button below) before you decide whether to participate."
        ),
        
        a(
          "Show Informed Consent Document",
          href   = "informed_consent.html",
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
        id    = "informed_consent_main",
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
      choices = c(
        "", "Under 19", "19-25", "26-30", "31-35", "36-40", "41-45",
        "46-50", "51-55", "56-60", "Over 60", "Prefer not to answer"
      )
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
      choices = c(
        "",
        "High School or Less",
        "Some Undergraduate Courses",
        "Undergraduate Degree",
        "Some Graduate Courses",
        "Graduate Degree",
        "Prefer not to answer"
      )
    ),
    
    likert("nfc_1", "I prefer complex to simple problems", labels_5),
    
    likert(
      "nfc_2",
      "I like to have the responsibility of handling a situation that requires a lot of thinking",
      labels_5
    ),
    
    likert("nfc_3", "Thinking is not my idea of fun", labels_5),
    
    likert(
      "nfc_4",
      "I would rather do something that requires little thought than something that is sure to challenge my thinking abilities",
      labels_5
    ),
    
    likert(
      "nfc_5",
      "I really enjoy a task that involves coming up with new solutions to problems",
      labels_5
    ),
    
    likert(
      "nfc_6",
      "I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought",
      labels_5
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
        
        likert(
          "tr_1",
          "I am familiar with the topic or data this visualization presents",
          labels_7
        ),
        
        likert(
          "tr_2",
          "I understand what this visualization is trying to tell me",
          labels_7
        ),
        
        likert("tr_3", "I believe the visualization shows real data", labels_7),
        
        likert(
          "tr_4",
          "I would rely on the facts in this Visualization",
          labels_7
        ),
        
        likert(
          "tr_5",
          "I would feel confident using the information to make a decision",
          labels_7
        ),
        
        likert("gtd_1", "Have a good word for everyone", labels_6),
        likert("gtd_2", "Retreat from others",           labels_6),
        likert("gtd_3", "Avoid contacts with other",     labels_6),
        likert("gtd_4", "Respect others",                labels_6),
        
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
    
    h4("Thank you for participating.")
  )
)