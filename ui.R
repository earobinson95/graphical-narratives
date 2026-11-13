library(shiny)
library(shinyjs)

navbarPage("Graphical Narratives", id = "inNavBar",
                 
  header = tagList(
    useShinyjs(),
    tags$head(tags$script(HTML("
      window.addEventListener('message', function(e) {
        if (e.origin !== window.location.origin) return;
        if (e.data && e.data.type === 'tracking') {
          Shiny.setInputValue('tracking', JSON.stringify(e.data), {priority: 'event'});
        }
      });
    ")))
    ),
                 
  tabPanel("Informed Consent",
           value = "informed-consent-tab",
           
           fluidRow(
             column(id = "informed_consent_sidebar", width = 3,
                    
                    helpText("Your response is voluntary and any information we collect from you will be kept confidential.",
                             "Please read the informed consent document (click the button below) before you decide whether to participate."),
                    
                    a("Show Informed Consent Document", href = "informed_consent.html", target = "_blank"),
                    
                    checkboxInput("consent","I have read the informed consent document and agree.", width = "100%"),
                    
                    actionButton("begin_exp", "Begin Experiment", class = "btn btn-info"),
                    
                    ), # end informed consent sidebar panel column
             
             column(id = "informed_consent_main", width = 9,
                    
                    h3("Welcome to a Survey on Graphical Narratives"),
                    HTML("hi :)"),
                    
                    ) # end informed consent main column
             
           ) # end informed consent fluid row
           
  ), # end informed consent tab
  
  
  tabPanel("Demographics",
           value = "demographics-tab",
           actionButton("submit_demo", "Submit")),
  
  
  tabPanel("Covariates*",
           value = "covariates-tab",
           radioButtons("nfc_1",
                        "I prefer complex to simple problems",
                        choiceNames  = c("Strongly disagree", "Disagree", "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                        choiceValues = 1:5,
                        selected = character(0),
                        inline = TRUE),
           radioButtons("nfc_2",
                        "I like to have the responsibility of handling a situation that requires a lot of thinking",
                        choiceNames  = c("Strongly disagree", "Disagree", "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                        choiceValues = 1:5,
                        selected = character(0),
                        inline = TRUE),
           radioButtons("nfc_3",
                        "Thinking is not my idea of fun",
                        choiceNames  = c("Strongly disagree", "Disagree", "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                        choiceValues = 1:5,
                        selected = character(0),
                        inline = TRUE),
           radioButtons("nfc_4",
                        "I would rather do something that requires little thought than something that is sure to challenge my thinking abilities",
                        choiceNames  = c("Strongly disagree", "Disagree", "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                        choiceValues = 1:5,
                        selected = character(0),
                        inline = TRUE),
           radioButtons("nfc_5",
                        "I really enjoy a task that involves coming up with new solutions to problems",
                        choiceNames  = c("Strongly disagree", "Disagree", "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                        choiceValues = 1:5,
                        selected = character(0),
                        inline = TRUE),
           radioButtons("nfc_6",
                        "I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought",
                        choiceNames  = c("Strongly disagree", "Disagree", "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                        choiceValues = 1:5,
                        selected = character(0),
                        inline = TRUE),
           radioButtons("gtd_1",
                        "Have a good word for everyone",
                        choiceNames  = c("Strongly disagree", "Disagree", "Slightly disagree",
                                         "Slightly agree", "Agree", "Strongly agree"),
                        choiceValues = 1:6,
                        selected = character(0),
                        inline = TRUE),
           radioButtons("gtd_2",
                        "Retreat from others",
                        choiceNames  = c("Strongly disagree", "Disagree", "Slightly disagree",
                                         "Slightly agree", "Agree", "Strongly agree"),
                        choiceValues = 1:6,
                        selected = character(0),
                        inline = TRUE),
           radioButtons("gtd_3",
                        "Avoid contacts with other",
                        choiceNames  = c("Strongly disagree", "Disagree", "Slightly disagree",
                                         "Slightly agree", "Agree", "Strongly agree"),
                        choiceValues = 1:6,
                        selected = character(0),
                        inline = TRUE),
           radioButtons("gtd_4",
                        "Respect others",
                        choiceNames  = c("Strongly disagree", "Disagree", "Slightly disagree",
                                         "Slightly agree", "Agree", "Strongly agree"),
                        choiceValues = 1:6,
                        selected = character(0),
                        inline = TRUE),
           actionButton("submit_cov", "Submit")),
  
  
  tabPanel("Study",
           value = "study-tab",
           uiOutput("narrative"),
           actionButton("finish_exp", "Finish")),
  
  
  tabPanel("Done",
           value = "done-tab",
           h4("Chunks received"),
           tableOutput("tracking_chunks"),
           h4("Events (unpacked)"),
           tableOutput("tracking_events"),
           downloadButton("dl_tracking", "Download events CSV")
  )
)