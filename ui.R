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