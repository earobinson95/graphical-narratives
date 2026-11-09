library(shiny)
library(shinyjs)

ui <- navbarPage("Graphical Narratives", id = "inNavBar",
                 
  tabPanel("Informed Consent",
           useShinyjs(),
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
           value = "done-tab")
)



server <- function(input, output, session) {
  shinyjs::disable(selector = ".navbar-nav a")
  
  condition <- reactiveVal(NULL)
  
  observeEvent(input$begin_exp, {
    if (input$consent) {
      updateTabsetPanel(session, "inNavBar", selected = "demographics-tab")
      condition(sample(c("author_driven", "reader_driven"), 1))
    } 
    else {
      showNotification("Please provide informed consent.")
    }
  })
  
  observeEvent(input$submit_demo, {
    updateTabsetPanel(session, "inNavBar", selected = "covariates-tab")
  })
  
  observeEvent(input$submit_cov, {
    updateTabsetPanel(session, "inNavBar", selected = "study-tab")
  })
  
  output$narrative <- renderUI({
    req(condition())
    
    src <- switch(condition(),
                  author_driven = "penguins_closeread.html",
                  reader_driven = "penguins_plotscaper.html")
    
    tags$iframe(
      src = src,
      width = "100%",
      height = "800px",
      style = "border: none;"
    )
  })
  
  observeEvent(input$finish_exp, {
    updateTabsetPanel(session, "inNavBar", selected = "done-tab")
  })
}

shinyApp(ui, server)