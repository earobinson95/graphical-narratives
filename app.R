library(shiny)
library(shinyjs)

ui <- navbarPage("Graphical Narratives", id = "inNavBar",
                 
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



server <- function(input, output, session) {
  shinyjs::disable(selector = ".navbar-nav a")
  
  condition <- reactiveVal(NULL)
  
  # temp while not connected to a database
  tracking_log <- reactiveVal(
    data.frame(
      condition    = character(),
      seq          = integer(),
      final        = logical(),
      n_events     = integer(),
      events_json  = character(),
      summary_json = character(),
      received_at  = character(),
      stringsAsFactors = FALSE
    )
  )
  
  observeEvent(input$begin_exp, {
    if (input$consent) {
      if (is.null(condition())) {
        condition(sample(c("author_driven", "reader_driven"), 1))
      }
      updateTabsetPanel(session, "inNavBar", selected = "demographics-tab")
    } else {
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
    
    tags$iframe(id = "study_iframe", src = src,
                style = "width:100%; height:80vh; border:none;")
  })
  
  observeEvent(input$finish_exp, {
    shinyjs::runjs("
    var f = document.getElementById('study_iframe');
    if (!f) {
      Shiny.setInputValue('tracker_missing', 'no iframe');
    } else if (!f.contentWindow.Tracker) {
      Shiny.setInputValue('tracker_missing',
        'iframe loaded, no Tracker. title=' + f.contentDocument.title);
    } else {
      f.contentWindow.Tracker.flush(true);
    }
  ")
  })
  
  observeEvent(input$tracker_missing, {
    showNotification(paste("Tracker problem:", input$tracker_missing), type = "warning")
    updateTabsetPanel(session, "inNavBar", selected = "done-tab")
  })
  
  observeEvent(input$tracking, {
    msg <- jsonlite::fromJSON(input$tracking, simplifyVector = FALSE)
    
    row <- data.frame(
      condition    = condition(),
      seq          = msg$seq,
      final        = isTRUE(msg$final),
      n_events     = length(msg$events),
      events_json  = as.character(jsonlite::toJSON(msg$events, auto_unbox = TRUE)),
      summary_json = if (isTRUE(msg$final))
        as.character(jsonlite::toJSON(msg$summary, auto_unbox = TRUE))
      else NA_character_,
      received_at  = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    
    tracking_log(rbind(tracking_log(), row))
    print(row[, c("condition", "seq", "final", "n_events")])
    
    if (isTRUE(msg$final)) {
      updateTabsetPanel(session, "inNavBar", selected = "done-tab")
    }
  })
  
  output$tracking_chunks <- renderTable({
    tracking_log()[, c("condition", "seq", "final", "n_events", "received_at")]
  })
  
  events_long <- reactive({
    log <- tracking_log()
    req(nrow(log) > 0)
    parts <- lapply(seq_len(nrow(log)), function(i) {
      ev <- jsonlite::fromJSON(log$events_json[i], flatten = TRUE)
      if (length(ev) == 0) return(NULL)
      ev$seq <- log$seq[i]
      ev$condition <- log$condition[i]
      ev
    })
    out <- dplyr::bind_rows(parts)
    
    out[] <- lapply(out, function(col) {
      if (is.list(col)) vapply(col, function(x) toString(unlist(x)), character(1)) else col
    })
    out
  })
  
  output$tracking_events <- renderTable(head(events_long(), 100))
  
  output$dl_tracking <- downloadHandler(
    filename = function() paste0("tracking-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
    content  = function(file) {
      out <- events_long()
      out[] <- lapply(out, function(col) if (is.list(col)) vapply(col, toString, "") else col)
      write.csv(out, file, row.names = FALSE)
    }
  )
}
shinyApp(ui, server)