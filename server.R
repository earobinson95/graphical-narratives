library(shiny)
library(shinyjs)
library(RSQLite)
library(DBI)

db_con <- dbConnect(SQLite(), dbname = here::here("study_data.db"))

onStop(function() dbDisconnect(db_con))

is_answered <- function(x) {
  !is.null(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    trimws(as.character(x)) != ""
}

all_answered <- function(input, ids) {
  all(vapply(ids, function(i) is_answered(input[[i]]), logical(1)))
}

or_na <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
}

int_answers <- function(input, ids) {
  setNames(lapply(ids, function(i) as.integer(input[[i]])), ids)
}

nfc_items   <- paste0("nfc_", 1:6)
comp_items  <- paste0("comp_", 1:3)
trust_items <- paste0("tr_", 1:5)
gtd_items <- paste0("gtd_", 1:4)

# ------ #
# Server #
# ------ #

function(input, output, session) {
  
  shinyjs::disable(selector = ".navbar-nav a")
  shinyjs::disable(selector = "#study_subtabs a")
  
  condition <- reactiveVal(NULL)
  
  saved_notes         <- reactiveVal(NULL)
  notes_submitted     <- reactiveVal(FALSE)
  saved_takeaways     <- reactiveVal(NULL)
  takeaways_submitted <- reactiveVal(FALSE)
  submission_started  <- reactiveVal(FALSE)
  saved_comprehension <- reactiveVal(NULL)
  comprehension_submitted <- reactiveVal(FALSE)
  
  # ---------------- #
  # Informed Consent #
  # ---------------- #
  
  observeEvent(input$begin_exp, {
    if (isTRUE(input$consent)) {
      if (is.null(condition())) {
        condition(sample(c("author_driven", "reader_driven"), 1))
      }
      
      updateTabsetPanel(session, "inNavBar", selected = "demographics-tab")
    } else {
      showNotification("Please provide informed consent.", type = "warning")
    }
  })
  
  # ------------ #
  # Demographics #
  # ------------ #
  
  observeEvent(input$submit_demo, {
    required <- c("prolific_id", "age", "gender", "education", nfc_items, gtd_items)
    
    unanswered <- !vapply(
      required,
      function(i) is_answered(input[[i]]),
      logical(1)
    )
    
    if (any(unanswered)) {
      showNotification(
        paste0("Please answer all questions (", sum(unanswered), " remaining)."),
        type = "warning"
      )
      return()
    }
    
    demographics <- data.frame(
      session_id  = session$token,
      condition   = condition(),
      prolific_id = trimws(input$prolific_id),
      age         = input$age,
      gender      = input$gender,
      education   = input$education,
      int_answers(input, nfc_items),
      int_answers(input, gtd_items),
      submitted_at = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    
    dbWriteTable(
      db_con,
      "demographics",
      demographics,
      append = TRUE,
      row.names = FALSE
    )
    
    updateTabsetPanel(session, "inNavBar", selected = "study-tab")
    updateTabsetPanel(session, "study_subtabs", selected = "explore-tab")
  })
  
  # --------- #
  # Narrative #
  # --------- #
  
  output$narrative <- renderUI({
    req(condition())
    
    src <- switch(
      condition(),
      author_driven = "penguins_closeread.html",
      reader_driven = "penguins_plotscaper.html"
    )
    
    tags$iframe(
      id    = "study_iframe",
      src   = src,
      style = "width:100%; height:80vh; border:none;"
    )
  })
  
  # Tell the iframe which section the participant is in, so every
  # tracked event is tagged with it (explore / takeaways / comprehension)
  observeEvent(input$study_subtabs, {
    section <- sub("-tab$", "", input$study_subtabs)
    
    shinyjs::runjs(sprintf("
      var f = document.getElementById('study_iframe');

      if (f && f.contentWindow && f.contentWindow.Tracker &&
          f.contentWindow.Tracker.setSection) {
        f.contentWindow.Tracker.setSection('%s');
      }
    ", section))
  })
  
  # ----- #
  # Study #
  # ----- #
  
  output$tasks <- renderUI({
    current_tab <- input$study_subtabs
    
    req(current_tab)
    
    if (current_tab == "explore-tab") {
      
      tagList(
        h4("Notes"),
        
        textAreaInput(
          "participant_notes",
          label       = NULL,
          placeholder = "Type here",
          width       = "100%",
          height      = "500px",
          resize      = "vertical"
        ),
        
        actionButton("next_notes", "Next", class = "btn btn-info")
      )
      
    } else if (current_tab == "takeaways-tab") {
      
      tagList(
        h4("Notes"),
        wellPanel(textOutput("saved_notes_display")),
        
        hr(),
        
        h4("Your Takeaways"),
        
        textAreaInput(
          "takeaway1",
          label       = NULL,
          placeholder = "Takeaway #1",
          width       = "100%",
          height      = "100px",
          resize      = "vertical"
        ),
        
        textAreaInput(
          "takeaway2",
          label       = NULL,
          placeholder = "Takeaway #2",
          width       = "100%",
          height      = "100px",
          resize      = "vertical"
        ),
        
        textAreaInput(
          "takeaway3",
          label       = NULL,
          placeholder = "Takeaway #3",
          width       = "100%",
          height      = "100px",
          resize      = "vertical"
        ),
        
        shinyjs::disabled(
          actionButton("next_takeaways", "Next", class = "btn btn-info")
        )
      )
      
    } else if (current_tab == "comprehension-tab") {
      
      tagList(
        h4("Notes"),
        wellPanel(textOutput("saved_notes_display")),
        
        hr(),
        
        h4("Comprehension"),
        
        radioButtons(
          inputId  = "comp_1",
          label    = "Which island has the most penguins?",
          choices  = c("Biscoe", "Dream", "Palmer", "Tordersen"),
          selected = character(0)
        ),
        
        numericInput(
          inputId = "comp_2",
          label   = "By how much more than the second most populated island?",
          value   = NA,
          min     = 0,
          step    = 1,
          width   = "100%"
        ),
        
        radioButtons(
          inputId  = "comp_3",
          label    = "If researchers find a new penguin on Biscoe Island, which species is it most likely to be?",
          choices  = c("Adélie", "Chinstrap", "Gentoo", "Pygoscelis"),
          selected = character(0)
        ),
        
        shinyjs::disabled(
          actionButton("next_comprehension", "Next", class = "btn btn-info")
        )
      )
    }
  })
  
  output$saved_notes_display <- renderText({
    saved_notes()
  })
  
  # Explore
  
  observeEvent(input$next_notes, {
    saved_notes(input$participant_notes)
    notes_submitted(TRUE)
    updateTabsetPanel(session, "study_subtabs", selected = "takeaways-tab")
  })
  
  # Takeaways
  
  takeaway_ids <- c("takeaway1", "takeaway2", "takeaway3")
  
  observe({
    req(input$study_subtabs == "takeaways-tab")
    shinyjs::toggleState("next_takeaways", all_answered(input, takeaway_ids))
  })
  
  observeEvent(input$next_takeaways, {
    if (!isTRUE(notes_submitted())) {
      return()
    }
    
    if (!all_answered(input, takeaway_ids)) {
      showNotification("Please complete all three takeaways.", type = "warning")
      return()
    }
    
    saved_takeaways(list(
      takeaway1 = input$takeaway1,
      takeaway2 = input$takeaway2,
      takeaway3 = input$takeaway3
    ))
    takeaways_submitted(TRUE)
    
    updateTabsetPanel(session, "study_subtabs", selected = "comprehension-tab")
  })
  
  # Comprehension
  
  observe({
    req(input$study_subtabs == "comprehension-tab")
    
    shinyjs::toggleState("next_comprehension", all_answered(input, comp_items))
  })
  
  observeEvent(input$next_comprehension, {
    if (!isTRUE(takeaways_submitted())) {
      return()
    }
    
    if (!all_answered(input, comp_items)) {
      showNotification(
        "Please answer all comprehension questions.",
        type = "warning"
      )
      return()
    }
    
    saved_comprehension(list(
      comp_1 = input$comp_1,
      comp_2 = input$comp_2,
      comp_3 = input$comp_3
    ))
    comprehension_submitted(TRUE)
    
    updateTabsetPanel(session, "study_subtabs", selected = "trust-tab")
  })
  
  # Trust
  
  observe({
    req(input$study_subtabs == "trust-tab")
    shinyjs::toggleState("finish_exp", all_answered(input, trust_items))
  })
  
  observeEvent(input$finish_exp, {
    
    # prevent duplicate submissions
    if (isTRUE(submission_started())) {
      return()
    }
    
    if (!isTRUE(notes_submitted()) ||
        !isTRUE(takeaways_submitted()) ||
        !isTRUE(comprehension_submitted())) {
      showNotification(
        "Please complete the previous sections first.",
        type = "warning"
      )
      return()
    }
    
    if (!all_answered(input, trust_items)) {
      showNotification("Please answer all trust questions.", type = "warning")
      return()
    }
    
    submission_started(TRUE)
    shinyjs::disable("finish_exp")
    
    takeaways     <- saved_takeaways()
    comprehension <- saved_comprehension()
    
    responses <- data.frame(
      session_id   = session$token,
      condition    = condition(),
      notes        = or_na(saved_notes()),
      takeaway_1   = or_na(takeaways$takeaway1),
      takeaway_2   = or_na(takeaways$takeaway2),
      takeaway_3   = or_na(takeaways$takeaway3),
      comp_1       = or_na(comprehension$comp_1),
      comp_2       = as.numeric(comprehension$comp_2),
      comp_3       = or_na(comprehension$comp_3),
      int_answers(input, trust_items),
      submitted_at = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    
    tryCatch(
      dbWriteTable(
        db_con,
        "responses",
        responses,
        append = TRUE,
        row.names = FALSE
      ),
      error = function(e) {
        message("responses write failed: ", conditionMessage(e))
      }
    )
    
    shinyjs::runjs("
      var f = document.getElementById('study_iframe');

      if (!f) {
        Shiny.setInputValue('tracker_missing', 'no iframe');
      } else if (!f.contentWindow.Tracker) {
        Shiny.setInputValue('tracker_missing', 'no Tracker in iframe');
      } else {
        f.contentWindow.Tracker.flush(true);
      }
    ")
  })  
  # -------- #
  # Tracking #
  # -------- #
  
  observeEvent(input$tracker_missing, {
    showNotification(
      paste("Tracker problem:", input$tracker_missing),
      type = "warning"
    )
    
    updateTabsetPanel(session, "inNavBar", selected = "done-tab")
  })
  
  observeEvent(input$tracking, {
    msg <- jsonlite::fromJSON(input$tracking, simplifyVector = FALSE)
    
    # pull one field from a list of events (NA if missing)
    num <- function(l, k) {
      vapply(
        l,
        function(e) if (is.null(e[[k]])) NA_real_ else as.numeric(e[[k]]),
        numeric(1)
      )
    }
    
    chr <- function(l, k) {
      vapply(
        l,
        function(e) if (is.null(e[[k]])) NA_character_ else as.character(e[[k]]),
        character(1)
      )
    }
    
    # pull element i of an array field, e.g. cellRect = [left, top, w, h]
    box <- function(l, k, i) {
      vapply(
        l,
        function(e) {
          if (is.null(e[[k]]) || length(e[[k]]) < i) {
            NA_real_
          } else {
            as.numeric(e[[k]][[i]])
          }
        },
        numeric(1)
      )
    }
    
    # ---- scrolling ----
    
    scroll <- Filter(
      function(e) e$type %in% c("scroll", "section_enter", "section_exit"),
      msg$events
    )
    
    if (length(scroll) > 0) {
      scroll_df <- data.frame(
        session_id = session$token,
        condition  = condition(),
        seq        = msg$seq,
        section    = chr(scroll, "section"),
        type       = chr(scroll, "type"),
        t_ms       = num(scroll, "t"),
        y          = num(scroll, "y"),
        depth      = num(scroll, "depth"),
        scroller   = chr(scroll, "scroller"),
        section_id = chr(scroll, "id"),
        dwell_ms   = num(scroll, "dwellMs"),
        at_end     = num(scroll, "atEnd"),
        stringsAsFactors = FALSE
      )
      
      dbWriteTable(
        db_con,
        "scroll_events",
        scroll_df,
        append = TRUE,
        row.names = FALSE
      )
    }
    
    # ---- clicks ----
    
    clicks <- Filter(
      function(e) e$type %in% c("down", "up"),
      msg$events
    )
    
    if (length(clicks) > 0) {
      click_df <- data.frame(
        session_id = session$token,
        condition  = condition(),
        seq        = msg$seq,
        section    = chr(clicks, "section"),
        type       = chr(clicks, "type"),
        t_ms       = num(clicks, "t"),
        button     = num(clicks, "button"),
        
        in_cell = ifelse(
          is.na(num(clicks, "inCell")),
          num(clicks, "startedInCell"),
          num(clicks, "inCell")
        ),
        
        x  = num(clicks, "x"),
        y  = num(clicks, "y"),
        px = num(clicks, "px"),
        py = num(clicks, "py"),
        
        cell_left = box(clicks, "cellRect", 1),
        cell_top  = box(clicks, "cellRect", 2),
        cell_w    = box(clicks, "cellRect", 3),
        cell_h    = box(clicks, "cellRect", 4),
        
        inner_left = box(clicks, "innerRect", 1),
        inner_top  = box(clicks, "innerRect", 2),
        inner_w    = box(clicks, "innerRect", 3),
        inner_h    = box(clicks, "innerRect", 4),
        
        tag      = chr(clicks, "tag"),
        el_id    = chr(clicks, "id"),
        el_class = chr(clicks, "cls"),
        stringsAsFactors = FALSE
      )
      
      dbWriteTable(
        db_con,
        "click_events",
        click_df,
        append = TRUE,
        row.names = FALSE
      )
    }
    
    if (isTRUE(msg$final)) {
      updateTabsetPanel(session, "inNavBar", selected = "done-tab")
    }
  })
}