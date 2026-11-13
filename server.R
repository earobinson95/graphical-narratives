library(shiny)
library(shinyjs)
library(RSQLite)
library(DBI)
library(tidyverse)

filename      <- here::here("study_data.db")
sqlite.driver <- dbDriver("SQLite")
db_con        <- dbConnect(sqlite.driver, dbname = filename)

function(input, output, session) {
  shinyjs::disable(selector = ".navbar-nav a")
  
  condition <- reactiveVal(NULL)
  
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
    items <- c(paste0("nfc_", 1:6), paste0("gtd_", 1:4))
    
    answered <- vapply(items, function(i) !is.null(input[[i]]), logical(1))
    if (!all(answered)) {
      showNotification(
        paste0("Please answer all questions (", sum(!answered), " remaining)."),
        type = "warning"
      )
      return()
    }
    
    answers <- setNames(lapply(items, function(i) as.integer(input[[i]])), items)
    
    covariates <- data.frame(
      session_id = session$token,
      answers,
      stringsAsFactors = FALSE
    )
    
    dbWriteTable(db_con, "covariates", covariates, append = TRUE, row.names = FALSE)
    
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
        Shiny.setInputValue('tracker_missing', 'no Tracker in iframe');
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
    
    num <- function(l, k) vapply(l, function(e) if (is.null(e[[k]])) NA_real_ else as.numeric(e[[k]]), numeric(1))
    chr <- function(l, k) vapply(l, function(e) if (is.null(e[[k]])) NA_character_ else as.character(e[[k]]), character(1))
    box <- function(l, k, i) vapply(l, function(e) if (is.null(e[[k]])) NA_real_ else as.numeric(e[[k]][[i]]), numeric(1))
    
    scroll <- Filter(function(e) e$type %in% c("scroll", "section_enter", "section_exit"), msg$events)
    
    if (length(scroll) > 0) {
      scroll_df <- data.frame(
        session_id = session$token,
        condition  = condition(),
        seq        = msg$seq,
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
      dbWriteTable(db_con, "scroll_events", scroll_df, append = TRUE, row.names = FALSE)
    }
    
    clicks <- Filter(function(e) e$type %in% c("down", "up"), msg$events)
    
    if (length(clicks) > 0) {
      click_df <- data.frame(
        session_id = session$token,
        condition  = condition(),
        seq        = msg$seq,
        type       = chr(clicks, "type"),
        t_ms       = num(clicks, "t"),
        button     = num(clicks, "button"),
        in_cell    = ifelse(is.na(num(clicks, "inCell")), num(clicks, "startedInCell"), num(clicks, "inCell")),
        x          = num(clicks, "x"),
        y          = num(clicks, "y"),
        px         = num(clicks, "px"),
        py         = num(clicks, "py"),
        cell_left  = box(clicks, "cellRect", 1),
        cell_top   = box(clicks, "cellRect", 2),
        cell_w     = box(clicks, "cellRect", 3),
        cell_h     = box(clicks, "cellRect", 4),
        inner_left = box(clicks, "innerRect", 1),
        inner_top  = box(clicks, "innerRect", 2),
        inner_w    = box(clicks, "innerRect", 3),
        inner_h    = box(clicks, "innerRect", 4),
        tag        = chr(clicks, "tag"),
        el_id      = chr(clicks, "id"),
        el_class   = chr(clicks, "cls"),
        stringsAsFactors = FALSE
      )
      dbWriteTable(db_con, "click_events", click_df, append = TRUE, row.names = FALSE)
    }
    
    if (isTRUE(msg$final)) {
      updateTabsetPanel(session, "inNavBar", selected = "done-tab")
    }
  })
}