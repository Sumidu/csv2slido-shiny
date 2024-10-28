library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(stringi)
library(jsonlite)
library(bslib)
#library(httr)
#library(elmer)

# Function to safely apply regex
safe_gsub <- function(pattern, replacement, text) {
  if (pattern == "") return(text)
  
  tryCatch({
    # Test if the pattern is valid
    grepl(pattern, "test string", perl = TRUE)
    # If valid, perform the replacement
    gsub(pattern, replacement, text)
  }, error = function(e) {
    # If there's an error, return original text and show notification
    showNotification(
      sprintf("Invalid regex pattern: %s", e$message),
      type = "warning",
      duration = 5
    )
    return(text)
  })
}

# Function to call OpenAI API
call_openai <- function(prompt, api_key) {
  Sys.setenv(OPENAI_API_KEY = api_key)
  
  #chat <- chat_openai(
  #  model = "gpt-4o-mini",
  #  system_prompt = "You are a helpful assistant that improves the clarity and understandability of questions while maintaining their meaning in German. You only respond with the improved question."
  #)
  
  #improved <- chat$chat(paste("Bitte verbessere die Verständlichkeit dieser Frage:", prompt))
  #return(improved)
  return("")
}

ui <- dashboardPage(
  dashboardHeader(title = "CSV Data Viewer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Data View", tabName = "data", icon = icon("table")),
      menuItem("Text Fields", tabName = "text", icon = icon("edit"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box { margin-bottom: 15px; }
        .action-button { margin: 2px; }
      "))
    ),
    tabItems(
      # Upload tab
      tabItem(tabName = "upload",
              fileInput("file", "Choose CSV File",
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
              textInput("openai_key", "OpenAI API Key", ""),
              helpText("Enter your OpenAI API key to enable question improvement functionality")
      ),
      
      # Data view tab
      tabItem(tabName = "data",
              fluidRow(
                column(12,
                       selectInput("column", "Select Column", choices = NULL),
                       DTOutput("table")
                )
              )
      ),
      
      # Modified Text fields tab
      tabItem(tabName = "text",
              fluidRow(
                column(12,
                       div(style = "margin-bottom: 15px",
                           actionButton("prev_btn", "Previous"),
                           actionButton("next_btn", "Next"),
                           selectInput("separator", "Select Separator",
                                       choices = list(
                                         "Tab" = "\t",
                                         "Comma" = ",",
                                         "Semicolon" = ";",
                                         "Pipe" = "|",
                                         "Space" = " ",
                                         "New Line" = "\n",
                                         "Dash" = "- "
                                       ),
                                       selected = "\t"
                           )
                       ),
                       
                       # Raw Content Box
                       box(
                         title = "Raw Content",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 12,
                         textAreaInput("unseparated", "Unseparated Column Content", "", rows = 8, width = "100%")
                       ),
                       
                       # Regex and Debug Box
                       box(
                         title = "Regex and Debug Info",
                         status = "info",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 12,
                         textInput("regex_pattern", "Regex Pattern to Delete", ""),
                         verbatimTextOutput("debug_output")
                       ),
                       
                       # Q&A Fields Box
                       box(
                         title = "Question and Answers",
                         status = "success",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         width = 12,
                         
                         # Question section
                         div(style = "margin-bottom: 15px",
                             textAreaInput("question", "Question", "", rows = 3, width = "100%"),
                             div(style = "margin-top: 5px",
                                 actionButton("copy_question", "Copy", icon = icon("copy")),
                                 actionButton("improve_question", "Improve Question Clarity", icon = icon("magic"))
                             ),
                             verbatimTextOutput("improved_question")
                         ),
                         
                         # Answers section
                         div(style = "margin-bottom: 15px",
                             textAreaInput("answer1", "Answer 1", "", rows = 2, width = "100%"),
                             actionButton("copy_answer1", "Copy", icon = icon("copy"))
                         ),
                         div(style = "margin-bottom: 15px",
                             textAreaInput("answer2", "Answer 2", "", rows = 2, width = "100%"),
                             actionButton("copy_answer2", "Copy", icon = icon("copy"))
                         ),
                         div(style = "margin-bottom: 15px",
                             textAreaInput("answer3", "Answer 3", "", rows = 2, width = "100%"),
                             actionButton("copy_answer3", "Copy", icon = icon("copy"))
                         ),
                         div(style = "margin-bottom: 15px",
                             textAreaInput("answer4", "Answer 4", "", rows = 2, width = "100%"),
                             actionButton("copy_answer4", "Copy", icon = icon("copy"))
                         )
                       )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  filtered_data <- reactiveVal(NULL)
  current_row <- reactiveVal(1)
  improved_question_text <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    df <- read_csv(input$file$datapath)
    data(df)
    
    # Set default column to "Texteingabe online" if it exists
    all_columns <- names(df)
    default_column <- if ("Texteingabe online" %in% all_columns) "Texteingabe online" else all_columns[1]
    
    updateSelectInput(session, "column", 
                      choices = all_columns,
                      selected = default_column)
  })
  
  output$table <- renderDT({
    req(data())
    datatable(data())
  })
  
  observeEvent(input$column, {
    req(data(), input$column)
    filtered <- data() %>%
      filter(!is.na(!!sym(input$column)) & !!sym(input$column) != "")
    filtered_data(filtered)
    current_row(1)
    updateTextInputs()
  })
  
  observeEvent(input$next_btn, {
    req(filtered_data(), input$column)
    current_row(min(current_row() + 1, nrow(filtered_data())))
    updateTextInputs()
  })
  
  observeEvent(input$prev_btn, {
    req(filtered_data(), input$column)
    current_row(max(current_row() - 1, 1))
    updateTextInputs()
  })
  
  observeEvent(input$regex_pattern, {
    updateTextInputs()
  })
  
  observeEvent(input$improve_question, {
    req(input$openai_key, input$question)
    
    withProgress(message = 'Improving question clarity...', {
      tryCatch({
        improved <- call_openai(input$question, input$openai_key)
        improved_question_text(improved)
      }, error = function(e) {
        showNotification(
          paste("Error improving question:", e$message),
          type = "error"
        )
      })
    })
  })
  
  output$improved_question <- renderText({
    req(improved_question_text())
    paste("Improved question:\n", improved_question_text())
  })
  
  updateTextInputs <- function() {
    req(filtered_data(), input$column)
    text <- filtered_data()[[input$column]][current_row()]
    
    # Apply regex pattern if provided, using safe_gsub
    text <- safe_gsub(input$regex_pattern, "", text)
    
    updateTextAreaInput(session, "unseparated", value = text)
    parts <- tryCatch({
      strsplit(text, input$separator)[[1]]
    }, error = function(e) {
      character(5)  # Return empty vector if split fails
    })
    
    # Ensure we have enough elements, pad with empty strings if needed
    parts_padded <- c(parts, rep("", max(0, 5 - length(parts))))
    
    updateTextAreaInput(session, "question", value = parts_padded[1])
    updateTextAreaInput(session, "answer1", value = parts_padded[2])
    updateTextAreaInput(session, "answer2", value = parts_padded[3])
    updateTextAreaInput(session, "answer3", value = parts_padded[4])
    updateTextAreaInput(session, "answer4", value = parts_padded[5])
    improved_question_text(NULL)  # Reset improved question when moving to new row
  }
  
  # Add observer for separator changes
  observeEvent(input$separator, {
    updateTextInputs()
  })
  
  observeEvent(input$copy_question, {
    copyToClipboard(input$question)
  })
  
  observeEvent(input$copy_answer1, {
    copyToClipboard(input$answer1)
  })
  
  observeEvent(input$copy_answer2, {
    copyToClipboard(input$answer2)
  })
  
  observeEvent(input$copy_answer3, {
    copyToClipboard(input$answer3)
  })
  
  observeEvent(input$copy_answer4, {
    copyToClipboard(input$answer4)
  })
  
  copyToClipboard <- function(text) {
    write_clip <- function(content) {
      writeLines(content, con = "clipboard")
    }
    
    if (Sys.info()["sysname"] == "Windows") {
      write_clip(text)
    } else if (Sys.info()["sysname"] == "Darwin") {
      system(sprintf("echo '%s' | pbcopy", text))
    } else {
      system(sprintf("echo '%s' | xclip -selection clipboard", text))
    }
  }
  
  output$debug_output <- renderText({
    req(filtered_data(), input$column)
    text <- filtered_data()[[input$column]][current_row()]
    
    # Apply regex pattern if provided, using safe_gsub
    processed_text <- safe_gsub(input$regex_pattern, "", text)
    
    escaped <- stri_escape_unicode(processed_text)
    paste("Original:", text, "\n",
          "After regex:", processed_text, "\n",
          "Escaped:", escaped)
  })
}

shinyApp(ui, server)
