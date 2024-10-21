library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(stringi)
 
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
    tabItems(
      # Upload tab
      tabItem(tabName = "upload",
              fileInput("file", "Choose CSV File",
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
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
      
      # Text fields tab
      tabItem(tabName = "text",
              fluidRow(
                column(12,
                       actionButton("prev_btn", "Previous"),
                       actionButton("next_btn", "Next"),
                       hr(),
                       textInput("regex_pattern", "Regex Pattern to Delete", ""),
                       textAreaInput("unseparated", "Unseparated Column Content", "", rows = 5),
                       hr(),
                       textInput("question", "Question", ""),
                       actionButton("copy_question", "Copy", icon = icon("copy")),
                       textInput("answer1", "Answer 1", ""),
                       actionButton("copy_answer1", "Copy", icon = icon("copy")),
                       textInput("answer2", "Answer 2", ""),
                       actionButton("copy_answer2", "Copy", icon = icon("copy")),
                       textInput("answer3", "Answer 3", ""),
                       actionButton("copy_answer3", "Copy", icon = icon("copy")),
                       textInput("answer4", "Answer 4", ""),
                       actionButton("copy_answer4", "Copy", icon = icon("copy")),
                       hr(),
                       verbatimTextOutput("debug_output")
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
  
  observeEvent(input$file, {
    req(input$file)
    df <- read_csv(input$file$datapath)
    data(df)
    updateSelectInput(session, "column", choices = names(df))
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
  
  updateTextInputs <- function() {
    req(filtered_data(), input$column)
    text <- filtered_data()[[input$column]][current_row()]
    
    # Apply regex pattern if provided
    if (input$regex_pattern != "") {
      text <- gsub(input$regex_pattern, "", text)
    }
    
    updateTextAreaInput(session, "unseparated", value = text)
    parts <- strsplit(text, "\t")[[1]]
    updateTextInput(session, "question", value = parts[1] %||% "")
    updateTextInput(session, "answer1", value = parts[2] %||% "")
    updateTextInput(session, "answer2", value = parts[3] %||% "")
    updateTextInput(session, "answer3", value = parts[4] %||% "")
    updateTextInput(session, "answer4", value = parts[5] %||% "")
  }
  
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
    
    # Apply regex pattern if provided
    if (input$regex_pattern != "") {
      text <- gsub(input$regex_pattern, "", text)
    }
    
    escaped <- stri_escape_unicode(text)
    paste("Original:", filtered_data()[[input$column]][current_row()], "\n",
          "After regex:", text, "\n",
          "Escaped:", escaped)
  })
}

shinyApp(ui, server)
