# Module UI function
csvFileInput <- function(id, label = "CSV file") {
  ns <- NS(id)    # Create a namespace function using the provided id
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}



# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    validate(need(input$file, message = FALSE))    # If no file is selected, don't do anything
    input$file
  })
  
  dataframe <- reactive({                          # The user's data, parsed into a data frame
    read.csv(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      csvFileInput("datafile", "User data (.csv format)")
    ),
    mainPanel(
      dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  datafile <- callModule(csvFile, "datafile", stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    datafile()
  })
}

shinyApp(ui, server)
