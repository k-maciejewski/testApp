library(shiny)
library(tidyverse)

testApp <- function(...) {
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Test upload download app"),
        # Show a plot of the generated distribution
        mainPanel(
        
          fileInput(
            inputId = "file",
            label = "Upload csv file",
            multiple = FALSE,
            accept = c(".csv"),
            width = NULL,
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
          downloadButton("report", "Pdf report"),
          tableOutput('table'),
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  table2 <- reactive({
    req(input$file)
    
    vroom::vroom(here::here(input$file$datapath)) %>%
      janitor::clean_names() %>% 
      head()
  })
  
  output$table <- renderTable({
    table2()
  })
  
  output$report <- downloadHandler(
    filename = function() {
      paste("ctgov_prs_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(
        input = tempReport,
        output_file = file,
        params = list(table = table2()),
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
}