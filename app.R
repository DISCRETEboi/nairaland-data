###--- Shiny app for the Nairaland data ---###

library(shiny)
library(shinydashboard)
library(stringr)
#library(dplyr)
#library(ggplot2)
library(shinyWidgets)
library(reactable)
library(shinycssloaders)

#data_list <- list.files("out-data")
setwd("out-data")
data_list <- list.files()
datasets <- sapply(data_list, read.csv, na.strings = "")
nrow_list <- sapply(datasets, nrow); ncol_list <- sapply(datasets, ncol)
data_list_dim <- str_c(data_list, " [", nrow_list, "x", ncol_list, "]")
full_data_list_dim <- c("full-18000.csv [18200x11]", "full-1800.csv [1874x11]", "full-mini1.csv [659x11]",
                        "full-mini2.csv [118x11]")
partial_data_list_dim <- setdiff(data_list_dim, full_data_list_dim)

ui <- dashboardPage(
  dashboardHeader(title = "NAIRALAND DATA"),
  dashboardSidebar(
    pickerInput("select_data", "Select dataset:", list(`Full Data` = full_data_list_dim,
                                                       `Partial Data` = partial_data_list_dim), selected = "default-18000.csv [1874x11]"),
    downloadButton("download", "Download the dataset", class = "btn-block", style = "color:black")
  ),
  dashboardBody(
    withSpinner(fluidRow(
      withSpinner(valueBoxOutput("vBox1", width = 6), proxy.height = "100px", color = "steelblue"),
      withSpinner(valueBoxOutput("vBox2", width = 6), proxy.height = "100px", color = "steelblue")
    )),
    h4("Nairaland users data, as extracted from their respective profile pages [you can verify through the profile links]"),
    fluidRow(
      box(width = 12, status = "primary", solidHeader = F, withSpinner(reactableOutput("table")))
    ),
    div("- The current table was generated from all threads on just the first page of 'https://www.nairaland.com/phones'", style = "color:red"),
    div("- There would be more features added to the app, soon [e.g. charts].", style = "color:red"),
    strong("- For all columns with wrapped text, kindly resize the column [drag its borders]", style = "color:red"),
    div("- The data is still in its raw form", style = "color:red"),
    strong("* Values recorded in Last Seen are those since the data was last collected, and may not be up to date", style = "color:red")
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    datasets[[str_replace(input$select_data, " .[0-9]+x[0-9]+.", "")]]
  })
  
  #nairaland_users_data <- read.csv("out-data/first-go-csv.csv", na.strings = "")
  box1 <- reactive({
    nrow(dataset())
  })
  box2 <- reactive({
    ncol(dataset())
  })
  
  output$vBox1 <- renderValueBox({
    valueBox(box1(), "Number Of Rows", icon = icon("th-list", lib = "glyphicon"))
  })
  output$vBox2 <- renderValueBox({
    valueBox(box2(), "Number Of Columns", icon = icon("th-list", lib = "glyphicon"))
  })
  
  output$download <- downloadHandler(
    filename = function() {
      str_replace(input$select_data, " .[0-9]+x[0-9]+.", "")
    },
    content = function(file) {
      write.csv(dataset(), file)
    }
  )
  
  output$table <- renderReactable({
    reactable(dataset(), resizable = T, searchable = T, bordered = T, rownames = F, #filterable = T, 
              showPageSizeOptions = T, highlight = T, outlined = T, striped = T,
              wrap = F, defaultPageSize = 25, columns = list(name = colDef(width = 140),
                                                             personal_text = colDef(width = 150),
                                                             profile_link = colDef(width = 300, html = T,
                                                                                   cell = function(value) str_c("<a href=\"", value, "\">", value, "</a>")),
                                                             time_registered = colDef(width = 150),
                                                             last_seen = colDef(name = "last_seen*", width = 150),
                                                             personal_text = colDef(width = 150),
                                                             location = colDef(width = 150),
                                                             time_spent_online = colDef(width = 150)),
              defaultColDef = colDef(na = "-", headerStyle = list(backgroundColor = "steelblue", color = "white"),
                                     header = function(value) gsub("_", " ", value) %>% str_to_title()))
  })
}

shinyApp(ui, server)

