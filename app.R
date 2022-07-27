###--- Shiny app for the Nairaland data ---###

library(shiny)
library(shinydashboard)
library(stringr)
#library(dplyr)
#library(ggplot2)
library(shinyWidgets)
library(reactable)

#data_list <- list.files("out-data")
setwd("out-data")
data_list <- list.files()
datasets <- sapply(data_list, read.csv, na.strings = "")
nrow_list <- sapply(datasets, nrow); ncol_list <- sapply(datasets, ncol)
data_list_dim <- str_c(data_list, " [", nrow_list, "x", ncol_list, "]")
full_data_list_dim <- c("default.csv [1874x11]", "first-go-csv8.csv [118x11]", "first-go-csv9.csv [659x11]")
partial_data_list_dim <- setdiff(data_list_dim, full_data_list_dim)

ui <- dashboardPage(
  dashboardHeader(title = "NAIRALAND DATA"),
  dashboardSidebar(
    pickerInput("select_data", "Select dataset:", list(`Full Data` = full_data_list_dim,
                                                       `Partial Data` = partial_data_list_dim), selected = "default.csv [1874x11]"),
    downloadButton("download", "Download the dataset")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("vBox1", width = 6),
      valueBoxOutput("vBox2", width = 6)
    ),
    h4("Nairaland users data, as extracted from their respective profile pages [you can verify through the profile links]"),
    fluidRow(
      box(width = 12, status = "primary", solidHeader = F, reactableOutput("table"))
    ),
    div("- The current table was generated from all threads on just the first page of 'https://www.nairaland.com/career'", style = "color:red"),
    div("- There would be more features added to the app, soon [e.g. charts].", style = "color:red"),
    div("- For all columns with wrapped text, kindly resize the column [drag its borders]", style = "color:red"),
    div("- The data is still in its raw form", style = "color:red")
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
    reactable(dataset(), resizable = T, searchable = T, bordered = T, rownames = F,
              filterable = T, showPageSizeOptions = T, highlight = T, outlined = T, striped = T,
              wrap = F, defaultPageSize = 25, columns = list(name = colDef(width = 140),
                                                             personal_text = colDef(width = 150),
                                                             profile_link = colDef(width = 300, html = T,
                                                                                   cell = function(value) str_c("<a href=\"", value, "\">", value, "</a>")),
                                                             time_registered = colDef(width = 150),
                                                             last_seen = colDef(width = 150),
                                                             personal_text = colDef(width = 150),
                                                             location = colDef(width = 150),
                                                             time_spent_online = colDef(width = 150)),
              defaultColDef = colDef(na = "-", headerStyle = list(background = "steelblue", color = "white"),
                                     header = function(value) gsub("_", " ", value) %>% str_to_title()))
  })
}

shinyApp(ui, server)

