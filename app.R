###--- Shiny app for the Nairaland data ---###

library(shiny)
library(shinydashboard)
#library(stringr)
#library(dplyr)
#library(ggplot2)
library(shinyWidgets)
library(reactable)

ui <- dashboardPage(
  dashboardHeader(title = "NAIRALAND DATA"),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      valueBoxOutput("vBox1", width = 6),
      valueBoxOutput("vBox2", width = 6)
    ),
    fluidRow(
      box(width = 12, title = "Nairaland users data, as extracted from their respective profile pages [ you can verify through the profile links ;) ]",
          status = "primary", solidHeader = T, reactableOutput("table"))
    ),
    div("- The current table was generated from all threads on just the first page of 'https://www.nairaland.com/career'", style = "color:red"),
    div("- There would be more features added to the app, soon [ e.g. charts ].", style = "color:red"),
    div("- For all columns with wrapped text, kindly resize the column [ just drag its borders ]", style = "color:red"),
    div("- The data is still in its raw form", style = "color:red")
  )
)

server <- function(input, output, session) {
  nairaland_users_data <- read.csv("out-data/first-go-csv.csv", na.strings = "")
  box1 <- nrow(nairaland_users_data)
  box2 <- ncol(nairaland_users_data)
  
  output$vBox1 <- renderValueBox({
    valueBox(box1, "Number Of Rows", icon = icon("th-list", lib = "glyphicon"))
  })
  output$vBox2 <- renderValueBox({
    valueBox(box2, "Number Of Columns", icon = icon("th-list", lib = "glyphicon"))
  })
  
  output$table <- renderReactable({
    reactable(nairaland_users_data, resizable = T, searchable = T, bordered = T, rownames = F,
              filterable = T, showPageSizeOptions = T, highlight = T, outlined = T, striped = T,
              wrap = F, defaultPageSize = 15, columns = list(personal_text = colDef(width = 150),
                                                             profile_link = colDef(width = 150),
                                                             time_registered = colDef(width = 150),
                                                             last_seen = colDef(width = 150),
                                                             personal_text = colDef(width = 150),
                                                             time_spent_online = colDef(width = 150)),
              defaultColDef = colDef(na = "-"))
  })
}

shinyApp(ui, server)
