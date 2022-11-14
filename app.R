library(shiny)
library(data.table)

ui <- fluidPage(
  numericInput("dst", "Distance", value=29.0, min=0, max=999),
  numericInput("spd", "Target Speed", value=44, min=0, max=999),
  numericInput("int", "Split", value=1, min=1, max=99),
  tableOutput("split_table")
)

server <- function(input, output) {
  output$split_table <- renderTable({
    split_table <- data.table(Distance=seq(0,input$dst,input$int))
    split_table[, Mins:=Distance*(60/input$spd)]
    if(split_table[, max(Mins)>=60]){
      split_table[, Time:=sprintf("%02.0f:%02.0f:%04.1f", Mins%/%60, Mins%%60%/%1, Mins%%60%%1*60)]
    } else {
      split_table[, Time:=sprintf("%02.0f:%04.1f", Mins%%60%/%1, Mins%%60%%1*60)]
    }
    split_table
  })
}

shinyApp(ui = ui, server = server)
