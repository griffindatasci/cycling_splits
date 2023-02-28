library(shiny)
library(data.table)

ui <- fluidPage(
    includeHTML("www/page_header.html"),
    includeHTML("www/page_top.html"),
    includeCSS("www/style.css"),
    
    # Get distance, target speed and split interval
    tags$input(name="dst", placeholder="Distance", type="number", value=NA, min=1),
    tags$input(name="spd", placeholder="Speed",    type="number", value=NA, min=1),
    tags$input(name="int", placeholder="Interval",    type="number", value=NA, min=1),

    tableOutput("split_table")
)

server <- function(input, output) {
  output$split_table <- renderTable({
    if(all(!sapply(input, is.na))) {
      split_table <- data.table(Distance=unique(c(seq(0,input$dst,input$int), input$dst)))
      split_table[, Mins:=Distance*(60/input$spd)]
      if(split_table[, max(Mins)>=60]){
        split_table[, Time:=sprintf("%02.0f:%02.0f:%04.1f", Mins%/%60, Mins%%60%/%1, Mins%%60%%1*60)]
      } else {
        split_table[, Time:=sprintf("%02.0f:%04.1f", Mins%%60%/%1, Mins%%60%%1*60)]
      }
      split_table[, .("Distance"=ifelse(.I==.N, "Finish", Distance), Time)]
    }
  })
}

shinyApp(ui = ui, server = server)
