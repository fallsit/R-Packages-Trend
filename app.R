

library(shiny)
library(cranlogs)
library(ggplot2)
library(tidyverse)
library(ggiraph)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  #---
   # Application title
   titlePanel("R packages trend"),
   fluidRow(
     
     helpText("If you wnat to know which package is popular in R,",
              "you can use this app to find out counts and trends.",
              "It also provides top 100 most downloaded packages of a month.")
   ),
   
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        div(textInput("R_Packages","Search for R Packages ", 
                  value = paste(cran_top_downloads(count = 3,"last-month")$package,collapse=",")),
        
        dateRangeInput("Date_range", "Date Range", start=Sys.Date() - 30, end = Sys.Date() - 2, 
                       separator = " - ", startview = "year"), 
        tags$b("Top 100 packages in month"), style = "font-size:120%"),
        tableOutput("view")
        ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("PackagesPlot"),
        plotlyOutput("boxPlot"),
        p("Data from",a(href="https://github.com/metacran/cranlogs","cranlogs")),
        p("Code on",a(href="https://github.com/metacran/cranlogs","github"))
        )
      
      
     
   )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   rank<- reactive(cran_top_downloads(count = 100, when="last-month")[,c(1,2,3)])
   packageNames<-reactive(strsplit(gsub(" ","",input$R_Packages),",")[[1]])
   df<-reactive(cran_downloads(packageNames(), from = input$Date_range[1], to = input$Date_range[2]))
  
   output$PackagesPlot <- renderPlotly({
     g <- ggplot(df(), aes(x = date, y = count)) + 
       geom_line(aes(color = package), size = 0.3) +
       scale_y_continuous(labels = scales::comma) +
       ggtitle("R packages trend") +
       labs( color = "Packages" )  
     
    p<-ggplotly(g)
    style(p,hoverinfo="y")
       #stat_smooth(method = "loess") +
      # theme_minimal() 
   })
   output$boxPlot <- renderPlotly({
     ggplot(df(), aes(x = package ,y = count, fill = package)) +
       geom_boxplot() + coord_flip() + ggtitle("BoxPlot of R packages") +
       labs( color = "packages" )
     
   })
   
  #---
   output$view <- renderTable({ rank()})
}

# Run the application 
shinyApp(ui = ui, server = server)

