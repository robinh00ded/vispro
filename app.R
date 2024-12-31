library(shiny)
library(ggplot2)
library(dplyr)
insurance <- read.csv("insurance.csv")


#1 

# UI
ui <- fluidPage(
  titlePanel("Insurance Charges by Group"),
  sidebarLayout(
    sidebarPanel(
      selectInput("group_by", "Group by:",
                  choices = c("sex", "smoker", "children")),
    ),
    mainPanel(
      plotOutput("boxplot"),
      verbatimTextOutput("summary")
    )
  )
)

# server
server <- function(input, output) {
  
  output$boxplot <- renderPlot({
    # Convert children to factor if it's not already. Important for boxplots
    if (input$group_by == "children") {
      insurance$children <- as.factor(insurance$children)
    }
    
    p <- ggplot(insurance, aes_string(x = input$group_by, y = "charges", fill = input$group_by)) +
      geom_boxplot() +
      labs(x = input$group_by, y = "Charges", fill = input$group_by) +
      theme_bw()
    
    
    print(p)
  })
  
  output$summary <- renderPrint({
    summary_data <- insurance %>% group_by(!!sym(input$group_by)) %>% summarise(
      mean_charges = mean(charges),
      median_charges = median(charges),
      sd_charges = sd(charges),
      n = n()
    )
    print(summary_data)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
